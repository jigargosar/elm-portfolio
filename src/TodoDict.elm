module TodoDict exposing
    ( TodoDict
    , completedForProjectList
    , completedList
    , markCompleted
    , markPending
    , moveAllToProjectId
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    )

import Basics.Extra
import Dict exposing (Dict)
import ProjectId exposing (ProjectId)
import Set exposing (Set)
import Todo exposing (Todo, TodoId)


type alias TodoDict =
    Dict TodoId Todo


filter : Todo.Filter -> TodoDict -> List Todo
filter f model =
    model
        |> Dict.values
        |> Todo.filter f


filterSort f s model =
    model |> Dict.values |> Todo.filterSort f s


pendingList : TodoDict -> List Todo
pendingList =
    filterSort Todo.Pending [ Todo.ByIdx ]


pendingByProjectId : TodoDict -> Dict ProjectId (List Todo)
pendingByProjectId todoDict =
    pendingList todoDict
        |> listGroupBy .projectId


listGroupBy : (a -> comparable) -> List a -> Dict comparable (List a)
listGroupBy toComparable =
    let
        dictUpdater : a -> Maybe (List a) -> Maybe (List a)
        dictUpdater a =
            Maybe.map (\listOfA -> listOfA ++ [ a ])
                >> Maybe.withDefault [ a ]
                >> Just
    in
    List.foldl
        (\a dict ->
            dict
                |> Dict.update (toComparable a) (dictUpdater a)
        )
        Dict.empty


completedList : TodoDict -> List Todo
completedList =
    filterSort Todo.Completed [ Todo.ByRecentlyModified ]


pendingWithProjectId pid model =
    pendingList model
        |> Todo.filter (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))


completedForProjectList pid model =
    completedList model
        |> Todo.filter (Todo.AndFilter Todo.Completed (Todo.BelongsToProject pid))


type alias Millis =
    Int


pendingWithId : TodoId -> TodoDict -> Maybe Todo
pendingWithId todoId =
    Dict.get todoId
        >> Maybe.andThen (Todo.filterSingle Todo.Pending)


markCompleted : TodoId -> Millis -> TodoDict -> Maybe ( List Todo.Msg, TodoDict )
markCompleted todoId now model =
    let
        msg =
            Todo.SetCompleted True
    in
    model
        |> Dict.get todoId
        |> Maybe.andThen (Todo.modify msg)
        |> Maybe.map
            (Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updatePendingSortIdx now
                >> Tuple.mapFirst (\msgList -> msgList ++ [ msg ])
            )


moveAllToProjectId : ProjectId -> Set TodoId -> Millis -> TodoDict -> Maybe TodoDict
moveAllToProjectId projectId todoIdSet now model =
    let
        updatedTodos =
            todoIdSet
                |> Set.toList
                |> List.filterMap
                    (\todoId ->
                        Dict.get todoId model
                            |> Maybe.andThen (Todo.setProjectId projectId)
                            |> Maybe.map
                                (Todo.setSortIdx Basics.Extra.maxSafeInteger
                                    >> Todo.setModifiedAt now
                                    >> Tuple.pair todoId
                                )
                    )
                |> Dict.fromList
    in
    if updatedTodos |> Dict.isEmpty then
        Nothing

    else
        Just (Dict.union updatedTodos model)


markPending todoId now model =
    model
        |> Dict.get todoId
        |> Maybe.andThen (Todo.setCompleted False)
        |> Maybe.map
            (Todo.setSortIdx Basics.Extra.maxSafeInteger
                >> Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updateSortIdx now
            )


updateSortIdx : Millis -> TodoDict -> TodoDict
updateSortIdx now todos =
    pendingList todos
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, t ) ->
                if t.sortIdx == i then
                    Nothing

                else
                    Just (Todo.setSortIdx i t |> Todo.setModifiedAt now)
            )
        |> List.foldl (\t -> Dict.insert t.id t) todos


updatePendingSortIdx : Millis -> TodoDict -> ( List Todo.Msg, TodoDict )
updatePendingSortIdx now todos =
    pendingByProjectId todos
        |> Dict.values
        |> List.concatMap
            (List.indexedMap Tuple.pair
                >> List.filterMap
                    (\( i, t ) ->
                        let
                            msg =
                                Todo.SetSortIdx i
                        in
                        Todo.modify msg t
                            |> Maybe.map (Todo.setModifiedAt now >> Tuple.pair msg)
                    )
            )
        |> List.foldl
            (\( msg, todo ) ( msgList, accTodoDict ) ->
                ( msgList ++ [ msg ], insert todo accTodoDict )
            )
            ( [], todos )


insert : Todo -> TodoDict -> TodoDict
insert todo =
    Dict.insert todo.id todo
