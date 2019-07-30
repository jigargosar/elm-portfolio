module TodoDict exposing
    ( Msg(..)
    , SyncMsg
    , TodoDict
    , completedForProjectList
    , completedList
    , moveAllToProjectId
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    , update
    , updateBulk
    , andThen
    )

import Basics.Extra
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import ProjectId exposing (ProjectId)
import Set exposing (Set)
import Todo exposing (Todo, TodoId)


type alias TodoDict =
    Dict TodoId Todo


type alias Millis =
    Int


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


pendingWithId : TodoId -> TodoDict -> Maybe Todo
pendingWithId todoId =
    Dict.get todoId
        >> Maybe.andThen (Todo.filterSingle Todo.Pending)



-- UPDATE


type Msg
    = MarkComplete
    | MarkPending
    | SetTitle String
    | MoveToProject ProjectId


type alias Return =
    ( TodoDict, List SyncMsg )


updateBulk : Millis -> Set TodoId -> Msg -> TodoDict -> Return
updateBulk now todoIdSet message model =
    todoIdSet
        |> Set.foldl (\todoId -> andThen (update now todoId message)) ( model, [] )


update : Millis -> TodoId -> Msg -> TodoDict -> Return
update now todoId message model =
    case message of
        MarkComplete ->
            let
                msg =
                    Todo.SetCompleted True
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    (Todo.modifyWithNow now msg
                        >> Maybe.map (\t -> insertWithMsg t msg model)
                    )
                |> Maybe.withDefault ( model, [] )

        MarkPending ->
            let
                msg =
                    Todo.SetCompleted False
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    (Todo.modifyWithNow now msg
                        >> Maybe.map
                            (\t ->
                                insertWithMsg t msg model
                                    |> andThen (moveToBottom now t.id)
                            )
                    )
                |> Maybe.withDefault ( model, [] )

        MoveToProject pid ->
            let
                msg =
                    Todo.SetProjectId pid
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    (Todo.modifyWithNow now msg
                        >> Maybe.map
                            (\t ->
                                insertWithMsg t msg model
                                    |> andThen (moveToBottom now t.id)
                            )
                    )
                |> Maybe.withDefault ( model, [] )

        SetTitle title ->
            let
                msg =
                    Todo.SetTitle title
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    (Todo.modifyWithNow now msg
                        >> Maybe.map (\t -> insertWithMsg t msg model)
                    )
                |> Maybe.withDefault ( model, [] )


insertWithMsg : Todo -> Todo.Msg -> TodoDict -> Return
insertWithMsg todo todoMsg model =
    ( insert todo model, [ TodoSync todo.id todoMsg ] )


andThen : (TodoDict -> Return) -> Return -> Return
andThen fn ( model, msgStack ) =
    let
        ( newModel, newMsgStack ) =
            fn model
    in
    ( newModel, newMsgStack ++ msgStack )


moveToBottom : Millis -> TodoId -> TodoDict -> Return
moveToBottom now todoId model =
    model
        |> Dict.get todoId
        |> Maybe.andThen
            (\todo ->
                let
                    bottomIdx =
                        todo.projectId
                            |> (\pid -> pendingWithProjectId pid model)
                            |> List.filter (.id >> (/=) todo.id)
                            |> (List.Extra.last
                                    >> Maybe.map (.sortIdx >> (+) 1)
                                    >> Maybe.withDefault 0
                               )

                    msg =
                        Todo.SetSortIdx bottomIdx
                in
                Todo.modifyWithNow now msg todo
                    |> Maybe.map (\t -> insertWithMsg t msg model)
            )
        |> Maybe.withDefault ( model, [] )


type SyncMsg
    = TodoSync TodoId Todo.Msg


markCompleted : TodoId -> Millis -> TodoDict -> Maybe Return
markCompleted todoId now model =
    let
        msg =
            Todo.SetCompleted True

        syncMsg =
            TodoSync todoId msg
    in
    model
        |> Dict.get todoId
        |> Maybe.andThen (Todo.modify msg)
        |> Maybe.map
            (Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updatePendingSortIdx now
                >> Tuple.mapSecond (\msgList -> msgList ++ [ syncMsg ])
            )


markPending : TodoId -> Millis -> TodoDict -> Maybe Return
markPending todoId now model =
    let
        msg =
            Todo.SetCompleted False

        syncMsg =
            TodoSync todoId msg
    in
    model
        |> Dict.get todoId
        |> Maybe.andThen (Todo.modify msg)
        |> Maybe.map
            (Todo.setSortIdx Basics.Extra.maxSafeInteger
                >> Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updatePendingSortIdx now
                >> Tuple.mapSecond (\msgList -> msgList ++ [ syncMsg ])
            )


moveAllToProjectId :
    ProjectId
    -> Set TodoId
    -> Millis
    -> TodoDict
    -> Maybe Return
moveAllToProjectId projectId todoIdSet now model =
    let
        msg =
            Todo.SetProjectId projectId

        ( updatedTodoDict, syncMsgList ) =
            todoIdSet
                |> Set.toList
                |> List.filterMap
                    (\todoId ->
                        let
                            syncMsg =
                                TodoSync todoId msg
                        in
                        Dict.get todoId model
                            |> Maybe.andThen (Todo.modify msg)
                            |> Maybe.map
                                (Todo.setSortIdx Basics.Extra.maxSafeInteger
                                    >> Todo.setModifiedAt now
                                    >> Tuple.pair todoId
                                    >> (\something -> ( something, syncMsg ))
                                )
                    )
                |> List.foldl
                    (\( t, m ) ( tList, mList ) ->
                        ( tList ++ [ t ], mList ++ [ m ] )
                    )
                    ( [], [] )
                |> Tuple.mapFirst Dict.fromList
    in
    if updatedTodoDict |> Dict.isEmpty then
        Nothing

    else
        Dict.union updatedTodoDict model
            |> updatePendingSortIdx now
            |> Tuple.mapSecond (\ml -> syncMsgList ++ ml)
            |> Just


updatePendingSortIdx : Millis -> TodoDict -> Return
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

                            syncMsg =
                                TodoSync t.id msg
                        in
                        Todo.modify msg t
                            |> Maybe.map (Todo.setModifiedAt now >> (\fst -> ( fst, syncMsg )))
                    )
            )
        |> List.foldl
            (\( todo, msg ) ( accTodoDict, msgList ) ->
                ( insert todo accTodoDict, msgList ++ [ msg ] )
            )
            ( todos, [] )


insert : Todo -> TodoDict -> TodoDict
insert todo =
    Dict.insert todo.id todo
