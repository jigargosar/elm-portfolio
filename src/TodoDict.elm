module TodoDict exposing
    ( SyncMsg
    , TodoDict
    , completedForProjectList
    , completedList
    , markCompleted
    , markPending
    , moveAllToProjectId
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    , setTitleAndMoveToProject
    )

import Basics.Extra
import Dict exposing (Dict)
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
    | SetTitleAndMoveToProject {title:String, projectId:String}
    | MoveToProject ProjectId


type alias Return = (List SyncMsg, TodoDict)

update :  Millis -> TodoId -> Msg -> TodoDict -> Return
update now todoId msg model = 
    let
        nc = 
            ([], model)
        
        unwrapNothing =
            Maybe.withDefault nc
    in
    
    case msg of
        MarkComplete ->
            markCompleted todoId now model 
                |> unwrapNothing
        MarkPending ->
            markPending todoId now model 
                |> unwrapNothing
    
        _ ->
            nc
            

type SyncMsg
    = TodoSync TodoId Todo.Msg


markCompleted : TodoId -> Millis -> TodoDict -> Maybe ( List SyncMsg, TodoDict )
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
                >> Tuple.mapFirst (\msgList -> msgList ++ [ syncMsg ])
            )


markPending : TodoId -> Millis -> TodoDict -> Maybe ( List SyncMsg, TodoDict )
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
                >> Tuple.mapFirst (\msgList -> msgList ++ [ syncMsg ])
            )


setTitleAndMoveToProject :
    TodoId
    -> { title : String, projectId : ProjectId }
    -> Millis
    -> TodoDict
    -> Maybe ( List SyncMsg, TodoDict )
setTitleAndMoveToProject todoId { title, projectId } now model =
    let
        _ =
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    (Todo.modifyMultiple
                        [ Todo.SetProjectId projectId, Todo.SetTitle title ]
                    )
    in
    Nothing


moveAllToProjectId :
    ProjectId
    -> Set TodoId
    -> Millis
    -> TodoDict
    -> Maybe ( List SyncMsg, TodoDict )
moveAllToProjectId projectId todoIdSet now model =
    let
        msg =
            Todo.SetProjectId projectId

        ( syncMsgList, updatedTodoDict ) =
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
                                    >> Tuple.pair syncMsg
                                )
                    )
                |> List.foldl
                    (\( m, t ) ( mList, tList ) ->
                        ( mList ++ [ m ], tList ++ [ t ] )
                    )
                    ( [], [] )
                |> Tuple.mapSecond Dict.fromList
    in
    if updatedTodoDict |> Dict.isEmpty then
        Nothing

    else
        Dict.union updatedTodoDict model
            |> updatePendingSortIdx now
            |> Tuple.mapFirst (\ml -> syncMsgList ++ ml)
            |> Just


updatePendingSortIdx : Millis -> TodoDict -> ( List SyncMsg, TodoDict )
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
                            |> Maybe.map (Todo.setModifiedAt now >> Tuple.pair syncMsg)
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
