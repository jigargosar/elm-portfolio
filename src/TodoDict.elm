module TodoDict exposing
    ( Msg(..)
    , Return
    , TodoDict
    , andThen
    , completedForProjectList
    , completedList
    , decoder
    , fromList
    , initial
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    , update
    , updateBulk
    )

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as JD exposing (Decoder)
import List.Extra
import Now exposing (Millis)
import ProjectId exposing (ProjectId)
import Set exposing (Set)
import Sync exposing (SyncMsg)
import Todo exposing (Todo, TodoId, TodoList)



-- MODEL


type alias TodoDict =
    Dict TodoId Todo


fromList : TodoList -> TodoDict
fromList =
    Dict.Extra.fromListBy .id


initial : TodoDict
initial =
    Dict.empty


decoder : Decoder TodoDict
decoder =
    JD.oneOf
        [ Todo.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        , JD.dict Todo.decoder
        ]



-- QUERY


filterSort f s model =
    model |> Dict.values |> Todo.filterSort f s


pendingList : TodoDict -> List Todo
pendingList =
    filterSort Todo.Pending [ Todo.ByIdx ]


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


insert : Todo -> TodoDict -> TodoDict
insert todo =
    Dict.insert todo.id todo


insertWithMsg : Todo -> Todo.Msg -> TodoDict -> Return
insertWithMsg todo todoMsg model =
    ( insert todo model, [ Sync.TodoSync todo.id todoMsg ] )


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
