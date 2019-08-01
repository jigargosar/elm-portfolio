module TodoCollection exposing
    ( Msg(..)
    , Return
    , TodoCollection
    , Update
    , andThen
    , completedForProjectList
    , completedList
    , decoder
    , initial
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    , update
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


type alias TodoCollection =
    Dict TodoId Todo


initial : TodoCollection
initial =
    Dict.empty


decoder : Decoder TodoCollection
decoder =
    JD.oneOf
        [ Todo.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        , JD.dict Todo.decoder
        ]



-- QUERY


filterSort f s model =
    model |> Dict.values |> Todo.filterSort f s


pendingList : TodoCollection -> List Todo
pendingList =
    filterSort Todo.Pending [ Todo.ByIdx ]


completedList : TodoCollection -> List Todo
completedList =
    filterSort Todo.Completed [ Todo.ByRecentlyModified ]


pendingWithProjectId pid model =
    pendingList model
        |> Todo.filter (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))


completedForProjectList pid model =
    completedList model
        |> Todo.filter (Todo.AndFilter Todo.Completed (Todo.BelongsToProject pid))


pendingWithId : TodoId -> TodoCollection -> Maybe Todo
pendingWithId todoId =
    Dict.get todoId
        >> Maybe.andThen (Todo.filterSingle Todo.Pending)



-- UPDATE


type alias Update =
    ( List TodoId, List Msg )


type Msg
    = MarkComplete
    | MarkPending
    | SetTitle String
    | MoveToProject ProjectId
    | Batch (List Msg)


type alias Return =
    ( TodoCollection, List SyncMsg )


update : Update -> Millis -> TodoCollection -> Return
update ( idList, msgList ) now model =
    let
        message =
            Batch msgList
    in
    idList
        |> List.foldl (\todoId -> andThen (updateHelp now todoId message)) ( model, [] )


updateHelp : Millis -> TodoId -> Msg -> TodoCollection -> Return
updateHelp now todoId message model =
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

        Batch msgList ->
            msgList
                |> List.foldl (\msg -> andThen (updateHelp now todoId msg))
                    ( model, [] )


insert : Todo -> TodoCollection -> TodoCollection
insert todo =
    Dict.insert todo.id todo


insertWithMsg : Todo -> Todo.Msg -> TodoCollection -> Return
insertWithMsg todo todoMsg model =
    ( insert todo model, [ Sync.TodoSync todo.id todoMsg ] )


andThen : (TodoCollection -> Return) -> Return -> Return
andThen fn ( model, msgStack ) =
    let
        ( newModel, newMsgStack ) =
            fn model
    in
    ( newModel, newMsgStack ++ msgStack )


moveToBottom : Millis -> TodoId -> TodoCollection -> Return
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
