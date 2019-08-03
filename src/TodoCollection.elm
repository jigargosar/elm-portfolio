module TodoCollection exposing
    ( Msg(..)
    , Return
    , TodoCollection
    , Update
    , completedForProjectList
    , completedList
    , decoder
    , encoder
    , initial
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    , update
    , updateFromServerResponse
    )

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra
import Now exposing (Millis)
import ProjectId exposing (ProjectId)
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
        [ JD.dict Todo.decoder
        , Todo.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        ]


encoder : TodoCollection -> Value
encoder model =
    JE.dict identity Todo.encoder model



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
    TodoCollection


updateFromServerResponse : List Todo -> TodoCollection -> Return
updateFromServerResponse todoList model =
    todoList |> List.foldl insert model


update : Update -> Millis -> TodoCollection -> Return
update ( idList, msgList ) now model =
    let
        message =
            Batch msgList
    in
    idList
        |> List.foldl (\todoId -> updateHelp now todoId message) model


updateHelp : Millis -> TodoId -> Msg -> TodoCollection -> Return
updateHelp now todoId message model =
    case message of
        MarkComplete ->
            let
                msg =
                    Todo.SetCompleted True
            in
            modifyTodoWithId now todoId msg model
                |> Maybe.withDefault model

        MarkPending ->
            let
                msg =
                    Todo.SetCompleted False
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    ((\todo -> modifyTodo msg now todo model)
                        >> Maybe.map (moveToBottom now todoId)
                    )
                |> Maybe.withDefault model

        MoveToProject pid ->
            let
                msg =
                    Todo.SetProjectId pid
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    ((\todo -> modifyTodo msg now todo model)
                        >> Maybe.map (moveToBottom now todoId)
                    )
                |> Maybe.withDefault model

        SetTitle title ->
            let
                msg =
                    Todo.SetTitle title
            in
            model
                |> Dict.get todoId
                |> Maybe.andThen
                    (\todo -> modifyTodo msg now todo model)
                |> Maybe.withDefault model

        Batch msgList ->
            msgList
                |> List.foldl (\msg -> updateHelp now todoId msg)
                    model


insert : Todo -> TodoCollection -> TodoCollection
insert todo =
    Dict.insert todo.id todo


insertWithMsg : Todo -> TodoCollection -> Return
insertWithMsg todo model =
    insert todo model


modifyTodoWithId : Millis -> TodoId -> Todo.Msg -> TodoCollection -> Maybe Return
modifyTodoWithId now todoId todoMsg model =
    model
        |> Dict.get todoId
        |> Maybe.andThen
            (\todo -> modifyTodo todoMsg now todo model)


modifyTodo : Todo.Msg -> Millis -> Todo -> TodoCollection -> Maybe Return
modifyTodo todoMsg now todo model =
    Todo.modify todoMsg now todo
        |> Maybe.map (\r -> insertWithMsg r model)


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
                modifyTodo msg now todo model
            )
        |> Maybe.withDefault model
