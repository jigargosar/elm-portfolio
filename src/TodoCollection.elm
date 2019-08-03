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


type alias Patch =
    { id : TodoId
    , msg : Todo.Msg
    }


type alias Return =
    ( TodoCollection, List Patch )


updateFromServerResponse : List Todo -> TodoCollection -> TodoCollection
updateFromServerResponse todoList model =
    todoList |> List.foldl insert model


update : Update -> Millis -> TodoCollection -> Return
update ( idList, msgList ) now model =
    idList
        |> List.foldl (updateWithMsgList now msgList) ( model, [] )


updateWithMsgList : Millis -> List Msg -> TodoId -> Return -> Return
updateWithMsgList now msgList todoId return =
    msgList
        |> List.foldl
            (\msg acc ->
                updateWithMsg now todoId msg acc
                    |> Maybe.withDefault acc
            )
            return


updateWithMsg : Millis -> TodoId -> Msg -> Return -> Maybe Return
updateWithMsg now todoId message =
    let
        moveToBottom =
            modifyTodo now todoId getMoveToBottomUpdateTodoMsg
    in
    case message of
        MarkComplete ->
            modifyTodo now todoId (\_ -> Todo.SetCompleted True)

        MarkPending ->
            modifyTodo now todoId (\_ -> Todo.SetCompleted False)
                >> Maybe.andThen moveToBottom

        MoveToProject pid ->
            modifyTodo now todoId (\_ -> Todo.SetProjectId pid)
                >> Maybe.andThen moveToBottom

        SetTitle title ->
            modifyTodo now todoId (\_ -> Todo.SetTitle title)


getMoveToBottomUpdateTodoMsg ( todo, model ) =
    let
        bottomIdx =
            todo.projectId
                |> (\pid -> pendingWithProjectId pid model)
                |> List.filter (.id >> (/=) todo.id)
                |> (List.Extra.last
                        >> Maybe.map (.sortIdx >> (+) 1)
                        >> Maybe.withDefault 0
                   )
    in
    Todo.SetSortIdx bottomIdx


modifyTodo :
    Millis
    -> TodoId
    -> (( Todo, TodoCollection ) -> Todo.Msg)
    -> Return
    -> Maybe Return
modifyTodo now todoId computeTodoMsg ( model, patches ) =
    Dict.get todoId model
        |> Maybe.andThen
            (\todo ->
                let
                    todoMsg =
                        computeTodoMsg ( todo, model )
                in
                Todo.modify todoMsg now todo
                    |> Maybe.map
                        (\newTodo ->
                            ( insert newTodo model
                            , patches ++ [ Patch todo.id todoMsg ]
                            )
                        )
            )


insert : Todo -> TodoCollection -> TodoCollection
insert todo =
    Dict.insert todo.id todo
