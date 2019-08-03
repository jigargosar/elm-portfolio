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
        |> List.foldl (updateWithMsgList now msgList >> andThen) (pure model)


updateWithMsgList : Millis -> List Msg -> TodoId -> TodoCollection -> Return
updateWithMsgList now msgList todoId model =
    msgList
        |> List.foldl (updateWithMsg now todoId >> andThen) (pure model)


updateWithMsg : Millis -> TodoId -> Msg -> TodoCollection -> Return
updateWithMsg now todoId message model =
    Dict.get todoId model
        |> Maybe.andThen (updateWithMsgHelp now message model)
        |> Maybe.withDefault (pure model)


updateWithMsgHelp : Millis -> Msg -> TodoCollection -> Todo -> Maybe Return
updateWithMsgHelp now message model todo =
    case message of
        MarkComplete ->
            modifyTodo now (Todo.SetCompleted True) todo model

        MarkPending ->
            modifyTodo now (Todo.SetCompleted False) todo model
                |> Maybe.map (andThen (moveToBottom now todo.id))

        MoveToProject pid ->
            modifyTodo now (Todo.SetProjectId pid) todo model
                |> Maybe.map (andThen (moveToBottom now todo.id))

        SetTitle title ->
            modifyTodo now (Todo.SetTitle title) todo model


pure : TodoCollection -> Return
pure model =
    ( model, [] )


andThen : (TodoCollection -> Return) -> Return -> Return
andThen fn ( model, patchList ) =
    let
        ( newModel, newPatchList ) =
            fn model
    in
    ( newModel, patchList ++ newPatchList )


modifyTodo : Millis -> Todo.Msg -> Todo -> TodoCollection -> Maybe Return
modifyTodo now todoMsg todo model =
    Todo.modify todoMsg now todo
        |> Maybe.map (insertWithMsg todoMsg model)


insertWithMsg : Todo.Msg -> TodoCollection -> Todo -> Return
insertWithMsg todoMsg model todo =
    ( insert todo model, [ Patch todo.id todoMsg ] )


insert : Todo -> TodoCollection -> TodoCollection
insert todo =
    Dict.insert todo.id todo


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
                modifyTodo now msg todo model
            )
        |> Maybe.withDefault (pure model)
