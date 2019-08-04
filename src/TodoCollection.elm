module TodoCollection exposing
    ( Msg(..)
    , Return
    , TodoCollection
    , Update
    , completedForProjectList
    , completedList
    , decoder
    , encoder
    , getEncodedPatches
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
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Now exposing (Millis)
import ProjectId exposing (ProjectId)
import Todo exposing (Todo, TodoId, TodoList)
import TodoId



-- MODEL


type alias TodoDict =
    Dict TodoId Todo


type alias PatchList =
    List Patch


type alias Patch =
    { todoId : TodoId
    , key : String
    , value : Value
    , modifiedAt : Int
    }


patchDecoder : Decoder Patch
patchDecoder =
    JD.succeed Patch
        |> JDP.required "todoId" TodoId.decoder
        |> JDP.required "key" JD.string
        |> JDP.required "value" JD.value
        |> JDP.required "modifiedAt" JD.int


patchEncoder : Patch -> Value
patchEncoder { todoId, key, value, modifiedAt } =
    JE.object
        [ ( "todoId", TodoId.encoder todoId )
        , ( "key", JE.string key )
        , ( "value", value )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


createPatch : TodoId -> Todo.Msg -> Millis -> Patch
createPatch todoId todoMsg now =
    let
        ( key, value ) =
            Todo.msgKVEncoder todoMsg
    in
    Patch todoId key value now


patchListDecoder : Decoder PatchList
patchListDecoder =
    JD.list patchDecoder


patchListEncoder : PatchList -> Value
patchListEncoder =
    JE.list patchEncoder


type alias TodoCollection =
    { dict : TodoDict, patches : PatchList }


initial : TodoCollection
initial =
    { dict = Dict.empty, patches = [] }


fromDict : TodoDict -> TodoCollection
fromDict dict =
    { dict = dict, patches = [] }


dictDecoder : Decoder TodoDict
dictDecoder =
    JD.oneOf
        [ JD.dict Todo.decoder
        , Todo.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        ]


modelDecoder : Decoder TodoCollection
modelDecoder =
    JD.succeed TodoCollection
        |> JDP.required "dict" (JD.dict Todo.decoder)
        |> JDP.required "patches" patchListDecoder


decoder : Decoder TodoCollection
decoder =
    JD.oneOf
        [ modelDecoder
        , dictDecoder |> JD.map fromDict
        ]


encoder : TodoCollection -> Value
encoder { dict, patches } =
    JE.object
        [ ( "dict", JE.dict identity Todo.encoder dict )
        , ( "patches", patchListEncoder patches )
        ]


getEncodedPatches : TodoCollection -> Value
getEncodedPatches model =
    model.patches |> patchListEncoder



-- QUERY


filterSort : Todo.Filter -> List Todo.CompareBy -> TodoCollection -> TodoList
filterSort f s model =
    model |> .dict |> Dict.values |> Todo.filterSort f s


pendingList : TodoCollection -> TodoList
pendingList =
    filterSort Todo.Pending [ Todo.ByIdx ]


completedList : TodoCollection -> TodoList
completedList =
    filterSort Todo.Completed [ Todo.ByRecentlyModified ]


pendingWithProjectId pid model =
    pendingList model
        |> Todo.filter (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))


completedForProjectList pid model =
    completedList model
        |> Todo.filter (Todo.AndFilter Todo.Completed (Todo.BelongsToProject pid))


get : TodoId -> TodoCollection -> Maybe Todo
get todoId =
    .dict >> Dict.get todoId


pendingWithId : TodoId -> TodoCollection -> Maybe Todo
pendingWithId todoId =
    get todoId
        >> Maybe.andThen (Todo.filterSingle Todo.Pending)



-- UPDATE


type alias Update =
    ( List TodoId, List Msg )


type Msg
    = MarkComplete
    | MarkPending
    | SetTitle String
    | MoveToProject ProjectId


type alias Return =
    TodoCollection


updateFromServerResponse : TodoList -> TodoCollection -> TodoCollection
updateFromServerResponse todoList model =
    todoList
        |> List.foldl insert model
        |> clearPatches


clearPatches : TodoCollection -> TodoCollection
clearPatches model =
    { model | patches = [] }


update : Update -> Millis -> TodoCollection -> Return
update ( idList, msgList ) now model =
    idList
        |> List.foldl (updateWithMsgList now msgList) model


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
            modifyTodo now todoId computeMoveToBottomTodoMsg
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


computeMoveToBottomTodoMsg ( todo, model ) =
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
modifyTodo now todoId computeTodoMsg model =
    get todoId model
        |> Maybe.andThen
            (\todo ->
                let
                    todoMsg =
                        computeTodoMsg ( todo, model )
                in
                Todo.modify todoMsg now todo
                    |> Maybe.map
                        (\newTodo ->
                            insertWithPatch newTodo todoMsg now model
                        )
            )


insertWithPatch : Todo -> Todo.Msg -> Millis -> TodoCollection -> TodoCollection
insertWithPatch todo todoMsg now model =
    { model
        | dict = Dict.insert todo.id todo model.dict
        , patches = model.patches ++ [ createPatch todo.id todoMsg now ]
    }


insert : Todo -> TodoCollection -> TodoCollection
insert todo model =
    { model
        | dict = Dict.insert todo.id todo model.dict
    }
