module Todo exposing
    ( CompareBy(..)
    , Filter(..)
    , Msg(..)
    , Todo
    , TodoId
    , TodoList
    , decoder
    , encoder
    , filter
    , filterSingle
    , filterSort
    , matchesFilter
    , modify
    , modifyMultiple
    , modifyWithNow
    , setModifiedAt
    , setSortIdx
    )

import Basics.Extra
import Compare exposing (Comparator)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)


type alias TodoId =
    String


type alias Millis =
    Int


type alias Todo =
    { id : TodoId
    , title : String
    , sortIdx : Int
    , projectId : ProjectId
    , isDone : Bool
    , createdAt : Int
    , modifiedAt : Int
    }


decoder : Decoder Todo
decoder =
    JD.succeed Todo
        |> JD.required "id" JD.string
        |> JD.required "title" JD.string
        |> JD.required "sortIdx" JD.int
        |> JD.optional "projectId" ProjectId.decoder ProjectId.default
        |> JD.required "isDone" JD.bool
        |> JD.required "createdAt" JD.int
        |> JD.required "modifiedAt" JD.int


encoder : Todo -> Value
encoder { id, title, sortIdx, projectId, isDone, createdAt, modifiedAt } =
    JE.object
        [ ( "id", JE.string id )
        , ( "title", JE.string title )
        , ( "sortIdx", JE.int sortIdx )
        , ( "projectId", ProjectId.encoder projectId )
        , ( "isDone", JE.bool isDone )
        , ( "createdAt", JE.int createdAt )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


type Msg
    = SetCompleted Bool
    | SetProjectId ProjectId
    | SetTitle String
    | SetSortIdx Int


update : Msg -> Todo -> Todo
update msg model =
    case msg of
        SetCompleted bool ->
            { model | isDone = bool }

        SetProjectId projectId ->
            { model | projectId = projectId }

        SetTitle title ->
            { model | title = title }

        SetSortIdx sortIdx ->
            { model | sortIdx = sortIdx }


modify : Msg -> Todo -> Maybe Todo
modify msg model =
    let
        newModel =
            update msg model
    in
    if newModel == model then
        Nothing

    else
        Just newModel


modifyWithNow : Millis -> Msg -> Todo -> Maybe Todo
modifyWithNow now msg todo =
    modify msg todo
        |> Maybe.map (setModifiedAt now)


modifyMultiple : List Msg -> Todo -> Maybe Todo
modifyMultiple msgList model =
    let
        newModel =
            List.foldl (\msg -> update msg) model msgList
    in
    if newModel == model then
        Nothing

    else
        Just newModel


setSortIdx sortIdx model =
    { model | sortIdx = sortIdx }


setModifiedAt now todo =
    { todo | modifiedAt = now }


type Filter
    = Pending
    | Completed
    | BelongsToProject ProjectId
    | NotInProject ProjectId
    | AndFilter Filter Filter


matchesFilter : Filter -> Todo -> Bool
matchesFilter filter_ todo =
    case filter_ of
        Pending ->
            not todo.isDone

        Completed ->
            todo.isDone

        BelongsToProject pid ->
            todo.projectId == pid

        NotInProject pid ->
            todo.projectId /= pid

        AndFilter a b ->
            matchesFilter a todo && matchesFilter b todo


type alias TodoList =
    List Todo


filter : Filter -> TodoList -> TodoList
filter filter_ =
    List.filter (matchesFilter filter_)


filterSingle : Filter -> Todo -> Maybe Todo
filterSingle filter_ =
    List.singleton >> filter filter_ >> List.head


type CompareBy
    = ByIdx
    | ByRecentlyModified


toComparator : CompareBy -> Comparator Todo
toComparator compareBy =
    case compareBy of
        ByIdx ->
            Compare.by .sortIdx

        ByRecentlyModified ->
            Compare.by .modifiedAt |> Compare.reverse


sortWith : List CompareBy -> TodoList -> TodoList
sortWith comps =
    List.sortWith (Compare.concat (List.map toComparator comps))


filterSort : Filter -> List CompareBy -> TodoList -> TodoList
filterSort fil comps =
    filter fil >> sortWith comps


isCompleted =
    .isDone


isPending =
    isCompleted >> not


idEq todoId model =
    todoId == model.id


projectIdEq projectId model =
    projectId == model.projectId


filterPending =
    List.filter isPending


filterCompleted =
    List.filter isCompleted


ascend : (a -> comparable) -> a -> a -> Order
ascend fn a1 a2 =
    compare (fn a1) (fn a2)


descend : (a -> comparable) -> a -> a -> Order
descend fn a1 a2 =
    ascend fn a1 a2 |> flipOrd


flipOrd ord =
    case ord of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
