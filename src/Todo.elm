module Todo exposing
    ( CompareBy(..)
    , Filter(..)
    , Todo
    , TodoId
    , decoder
    , filter
    , filterCompleted
    , filterPending
    , filterSort
    , idEq
    , isCompleted
    , isPending
    , mapCompleted
    , markCompleted
    , markPending
    , projectIdEq
    , setModifiedAt
    , setSortIdx
    , sortCompleted
    , sortPending
    , toggleCompleted
    )

import Compare exposing (Comparator)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)


type alias TodoId =
    String


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


mapCompleted fn model =
    { model | isDone = fn model.isDone }


setCompleted bool model =
    mapCompleted (always bool) model


setSortIdx sortIdx model =
    { model | sortIdx = sortIdx }


markCompleted =
    setCompleted True


markPending =
    setCompleted False


toggleCompleted model =
    mapCompleted not model


setModifiedAt now todo =
    { todo | modifiedAt = now }


type Filter
    = Pending
    | Completed
    | BelongsToProject ProjectId
    | AndFilter Filter Filter


matchesFilter : Filter -> Todo -> Bool
matchesFilter filter_ todo =
    case filter_ of
        Pending ->
            todo.isDone

        Completed ->
            not todo.isDone

        BelongsToProject pid ->
            todo.projectId == pid

        AndFilter a b ->
            matchesFilter a todo && matchesFilter b todo


type alias TodoList =
    List Todo


filter : Filter -> TodoList -> TodoList
filter filter_ =
    List.filter (matchesFilter filter_)


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


sortPending =
    List.sortBy (\t -> t.sortIdx)


sortCompleted =
    List.sortWith (descend .modifiedAt)


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
