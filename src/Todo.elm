module Todo exposing
    ( Todo
    , TodoId
    , decoder
    , filterCompleted
    , filterPending
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


matchesFilter : Filter -> Todo -> Bool
matchesFilter filter model =
    case filter of
        Pending ->
            model.isDone

        Completed ->
            not model.isDone

        BelongsToProject pid ->
            model.projectId == pid


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
