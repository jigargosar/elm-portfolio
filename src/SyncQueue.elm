module SyncQueue exposing
    ( SyncQueue
    , decoder
    , encoder
    , initial
    , update
    )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Return
import TodoPatch exposing (TodoPatch)


type SyncQueue
    = SyncQueue (List TodoPatch)


initial : SyncQueue
initial =
    SyncQueue []


decoder : Decoder SyncQueue
decoder =
    JD.list TodoPatch.decoder
        |> JD.map SyncQueue


encoder : SyncQueue -> Value
encoder (SyncQueue list) =
    JE.list TodoPatch.encoder list


appendTodoPatches : List TodoPatch -> SyncQueue -> SyncQueue
appendTodoPatches newList (SyncQueue list) =
    list
        ++ newList
        |> SyncQueue


type alias Return =
    Return.Return Msg SyncQueue


type Msg
    = Init
    | AppendTodoPatches (List TodoPatch)


update : Msg -> SyncQueue -> Return
update msg model =
    case msg of
        Init ->
            ( model, Cmd.none )

        AppendTodoPatches newList ->
            ( appendTodoPatches newList model, Cmd.none )
