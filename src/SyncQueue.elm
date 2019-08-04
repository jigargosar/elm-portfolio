module SyncQueue exposing
    ( Msg(..)
    , OutMsg(..)
    , SyncQueue
    , decoder
    , encoder
    , initial
    , update
    )

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Project exposing (Project)
import Todo exposing (Todo)
import TodoPatch exposing (TodoPatch)


type SyncQueue
    = SyncQueue (List TodoPatch)


type alias Model =
    SyncQueue


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
    ( SyncQueue, Cmd Msg, OutMsg )


type alias DB =
    { projectList : List Project
    , todoList : List Todo
    }


dbDecoder : Decoder DB
dbDecoder =
    JD.succeed DB
        |> JDP.required "projectList" Project.listDecoder
        |> JDP.required "todoList" Todo.listDecoder


type Msg
    = Init
    | AppendTodoPatches (List TodoPatch)
    | OnHttpResponse (Result Http.Error DB)


type OutMsg
    = NoOutMsg
    | SyncResponse DB


update : Msg -> SyncQueue -> Return
update msg model =
    case msg of
        Init ->
            let
                cmd =
                    syncEffect model
            in
            ( model, cmd, NoOutMsg )

        AppendTodoPatches newList ->
            ( appendTodoPatches newList model, Cmd.none, NoOutMsg )

        OnHttpResponse result ->
            case result of
                Ok db ->
                    ( initial, Cmd.none, SyncResponse db )

                Err e ->
                    let
                        _ =
                            Debug.log "http db get error" e
                    in
                    ( model, Cmd.none, NoOutMsg )


syncEffect : Model -> Cmd Msg
syncEffect model =
    Http.post
        { url = "/api/sync"
        , body = Http.jsonBody (syncJsonBody model)
        , expect = Http.expectJson OnHttpResponse dbDecoder
        }


syncJsonBody : Model -> Value
syncJsonBody (SyncQueue list) =
    JE.object
        [ ( "todos", JE.list TodoPatch.encoder list )
        ]
