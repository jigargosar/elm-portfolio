module Server exposing (Model, main)

import Json.Encode exposing (Value)
import Return


type alias Model =
    {}


type alias Return =
    Return.Return Msg Model


init : Value -> Return
init _ =
    ( {}, Cmd.none )


type Msg
    = NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


update : Msg -> Model -> Return
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


main : Program Value Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }
