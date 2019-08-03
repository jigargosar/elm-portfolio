module Size exposing (Size, decoder, encoder, initial)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)


type alias Size =
    { width : Int, height : Int }


initial : Size
initial =
    { width = 0, height = 0 }


decoder : Decoder Size
decoder =
    JD.succeed Size
        |> JDP.required "width" JD.int
        |> JDP.required "height" JD.int


encoder : Size -> Value
encoder { width, height } =
    JE.object
        [ ( "width", JE.int width )
        , ( "height", JE.int height )
        ]
