module ProjectId exposing (ProjectId, decoder, default)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)


type alias ProjectId =
    String


decoder : Decoder ProjectId
decoder =
    JD.string


default : ProjectId
default =
    ""
