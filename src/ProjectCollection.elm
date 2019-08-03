module ProjectCollection exposing (ProjectCollection, decoder, encoder, initial)

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)


type alias ProjectCollection =
    Dict ProjectId Project


initial : ProjectCollection
initial =
    Dict.empty


decoder : Decoder ProjectCollection
decoder =
    JD.oneOf
        [ JD.dict Project.decoder
        , Project.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        ]


encoder : ProjectCollection -> Value
encoder model =
    JE.dict identity Project.encoder model
