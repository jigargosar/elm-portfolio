module ProjectCollection exposing (ProjectCollection, decoder, initial)

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as JD exposing (Decoder)
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
        [ Project.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        , JD.dict Project.decoder
        ]
