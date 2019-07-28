module Project exposing
    ( Project
    , decoder
    , setModifiedAt
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)


type alias Project =
    { id : ProjectId
    , title : String
    , sortIdx : Int
    , createdAt : Int
    , modifiedAt : Int
    }


decoder : Decoder Project
decoder =
    JD.succeed Project
        |> JD.required "id" JD.string
        |> JD.required "title" JD.string
        |> JD.required "sortIdx" JD.int
        |> JD.required "createdAt" JD.int
        |> JD.required "modifiedAt" JD.int


setModifiedAt now todo =
    { todo | modifiedAt = now }
