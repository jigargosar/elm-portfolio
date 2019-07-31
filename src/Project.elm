module Project exposing
    ( Project
    , ProjectList
    , decoder
    , encoder
    , listDecoder
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


type alias ProjectList =
    List Project


decoder : Decoder Project
decoder =
    JD.succeed Project
        |> JD.required "id" JD.string
        |> JD.required "title" JD.string
        |> JD.required "sortIdx" JD.int
        |> JD.required "createdAt" JD.int
        |> JD.required "modifiedAt" JD.int


listDecoder : Decoder ProjectList
listDecoder =
    JD.list decoder


encoder : Project -> Value
encoder { id, title, sortIdx, createdAt, modifiedAt } =
    JE.object
        [ ( "id", JE.string id )
        , ( "title", JE.string title )
        , ( "sortIdx", JE.int sortIdx )
        , ( "createdAt", JE.int createdAt )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


setModifiedAt now todo =
    { todo | modifiedAt = now }
