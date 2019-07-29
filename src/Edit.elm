module Edit exposing (Edit(..), encoder)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import Todo exposing (Todo, TodoId)


type Edit
    = None
    | InlineTodo Todo
    | Bulk (Set TodoId)


encoder : Edit -> Value
encoder edit =
    case edit of
        None ->
            JE.object
                [ ( "type", JE.string "None" )
                ]

        Bulk idSet ->
            JE.object
                [ ( "idSet", JE.set JE.string idSet )
                ]

        InlineTodo _ ->
            encoder None
