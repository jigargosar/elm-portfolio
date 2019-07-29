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
                [ ( "type", JE.string "Bulk" )
                , ( "idSet", JE.set JE.string idSet )
                ]

        InlineTodo _ ->
            encoder None


decoder : Decoder Edit
decoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "None" ->
                        None |> JD.succeed

                    "Bulk" ->
                        JD.field "idSet" (JD.list JD.string)
                            |> JD.map (Set.fromList >> Bulk)

                    "InlineTodo" ->
                        JD.fail "Unable to decode inline todo"

                    _ ->
                        JD.fail ("Unable to decode Edit type: " ++ type_)
            )