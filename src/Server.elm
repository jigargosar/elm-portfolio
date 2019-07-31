module Server exposing (Model, main)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode exposing (Value)
import Project
import ProjectDict exposing (ProjectDict)
import Return
import Todo
import TodoDict exposing (TodoDict)


type alias Flags =
    { todoList : Value
    , projectList : Value
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "todoList" JD.value
        |> JDP.required "projectList" JD.value


type alias Error =
    String


type alias Model =
    { todos : TodoDict
    , projects : ProjectDict
    , errors : List Error
    }


type alias Return =
    Return.Return Msg Model


dictFromListBy : (item -> comparable) -> List item -> Dict comparable item
dictFromListBy keyFn =
    List.map (\item -> ( keyFn item, item )) >> Dict.fromList


decodeTodoList encoded =
    encoded
        |> JD.decodeValue (JD.list Todo.decoder)
        |> Result.map (dictFromListBy .id)
        |> Result.mapError (\e -> ( "Error decoding todos", e ))


decodeProjectList encoded =
    encoded
        |> JD.decodeValue (JD.list Project.decoder)
        |> Result.map (dictFromListBy .id)
        |> Result.mapError (\e -> ( "Error decoding projects", e ))


init : Value -> Return
init encodedFlags =
    let
        emptyModel : Model
        emptyModel =
            { todos = Dict.empty
            , projects = Dict.empty
            , errors = []
            }

        initHelp todos projects =
            { emptyModel
                | todos = todos
                , projects = projects
            }

        initFromError ( prefix, error ) =
            emptyModel
                |> prependError (prefix ++ " : " ++ JD.errorToString error)

        initFromFlags flags =
            Result.map2 initHelp
                (decodeTodoList flags.todoList)
                (decodeProjectList flags.projectList)
                |> unpackErr initFromError
    in
    ( Result.map initFromFlags
        (JD.decodeValue flagsDecoder encodedFlags
            |> Result.mapError (\e -> ( "Error decoding Flags", e ))
        )
        |> unpackErr initFromError
    , Cmd.none
    )


prependError : String -> Model -> Model
prependError error model =
    { model | errors = error :: model.errors }


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



-- CORE HELPERS


unpackErr : (e -> v) -> Result e v -> v
unpackErr fn result =
    case result of
        Err e ->
            fn e

        Ok v ->
            v
