port module Main exposing (main)

import Basics.Extra
import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, button, div, i, input, label, option, select, text, textarea)
import Html.Attributes exposing (autofocus, class, style, tabindex, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Return
import Route
import Task
import Time
import Todo exposing (Todo, TodoId)
import TodoDict exposing (TodoDict)
import Url exposing (Url)



-- PORTS


port cacheTodoList : List Todo -> Cmd msg


type alias Flags =
    { todoList : Value
    , projectList : Value
    }


type alias ProjectDict =
    Dict ProjectId Project


type alias Error =
    String


type Edit
    = NoEdit
    | InlineEditTodo Todo


type Page
    = DefaultPage
    | ProjectPage ProjectId


type alias Model =
    { todos : TodoDict
    , projects : ProjectDict
    , errors : List Error
    , edit : Edit
    , page : Page
    , key : Nav.Key
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


routeToPage route =
    case route of
        Route.Default ->
            DefaultPage

        Route.Project pid ->
            ProjectPage pid

        Route.NotFound url ->
            DefaultPage


init : Flags -> Url -> Nav.Key -> Return
init flags url key =
    let
        page =
            Route.fromUrl url |> routeToPage

        emptyModel : Model
        emptyModel =
            Model Dict.empty Dict.empty [] NoEdit page key

        initHelp todos projects =
            { emptyModel | todos = todos, projects = projects }

        initFromError ( prefix, error ) =
            { emptyModel | errors = [ prefix ++ " : " ++ JD.errorToString error ] }
    in
    ( Result.map2 initHelp
        (decodeTodoList flags.todoList)
        (decodeProjectList flags.projectList)
        |> unpackErr initFromError
    , Cmd.none
    )


mapTodos : (TodoDict -> TodoDict) -> Model -> Model
mapTodos mapper model =
    { model | todos = mapper model.todos }


setTodos : TodoDict -> Model -> Model
setTodos todos model =
    { model | todos = todos }


activeProjectList projects =
    projects
        |> Dict.values
        |> List.sortBy .sortIdx


type WithNow
    = MarkCompleted TodoId
    | MarkPending TodoId


type alias Millis =
    Int


type alias DomFocusResult =
    Result String ()


type InlineEditTodoMsg
    = IET_Title String
    | IET_ProjectId ProjectId
    | IET_Save
    | IET_Cancel


type Msg
    = NoOp
    | OnDomFocusResult DomFocusResult
    | OnTodoChecked TodoId
    | OnTodoTitleClicked TodoId
    | OnTodoCheckedWithNow TodoId Millis
    | OnTodoUnChecked TodoId
    | OnTodoUnCheckedWithNow TodoId Millis
    | WrapInlineEditTodoMsg InlineEditTodoMsg
    | NavTo String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        NavTo urlString ->
            ( model, Nav.pushUrl model.key urlString )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                page =
                    Route.fromUrl url |> routeToPage
            in
            ( { model | page = page }, Cmd.none )

        OnDomFocusResult res ->
            res
                |> Result.map (\_ -> ( model, Cmd.none ))
                |> unpackErr (\err -> { model | errors = err :: model.errors } |> pure)

        OnTodoChecked todoId ->
            ( model, withNow (OnTodoCheckedWithNow todoId) )

        OnTodoTitleClicked todoId ->
            TodoDict.pendingWithId todoId model.todos
                |> Maybe.map
                    (\t ->
                        ( { model | edit = InlineEditTodo t }
                        , focusInlineEditTodoTitleCmd
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        OnTodoCheckedWithNow todoId now ->
            TodoDict.markCompleted todoId now model.todos
                |> Maybe.map (setAndCacheTodosIn model)
                |> Maybe.withDefault ( model, Cmd.none )

        OnTodoUnChecked todoId ->
            ( model, withNow (OnTodoUnCheckedWithNow todoId) )

        OnTodoUnCheckedWithNow todoId now ->
            TodoDict.markPending todoId now model.todos
                |> Maybe.map (setAndCacheTodosIn model)
                |> Maybe.withDefault ( model, Cmd.none )

        WrapInlineEditTodoMsg msg ->
            case model.edit of
                NoEdit ->
                    ( model, Cmd.none )

                InlineEditTodo todo ->
                    updateInlineEditTodo msg todo model


setAndCacheTodosIn model todos =
    ( setTodos todos model
    , cacheTodoList (Dict.values todos)
    )


updateInlineEditTodo msg todo model =
    case msg of
        IET_Title title ->
            ( { model
                | edit = InlineEditTodo { todo | title = title }
              }
            , Cmd.none
            )

        IET_ProjectId projectId ->
            ( { model
                | edit = InlineEditTodo { todo | projectId = projectId }
              }
            , Cmd.none
            )

        IET_Save ->
            Dict.insert todo.id todo model.todos
                |> setAndCacheTodosIn model
                |> Tuple.mapFirst setNoEdit

        IET_Cancel ->
            ( setNoEdit model, Cmd.none )


setNoEdit model =
    { model | edit = NoEdit }


focusInlineEditTodoTitleCmd =
    Browser.Dom.focus editTodoTitleDomid
        |> Task.attempt
            (Result.mapError
                (\_ -> "Dom Focus Failed:" ++ editTodoTitleDomid)
                >> OnDomFocusResult
            )


sortPendingTodos : Millis -> TodoDict -> TodoDict
sortPendingTodos now todos =
    TodoDict.pendingList todos
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, t ) ->
                if t.sortIdx == i then
                    Nothing

                else
                    Just (Todo.setSortIdx i t |> Todo.setModifiedAt now)
            )
        |> List.foldl (\t -> Dict.insert t.id t) todos


withNow : (Millis -> msg) -> Cmd msg
withNow msg =
    Time.now |> Task.perform (Time.posixToMillis >> msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    if model.errors |> List.isEmpty |> not then
        { title = "ElmDoist"
        , body =
            [ div [ class "pa3 vs3" ]
                (List.map viewError model.errors)
            ]
        }

    else
        viewPage model.page model


viewPage page model =
    case page of
        DefaultPage ->
            { title = "ElmDoist"
            , body = [ viewDefaultPage model ]
            }

        ProjectPage pid ->
            model.projects
                |> Dict.get pid
                |> Maybe.map (viewProjectPageHelp model)
                |> Maybe.withDefault (viewPage DefaultPage model)


viewDefaultPage model =
    div [ class "pa3 vs3" ]
        [ div [ class "vs3" ]
            [ div [ class "vs3" ] [ text "Projects" ]
            , div [ class "" ] (List.map viewProjectItem (activeProjectList model.projects))
            ]
        , div [ class "vs3" ]
            [ div [ class "" ] [ text "Pending" ]
            , div [ class "vs3" ]
                (List.map (viewPendingTodoItem model.edit model.projects)
                    (TodoDict.pendingList model.todos)
                )
            ]
        , div [ class "vs3" ]
            [ div [ class "" ] [ text "Done" ]
            , div [ class "vs3" ] (List.map viewCompletedTodoItem (TodoDict.completedList model.todos))
            ]
        ]


viewProjectPageHelp model project =
    { title = "ElmDoist"
    , body =
        [ div [ class "pa3 vs3" ]
            [ div [ class "hs3 flex" ]
                [ div [] [ text "Project:" ]
                , div [] [ text project.title ]
                ]
            , div [ class "vs3" ]
                [ div [] [ text "Pending" ]
                , div [ class "vs3" ]
                    (List.map
                        (viewPendingTodoItem model.edit model.projects)
                        (TodoDict.pendingWithProjectId project.id model.todos)
                    )
                ]
            , div [ class "vs3" ]
                [ div [] [ text "Done" ]
                , div [ class "vs3" ]
                    (List.map viewCompletedTodoItem
                        (TodoDict.completedForProjectList project.id model.todos)
                    )
                ]
            ]
        ]
    }


viewError error =
    div [ class "red" ] [ text error ]


viewProjectItem : Project -> Html Msg
viewProjectItem project =
    div [ class "flex" ]
        [ {- div [ class "pointer no-sel" ]
                 [ i [ class "material-icons" ] [ text "radio_button_unchecked" ]
                 ]
             ,
          -}
          Html.a
            [ class "pa1 link flex-grow-1 pointer hover-bg-light-yellow lh-copy"
            , title (Debug.toString project)

            -- , onClick (NavTo (Route.projectUrl project.id))
            , Html.Attributes.href (Route.projectUrl project.id)
            ]
            [ text project.title ]
        ]


viewPendingTodoItem : Edit -> ProjectDict -> Todo -> Html Msg
viewPendingTodoItem edit projects todo =
    case edit of
        NoEdit ->
            viewNonEditingTodoItem todo

        InlineEditTodo editingTodo ->
            if editingTodo.id == todo.id then
                viewInlineInlineEditTodoItem projects editingTodo

            else
                viewNonEditingTodoItem todo


viewNonEditingTodoItem todo =
    div [ class "flex hs3 _bg-black", title (Debug.toString todo) ]
        [ div [ class "pointer no-sel", onClick (OnTodoChecked todo.id) ]
            [ i [ class "material-icons" ] [ text "radio_button_unchecked" ]
            ]
        , div [ class "flex-grow-1", onClick (OnTodoTitleClicked todo.id) ]
            [ div [ class "" ] [ text todo.title ] ]
        ]


editTodoTitleDomid =
    "edit-todo-title-domid"


viewInlineInlineEditTodoItem projects todo =
    div [ class "flex hs3 _bg-black", title (Debug.toString todo) ]
        [ div [ class "flex-grow-1" ]
            [ div [ class "flex" ]
                [ Html.textarea
                    [ Html.Attributes.id editTodoTitleDomid
                    , class "flex-grow-1"
                    , value todo.title
                    , onInput IET_Title
                    ]
                    []
                ]
            , viewProjectSelect projects todo
            , button [ onClick IET_Save ] [ text "save" ]
            , button [ onClick IET_Cancel ] [ text "cancel" ]
            ]
        ]
        |> Html.map WrapInlineEditTodoMsg


viewProjectSelect : ProjectDict -> Todo -> Html InlineEditTodoMsg
viewProjectSelect projects todo =
    let
        projectList =
            activeProjectList projects

        selectedAttrForPrjId id =
            Html.Attributes.selected (id == todo.projectId)

        viewProjectOption p =
            Html.option [ value p.id, selectedAttrForPrjId p.id ] [ text p.title ]
    in
    div [ class "" ]
        [ Html.select [ onInput IET_ProjectId ]
            (Html.option [ value "", selectedAttrForPrjId "" ] [ text "Inbox" ]
                :: List.map viewProjectOption projectList
            )
        ]


viewCompletedTodoItem : Todo -> Html Msg
viewCompletedTodoItem todo =
    div [ class "flex hs3 _bg-black" ]
        [ div [ class "pointer no-sel", onClick (OnTodoUnChecked todo.id) ]
            [ i [ class "material-icons" ] [ text "check_circle_outline" ]
            ]
        , div [ class "flex-grow-1", title (Debug.toString todo) ] [ text todo.title ]
        ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- UPDATE HELPERS


pure =
    Return.singleton


effect =
    Return.effect_


andThen =
    Return.andThen


command =
    Return.command



-- CORE HELPERS


unpackErr : (e -> v) -> Result e v -> v
unpackErr fn result =
    case result of
        Err e ->
            fn e

        Ok v ->
            v
