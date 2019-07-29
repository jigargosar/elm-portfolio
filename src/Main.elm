port module Main exposing (main)

import Basics.Extra
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Edit exposing (Edit)
import Html exposing (Html, button, div, i, input, label, option, select, text, textarea)
import Html.Attributes exposing (autofocus, class, classList, href, style, tabindex, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import MediaQuery
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Return
import Route exposing (Route)
import Set exposing (Set)
import Task
import Time
import Todo exposing (Todo, TodoId)
import TodoDict exposing (TodoDict)
import Url exposing (Url)



-- PORTS


port cacheTodoList : List Todo -> Cmd msg


port cacheEdit : Value -> Cmd msg


type alias Flags =
    { todoList : Value
    , projectList : Value
    }


type alias ProjectDict =
    Dict ProjectId Project


type alias Error =
    String


type Page
    = DefaultPage
    | ProjectPage ProjectId
    | InboxPage


type alias Model =
    { todos : TodoDict
    , projects : ProjectDict
    , errors : List Error
    , edit : Edit
    , page : Page
    , key : Nav.Key
    , route : Route
    , isSidebarOpen : Bool
    , size : { width : Int, height : Int }
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

        Route.Inbox ->
            InboxPage

        Route.Project pid ->
            ProjectPage pid

        Route.NotFound url ->
            DefaultPage


init : Flags -> Url -> Nav.Key -> Return
init flags url key =
    let
        route =
            Route.fromUrl url

        page =
            routeToPage route

        emptyModel : Model
        emptyModel =
            Model Dict.empty
                Dict.empty
                []
                (Edit.Bulk Set.empty)
                page
                key
                route
                False
                { width = 0, height = 0 }

        initHelp todos projects =
            { emptyModel | todos = todos, projects = projects }

        initFromError ( prefix, error ) =
            { emptyModel | errors = [ prefix ++ " : " ++ JD.errorToString error ] }
    in
    ( Result.map2 initHelp
        (decodeTodoList flags.todoList)
        (decodeProjectList flags.projectList)
        |> unpackErr initFromError
    , Browser.Dom.getViewport |> Task.perform OnViewPort
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
    | OnBrowserResize Int Int
    | OnViewPort Browser.Dom.Viewport
    | OnTodoChecked TodoId
    | OnTodoTitleClicked TodoId
    | OnTodoCheckedWithNow TodoId Millis
    | OnTodoUnChecked TodoId
    | OnTodoUnCheckedWithNow TodoId Millis
    | WrapInlineEditTodoMsg InlineEditTodoMsg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnMenuClicked
    | OnSidebarOverlayClicked


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if Route.fromUrl url == model.route then
                        ( model, Nav.replaceUrl model.key (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url

                page =
                    routeToPage route
            in
            ( { model | page = page, route = route, isSidebarOpen = False }, Cmd.none )

        OnBrowserResize width height ->
            ( { model | size = { width = width, height = height } }, Cmd.none )

        OnViewPort vp ->
            let
                { width, height } =
                    vp.viewport
            in
            ( { model | size = { width = round width, height = round height } }, Cmd.none )

        OnDomFocusResult res ->
            res
                |> Result.map (\_ -> ( model, Cmd.none ))
                |> unpackErr (\err -> { model | errors = err :: model.errors } |> pure)

        OnTodoChecked todoId ->
            ( model, withNow (OnTodoCheckedWithNow todoId) )

        OnTodoTitleClicked todoId ->
            case model.edit of
                Edit.None ->
                    TodoDict.pendingWithId todoId model.todos
                        |> Maybe.map
                            (\t ->
                                setAndCacheEdit (Edit.InlineEditTodo t) model
                                    |> command focusInlineEditTodoTitleCmd
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

                Edit.Bulk idSet ->
                    let
                        toggleMember mem set =
                            if Set.member mem set then
                                Set.remove mem set

                            else
                                Set.insert mem set
                    in
                    model
                        |> setAndCacheEdit (idSet |> toggleMember todoId |> Edit.Bulk)

                Edit.InlineEditTodo _ ->
                    ( model, Cmd.none )

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
                Edit.None ->
                    ( model, Cmd.none )

                Edit.Bulk _ ->
                    ( model, Cmd.none )

                Edit.InlineEditTodo todo ->
                    updateInlineEditTodo msg todo model

        OnMenuClicked ->
            ( { model | isSidebarOpen = True }, Cmd.none )

        OnSidebarOverlayClicked ->
            ( { model | isSidebarOpen = False }, Cmd.none )


setAndCacheTodosIn model todos =
    ( setTodos todos model
    , cacheTodoList (Dict.values todos)
    )


updateInlineEditTodo msg todo model =
    case msg of
        IET_Title title ->
            model |> setAndCacheEdit (Edit.InlineEditTodo { todo | title = title })

        IET_ProjectId projectId ->
            model
                |> setAndCacheEdit (Edit.InlineEditTodo { todo | projectId = projectId })

        IET_Save ->
            Dict.insert todo.id todo model.todos
                |> setAndCacheTodosIn model
                |> andThen (setAndCacheEdit Edit.None)

        IET_Cancel ->
            setAndCacheEdit Edit.None model


setAndCacheEdit newEdit model =
    ( { model | edit = newEdit }, Edit.encoder newEdit |> cacheEdit )


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
    Sub.batch
        [ Browser.Events.onResize OnBrowserResize
        ]


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
            viewDefaultPage model

        InboxPage ->
            viewInboxPage model

        ProjectPage pid ->
            model.projects
                |> Dict.get pid
                |> Maybe.map (viewProjectPage model)
                |> Maybe.withDefault (viewPage DefaultPage model)


viewMaster { title, content } model =
    let
        viewProjectItem : Route -> Project -> Html Msg
        viewProjectItem route project =
            viewNavItem (route == Route.Project project.id)
                (Route.projectUrl project.id)
                project.title

        viewInboxItem : Route -> Html Msg
        viewInboxItem route =
            viewNavItem (route == Route.Inbox)
                Route.inboxUrl
                "Inbox"

        viewSidebar =
            div [ class "pa3 vs3" ]
                [ div [ class "vs3" ] [ viewInboxItem model.route ]
                , div [ class "vs3" ]
                    [ div [ class "vs3" ] [ text "Projects" ]
                    , div [ class "" ]
                        (List.map (viewProjectItem model.route)
                            (activeProjectList model.projects)
                        )
                    ]
                ]

        isSidebarOpen =
            model.isSidebarOpen && MediaQuery.isSmall model.size.width

        viewSidebarOverlay =
            div [ class "fixed absolute--fill flex" ]
                [ div [ class "w-80 shadow-1 bg-white black" ]
                    [ viewSidebar
                    ]
                , div
                    [ class "flex-grow-1 bg-black-60"
                    , onClick OnSidebarOverlayClicked
                    ]
                    []
                ]

        viewToolbar =
            div [ class "fixed w-100 hs3 flex bg-black-80 white" ]
                [ div [ class "" ] []
                , div
                    [ class "lh-copy pa1 tracked"
                    , onClick OnMenuClicked
                    ]
                    [ text "|||" ]
                , contentHeader
                ]

        splitView left right =
            div [ class "flex justify-center" ]
                [ div [ class "dn db-ns mw5 w-30-m w-40-l" ] [ left ]
                , div [ class "w-100 w-70-m w-40-l" ] [ right ]
                ]

        sidebarHeader =
            div [ class "bg-black-20 lh-copy pa1" ] [ text "ElmDOist" ]

        contentHeader =
            div [ class "flex w-100 bg-black-50 lh-copy pa1" ]
                [ div [ class "flex-grow-1" ] [ text title ]
                , case model.edit of
                    Edit.None ->
                        text ""

                    Edit.Bulk idSet ->
                        div [ class "flex hs3" ]
                            [ div [ class "" ] [ text "BulkMode" ]
                            , div [ class "" ] [ text "cancel" ]
                            ]

                    Edit.InlineEditTodo _ ->
                        text ""
                ]
    in
    { title = title
    , body =
        [ div [ class "" ]
            [ div [ class "dn-ns" ] [ viewToolbar ]
            , div [ class "dn db-ns w-100 fixed bg-black-70 white" ]
                [ splitView sidebarHeader contentHeader ]
            , div [ class "pt4" ] []
            , splitView viewSidebar content
            , viewIf isSidebarOpen viewSidebarOverlay
            ]
        ]
    }


viewIf bool v =
    if bool then
        v

    else
        text ""


viewNavItem sel url txt =
    div [ class "flex" ]
        [ Html.a
            [ class "truncate pa1 link flex-grow-1 pointer hov-bg-light-yellow lh-copy"
            , classList [ ( "dark-pink underline", sel ) ]
            , href url
            ]
            [ text txt ]
        ]


viewDefaultPage model =
    viewMaster
        { title = "Home"
        , content =
            div [ class "pa3 vs3" ]
                [ div [ class "vs3" ]
                    [ div [ class "" ] [ text "Pending" ]
                    , div [ class "vs3" ]
                        (viewPendingTodoList model.edit
                            model.projects
                            (TodoDict.pendingList model.todos)
                        )
                    ]
                , div [ class "vs3" ]
                    [ div [ class "" ] [ text "Done" ]
                    , div [ class "vs3" ] (List.map viewCompletedTodoItem (TodoDict.completedList model.todos))
                    ]
                ]
        }
        model


viewProjectPage model project =
    viewMaster
        { title = project.title
        , content =
            div [ class "pa3 vs3" ]
                [ div [ class "vs3" ]
                    [ div [] [ text "Pending" ]
                    , div [ class "vs3" ]
                        (viewPendingTodoList model.edit
                            model.projects
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
        }
        model


viewInboxPage model =
    viewMaster
        { title = "Inbox"
        , content =
            div [ class "pa3 vs3" ]
                [ div [ class "vs3" ]
                    [ div [] [ text "Pending" ]
                    , div [ class "vs3" ]
                        (viewPendingTodoList model.edit
                            model.projects
                            (TodoDict.pendingWithProjectId "" model.todos)
                        )
                    ]
                , div [ class "vs3" ]
                    [ div [] [ text "Done" ]
                    , div [ class "vs3" ]
                        (List.map viewCompletedTodoItem
                            (TodoDict.completedForProjectList "" model.todos)
                        )
                    ]
                ]
        }
        model


viewError error =
    div [ class "red" ] [ text error ]


viewPendingTodoList : Edit -> ProjectDict -> List Todo -> List (Html Msg)
viewPendingTodoList edit projects todoList =
    case edit of
        Edit.None ->
            List.map viewNonEditingTodoItem todoList

        Edit.Bulk idSet ->
            List.map
                (\todo ->
                    viewBulkTodoItem (Set.member todo.id idSet) todo
                )
                todoList

        Edit.InlineEditTodo editingTodo ->
            List.map
                (\todo ->
                    if editingTodo.id == todo.id then
                        viewInlineInlineEditTodoItem projects editingTodo

                    else
                        viewNonEditingTodoItem todo
                )
                todoList


viewNonEditingTodoItem todo =
    div [ class "flex hs3 _bg-black", title (Debug.toString todo) ]
        [ div [ class "pointer no-sel", onClick (OnTodoChecked todo.id) ]
            [ i [ class "material-icons" ] [ text "radio_button_unchecked" ]
            ]
        , div [ class "flex-grow-1", onClick (OnTodoTitleClicked todo.id) ]
            [ div [ class "" ] [ text todo.title ] ]
        ]


viewBulkTodoItem isSelected todo =
    div [ class "flex hs3 _bg-black", title (Debug.toString todo) ]
        [ {- div [ class "pointer no-sel", onClick (OnTodoChecked todo.id) ]
                 [ i [ class "material-icons" ] [ text "radio_button_unchecked" ]
                 ]
             ,
          -}
          div
            [ class "flex-grow-1"
            , classList [ ( "bg-light-yellow", isSelected ) ]
            , onClick (OnTodoTitleClicked todo.id)
            ]
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
