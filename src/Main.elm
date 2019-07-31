port module Main exposing (effect, main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Edit exposing (Edit)
import Html exposing (Html, button, div, i, text)
import Html.Attributes exposing (class, classList, href, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import MediaQuery
import Project exposing (Project, ProjectList)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import Result.Extra
import Return
import Route exposing (Route)
import Set exposing (Set)
import Sync exposing (SyncMsg, SyncQueue)
import Task
import Time
import Todo exposing (Todo, TodoId, TodoList)
import TodoDict exposing (TodoDict)
import Url exposing (Url)



-- PORTS


port cacheTodoList : Value -> Cmd msg


port cacheEdit : Value -> Cmd msg



-- FLAGS


type alias Flags =
    { todoList : TodoList
    , projects : ProjectCollection
    , syncQueue : SyncQueue
    , edit : Edit
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "todoList" Todo.listDecoder
        |> JDP.required "projectList" ProjectCollection.decoder
        |> JDP.required "syncQueue" Sync.queueDecoder
        |> JDP.required "edit" Edit.decoder


type alias Error =
    String


type Page
    = DefaultPage
    | ProjectPage ProjectId
    | InboxPage


type alias Model =
    { todos : TodoDict
    , projects : ProjectCollection
    , errors : List Error
    , edit : Edit
    , syncQueue : SyncQueue
    , page : Page
    , key : Nav.Key
    , route : Route
    , isSidebarOpen : Bool
    , size : { width : Int, height : Int }
    }


type alias Return =
    Return.Return Msg Model


routeToPage : Route -> Page
routeToPage route =
    case route of
        Route.Default ->
            DefaultPage

        Route.Inbox ->
            InboxPage

        Route.Project pid ->
            ProjectPage pid

        Route.NotFound _ ->
            DefaultPage


updateWithEncodedFlags : Value -> Model -> Model
updateWithEncodedFlags encodedFlags model =
    case JD.decodeValue flagsDecoder encodedFlags of
        Ok flags ->
            { model
                | todos = TodoDict.fromList flags.todoList
                , projects = flags.projects
                , edit = flags.edit
                , syncQueue = flags.syncQueue
            }

        Err decodeErr ->
            model |> prependError (JD.errorToString decodeErr)


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    ( let
        route =
            Route.fromUrl url
      in
      { todos = TodoDict.initial
      , projects = ProjectCollection.initial
      , errors = []
      , edit = Edit.initial
      , syncQueue = Sync.initialQueue
      , page = routeToPage route
      , key = key
      , route = route
      , isSidebarOpen = False
      , size = { width = 0, height = 0 }
      }
        |> updateWithEncodedFlags encodedFlags
    , Cmd.batch
        [ Browser.Dom.getViewport |> Task.perform OnViewPort
        ]
    )


setTodos : TodoDict -> Model -> Maybe Model
setTodos todos model =
    if todos == model.todos then
        Nothing

    else
        { model | todos = todos } |> Just


activeProjectList projects =
    projects
        |> Dict.values
        |> List.sortBy .sortIdx


type alias Millis =
    Int


type alias DomFocusResult =
    Result String ()


type InlineEditTodoMsg
    = IET_SetTitle String
    | IET_SetProjectId ProjectId
    | IET_Save
    | IET_SaveWithNow Millis
    | IET_Cancel


type Msg
    = NoOp
    | OnDomFocusResult DomFocusResult
    | OnBrowserResize Int Int
    | OnViewPort Browser.Dom.Viewport
    | OnTodoTitleClicked TodoId
    | OnTodoChecked TodoId
    | OnTodoCheckedWithNow TodoId Millis
    | OnTodoUnChecked TodoId
    | OnTodoUnCheckedWithNow TodoId Millis
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnMenuClicked
    | OnSidebarOverlayClicked
    | OnInlineEditTodoMsg InlineEditTodoMsg
    | OnBulkCancelClicked
    | OnSelectMultipleClicked
    | OnBulkMoveToProjectSelected ProjectId
    | OnBulkMoveToProjectSelectedWithNow ProjectId Millis



--


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
                |> unpackErr (prependErrorIn model >> pure)

        OnTodoChecked todoId ->
            ( model, withNow (OnTodoCheckedWithNow todoId) )

        OnTodoTitleClicked todoId ->
            case model.edit of
                Edit.None ->
                    TodoDict.pendingWithId todoId model.todos
                        |> Maybe.map
                            (\t ->
                                updateEdit (Edit.InlineTodo t) model
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
                        |> updateEdit (idSet |> toggleMember todoId |> Edit.Bulk)

                Edit.InlineTodo _ ->
                    ( model, Cmd.none )

        OnTodoCheckedWithNow todoId now ->
            TodoDict.update now todoId TodoDict.MarkComplete model.todos
                |> setAndCacheTodosWithMsgIn model now

        OnTodoUnChecked todoId ->
            ( model, withNow (OnTodoUnCheckedWithNow todoId) )

        OnTodoUnCheckedWithNow todoId now ->
            TodoDict.update now todoId TodoDict.MarkPending model.todos
                |> setAndCacheTodosWithMsgIn model now

        OnInlineEditTodoMsg msg ->
            case model.edit of
                Edit.InlineTodo todo ->
                    updateInlineEditTodo msg todo model

                _ ->
                    ( model, Cmd.none )

        OnMenuClicked ->
            ( { model | isSidebarOpen = True }, Cmd.none )

        OnSidebarOverlayClicked ->
            ( { model | isSidebarOpen = False }, Cmd.none )

        OnBulkCancelClicked ->
            model |> updateEdit Edit.None

        OnSelectMultipleClicked ->
            model |> updateEdit (Edit.Bulk Set.empty)

        OnBulkMoveToProjectSelected projectId ->
            ( model, OnBulkMoveToProjectSelectedWithNow projectId |> withNow )

        OnBulkMoveToProjectSelectedWithNow projectId now ->
            case model.edit of
                Edit.None ->
                    ( model, Cmd.none )

                Edit.Bulk idSet ->
                    model.todos
                        |> TodoDict.updateBulk now idSet (TodoDict.MoveToProject projectId)
                        |> setAndCacheTodosWithMsgIn model now
                        |> andThen (updateEdit Edit.None)

                Edit.InlineTodo _ ->
                    ( model, Cmd.none )


setAndCacheTodosWithMsgIn :
    Model
    -> Millis
    -> ( TodoDict, List SyncMsg )
    -> Return
setAndCacheTodosWithMsgIn model now ( todos, syncMessages ) =
    let
        _ =
            Debug.log "now, syncMessages" ( now, syncMessages )
    in
    setTodos todos model
        |> Maybe.map
            (pure >> effect cacheTodosEffect
             --        >> command (Sync.batchEncoder now syncMessages |> cacheSyncQueue)
            )
        |> Maybe.withDefault ( model, Cmd.none )


cacheTodosEffect : Model -> Cmd Msg
cacheTodosEffect model =
    cacheTodoList (Dict.values model.todos |> JE.list Todo.encoder)


prependError : String -> Model -> Model
prependError error model =
    { model | errors = error :: model.errors }


prependErrorIn : Model -> String -> Model
prependErrorIn model error =
    prependError error model


updateInlineEditTodo : InlineEditTodoMsg -> Todo -> Model -> Return
updateInlineEditTodo msg todo model =
    case msg of
        IET_SetTitle title ->
            model |> updateEdit (Edit.InlineTodo { todo | title = title })

        IET_SetProjectId projectId ->
            model
                |> updateEdit (Edit.InlineTodo { todo | projectId = projectId })

        IET_Save ->
            ( model, withNow (IET_SaveWithNow >> OnInlineEditTodoMsg) )

        IET_SaveWithNow now ->
            model.todos
                |> TodoDict.update now todo.id (TodoDict.SetTitle todo.title)
                |> TodoDict.andThen
                    (TodoDict.update now todo.id (TodoDict.MoveToProject todo.projectId))
                |> setAndCacheTodosWithMsgIn model now
                |> andThen (updateEdit Edit.None)

        IET_Cancel ->
            updateEdit Edit.None model


updateEdit newEdit model =
    ( { model | edit = newEdit }
    , Edit.encoder newEdit |> cacheEdit
    )


focusInlineEditTodoTitleCmd =
    Browser.Dom.focus editTodoTitleDomid
        |> Task.attempt
            (Result.mapError
                (\_ -> "Dom Focus Failed:" ++ editTodoTitleDomid)
                >> OnDomFocusResult
            )


withNow : (Millis -> msg) -> Cmd msg
withNow msg =
    Time.now |> Task.perform (Time.posixToMillis >> msg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnBrowserResize
        ]


view : Model -> Browser.Document Msg
view model =
    let
        { title, body } =
            viewPage model.page model

        errors =
            -- [ "Testing errors" ]
            model.errors
    in
    { title = title
    , body =
        body
            ++ [ viewUnless (List.isEmpty errors) (viewErrorOverlay errors) ]
    }


viewErrorOverlay errors =
    div
        [ class "fixed absolute--fill bg-black-70 white"
        , class "f4 flex justify-center items-center"
        ]
        [ div [ class "h5 w-80 pa3 vs3 shadow-1 bg-white  black" ]
            [ div [ class "pa1 b red" ] [ text "Error:" ]
            , div [ class "pa1" ] (List.map viewErrorItem errors)
            ]
        ]


viewErrorItem error =
    div [ class "" ] [ text error ]


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
                        div [ class "flex hs3" ]
                            [ div
                                [ class "underline pointer"
                                , onClick OnSelectMultipleClicked
                                ]
                                [ text "Select Multiple" ]
                            ]

                    Edit.Bulk _ ->
                        div [ class "flex hs3" ]
                            [ div [ class "" ] [ text "BulkMode" ]
                            , viewMoveToProjectsMenu (activeProjectList model.projects)
                            , div
                                [ class "underline pointer"
                                , onClick OnBulkCancelClicked
                                ]
                                [ text "cancel" ]
                            ]

                    Edit.InlineTodo _ ->
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


viewMoveToProjectsMenu projectList =
    let
        viewProjectMenuItem p =
            div
                [ class "w4 truncate pa2 hover-bg-black  pointer"
                , onClick (OnBulkMoveToProjectSelected p.id)
                ]
                [ text p.title ]
    in
    div [ class "relative hover-db-parent" ]
        [ div [ class "c-default" ] [ text "Move To..." ]
        , div [ class "hover-db-child bg-black-90 absolute" ]
            (viewProjectMenuItem { id = "", title = "Inbox" }
                :: List.map viewProjectMenuItem projectList
            )
        ]


viewIf bool v =
    if bool then
        v

    else
        text ""


viewUnless bool v =
    viewIf (not bool) v


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


viewPendingTodoList : Edit -> ProjectCollection -> List Todo -> List (Html Msg)
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

        Edit.InlineTodo editingTodo ->
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
        , div [ class "flex-grow-1 pointer", onClick (OnTodoTitleClicked todo.id) ]
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
            [ class "flex-grow-1 pointer"
            , classList [ ( "bg-light-yellow", isSelected ) ]
            , onClick (OnTodoTitleClicked todo.id)
            ]
            [ div [ class "" ] [ text todo.title ] ]
        ]


editTodoTitleDomid =
    "edit-todo-title-domid"


viewInlineInlineEditTodoItem : ProjectCollection -> Todo -> Html Msg
viewInlineInlineEditTodoItem projects todo =
    div [ class "flex hs3 _bg-black", title (Debug.toString todo) ]
        [ div [ class "flex-grow-1" ]
            [ div [ class "flex" ]
                [ Html.textarea
                    [ Html.Attributes.id editTodoTitleDomid
                    , class "flex-grow-1"
                    , value todo.title
                    , onInput IET_SetTitle
                    ]
                    []
                ]
            , viewProjectSelect projects todo
            , button [ onClick IET_Save ] [ text "save" ]
            , button [ onClick IET_Cancel ] [ text "cancel" ]
            ]
        ]
        |> Html.map OnInlineEditTodoMsg


viewProjectSelect : ProjectCollection -> Todo -> Html InlineEditTodoMsg
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
        [ Html.select [ onInput IET_SetProjectId ]
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


main : Program Value Model Msg
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
