module Pages.Pages exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Global
import Page exposing (Bundle, Document)
import Pages.NotFound
import Pages.Play
import Pages.Top
import Pages.Waiting
import Route as Route exposing (Route)



-- TYPES


type Model
    = Top_Model Pages.Top.Model
    | NotFound_Model Pages.NotFound.Model
    | Play_Model Pages.Play.Model
    | Waiting_Model Pages.Waiting.Model


type Msg
    = Top_Msg Pages.Top.Msg
    | NotFound_Msg Pages.NotFound.Msg
    | Play_Msg Pages.Play.Msg
    | Waiting_Msg Pages.Waiting.Msg



-- PAGES


type alias UpgradedPage flags model msg =
    { init : flags -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , update : msg -> model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
    , bundle : model -> Global.Model -> Bundle Msg
    }


type alias UpgradedPages =
    { top : UpgradedPage Pages.Top.Flags Pages.Top.Model Pages.Top.Msg
    , notFound : UpgradedPage Pages.NotFound.Flags Pages.NotFound.Model Pages.NotFound.Msg
    , play : UpgradedPage Pages.Play.Flags Pages.Play.Model Pages.Play.Msg
    , waiting : UpgradedPage Pages.Waiting.Flags Pages.Waiting.Model Pages.Waiting.Msg
    }


pages : UpgradedPages
pages =
    { top = Pages.Top.page |> Page.upgrade Top_Model Top_Msg
    , notFound = Pages.NotFound.page |> Page.upgrade NotFound_Model NotFound_Msg
    , play = Pages.Play.page |> Page.upgrade Play_Model Play_Msg
    , waiting = Pages.Waiting.page |> Page.upgrade Waiting_Model Waiting_Msg
    }



-- INIT


init : Route -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
init route =
    case route of
        Route.Top ->
            pages.top.init ()

        Route.NotFound ->
            pages.notFound.init ()

        Route.Play gameId ->
            pages.play.init { gameId = gameId }

        Route.Waiting gameId gameUrl gameCreator ->
            pages.waiting.init { gameId = gameId, gameUrl = gameUrl, gameCreator = gameCreator }



-- UPDATE


update : Msg -> Model -> Global.Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update bigMsg bigModel =
    case ( bigMsg, bigModel ) of
        ( Top_Msg msg, Top_Model model ) ->
            pages.top.update msg model

        ( NotFound_Msg msg, NotFound_Model model ) ->
            pages.notFound.update msg model

        ( Play_Msg msg, Play_Model model ) ->
            pages.play.update msg model

        ( Waiting_Msg msg, Waiting_Model model ) ->
            pages.waiting.update msg model

        _ ->
            always ( bigModel, Cmd.none, Cmd.none )



-- BUNDLE - (view + subscriptions)


bundle : Model -> Global.Model -> Bundle Msg
bundle bigModel =
    case bigModel of
        Top_Model model ->
            pages.top.bundle model

        NotFound_Model model ->
            pages.notFound.bundle model

        Play_Model model ->
            pages.play.bundle model

        Waiting_Model model ->
            pages.waiting.bundle model


view : Model -> Global.Model -> Document Msg
view model =
    bundle model >> .view


subscriptions : Model -> Global.Model -> Sub Msg
subscriptions model =
    bundle model >> .subscriptions
