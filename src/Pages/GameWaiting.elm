module Pages.GameWaiting exposing (Flags, Model, Msg, page)

import Browser
import Global
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Page exposing (Document, Page)
import Ports
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { gameUrl : String }


type Msg
    = StartNewGame
    | NewGameCreated String
    | SelectGameUrlInput
    | GotoGame


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg model =
    case msg of
        StartNewGame ->
            ( model, Ports.createNewGame "String", Cmd.none )

        NewGameCreated gameId ->
            ( { model | gameUrl = global.flags.url ++ gameIdParam ++ gameId }, Cmd.none, Cmd.none )

        SelectGameUrlInput ->
            ( model, Ports.focus "url_input", Cmd.none )

        GotoGame ->
            ( model, Cmd.none, Cmd.none )


gameIdParam : String
gameIdParam =
    "?game_id="


view : Global.Model -> Model -> Document Msg
view global model =
    let
        btnOrCopy =
            if String.contains gameIdParam model.gameUrl then
                div []
                    [ input
                        [ HA.readonly True
                        , HA.id "url_input"
                        , HE.onClick SelectGameUrlInput
                        , HA.value model.gameUrl
                        , HA.style "width" "30%"
                        ]
                        []
                    , br [] []
                    , br [] []
                    , button
                        [ HA.id "copy_url_btn"
                        , HA.attribute "data-clipboard-target" "#url_input"
                        , HA.attribute "data-clipboard-text" model.gameUrl
                        , HE.onClick GotoGame
                        ]
                        [ text "Copy To Clipboard"
                        ]
                    ]

            else
                button [ HE.onClick StartNewGame ] [ text "Start New Game" ]
    in
    { title = "Sara Cards"
    , body =
        [ h1 []
            [ text "Welcome to the cards game!"
            ]
        , btnOrCopy
        ]
    }


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Ports.newGameCreated NewGameCreated


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        gameUrl =
            case global.flags.gameId of
                Just gameId ->
                    global.flags.url ++ gameIdParam ++ gameId

                Nothing ->
                    global.flags.url
    in
    ( { gameUrl = gameUrl }, Cmd.none, Cmd.none )
