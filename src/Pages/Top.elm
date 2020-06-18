module Pages.Top exposing (Flags, Model, Msg, page)

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Page exposing (Document, Page)
import Ports
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { gameUrl : Maybe String }


type Msg
    = StartNewGame
    | NewGameCreated String
    | SelectGameUrlInput


page : Page Flags Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartNewGame ->
            ( model, Ports.createNewGame "String" )

        NewGameCreated gameUrl ->
            ( { model | gameUrl = Just gameUrl }, Cmd.none )

        SelectGameUrlInput ->
            ( model, Ports.focus "url_input" )


view : Model -> Browser.Document Msg
view model =
    let
        btnOrCopy =
            case model.gameUrl of
                Just url ->
                    div []
                        [ input
                            [ HA.readonly True
                            , HA.id "url_input"
                            , HE.onClick SelectGameUrlInput
                            , HA.value url
                            , HA.style "width" "20%"
                            ]
                            []
                        , br [] []
                        , br [] []
                        , button
                            [ HA.id "copy_url_btn"
                            , HA.attribute "data-clipboard-target" "#url_input"
                            , HA.attribute "data-clipboard-text" url
                            ]
                            [ text "Copy To Clipboard"
                            ]
                        ]

                Nothing ->
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.newGameCreated NewGameCreated


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gameUrl = Nothing }, Cmd.none )
