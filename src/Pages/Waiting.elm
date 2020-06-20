module Pages.Waiting exposing (Flags, Model, Msg, page)

import Browser
import Global
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Page exposing (Document, Page)
import Ports
import Url exposing (Url)


type alias Flags =
    { gameId : String, gameUrl : Maybe String }


type alias Model =
    { gameId : String, gameUrl : Maybe String }


type Msg
    = PlayerJoined
    | SelectGameUrlInput


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
        PlayerJoined ->
            ( model, Cmd.none, Cmd.none )

        SelectGameUrlInput ->
            ( model, Ports.focus "url_input", Cmd.none )


view : Global.Model -> Model -> Document Msg
view global model =
    { title = "Sara Cards"
    , body =
        [ h1 []
            [ text "Welcome to the cards game!"
            ]
        , h2 [] [ text "Waiting for all players to join" ]
        , div []
            [ input
                [ HA.readonly True
                , HA.id "url_input"
                , HE.onClick SelectGameUrlInput
                , HA.value (model.gameUrl |> Maybe.withDefault "xx")
                , HA.style "width" "20%"
                ]
                []
            , br [] []
            , br [] []
            , button
                [ HA.id "copy_url_btn"
                , HA.attribute "data-clipboard-target" "#url_input"

                -- , HA.attribute "data-clipboard-text" model.gameUrl
                ]
                [ text "Copy To Clipboard"
                ]
            ]
        ]
    }


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.none


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    ( flags, Cmd.none, Cmd.none )
