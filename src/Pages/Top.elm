module Pages.Top exposing (Flags, Model, Msg, page)

import Browser
import Global
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Page exposing (Document, Page)
import Ports
import Route
import Url exposing (Url)


type alias Flags =
    ()


type alias Model =
    { newGameId : String }


type Msg
    = StartNewGame
    | NewGameCreated String


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
            ( model
            , Cmd.none
            , Global.navigate (Route.Waiting gameId (Just (global.flags.url ++ "/" ++ gameId ++ "/waiting")) (Just "1"))
            )


view : Global.Model -> Model -> Document Msg
view global model =
    let
        btnOrCopy =
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
        newGameId =
            case global.flags.gameId of
                Just gameId ->
                    gameId

                Nothing ->
                    ""
    in
    ( { newGameId = newGameId }, Cmd.none, Cmd.none )
