module Pages.Waiting exposing (Flags, Model, Msg, page)

import Browser
import Global
import Helpers
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Maybe.Extra
import Page exposing (Document, Page)
import Ports
import Url exposing (Url)


type alias Flags =
    { gameId : String, gameUrl : Maybe String, gameCreator : Maybe String }


type alias Model =
    { gameId : String
    , gameUrl : Maybe String
    , gameCreator : Maybe String
    , pageState : PageState
    , userName : String
    , joinedPlayers : List String
    }


type Msg
    = PlayerJoined
    | SelectGameUrlInput
    | StartWaiting
    | SetUserName String
    | UsernameSelected
    | SetPlayers (List String)


type PageState
    = NewGameCreated
    | SelectingUsername
    | Waiting


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

        StartWaiting ->
            ( { model | pageState = SelectingUsername }, Cmd.none, Cmd.none )

        SetUserName v ->
            ( { model | userName = v }, Cmd.none, Cmd.none )

        UsernameSelected ->
            ( { model | pageState = Waiting }, Ports.usernameSelected model.userName, Cmd.none )

        SetPlayers players ->
            ( { model | joinedPlayers = players }, Cmd.none, Cmd.none )


view : Global.Model -> Model -> Document Msg
view global model =
    let
        copyOrWaiting =
            case model.pageState of
                NewGameCreated ->
                    div []
                        [ h2 [] [ text "Successfully created new Game. Copy and share the Game url." ]
                        , input
                            [ HA.readonly True
                            , HA.id "url_input"
                            , HE.onClick SelectGameUrlInput
                            , HA.value (model.gameUrl |> Maybe.withDefault "xx")
                            , HA.style "width" "30%"
                            ]
                            []
                        , br [] []
                        , br [] []
                        , button
                            [ HA.id "copy_url_btn"
                            , HA.attribute "data-clipboard-target" "#url_input"
                            , HE.onClick StartWaiting
                            ]
                            [ text "Copy To Clipboard"
                            ]
                        ]

                SelectingUsername ->
                    div []
                        [ h2 [] [ text "Select your username" ]
                        , input [ HE.onInput SetUserName, HA.value model.userName, HA.placeholder "Batman" ] []
                        , button [ HE.onClick UsernameSelected, HA.disabled (Helpers.isBlank model.userName) ] [ text "Start" ]
                        ]

                Waiting ->
                    h2 [] [ text "Waiting for all players to join" ]
    in
    { title = "Sara Cards"
    , body =
        [ copyOrWaiting
        ]
    }


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Ports.setPlayers SetPlayers


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init global flags =
    let
        model =
            { gameId = flags.gameId
            , gameUrl = flags.gameUrl
            , gameCreator = flags.gameCreator
            , pageState =
                if Maybe.Extra.isNothing flags.gameCreator then
                    SelectingUsername

                else
                    NewGameCreated
            , userName = ""
            , joinedPlayers = []
            }
    in
    ( model, Cmd.none, Cmd.none )
