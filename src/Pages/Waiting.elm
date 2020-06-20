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
import Route
import Url exposing (Url)


type alias Flags =
    { gameId : String, gameUrl : Maybe String, gameCreator : Maybe String }


type alias Model =
    { gameId : String
    , gameUrl : Maybe String
    , gameCreator : Maybe String
    , pageState : PageState
    }


type Msg
    = PlayerJoined
    | SelectGameUrlInput
    | StartSelectingUsername
    | SetUserName String
    | UsernameSelected
    | SetPlayers (List String)
    | StartPlaying


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
            model
                |> Helpers.noneNone

        SelectGameUrlInput ->
            ( model, Ports.focus "url_input", Cmd.none )

        StartSelectingUsername ->
            { model | pageState = SelectingUsername }
                |> Helpers.noneNone

        SetUserName v ->
            ( model, Cmd.none, Global.setPlayerName v )

        UsernameSelected ->
            ( { model | pageState = Waiting }, Ports.usernameSelected global.myPlayerName, Cmd.none )

        SetPlayers players ->
            ( model, Cmd.none, Global.setPlayers players )

        StartPlaying ->
            ( model, Cmd.none, Global.navigate (Route.Play model.gameId) )


view : Global.Model -> Model -> Document Msg
view ({ gameDefinition } as global) model =
    let
        body =
            case model.pageState of
                NewGameCreated ->
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
                        , HE.onClick StartSelectingUsername
                        ]
                        [ text "Copy To Clipboard"
                        ]
                    ]

                SelectingUsername ->
                    [ h2 [] [ text "Select your username" ]
                    , input [ HE.onInput SetUserName, HA.value global.myPlayerName, HA.placeholder "Batman" ] []
                    , button [ HE.onClick UsernameSelected, HA.disabled (Helpers.isBlank global.myPlayerName) ] [ text "Start" ]
                    ]

                Waiting ->
                    [ h2 [] [ text "Waiting for all players to join" ]
                    , h3 [] [ text "Joined players" ]
                    , ul [] (List.map (\player -> li [] [ text player ]) global.joinedPlayers)
                    ]
                        ++ (case ( List.length global.joinedPlayers == gameDefinition.numberOfPlayers, Maybe.Extra.isJust model.gameCreator ) of
                                ( True, True ) ->
                                    [ h3 [] [ text "All players have joined! Yay!!" ]
                                    , button [ HE.onClick StartPlaying ] [ text "Start Playing" ]
                                    ]

                                ( True, False ) ->
                                    [ h3 [] [ text "All players have joined! Yay!!" ]
                                    , h4 [] [ text "Ask game creator to start the Game already!" ]
                                    ]

                                _ ->
                                    []
                           )
    in
    { title = "Sara's Cards"
    , body = body
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
            }
    in
    ( model, Cmd.none, Cmd.none )
