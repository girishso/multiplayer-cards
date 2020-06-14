module Pages.Play exposing (Flags, Model, Msg, page)

import Browser
import Browser.Dom
import Browser.Events
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Helpers
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Id exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import List.Extra
import Maybe.Extra
import Page exposing (Document, Page)
import Pile exposing (Pile)
import Player exposing (Player)
import Random
import Task
import Time
import Types


type alias Flags =
    ()


type alias Model =
    { gameDefinition : GameDefinition
    , playState : PlayState
    , localState : LocalState
    }


type alias GameDefinition =
    { numberOfPlayers : Int
    , numberOfDecks : Int
    , numberOfPiles : Int
    }


type alias PlayState =
    { players : List Player
    , piles : List Pile
    , currentPlayerIx : Int
    }


type alias LocalState =
    { selfPlayerIx : Int
    , selectedCard : Maybe Card
    , windowHeight : Int
    , windowWidth : Int
    }


type Msg
    = ShuffleDeck Deck
    | CardSelected Card
    | CardDroppedOnPile Pile Types.HeadOrTail Card
    | Shuffle
    | WindowResized Int Int
    | OnTime Time.Posix
    | OnViewport Browser.Dom.Viewport


page : Page Flags Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ playState, gameDefinition, localState } as model) =
    case msg of
        ShuffleDeck deck ->
            let
                players =
                    Deck.distribute2 playState.players deck
            in
            ( setPlayState { playState | players = players } model, Cmd.none )

        CardSelected card ->
            ( setLocalState { localState | selectedCard = Just card } model, Cmd.none )

        CardDroppedOnPile pile headOrTail card ->
            let
                newPlayState =
                    Player.dropCardOnPile card headOrTail pile (getCurrentPlayer playState)
                        |> Tuple.mapBoth (\newPlayer -> List.Extra.updateAt (rawPlayerId newPlayer.id) (always newPlayer) playState.players)
                            (\newPile -> Pile.updatePile newPile playState.piles)
                        |> (\( newPlayers, newPiles ) ->
                                { players = newPlayers
                                , piles = newPiles
                                , currentPlayerIx = setNextPlayerIx newPlayers playState.currentPlayerIx
                                }
                           )
            in
            ( model
                |> setLocalState { localState | selectedCard = Nothing, selfPlayerIx = setNextPlayerIx newPlayState.players localState.selfPlayerIx }
                |> setPlayState newPlayState
            , Cmd.none
            )

        Shuffle ->
            ( model, shuffle gameDefinition )

        WindowResized w h ->
            let
                _ =
                    Debug.log "WindowResized" ( w, h )
            in
            ( model |> setLocalState { localState | windowWidth = w, windowHeight = h }, Cmd.none )

        OnViewport viewport ->
            let
                _ =
                    Debug.log "viewport" viewport
            in
            ( model, Cmd.none )

        OnTime posix ->
            let
                _ =
                    Debug.log "posix" posix
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResized


setNextPlayerIx : List Player -> Int -> Int
setNextPlayerIx players currentPlayerIx =
    if List.length players == currentPlayerIx + 1 then
        0

    else
        currentPlayerIx + 1


setPlayState : a -> { b | playState : a } -> { b | playState : a }
setPlayState v model =
    { model | playState = v }


setLocalState : a -> { b | localState : a } -> { b | localState : a }
setLocalState v model =
    { model | localState = v }


setGameDefinition : a -> { b | gameDefinition : a } -> { b | gameDefinition : a }
setGameDefinition v model =
    { model | gameDefinition = v }


isCardSelected : Model -> Bool
isCardSelected { localState } =
    Maybe.Extra.isJust localState.selectedCard


isSelfPlayersTurn : PlayState -> LocalState -> Bool
isSelfPlayersTurn playState localState =
    localState.selfPlayerIx == playState.currentPlayerIx


getSelectedCard : Model -> Maybe Card
getSelectedCard { localState } =
    localState.selectedCard


getSelfPlayer : List Player -> LocalState -> Player
getSelfPlayer players localState =
    players
        |> List.Extra.getAt localState.selfPlayerIx
        |> Maybe.withDefault Player.default


getCurrentPlayer : PlayState -> Player
getCurrentPlayer playState =
    playState.players
        |> List.Extra.getAt playState.currentPlayerIx
        |> Maybe.withDefault Player.default



---- VIEW ----


view : Model -> Browser.Document Msg
view ({ playState, gameDefinition, localState } as model) =
    { title = "URL Interceptor"
    , body =
        [ Html.div
            [ HA.class "main" ]
            [ div
                (mainDivsHelper { w = 100, h = 10, t = 0, l = 0, c = "top-row" })
                [ "Current player: " ++ (getCurrentPlayer playState).name |> text ]
            , div (mainDivsHelper { w = 100, h = 32, t = 10, l = 0, c = "top-player-row player-container" })
                (List.filter (\p -> rawPlayerId p.id == 2) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            , div (mainDivsHelper { w = 20, h = 80, t = 10, l = 2, c = "mid-player-left rotate-270 player-container" })
                (List.filter (\p -> rawPlayerId p.id == 3) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            , div (mainDivsHelper { w = 60, h = 50, t = 30, l = 20, c = "piles-container" }) (List.map (viewPile model) playState.piles)
            , div (mainDivsHelper { w = 20, h = 80, t = 10, l = 77, c = "mid-player-right rotate-90 player-container" })
                (List.filter (\p -> rawPlayerId p.id == 1) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            , div (mainDivsHelper { w = 100, h = 30, t = 70, l = 0, c = "bottom-player-row player-container" })
                (List.filter (\p -> rawPlayerId p.id == 0) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            ]
        ]
    }


viewPile : Model -> Pile -> Html Msg
viewPile ({ localState, playState } as model) pile =
    let
        viewPile_ pile_ =
            List.singleton <|
                case isSelfPlayersTurn playState localState of
                    True ->
                        Pile.view CardSelected CardDroppedOnPile (getSelectedCard model) pile

                    False ->
                        Pile.viewOnly pile
    in
    Html.div []
        [ Html.div [ HA.class "pile playingCards faceImages suitTop" ]
            (viewPile_ pile)
        ]


viewPlayer : LocalState -> PlayState -> Int -> Player -> Html Msg
viewPlayer localState playState playerIx player =
    let
        viewCards =
            case ( playerIx == localState.selfPlayerIx, isSelfPlayersTurn playState localState ) of
                ( True, True ) ->
                    Player.viewSpanWithClick CardSelected localState.selectedCard player

                ( True, False ) ->
                    Player.viewSpanNoClick player

                ( _, _ ) ->
                    Player.viewBack player
    in
    Html.div []
        [ Html.div [ HA.class "player playingCards faceImages" ]
            [ Html.ul
                [ HA.class "hand"
                ]
                viewCards
            ]
        , text player.name
        ]


mainDivsHelper : { h : Int, l : Int, t : Int, w : Int, c : String } -> List (Attribute msg)
mainDivsHelper { w, h, t, l, c } =
    let
        toPercent i =
            String.fromInt i ++ "%"
    in
    [ HA.style "width" (toPercent w)
    , HA.style "height" (toPercent h)
    , HA.style "top" (toPercent t)
    , HA.style "left" (toPercent l)
    , HA.style "position" "absolute"
    , HA.class c
    ]


shuffle : { a | numberOfDecks : Int } -> Cmd Msg
shuffle { numberOfDecks } =
    Random.generate ShuffleDeck (Deck.randomDeck numberOfDecks)



---- inits


initGameDefinition : GameDefinition
initGameDefinition =
    { numberOfPlayers = 4
    , numberOfDecks = 1
    , numberOfPiles = 4
    }


initPlayState : GameDefinition -> PlayState
initPlayState gameDefinition =
    { players = Helpers.makeListOf gameDefinition.numberOfPlayers (\ix n -> Player (playerId ix) ("Player " ++ String.fromInt n) [])
    , piles = Helpers.makeListOf gameDefinition.numberOfPiles (\ix _ -> Pile.newTwoWayPile ix [])
    , currentPlayerIx = 0
    }


initLocalState : PlayState -> LocalState
initLocalState { players } =
    { selfPlayerIx = 0
    , selectedCard = Nothing
    , windowHeight = 0
    , windowWidth = 0
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        gameDefinition =
            initGameDefinition

        playState =
            initPlayState gameDefinition
    in
    ( { gameDefinition = gameDefinition
      , playState = playState
      , localState = initLocalState playState
      }
    , Cmd.batch
        [ shuffle gameDefinition
        , getWindowSize
        ]
    )


getWindowSize : Cmd Msg
getWindowSize =
    Task.perform OnViewport Browser.Dom.getViewport



---- encoders / decoders ---------


playStateEncoder : PlayState -> Encode.Value
playStateEncoder v =
    Encode.object
        [ ( "players"
          , Encode.list Player.encoder v.players
          )
        , ( "piles"
          , Encode.list Pile.encoder v.piles
          )
        ]


playStateDecoder : Decode.Decoder PlayState
playStateDecoder =
    Decode.map3 PlayState
        (Decode.field "players" (Decode.list Player.decoder))
        (Decode.field "piles" (Decode.list Pile.decoder))
        (Decode.field "currentPlayerIx" Decode.int)


gameDefinitionEncoder : GameDefinition -> Encode.Value
gameDefinitionEncoder v =
    Encode.object
        [ ( "numberOfPlayers", Encode.int v.numberOfPlayers )
        , ( "numberOfDecks", Encode.int v.numberOfDecks )
        , ( "numberOfPiles", Encode.int v.numberOfPiles )
        ]


gameDefinitionDecoder : Decode.Decoder GameDefinition
gameDefinitionDecoder =
    Decode.map3 GameDefinition
        (Decode.field "numberOfPlayers" Decode.int)
        (Decode.field "numberOfDecks" Decode.int)
        (Decode.field "numberOfPiles" Decode.int)


modelEncoder : Model -> Encode.Value
modelEncoder v =
    Encode.object
        [ ( "gameDefinition", gameDefinitionEncoder v.gameDefinition )
        , ( "playState", playStateEncoder v.playState )
        ]


modelDecoder : Decode.Decoder (LocalState -> Model)
modelDecoder =
    Decode.map2 Model
        (Decode.field "gameDefinition" gameDefinitionDecoder)
        (Decode.field "playState" playStateDecoder)
