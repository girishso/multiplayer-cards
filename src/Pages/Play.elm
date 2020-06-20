module Pages.Play exposing (Flags, Model, Msg, page)

import Browser
import Browser.Dom
import Browser.Events
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Global
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
    { gameId : String }


type alias Model =
    { playState : PlayState
    , localState : LocalState
    }


type alias PlayState =
    { players : List Player
    , piles : List Pile
    , currentPlayerIx : Int
    }


type alias LocalState =
    { myIx : Int
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
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update ({ gameDefinition } as global) msg ({ playState, localState } as model) =
    case msg of
        ShuffleDeck deck ->
            let
                players =
                    Deck.distribute2 playState.players deck
            in
            ( setPlayState { playState | players = players } model, Cmd.none, Cmd.none )

        CardSelected card ->
            ( setLocalState { localState | selectedCard = Just card } model, Cmd.none, Cmd.none )

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
                |> setLocalState { localState | selectedCard = Nothing, myIx = setNextPlayerIx newPlayState.players localState.myIx }
                |> setPlayState newPlayState
            , Cmd.none
            , Cmd.none
            )

        Shuffle ->
            ( model, shuffle gameDefinition, Cmd.none )

        WindowResized w h ->
            let
                _ =
                    Debug.log "WindowResized" ( w, h )
            in
            ( model |> setLocalState { localState | windowWidth = w, windowHeight = h }, Cmd.none, Cmd.none )

        OnViewport viewport ->
            let
                _ =
                    Debug.log "viewport" viewport
            in
            ( model, Cmd.none, Cmd.none )

        OnTime posix ->
            let
                _ =
                    Debug.log "posix" posix
            in
            ( model, Cmd.none, Cmd.none )


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
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


isCardSelected : Model -> Bool
isCardSelected { localState } =
    Maybe.Extra.isJust localState.selectedCard


isSelfPlayersTurn : PlayState -> LocalState -> Bool
isSelfPlayersTurn playState localState =
    localState.myIx == playState.currentPlayerIx


getSelectedCard : Model -> Maybe Card
getSelectedCard { localState } =
    localState.selectedCard


getSelfPlayer : List Player -> LocalState -> Player
getSelfPlayer players localState =
    players
        |> List.Extra.getAt localState.myIx
        |> Maybe.withDefault Player.default


getCurrentPlayer : PlayState -> Player
getCurrentPlayer playState =
    playState.players
        |> List.Extra.getAt playState.currentPlayerIx
        |> Maybe.withDefault Player.default



---- VIEW ----


view : Global.Model -> Model -> Browser.Document Msg
view ({ gameDefinition } as global) ({ playState, localState } as model) =
    { title = "URL Interceptor"
    , body =
        [ Html.div
            [ HA.class "main" ]
            [ div ({ c = "top-row", w = 100, h = 10, t = 0, l = 0 } |> mainDivsHelper)
                [ "Current player: " ++ (getCurrentPlayer playState).name |> text ]
            , div ({ c = "top-player player-container rotate-180", w = 100, h = 10, t = 10, l = 0 } |> mainDivsHelper)
                (List.filter (\p -> rawPlayerId p.id == 2) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            , div ({ c = "left-player rotate-270x player-container", w = 30, h = 15, t = 20, l = 5 } |> mainDivsHelper)
                (List.filter (\p -> rawPlayerId p.id == 3) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            , div ({ c = "piles-container", w = 60, h = 40, t = 20, l = 22 } |> mainDivsHelper)
                (List.map (viewPile model) playState.piles)
            , div ({ c = "right-player rotate-90x player-container", w = 30, h = 15, t = 20, l = 100 } |> mainDivsHelper)
                (List.filter (\p -> rawPlayerId p.id == 1) playState.players
                    |> List.map (\p -> viewPlayer localState playState (rawPlayerId p.id) p)
                )
            , div ({ c = "bottom-player player-container", w = 55, h = 30, t = 60, l = 22 } |> mainDivsHelper)
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
            case ( playerIx == localState.myIx, isSelfPlayersTurn playState localState ) of
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

        -- , text player.name
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


initPlayState : Global.GameDefinition -> PlayState
initPlayState gameDefinition =
    { players = Helpers.makeListOf gameDefinition.numberOfPlayers (\ix n -> Player (playerId ix) ("Player " ++ String.fromInt n) [])
    , piles = Helpers.makeListOf gameDefinition.numberOfPiles (\ix _ -> Pile.newTwoWayPile ix [])
    , currentPlayerIx = 0
    }


initLocalState : PlayState -> LocalState
initLocalState { players } =
    { myIx = 0
    , selectedCard = Nothing
    , windowHeight = 0
    , windowWidth = 0
    }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init ({ gameDefinition } as global) flags =
    let
        playState =
            initPlayState gameDefinition
    in
    ( { playState = playState
      , localState = initLocalState playState
      }
    , Cmd.batch
        [ shuffle gameDefinition
        , getWindowSize
        ]
    , Cmd.none
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


modelEncoder : Model -> Encode.Value
modelEncoder v =
    Encode.object
        [ ( "playState", playStateEncoder v.playState )
        ]


modelDecoder : Decode.Decoder (LocalState -> Model)
modelDecoder =
    Decode.map Model
        (Decode.field "playState" playStateDecoder)
