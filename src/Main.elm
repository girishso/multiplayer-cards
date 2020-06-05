module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Helpers
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Id exposing (playerId)
import List.Extra
import Maybe.Extra
import Pile exposing (Pile)
import Player exposing (Player)
import Random



---- MODEL ----


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
    }


type alias LocalState =
    { thisPlayerIx : Int
    , selectedCard : Maybe Card
    }



---- UPDATE ----


type Msg
    = ShuffleDeck Deck
    | CardSelected Card
    | CardDroppedOnPile Pile Pile.HeadOrTail Card
    | Shuffle


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
            let
                _ =
                    Debug.log "card" card
            in
            ( setLocalState { localState | selectedCard = Just card } model, Cmd.none )

        CardDroppedOnPile pile headOrTail card ->
            let
                ( newPlayer, newPile ) =
                    Player.dropCardOnPile card headOrTail pile (getThisPlayer playState.players localState)
            in
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffle gameDefinition )


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


getSelectedCard : Model -> Maybe Card
getSelectedCard { localState } =
    localState.selectedCard


getThisPlayer : List Player -> LocalState -> Player
getThisPlayer players localState =
    players
        |> List.Extra.getAt localState.thisPlayerIx
        |> Maybe.withDefault Player.default



---- VIEW ----


view : Model -> Html Msg
view ({ playState, gameDefinition } as model) =
    Html.div [ HA.class "main" ]
        [ Html.div [ HA.class "piles" ]
            (List.map (viewPile model) playState.piles)
        , Html.div [ HA.class "players" ]
            (List.map viewPlayer playState.players)
        ]


viewPile : Model -> Pile -> Html Msg
viewPile model pile =
    Html.div []
        [ Html.div [ HA.class "pile playingCards faceImages" ]
            [ Pile.view CardSelected CardDroppedOnPile (getSelectedCard model) pile
            ]
        ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    Html.div []
        [ Html.div [ HA.class "player playingCards faceImages" ]
            [ Html.h3 []
                [ Html.text player.name
                , Html.text "("
                , player.cards |> List.length |> String.fromInt |> Html.text
                , Html.text ")"
                ]
            , Html.ul
                [ HA.class "hand"
                , HA.style "margin" "0 0 0 0"
                ]
                (List.map (Cards.viewA CardSelected) player.cards)
            ]
        ]



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
    }


initLocalState : PlayState -> LocalState
initLocalState { players } =
    { thisPlayerIx = 0
    , selectedCard = Nothing
    }


init : ( Model, Cmd Msg )
init =
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
    , shuffle gameDefinition
    )


shuffle : { a | numberOfDecks : Int } -> Cmd Msg
shuffle { numberOfDecks } =
    Random.generate ShuffleDeck (Deck.randomDeck numberOfDecks)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
