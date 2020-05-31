module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Helpers
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Pile exposing (Pile)
import Player exposing (Player)
import Random



---- MODEL ----


type alias Model =
    { gameDefinition : GameDefinition
    , playState : PlayState
    }


type alias GameDefinition =
    { numberOfPlayers : Int
    , numberOfDecks : Int
    , numberOfPiles : Int
    }


type alias PlayState =
    { players : List Player
    , piles : List Pile
    , thisPlayer : Player
    }


initGameDefinition =
    { numberOfPlayers = 4
    , numberOfDecks = 1
    , numberOfPiles = 4
    }


initPlayState gameDefinition =
    let
        players =
            Helpers.makeListOf gameDefinition.numberOfPlayers (\n -> Player ("Player " ++ String.fromInt n) [])
    in
    { players = players
    , piles = Helpers.makeListOf gameDefinition.numberOfPiles (\n -> Pile.newTwoWayPile [])
    , thisPlayer =
        players
            |> List.Extra.getAt 0
            |> Maybe.withDefault Player.default
    }


init : ( Model, Cmd Msg )
init =
    let
        gameDefinition =
            initGameDefinition
    in
    ( { gameDefinition = gameDefinition
      , playState = initPlayState gameDefinition
      }
    , shuffle gameDefinition
    )


shuffle { numberOfDecks } =
    Random.generate ShuffleDeck (Deck.randomDeck numberOfDecks)



---- UPDATE ----


type Msg
    = ShuffleDeck Deck
    | CardSelected Card
    | Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ playState, gameDefinition } as model) =
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
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffle gameDefinition )


setPlayState playState model =
    { model | playState = playState }


setGameDefinition gameDefinition model =
    { model | gameDefinition = gameDefinition }



---- VIEW ----


view : Model -> Html Msg
view ({ playState, gameDefinition } as model) =
    Html.div [ HA.class "main" ]
        [ --  Html.div [ HA.class "playingCards simpleCards suitTop rotateHand" ]
          --     [ Html.ul
          --         [ HA.class "twowaypile"
          --         ]
          --         (List.map (Cards.viewA CardSelected) (List.take 5 cards))
          --     , Html.button [ HE.onClick Shuffle ] [ Html.text "Shuffle" ]
          --     ]
          -- ,
          Html.div [ HA.class "piles" ]
            (List.map viewPile playState.piles)
        , Html.div [ HA.class "players" ]
            (List.map viewPlayer playState.players)
        ]


viewPile pile =
    Html.div []
        [ Html.div [ HA.class "pile playingCards faceImages" ]
            [ Pile.view CardSelected pile
            ]
        ]


viewPlayer player =
    Html.div []
        [ Html.div [ HA.class "player playingCards faceImages" ]
            [ Html.h2 []
                [ Html.text player.name ]
            , Html.h3 []
                [ player.cards |> List.length |> String.fromInt |> Html.text ]
            , Html.ul
                [ HA.class "hand"
                , HA.style "margin" "0 0 0 0"
                ]
                (List.map (Cards.viewA CardSelected) player.cards)
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
