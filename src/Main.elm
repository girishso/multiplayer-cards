module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
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
    }


initGameDefinition =
    { numberOfPlayers = 3
    , numberOfDecks = 2
    , numberOfPiles = 4
    }


initPlayState gameDefinition =
    { players = makeListOf gameDefinition.numberOfPlayers (\n -> Player ("Player " ++ String.fromInt n) [])
    , piles = makeListOf gameDefinition.numberOfPiles (\n -> Pile.newTwoWayPile [])
    }


init : ( Model, Cmd Msg )
init =
    let
        deck =
            Deck.fullDeck

        gameDefinition =
            initGameDefinition
    in
    ( { gameDefinition = gameDefinition
      , playState = initPlayState gameDefinition
      }
    , shuffle
    )


makeListOf n f =
    List.map f (List.range 1 n)


shuffle =
    Random.generate ShuffleDeck Deck.randomDeck



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

                -- _ =
                --     cards |> List.length |> Debug.log "distribute"
                -- players =
                --     List.map2 (\player cards_ -> { player | cards = cards_ }) model.players cards
            in
            ( setPlayState { playState | players = players } model, Cmd.none )

        CardSelected card ->
            let
                _ =
                    Debug.log "card" card
            in
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffle )


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
          Html.div [ HA.class "players" ]
            (List.map viewPlayer playState.players)
        ]


viewPlayer player =
    Html.div []
        [ Html.div [ HA.class "player playingCards faceImages rotateHand" ]
            [ Html.h1 []
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
