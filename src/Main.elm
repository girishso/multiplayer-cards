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
    { deck : Deck, players : List Player }


numberOfPlayers =
    2


init : ( Model, Cmd Msg )
init =
    let
        deck =
            Deck.fullDeck

        players =
            List.map (\n -> Player ("Player " ++ String.fromInt n) []) (List.range 1 numberOfPlayers)

        _ =
            players
                |> List.map .name
                |> Debug.log "playersxx"
    in
    ( { deck = deck, players = players }
    , shuffle
    )


shuffle =
    Random.generate ShuffleDeck Deck.randomDeck



---- UPDATE ----


type Msg
    = ShuffleDeck Deck
    | CardSelected Card
    | Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck deck ->
            let
                players =
                    Deck.distribute2 model.players deck

                -- _ =
                --     cards |> List.length |> Debug.log "distribute"
                -- players =
                --     List.map2 (\player cards_ -> { player | cards = cards_ }) model.players cards
            in
            ( { model | players = players }, Cmd.none )

        CardSelected card ->
            let
                _ =
                    Debug.log "card" card
            in
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffle )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cards =
            Deck.getCards model.deck
    in
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
            (List.map viewPlayer model.players)
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
