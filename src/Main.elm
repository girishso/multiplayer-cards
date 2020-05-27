module Main exposing (..)

import Browser
import Cards
import Deck exposing (Deck)
import Html exposing (Html)
import Html.Attributes as HA
import Random



---- MODEL ----


type alias Model =
    { deck : Deck.ShuffledDeck }


init : ( Model, Cmd Msg )
init =
    ( { deck = Deck.fullDeck }
    , Random.generate ShuffleDeck Deck.randomDeck
    )



---- UPDATE ----


type Msg
    = ShuffleDeck Deck.ShuffledDeck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck shuffledDeckDeck ->
            ( Model shuffledDeckDeck, Cmd.none )
                |> Debug.log "mcmd"



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cards =
            Deck.getCards model.deck
                |> Debug.log "md"
    in
    Html.div []
        [ Html.h1 []
            [ Html.text "String.String" ]
        , Html.div
            [ HA.class "playingCards faceImages rotateHand hand"
            ]
            (List.map Cards.viewCard2 (List.take 10 cards))
        ]



-- (case model.deck of
--     Deck.ShuffledDeck (Deck.Deck x) ->
--         List.map Debug.toString deck
-- )
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
