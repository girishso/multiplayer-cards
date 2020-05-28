module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Html exposing (Html)
import Html.Attributes as HA
import Pile exposing (Pile)
import Random



---- MODEL ----


type alias Model =
    { deck : Deck }


init : ( Model, Cmd Msg )
init =
    let
        deck =
            Deck.fullDeck
    in
    ( { deck = deck }
    , Random.generate ShuffleDeck Deck.randomDeck
    )



---- UPDATE ----


type Msg
    = ShuffleDeck Deck
    | CardSelected Card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck deck ->
            ( { model | deck = deck }, Cmd.none )

        CardSelected card ->
            let
                _ =
                    Debug.log "card" card
            in
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cards =
            Deck.getCards model.deck
    in
    Html.div [ HA.class "main" ]
        [ Html.div [ HA.class "playingCards simpleCards suitTop rotateHand" ]
            [ Html.ul
                [ HA.class "twowaypile"
                ]
                (List.map (Cards.viewA CardSelected) (List.take 10 cards))
            ]
        , Html.div [ HA.class "playingCards faceImages rotateHand" ]
            [ Html.h1 []
                [ Html.text "String.String" ]
            , Html.ul
                [ HA.class "hand"
                , HA.style "margin" "10em 0 0 0"
                ]
                (List.map (Cards.viewA CardSelected) (List.take 13 cards))
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
