module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Cards
import Deck exposing (Deck)
import Html exposing (Html)
import Html.Attributes as HA
import Pile exposing (Pile)
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
    Html.div [ HA.class "main" ]
        [ Html.div [ HA.class "playingCards simpleCards suitTop rotateHand" ]
            [ Html.ul
                [ HA.class "twowaypile"
                ]
                (List.map Cards.viewA (List.take 10 cards))
            ]
        , Html.div [ HA.class "playingCards faceImages rotateHand" ]
            [ Html.h1 []
                [ Html.text "String.String" ]
            , Html.ul
                [ HA.class "hand"
                , HA.style "margin" "10em 0 0 0"
                ]
                (List.map Cards.viewA (List.take 10 cards))
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
