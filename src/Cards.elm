module Cards exposing (..)

import Html as Html exposing (Html)
import Html.Attributes as HA
import Html.Entity
import Html.Events as HE
import Json.Encode as JE
import List exposing (..)
import Random
import Tuple exposing (..)


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias Card =
    { suit : Suit, rank : Rank }


suits : List Suit
suits =
    [ Clubs, Diamonds, Hearts, Spades ]


ranks : List Rank
ranks =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]



--
-- fullDeck : List Card
-- fullDeck =
--     suits |> concatMap fullSuit
--
--
-- fullSuit : Suit -> List Card
-- fullSuit suit =
--     map (Card suit) ranks
--


toRank : Card -> Rank
toRank { suit, rank } =
    rank


toSuit : Card -> Suit
toSuit { suit, rank } =
    suit


shuffleOf : List Card -> Random.Generator (List Card)
shuffleOf cards =
    randomListGenerator cards |> Random.map (shuffleList cards)


shuffleList : List a -> List Int -> List a
shuffleList toShuffle randomList =
    randomList
        |> map2 Tuple.pair toShuffle
        |> sortBy Tuple.second
        |> map Tuple.first


randomListGenerator : List a -> Random.Generator (List Int)
randomListGenerator list =
    Random.list
        (List.length list)
        (Random.int Random.minInt Random.maxInt)


resolveRank : Int -> Maybe Rank
resolveRank face =
    case face of
        1 ->
            Just Ace

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        10 ->
            Just Ten

        11 ->
            Just Jack

        12 ->
            Just Queen

        13 ->
            Just King

        _ ->
            Nothing


{-| Default resoltuion of Faces to integers, in A-K order.

    defaultFace Ace == 1

    defaultFace King == 13

-}
rankToInt : Rank -> Int
rankToInt face =
    case face of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13


rankStr : Rank -> String
rankStr rank =
    case rank of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"


viewCard2 : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> (Card -> msg) -> Card -> Html msg
viewCard2 innerWrapper onClickHandler ({ rank, suit } as card) =
    let
        ( suite, face, suitesEnyity ) =
            case suit of
                Spades ->
                    ( "spades", rankStr rank, Html.Entity.spades )

                Diamonds ->
                    ( "diams", rankStr rank, Html.Entity.diams )

                Clubs ->
                    ( "clubs", rankStr rank, Html.Entity.clubs )

                Hearts ->
                    ( "hearts", rankStr rank, Html.Entity.hearts )

        -- Back ->
        --     ( "back", "", Html.Entity.nbsp )
    in
    Html.li [ HE.onClick (onClickHandler card) ]
        [ innerWrapper [ HA.class "card", HA.class ("rank-" ++ String.toLower face), HA.class suite ]
            [ Html.span [ HA.class "rank" ] [ Html.text face ]
            , Html.span [ HA.class "suit" ]
                [ Html.text suitesEnyity ]
            ]
        ]


viewA : (Card -> msg) -> Card -> Html msg
viewA onClickHandler card =
    viewCard2 Html.a onClickHandler card


viewSpan : (Card -> msg) -> Card -> Html msg
viewSpan onClickHandler card =
    viewCard2 Html.span onClickHandler card


viewDiv : (Card -> msg) -> Card -> Html msg
viewDiv onClickHandler card =
    viewCard2 Html.div onClickHandler card


viewLabel : (Card -> msg) -> Card -> Html msg
viewLabel onClickHandler card =
    viewCard2 Html.label onClickHandler card


viewCardsDiv : (Card -> msg) -> List Card -> List (Html msg)
viewCardsDiv onClickHandler cardsList =
    List.map (viewDiv onClickHandler) (List.take 10 cardsList)


viewBlank : Html msg
viewBlank =
    Html.li []
        [ Html.span [ HA.class "card blank" ]
            [ Html.text "*"
            ]
        ]


viewDropzone : Html msg
viewDropzone =
    Html.li []
        [ Html.span [ HA.class "card dropzone" ]
            [ Html.text "*"
            ]
        ]


viewDropzoneActive : (Card -> msg) -> Card -> Html msg
viewDropzoneActive onDrop card =
    Html.li [ HE.onClick (onDrop card) ]
        [ Html.span [ HA.class "card active dropzone" ]
            [ Html.text "*"
            ]
        ]


setInnerHTML str =
    HA.property "innerHTML" (JE.string str)
