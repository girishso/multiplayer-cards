module Cards exposing (..)

import Html as Html exposing (Html)
import Html.Attributes as HA
import Html.Entity
import Html.Events as HE
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import List exposing (..)
import Maybe.Extra
import Random
import Tuple exposing (..)
import Types exposing (HeadOrTail)


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


eql : Card -> Card -> Bool
eql a b =
    (a.suit == b.suit) && (a.rank == b.rank)


maybeEql : Maybe Card -> Card -> Bool
maybeEql ma b =
    ma
        |> Maybe.map (\a -> (a.suit == b.suit) && (a.rank == b.rank))
        |> Maybe.withDefault False


viewCard2 : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> Maybe (Card -> msg) -> Maybe Card -> Card -> Html msg
viewCard2 innerWrapper maybeOnClickHandler maybeSelectedCard ({ rank, suit } as card) =
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

        attrs =
            maybeOnClickHandler |> Maybe.map (\x -> [ HE.onClick (x card) ]) |> Maybe.withDefault []

        classes =
            HA.classList
                [ ( "rank-" ++ String.toLower face, True )
                , ( suite, True )
                , ( "selected", maybeEql maybeSelectedCard card )
                ]

        -- Back ->
        --     ( "back", "", Html.Entity.nbsp )
    in
    Html.li attrs
        [ innerWrapper [ HA.class "card", classes ]
            [ Html.span [ HA.class "rank" ] [ Html.text face ]
            , Html.span [ HA.class "suit" ]
                [ Html.text suitesEnyity ]
            ]
        ]


viewA : (Card -> msg) -> Maybe Card -> Card -> Html msg
viewA onClickHandler maybeSelectedCard card =
    viewCard2 Html.a (Just onClickHandler) maybeSelectedCard card


viewSpanWithClick : (Card -> msg) -> Maybe Card -> Card -> Html msg
viewSpanWithClick onClickHandler maybeSelectedCard card =
    viewCard2 Html.label (Just onClickHandler) maybeSelectedCard card


viewSpan : (Card -> msg) -> Card -> Html msg
viewSpan onClickHandler card =
    viewCard2 Html.span (Just onClickHandler) Nothing card


viewSpanNoClick : Card -> Html msg
viewSpanNoClick card =
    viewCard2 Html.span Nothing Nothing card



--
-- viewDiv : (Card -> msg) -> Card -> Html msg
-- viewDiv onClickHandler card =
--     viewCard2 Html.div (Just onClickHandler) card
--
--
-- viewLabel : (Card -> msg) -> Card -> Html msg
-- viewLabel onClickHandler card =
--     viewCard2 Html.label (Just onClickHandler) card
--
--
--
-- viewCardsDiv : (Card -> msg) -> List Card -> List (Html msg)
-- viewCardsDiv onClickHandler cardsList =
--     List.map (viewDiv onClickHandler) (List.take 10 cardsList)
--


viewBlank : Card -> Html msg
viewBlank _ =
    Html.li []
        [ Html.span [ HA.class "card blank" ]
            [ Html.text "*"
            ]
        ]


viewBack : Card -> Html msg
viewBack _ =
    Html.li []
        [ Html.span [ HA.class "card back" ]
            [ Html.text "*"
            ]
        ]


viewDropzoneBlank : Html msg
viewDropzoneBlank =
    Html.li []
        [ Html.span [ HA.class "card blank" ]
            [ Html.text "*"
            ]
        ]


viewDropzone : HeadOrTail -> Html msg
viewDropzone hot =
    Html.li []
        [ Html.span [ HA.class "card dropzone", HA.class (Types.headOrTailStr hot |> String.toLower) ]
            [ Html.text "*"
            ]
        ]


viewDropzoneActive : HeadOrTail -> (Card -> msg) -> Card -> Html msg
viewDropzoneActive hot onDrop card =
    Html.li [ HE.onClick (onDrop card) ]
        [ Html.span [ HA.class "card active dropzone", HA.class (Types.headOrTailStr hot |> String.toLower) ]
            [ Html.text "*"
            ]
        ]


setInnerHTML : String -> Html.Attribute msg
setInnerHTML str =
    HA.property "innerHTML" (Encode.string str)


decoder : Decode.Decoder Card
decoder =
    Decode.map2 Card
        (Decode.field "suit"
            (Decode.string
                |> Decode.andThen
                    (\string ->
                        case string of
                            "Clubs" ->
                                Decode.succeed Clubs

                            "Diamonds" ->
                                Decode.succeed Diamonds

                            "Hearts" ->
                                Decode.succeed Hearts

                            "Spades" ->
                                Decode.succeed Spades

                            _ ->
                                Decode.fail "Invalid Suit"
                    )
            )
        )
        (Decode.field "rank"
            (Decode.string
                |> Decode.andThen
                    (\string ->
                        case string of
                            "Ace" ->
                                Decode.succeed Ace

                            "Two" ->
                                Decode.succeed Two

                            "Three" ->
                                Decode.succeed Three

                            "Four" ->
                                Decode.succeed Four

                            "Five" ->
                                Decode.succeed Five

                            "Six" ->
                                Decode.succeed Six

                            "Seven" ->
                                Decode.succeed Seven

                            "Eight" ->
                                Decode.succeed Eight

                            "Nine" ->
                                Decode.succeed Nine

                            "Ten" ->
                                Decode.succeed Ten

                            "Jack" ->
                                Decode.succeed Jack

                            "Queen" ->
                                Decode.succeed Queen

                            "King" ->
                                Decode.succeed King

                            _ ->
                                Decode.fail "Invalid Rank"
                    )
            )
        )


encoder : Card -> Encode.Value
encoder v =
    Encode.object
        [ ( "suit"
          , case v.suit of
                Clubs ->
                    Encode.string "Clubs"

                Diamonds ->
                    Encode.string "Diamonds"

                Hearts ->
                    Encode.string "Hearts"

                Spades ->
                    Encode.string "Spades"
          )
        , ( "rank"
          , case v.rank of
                Ace ->
                    Encode.string "Ace"

                Two ->
                    Encode.string "Two"

                Three ->
                    Encode.string "Three"

                Four ->
                    Encode.string "Four"

                Five ->
                    Encode.string "Five"

                Six ->
                    Encode.string "Six"

                Seven ->
                    Encode.string "Seven"

                Eight ->
                    Encode.string "Eight"

                Nine ->
                    Encode.string "Nine"

                Ten ->
                    Encode.string "Ten"

                Jack ->
                    Encode.string "Jack"

                Queen ->
                    Encode.string "Queen"

                King ->
                    Encode.string "King"
          )
        ]
