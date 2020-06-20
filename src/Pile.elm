module Pile exposing (..)

import Cards exposing (Card)
import Html as Html exposing (Html)
import Html.Attributes as HA
import Id exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import List.Extra
import Maybe.Extra
import Types exposing (..)


type Pile
    = SimplePile (Id PileId) (List Card)
    | TwoWayPile (Id PileId) (List Card)


newSimplePile : Int -> List Card -> Pile
newSimplePile id =
    SimplePile (pileId id)


newTwoWayPile : Int -> List Card -> Pile
newTwoWayPile id =
    TwoWayPile (pileId id)


add : Card -> HeadOrTail -> Pile -> Pile
add card headOrTail pile =
    let
        appendCard cards =
            List.append cards [ card ]
    in
    case pile of
        SimplePile id cards ->
            appendCard cards |> SimplePile id

        TwoWayPile id cards ->
            TwoWayPile id <|
                case headOrTail of
                    Head ->
                        card :: cards

                    Tail ->
                        appendCard cards

                    DoesntMatter ->
                        appendCard cards


take : Pile -> ( Maybe Card, Pile )
take pile =
    case pile of
        SimplePile id cards ->
            ( List.head cards, List.drop 1 cards |> SimplePile id )

        TwoWayPile id cards ->
            ( Nothing, pile )


nCards : Pile -> Int
nCards pile =
    case pile of
        SimplePile id cards ->
            List.length cards

        TwoWayPile id cards ->
            List.length cards


view : (Card -> msg) -> (Pile -> HeadOrTail -> Card -> msg) -> Maybe Card -> Pile -> Html msg
view onClickHandler onDrop maybeSelectedCard pile =
    let
        viewDropzone hot =
            Maybe.Extra.unwrap (Cards.viewDropzone hot) (Cards.viewDropzoneActive hot (onDrop pile hot)) maybeSelectedCard

        viewSimplePile cards =
            Html.ul
                [ HA.class "twowaypile"
                ]
                (case cards of
                    [] ->
                        [ viewDropzone DoesntMatter ]

                    _ ->
                        view_ onClickHandler cards ++ [ viewDropzone DoesntMatter ]
                )

        viewTwoWayPile cards =
            Html.ul
                [ HA.class "twowaypile"
                ]
                (case cards of
                    [] ->
                        [ viewDropzone DoesntMatter ]

                    _ ->
                        [ viewDropzone Head ] ++ view_ onClickHandler cards ++ [ viewDropzone Tail ]
                )
    in
    Html.div []
        [ case pile of
            SimplePile id cards ->
                viewSimplePile cards

            TwoWayPile id cards ->
                viewTwoWayPile cards
        ]


view_ : (Card -> msg) -> List Card -> List (Html msg)
view_ onClickHandler cards =
    List.map (Cards.viewSpan onClickHandler) cards


viewOnly_ : List Card -> List (Html msg)
viewOnly_ cards =
    List.map Cards.viewSpanNoClick cards


viewOnly : Pile -> Html msg
viewOnly pile =
    let
        viewSimplePile cards =
            Html.ul
                [ HA.class "twowaypile"
                ]
                (case cards of
                    [] ->
                        [ Cards.viewDropzoneBlank ]

                    _ ->
                        viewOnly_ cards
                )

        viewTwoWayPile cards =
            Html.ul
                [ HA.class "twowaypile"
                ]
                (case cards of
                    [] ->
                        [ Cards.viewDropzoneBlank ]

                    _ ->
                        viewOnly_ cards
                )
    in
    Html.div []
        [ case pile of
            SimplePile id cards ->
                viewSimplePile cards

            TwoWayPile id cards ->
                viewTwoWayPile cards
        ]


updatePile : Pile -> List Pile -> List Pile
updatePile pile piles =
    case pile of
        SimplePile pileId cardList ->
            List.Extra.updateAt (rawPileId pileId) (always pile) piles

        TwoWayPile pileId cardList ->
            List.Extra.updateAt (rawPileId pileId) (always pile) piles


encoder : Pile -> Encode.Value
encoder v =
    let
        pileEncoder kind cards pileId =
            Encode.object
                [ ( "kind", Encode.string kind )
                , ( "id", pileId |> rawPileId |> Encode.int )
                , ( "cards", Encode.list Cards.encoder cards )
                ]
    in
    case v of
        SimplePile pileId cards ->
            pileEncoder "simple-pile" cards pileId

        TwoWayPile pileId cards ->
            pileEncoder "two-way-pile" cards pileId


decoder : Decode.Decoder Pile
decoder =
    let
        pileDecoder v =
            Decode.map2 v
                (Decode.field "id" (Decode.int |> Decode.map pileId))
                (Decode.field "cards" (Decode.list Cards.decoder))
    in
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "simple-pile" ->
                        pileDecoder SimplePile

                    "two-way-pile" ->
                        pileDecoder TwoWayPile

                    _ ->
                        Decode.fail "Invalid Pile"
            )



--
--
-- pileEql : Pile -> Pile -> Bool
-- pileEql pile1 pile2 =
--     rawPileId pile1 == rawPileId pile2
