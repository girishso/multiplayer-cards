module Pile exposing (..)

import Cards exposing (Card)
import Html as Html exposing (Html)
import Html.Attributes as HA


type Pile
    = SimplePile (List Card)
    | TwoWayPile (List Card)


type HeadOrTail
    = Head
    | Tail
    | DoesntMatter


newSimplePile : List Card -> Pile
newSimplePile =
    SimplePile


newTwoWayPile : List Card -> Pile
newTwoWayPile =
    TwoWayPile


add : Card -> HeadOrTail -> Pile -> Pile
add card headOrTail pile =
    let
        appendCard cards =
            List.append cards [ card ]
    in
    case pile of
        SimplePile cards ->
            appendCard cards |> SimplePile

        TwoWayPile cards ->
            TwoWayPile <|
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
        SimplePile cards ->
            ( List.head cards, List.drop 1 cards |> SimplePile )

        TwoWayPile cards ->
            ( Nothing, pile )


nCards : Pile -> Int
nCards pile =
    case pile of
        SimplePile cards ->
            List.length cards

        TwoWayPile cards ->
            List.length cards


view : (Card -> msg) -> Pile -> Html msg
view onClickHandler pile =
    let
        viewPile cards =
            Html.ul
                [ HA.class "deck"
                ]
                (case cards of
                    [] ->
                        [ Cards.viewBlank ]

                    _ ->
                        Cards.viewCardsDiv onClickHandler cards
                )
    in
    Html.div []
        [ case pile of
            SimplePile cards ->
                viewPile cards

            TwoWayPile cards ->
                viewPile cards
        ]
