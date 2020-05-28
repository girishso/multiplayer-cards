module Pile exposing (..)

import Cards exposing (Card(..), Face(..), Suit(..))
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
        appendCard cardsList =
            List.append cardsList [ card ]
    in
    case pile of
        SimplePile cardsList ->
            appendCard cardsList |> SimplePile

        TwoWayPile cardsList ->
            TwoWayPile <|
                case headOrTail of
                    Head ->
                        card :: cardsList

                    Tail ->
                        appendCard cardsList

                    DoesntMatter ->
                        appendCard cardsList


take : Pile -> ( Maybe Card, Pile )
take pile =
    case pile of
        SimplePile cardsList ->
            ( List.head cardsList, List.drop 1 cardsList |> SimplePile )

        TwoWayPile cardsList ->
            ( Nothing, pile )


view : (Card -> msg) -> Pile -> Html msg
view onClickHandler pile =
    Html.div []
        [ case pile of
            SimplePile cardsList ->
                Html.ul
                    [ HA.class "deck"
                    ]
                    (Cards.viewCardsDiv onClickHandler cardsList)

            TwoWayPile cardsList ->
                Html.ul
                    [ HA.class "deck"
                    ]
                    (Cards.viewCardsDiv onClickHandler cardsList)
        ]
