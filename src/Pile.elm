module Pile exposing (..)

import Cards exposing (Card)
import Html as Html exposing (Html)
import Html.Attributes as HA
import Id exposing (..)
import Maybe.Extra


type Pile
    = SimplePile (Id PileId) (List Card)
    | TwoWayPile (Id PileId) (List Card)


type HeadOrTail
    = Head
    | Tail
    | DoesntMatter


newSimplePile : Int -> List Card -> Pile
newSimplePile id =
    SimplePile (pileId id)


newTwoWayPile : Int -> List Card -> Pile
newTwoWayPile id =
    TwoWayPile (pileId id)


drop : Card -> HeadOrTail -> Pile -> Pile
drop card headOrTail pile =
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
        viewPile cards =
            Html.ul
                [ HA.class "deck"
                ]
                (case cards of
                    [] ->
                        [ Maybe.Extra.unwrap Cards.viewDropzone (Cards.viewDropzoneActive (onDrop pile DoesntMatter)) maybeSelectedCard ]

                    _ ->
                        Cards.viewCardsDiv onClickHandler cards
                )
    in
    Html.div []
        [ case pile of
            SimplePile id cards ->
                viewPile cards

            TwoWayPile id cards ->
                viewPile cards
        ]
