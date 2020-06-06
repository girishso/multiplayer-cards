module Pile exposing (..)

import Cards exposing (Card)
import Html as Html exposing (Html)
import Html.Attributes as HA
import Id exposing (..)
import List.Extra
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


add : Card -> HeadOrTail -> Pile -> Pile
add card headOrTail pile =
    let
        _ =
            Debug.log "add card headOrTail pile" ( card, headOrTail, pile )

        appendCard cards =
            List.append cards [ card ]
    in
    case pile of
        SimplePile id cards ->
            appendCard cards |> SimplePile id

        TwoWayPile id cards ->
            (TwoWayPile id |> Debug.log "twowaypile") <|
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


updatePile : Pile -> List Pile -> List Pile
updatePile pile piles =
    case pile of
        SimplePile pileId cardList ->
            List.Extra.updateAt (rawPileId pileId) (always pile) piles

        TwoWayPile pileId cardList ->
            List.Extra.updateAt (rawPileId pileId) (always pile) piles



--
--
-- pileEql : Pile -> Pile -> Bool
-- pileEql pile1 pile2 =
--     rawPileId pile1 == rawPileId pile2
