module Player exposing (..)

import Cards exposing (Card)
import List.Extra
import Pile exposing (Pile)


type alias Player =
    { name : String
    , cards : List Card
    }


default =
    Player "" []


giveCard : Card -> Player -> Player
giveCard card player =
    { player | cards = card :: player.cards }


takeCard : Card -> Player -> Player
takeCard card player =
    { player | cards = List.Extra.remove card player.cards }


dropCardOnPile : Card -> Pile.HeadOrTail -> Pile -> Player -> ( Player, Pile )
dropCardOnPile card headOrTail pile player =
    ( takeCard card player, Pile.drop card headOrTail pile )
