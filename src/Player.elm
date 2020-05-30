module Player exposing (..)

import Cards exposing (Card)


type alias Player =
    { name : String
    , cards : List Card
    }


addCard : Card -> Player -> Player
addCard card player =
    { player | cards = card :: player.cards }
