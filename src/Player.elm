module Player exposing (..)

import Cards exposing (Card)


type alias Player =
    { name : String
    , cards : List Card
    }


giveCard : Card -> Player -> Player
giveCard card player =
    { player | cards = card :: player.cards }
