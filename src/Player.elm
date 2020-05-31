module Player exposing (..)

import Cards exposing (Card)


type alias Player =
    { name : String
    , cards : List Card
    }


default =
    Player "" []


giveCard : Card -> Player -> Player
giveCard card player =
    { player | cards = card :: player.cards }
