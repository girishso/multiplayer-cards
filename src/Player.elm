module Player exposing (..)

import Cards exposing (Card)
import Id exposing (..)
import List.Extra
import Pile exposing (Pile)
import Types


type alias Player =
    { id : Id PlayerId
    , name : String
    , cards : List Card
    }


default : Player
default =
    Player (playerId 0) "" []


giveCard : Card -> Player -> Player
giveCard card player =
    { player | cards = card :: player.cards }


takeCard : Card -> Player -> Player
takeCard card player =
    { player | cards = List.Extra.remove card player.cards }


dropCardOnPile : Card -> Types.HeadOrTail -> Pile -> Player -> ( Player, Pile )
dropCardOnPile card headOrTail pile player =
    ( takeCard card player, Pile.add card headOrTail pile )
        |> Debug.log "dropCardOnPile"
