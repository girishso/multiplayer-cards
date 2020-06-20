module Player exposing (..)

import Cards exposing (Card)
import Html exposing (..)
import Id exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
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


viewA : (Card -> msg) -> Maybe Card -> { a | cards : List Card } -> List (Html msg)
viewA cardSelectedMsg maybeSelectedCard player =
    List.map (Cards.viewA cardSelectedMsg maybeSelectedCard) player.cards


viewSpanWithClick : (Card -> msg) -> Maybe Card -> { a | cards : List Card } -> List (Html msg)
viewSpanWithClick cardSelectedMsg maybeSelectedCard player =
    List.map (Cards.viewSpanWithClick cardSelectedMsg maybeSelectedCard) player.cards


viewSpanNoClick : { a | cards : List Card } -> List (Html msg)
viewSpanNoClick player =
    List.map Cards.viewSpanNoClick player.cards


viewBack : { a | cards : List Card } -> List (Html msg)
viewBack player =
    List.map Cards.viewBack player.cards


encoder : Player -> Encode.Value
encoder v =
    Encode.object
        [ ( "id", Encode.int (rawPlayerId v.id) )
        , ( "name", Encode.string v.name )
        , ( "cards", Encode.list Cards.encoder v.cards )
        ]


decoder : Decode.Decoder Player
decoder =
    Decode.map3 Player
        (Decode.field "id" (Decode.map playerId Decode.int))
        (Decode.field "name" Decode.string)
        (Decode.field "cards" (Decode.list Cards.decoder))
