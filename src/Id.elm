module Id exposing (..)


type Id a
    = Id Int


type PlayerId
    = PlayerIdUnused


playerId : Int -> Id PlayerId
playerId =
    Id


type PileId
    = PileIdUnused


pileId : Int -> Id PileId
pileId =
    Id
