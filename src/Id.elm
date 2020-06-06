module Id exposing (..)


type Id a
    = Id Int


type PlayerId
    = PlayerIdUnused


playerId : Int -> Id PlayerId
playerId =
    Id


rawPlayerId : Id PlayerId -> Int
rawPlayerId (Id id) =
    id


type PileId
    = PileIdUnused


pileId : Int -> Id PileId
pileId =
    Id


rawPileId : Id PileId -> Int
rawPileId (Id id) =
    id


pileEql : Id PileId -> Id PileId -> Bool
pileEql pile1Id pile2Id =
    rawPileId pile1Id == rawPileId pile2Id
