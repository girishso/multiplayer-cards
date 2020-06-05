module Helpers exposing (..)


makeListOf : Int -> (Int -> Int -> b) -> List b
makeListOf n f =
    List.indexedMap f (List.range 1 n)
