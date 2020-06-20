module Helpers exposing (..)

import List.Extra


makeListOf : Int -> (Int -> Int -> b) -> List b
makeListOf n f =
    List.indexedMap f (List.range 1 n)


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


noneNone : a -> ( a, Cmd msg, Cmd msg1 )
noneNone model =
    ( model, Cmd.none, Cmd.none )


rotate n list =
    list
        |> List.Extra.splitAt n
        |> (\( l1, l2 ) ->
                [ l2, l1 ]
                    |> List.concat
           )
