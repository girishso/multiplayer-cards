module Helpers exposing (..)


makeListOf n f =
    List.map f (List.range 1 n)
