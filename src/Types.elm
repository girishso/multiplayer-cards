module Types exposing (..)


type HeadOrTail
    = Head
    | Tail
    | DoesntMatter


headOrTailStr : HeadOrTail -> String
headOrTailStr hot =
    case hot of
        Head ->
            "Head"

        Tail ->
            "Tail"

        DoesntMatter ->
            "DoesntMatter"
