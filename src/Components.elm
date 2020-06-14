module Components exposing (layout)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, href, style)
import Route as Route exposing (Route)


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ div [ class "column spacing--large pad--medium container h--fill" ]
            [ navbar
            , div [ class "column", style "flex" "1 0 auto" ] page.body
            , footer
            ]
        ]
    }


navbar : Html msg
navbar =
    header [ class "row center-y spacing--between" ]
        [ a [ class "link font--h5", href (Route.toHref Route.Top) ] [ text "Home" ]
        , text "|"
        , div [ class "row center-y spacing--medium" ]
            [ text "|"
            , a [ class "link", href (Route.toHref Route.NotFound) ] [ text "A broken link" ]
            , text "|"
            , a [ class "link", href (Route.toHref Route.Play) ] [ text "Play" ]
            ]
        ]


footer : Html msg
footer =
    Html.footer [] []
