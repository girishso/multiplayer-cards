module Components exposing (layout)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes as HA exposing (class, href, style)
import Route as Route exposing (Route)


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ div [ class "" ]
            [ -- navbar
              div [] page.body

            -- , footer
            ]
        ]
    }


navbar : Html msg
navbar =
    nav [ HA.attribute "role" "navigation" ]
        [ div [ HA.id "menuToggle" ]
            [ text "    "
            , input [ HA.type_ "checkbox" ]
                []
            , text "        "
            , span []
                []
            , span []
                []
            , span []
                []
            , ul [ HA.id "menu" ]
                [ a [ HA.href (Route.toHref Route.Top) ]
                    [ li []
                        [ text "Home" ]
                    ]

                -- , a [ HA.href (Route.toHref Route.Play) ]
                --     [ li []
                --         [ text "Play" ]
                --     ]
                ]
            ]
        ]



-- header
-- [ class "row center-y spacing--between" ]
-- [ a [ class "link font--h5", href (Route.toHref Route.Top) ] [ text "Home" ]
-- , text "|"
-- , div [ class "row center-y spacing--medium" ]
--     [ text "|"
--     , a [ class "link", href (Route.toHref Route.NotFound) ] [ text "A broken link" ]
--     , text "|"
--     , a [ class "link", href (Route.toHref Route.Play) ] [ text "Play" ]
--     ]
-- ]


footer : Html msg
footer =
    Html.footer [] []
