module Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type alias GameId =
    String


type Route
    = Top
    | NotFound
    | Play GameId
    | Waiting GameId (Maybe String) (Maybe String)


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse routes


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Top Parser.top
        , Parser.map NotFound (Parser.s "not-found")
        , Parser.map Play (Parser.string </> Parser.s "play")
        , Parser.map Waiting (Parser.string </> Parser.s "waiting" <?> Query.string "game_url" <?> Query.string "game_creator")
        ]


toHref : Route -> String
toHref route =
    case route of
        Top ->
            Builder.absolute [] []

        NotFound ->
            Builder.absolute [ "not-found" ] []

        Play gameId ->
            Builder.absolute [ gameId, "play" ] []

        Waiting gameId gameUrl gameCreator ->
            Builder.absolute [ gameId, "waiting" ]
                (Maybe.map2
                    (\gameUrl_ gameCreator_ ->
                        [ Builder.string "game_url" gameUrl_
                        , Builder.string "game_creator" gameCreator_
                        ]
                    )
                    gameUrl
                    gameCreator
                    |> Maybe.withDefault []
                )
