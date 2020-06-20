port module Ports exposing (..)

import Json.Decode as Decode



--
-- port alert : String -> Cmd msg
--
--
--
--
--
--
-- port newSharedGameCreated : (String -> msg) -> Sub msg
--
--
-- port setThisPlayer : (String -> msg) -> Sub msg
--
--
-- port copyUrl : String -> Cmd msg
--
--


port focus : String -> Cmd msg


port createNewGame : String -> Cmd msg


port newGameCreated : (String -> msg) -> Sub msg


port usernameSelected : String -> Cmd msg


port setPlayers : (List String -> msg) -> Sub msg


port sendGameStateNDef : String -> Cmd msg


port gameStateNDefChanged : (Decode.Value -> msg) -> Sub msg


port gameStarted : (() -> msg) -> Sub msg
