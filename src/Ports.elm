port module Ports exposing (..)

import Json.Decode as Decode



--
-- port alert : String -> Cmd msg
--
--
--
--
-- port gameStateChanged : (Decode.Value -> msg) -> Sub msg
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


port sendGameState : String -> Cmd msg
