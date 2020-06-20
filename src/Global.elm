module Global exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import Route as Route exposing (Route)
import Task
import Url exposing (Url)



-- INIT


type alias Flags =
    { windowWidth : Int, windowHeight : Int, gameId : Maybe String, url : String }


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , gameDefinition : GameDefinition
    , waitingPlayers : List String
    }


type alias GameDefinition =
    { numberOfPlayers : Int
    , numberOfDecks : Int
    , numberOfPiles : Int
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { flags = flags |> Debug.log "flags"
      , url = url
      , key = key
      , gameDefinition = initGameDefinition
      , waitingPlayers = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Navigate Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "globalMsg" msg of
        Navigate route ->
            ( model
            , Nav.pushUrl model.key (Route.toHref route)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg
    , global : Model
    , toMsg : Msg -> msg
    }
    -> Document msg
view { page, global, toMsg } =
    Components.layout
        { page = page
        }



-- COMMANDS


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity


navigate : Route -> Cmd Msg
navigate route =
    send (Navigate route)


initGameDefinition : GameDefinition
initGameDefinition =
    { numberOfPlayers = 2
    , numberOfDecks = 1
    , numberOfPiles = 4
    }


setGameDefinition : a -> { b | gameDefinition : a } -> { b | gameDefinition : a }
setGameDefinition v model =
    { model | gameDefinition = v }


gameDefinitionEncoder : GameDefinition -> Encode.Value
gameDefinitionEncoder v =
    Encode.object
        [ ( "numberOfPlayers", Encode.int v.numberOfPlayers )
        , ( "numberOfDecks", Encode.int v.numberOfDecks )
        , ( "numberOfPiles", Encode.int v.numberOfPiles )
        ]


gameDefinitionDecoder : Decode.Decoder GameDefinition
gameDefinitionDecoder =
    Decode.map3 GameDefinition
        (Decode.field "numberOfPlayers" Decode.int)
        (Decode.field "numberOfDecks" Decode.int)
        (Decode.field "numberOfPiles" Decode.int)
