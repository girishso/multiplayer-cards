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
    { windowWidth : Int
    , windowHeight : Int
    , gameId : Maybe String
    , url : String
    , playerName : Maybe String
    }


type alias Model =
    { flags : Flags
    , url : Url
    , key : Nav.Key
    , gameDefinition : GameDefinition
    , joinedPlayers : List String
    , myPlayerName : String
    }


type alias GameDefinition =
    { numberOfPlayers : Int
    , numberOfDecks : Int
    , numberOfPiles : Int
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { flags = flags
      , url = url
      , key = key
      , gameDefinition = initGameDefinition
      , joinedPlayers = []
      , myPlayerName = flags.playerName |> Maybe.withDefault ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Navigate Route
    | SetPlayerName String
    | SetPlayers (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate route ->
            ( model
            , Nav.pushUrl model.key (Route.toHref route)
            )

        SetPlayerName v ->
            ( { model | myPlayerName = v }, Cmd.none )

        SetPlayers v ->
            ( { model | joinedPlayers = v }, Cmd.none )



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
view { page } =
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


setPlayerName : String -> Cmd Msg
setPlayerName v =
    send (SetPlayerName v)


setPlayers : List String -> Cmd Msg
setPlayers v =
    send (SetPlayers v)


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
