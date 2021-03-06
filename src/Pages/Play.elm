module Pages.Play exposing (Flags, Model, Msg, page)

import Browser
import Browser.Dom
import Browser.Events
import Cards exposing (Card)
import Deck as Deck exposing (Deck)
import Global
import Helpers
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Id exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import List.Extra
import Maybe.Extra
import Page exposing (Document, Page)
import Pile exposing (Pile)
import Player exposing (Player)
import Ports
import Random
import Task
import Time
import Types


type alias Flags =
    { gameId : String }


type alias Model =
    { playState : PlayState
    , localState : LocalState
    }


type alias PlayState =
    { players : List Player
    , piles : List Pile
    , currentPlayerIx : Int
    }


type alias LocalState =
    { myIx : Int
    , selectedCard : Maybe Card
    , windowHeight : Int
    , windowWidth : Int
    , prevPlayState : Maybe PlayState
    }


type Msg
    = DeckShuffled Deck
    | CardSelected Card
    | CardDroppedOnPile Pile Types.HeadOrTail Card
    | Shuffle
    | WindowResized Int Int
    | OnViewport Browser.Dom.Viewport
    | GameStateNDefChanged (Result Decode.Error PlayStateNDef)
    | PassClicked
    | UndoClicked


page : Page Flags Model Msg
page =
    Page.component
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Global.Model -> Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update global msg ({ playState, localState } as model) =
    case msg of
        DeckShuffled deck ->
            let
                players =
                    Deck.distribute2 playState.players deck

                hearts7Card =
                    Cards.init Cards.Hearts Cards.Seven

                playerWHearts7Ix =
                    players
                        |> List.filter (Player.hasCard hearts7Card)
                        |> List.head
                        |> Maybe.map (\p -> rawPlayerId p.id)
                        |> Maybe.withDefault 0

                newModel =
                    setPlayState { playState | players = players, currentPlayerIx = playerWHearts7Ix } model
            in
            ( newModel
            , sendGameStateNDefHelper global newModel.playState localState
            , Cmd.none
            )

        CardSelected card ->
            ( setLocalState { localState | selectedCard = Just card } model, Cmd.none, Cmd.none )

        CardDroppedOnPile pile headOrTail card ->
            let
                newPlayState =
                    getCurrentPlayer playState
                        |> Player.dropCardOnPile card headOrTail pile
                        |> Tuple.mapBoth (\newPlayer -> List.Extra.updateAt (rawPlayerId newPlayer.id) (always newPlayer) playState.players)
                            (\newPile -> Pile.updatePile newPile playState.piles)
                        |> (\( newPlayers, newPiles ) ->
                                { players = newPlayers
                                , piles = newPiles
                                , currentPlayerIx = getNextPlayerIx playState
                                }
                           )

                newModel =
                    model
                        |> setLocalState
                            { localState
                                | selectedCard = Nothing
                                , prevPlayState = Just playState

                                -- for testing locally
                                -- , myIx = getNextPlayerIx newPlayState.players playState.currentPlayerIx
                            }
                        |> setPlayState newPlayState
            in
            ( newModel
            , sendGameStateNDefHelper global newModel.playState newModel.localState
            , Cmd.none
            )

        UndoClicked ->
            let
                ( newPlayState, newCmd ) =
                    localState.prevPlayState
                        |> Maybe.map (\ps -> ( ps, sendGameStateNDefHelper global ps localState ))
                        |> Maybe.withDefault ( playState, Cmd.none )

                newModel =
                    model
                        |> setLocalState
                            { localState
                                | selectedCard = Nothing
                                , prevPlayState = Nothing
                            }
                        |> setPlayState newPlayState
            in
            ( newModel
            , newCmd
            , Cmd.none
            )

        PassClicked ->
            let
                newPlayState =
                    { playState
                        | currentPlayerIx = getNextPlayerIx playState
                    }

                newModel =
                    model
                        |> setLocalState
                            { localState
                                | selectedCard = Nothing
                                , prevPlayState = Just playState
                            }
                        |> setPlayState newPlayState
            in
            ( newModel
            , sendGameStateNDefHelper global newModel.playState localState
            , Cmd.none
            )

        Shuffle ->
            ( model, shuffle global.gameDefinition, Cmd.none )

        WindowResized w h ->
            -- let
            --     _ =
            --         Debug.log "WindowResized" ( w, h )
            -- in
            ( model |> setLocalState { localState | windowWidth = w, windowHeight = h }, Cmd.none, Cmd.none )

        OnViewport { viewport } ->
            -- let
            --     _ =
            --         Debug.log "viewport" viewport
            -- in
            ( model |> setLocalState { localState | windowWidth = round viewport.width, windowHeight = round viewport.height }, Cmd.none, Cmd.none )

        GameStateNDefChanged json ->
            let
                gsnd =
                    case json of
                        Ok value ->
                            value

                        Err error ->
                            -- let
                            --     _ =
                            --         Debug.log "GameStateChanged err" error
                            -- in
                            { playState = model.playState, gameDefinition = global.gameDefinition }
            in
            ( model |> setPlayState gsnd.playState
            , Cmd.none
            , Cmd.none
            )


sendGameStateNDefHelper global playState localState =
    let
        name =
            playState.players
                |> List.Extra.getAt localState.myIx
                |> Maybe.map .name
                |> Maybe.withDefault "anon"
    in
    ( name, Encode.encode 0 (playStateNDefEncoder { playState = playState, gameDefinition = global.gameDefinition }) )
        |> Ports.sendGameStateNDef


subscriptions : Global.Model -> Model -> Sub Msg
subscriptions global model =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Ports.gameStateNDefChanged (GameStateNDefChanged << Decode.decodeValue playStateNDefDecoder)
        ]


getNextPlayerIx : PlayState -> Int
getNextPlayerIx { players, currentPlayerIx } =
    if List.length players == currentPlayerIx + 1 then
        0

    else
        currentPlayerIx + 1


getPrevPlayerIx : PlayState -> Int
getPrevPlayerIx { players, currentPlayerIx } =
    if currentPlayerIx == 0 then
        List.length players - 1

    else
        currentPlayerIx - 1


setPlayState : PlayState -> Model -> Model
setPlayState state modelx =
    { modelx | playState = state }


setLocalState : a -> { b | localState : a } -> { b | localState : a }
setLocalState v model =
    { model | localState = v }


isCardSelected : Model -> Bool
isCardSelected { localState } =
    Maybe.Extra.isJust localState.selectedCard


isMyTurn : PlayState -> LocalState -> Bool
isMyTurn playState localState =
    localState.myIx == playState.currentPlayerIx


getSelectedCard : Model -> Maybe Card
getSelectedCard { localState } =
    localState.selectedCard


getSelfPlayer : List Player -> LocalState -> Player
getSelfPlayer players localState =
    players
        |> List.Extra.getAt localState.myIx
        |> Maybe.withDefault Player.default


getCurrentPlayer : PlayState -> Player
getCurrentPlayer playState =
    playState.players
        |> List.Extra.getAt playState.currentPlayerIx
        |> Maybe.withDefault Player.default



---- VIEW ----


view : Global.Model -> Model -> Browser.Document Msg
view ({ gameDefinition } as global) ({ playState, localState } as model) =
    let
        rotatedPlayers =
            Helpers.rotate localState.myIx playState.players

        getNthRotatedPlayersList n =
            rotatedPlayers
                |> List.indexedMap
                    (\ix p ->
                        if ix == n then
                            Just p

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.head
    in
    { title = "Badaam Saat - (Seven of Hearts)"
    , body =
        [ Html.div
            [ HA.class "main" ]
            [ -- [HA.class = "top-row" ]
              --     [ "Current player: " ++ (getCurrentPlayer playState).name |> text ]
              -- Piles
              div [ HA.class "piles-container" ]
                (List.map (viewPile model) playState.piles)

            -- Top
            , div [ HA.class "top-player player-container" ]
                (getNthRotatedPlayersList 2
                    |> viewPlayer localState playState False False
                )

            -- Right
            , div [ HA.class "right-player player-container" ]
                (getNthRotatedPlayersList 1
                    |> viewPlayer localState playState False True
                )

            -- Bottom
            , div [ HA.class "bottom-player player-container" ]
                (getNthRotatedPlayersList 0
                    |> viewPlayer localState playState True False
                )

            -- Left
            , div [ HA.class "left-player player-container" ]
                (getNthRotatedPlayersList 3
                    |> viewPlayer localState playState False True
                )
            ]
        ]
    }


viewPlayer : LocalState -> PlayState -> Bool -> Bool -> Maybe Player -> List (Html Msg)
viewPlayer localState playState me rotated mbPlayer =
    let
        viewCards player =
            case ( me, isMyTurn playState localState ) of
                ( True, True ) ->
                    Player.viewSpanWithClick CardSelected localState.selectedCard player

                ( True, False ) ->
                    Player.viewSpanNoClick player

                ( _, _ ) ->
                    Player.viewBack player

        numCards player =
            List.length player.cards
    in
    case mbPlayer of
        Just player ->
            [ div
                [ HA.class "player-name"
                , HA.classList [ ( "current-player", rawPlayerId player.id == playState.currentPlayerIx ) ]
                ]
                [ text (player.name ++ "(" ++ String.fromInt (numCards player) ++ ")") ]
            , Html.div [ HA.class "player playingCards faceImages" ]
                [ div [ HA.class "hand-w" ]
                    [ Html.ul
                        [ HA.classList [ ( "rotated", rotated ), ( "hand", True ) ]

                        -- , HA.style "left" ((min (numCards player - 25) 14 |> String.fromInt) ++ "em")
                        ]
                        (viewCards player)
                    ]

                -- [ Html.text "Meee! Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed iaculis libero id tincidunt consectetur. Etiam eget nunc quis justo aliquam mollis ac ac enim. Donec vel dignissim magna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Aliquam varius, sem et consequat tincidunt, enim erat tincidunt felis, eget auctor eros est ut lectus." ]
                ]
            , Helpers.showIf me <|
                Html.div [ HA.class "buttons" ]
                    [ Helpers.showIf me (button [ HA.class "pass-btn", HE.onClick PassClicked ] [ text "Pass" ])
                    , Helpers.showIf (me && (getPrevPlayerIx playState == localState.myIx)) (button [ HA.class "undo-btn", HE.onClick UndoClicked ] [ text "Undo" ])
                    ]
            ]

        Nothing ->
            []


viewPile : Model -> Pile -> Html Msg
viewPile ({ localState, playState } as model) pile =
    let
        viewPile_ pile_ =
            List.singleton <|
                case isMyTurn playState localState of
                    True ->
                        Pile.view CardSelected CardDroppedOnPile (getSelectedCard model) pile

                    False ->
                        Pile.viewOnly pile
    in
    Html.div []
        [ Html.div [ HA.class "pile playingCards simpleCards suitTop" ]
            (viewPile_ pile)
        ]


shuffle : { a | numberOfDecks : Int } -> Cmd Msg
shuffle { numberOfDecks } =
    Random.generate DeckShuffled (Deck.randomDeck numberOfDecks)



---- inits


initPlayState : Global.GameDefinition -> List String -> PlayState
initPlayState gameDefinition joinedPlayers =
    case joinedPlayers of
        [] ->
            -- all local for testing
            let
                cards =
                    Deck.fullSuit Cards.Hearts |> List.take 0

                players =
                    Helpers.makeListOf
                        gameDefinition.numberOfPlayers
                        (\ix n -> Player (playerId ix) ("Player " ++ String.fromInt n) [])
            in
            { players = players
            , piles = Helpers.makeListOf gameDefinition.numberOfPiles (\ix _ -> Pile.newTwoWayPile ix cards)
            , currentPlayerIx = 0
            }

        _ ->
            { players = joinedPlayers |> List.indexedMap (\ix name -> Player (playerId ix) name [])
            , piles = Helpers.makeListOf gameDefinition.numberOfPiles (\ix _ -> Pile.newTwoWayPile ix [])
            , currentPlayerIx = 0
            }


initLocalState : PlayState -> Global.Model -> LocalState
initLocalState { players } { joinedPlayers, myPlayerName } =
    case joinedPlayers of
        [] ->
            { myIx = 0
            , selectedCard = Nothing
            , windowHeight = 0
            , windowWidth = 0
            , prevPlayState = Nothing
            }

        _ ->
            { myIx =
                players
                    |> List.Extra.findIndex (\player -> player.name == myPlayerName)
                    |> Maybe.withDefault 0
            , selectedCard = Nothing
            , windowHeight = 0
            , windowWidth = 0
            , prevPlayState = Nothing
            }


init : Global.Model -> Flags -> ( Model, Cmd Msg, Cmd Global.Msg )
init ({ gameDefinition, joinedPlayers } as global) flags =
    let
        playState =
            initPlayState gameDefinition joinedPlayers
    in
    ( { playState = playState
      , localState = initLocalState playState global
      }
    , Cmd.batch
        [ shuffle gameDefinition
        , getWindowSize
        ]
    , Cmd.none
    )


getWindowSize : Cmd Msg
getWindowSize =
    Task.perform OnViewport Browser.Dom.getViewport


type alias PlayStateNDef =
    { playState : PlayState, gameDefinition : Global.GameDefinition }



---- encoders / decoders ---------


playStateNDefEncoder : PlayStateNDef -> Encode.Value
playStateNDefEncoder v =
    Encode.object
        [ ( "playState"
          , playStateEncoder v.playState
          )
        , ( "gameDefinition"
          , Global.gameDefinitionEncoder v.gameDefinition
          )
        ]


playStateNDefDecoder : Decode.Decoder PlayStateNDef
playStateNDefDecoder =
    Decode.map2 PlayStateNDef
        (Decode.field "playState" playStateDecoder)
        (Decode.field "gameDefinition" Global.gameDefinitionDecoder)


playStateEncoder : PlayState -> Encode.Value
playStateEncoder v =
    Encode.object
        [ ( "players"
          , Encode.list Player.encoder v.players
          )
        , ( "piles"
          , Encode.list Pile.encoder v.piles
          )
        , ( "currentPlayerIx"
          , Encode.int v.currentPlayerIx
          )
        ]


playStateDecoder : Decode.Decoder PlayState
playStateDecoder =
    Decode.map3 PlayState
        (Decode.field "players" (Decode.list Player.decoder))
        (Decode.field "piles" (Decode.list Pile.decoder))
        (Decode.field "currentPlayerIx" Decode.int)


modelEncoder : Model -> Encode.Value
modelEncoder v =
    Encode.object
        [ ( "playState", playStateEncoder v.playState )
        ]


modelDecoder : Decode.Decoder (LocalState -> Model)
modelDecoder =
    Decode.map Model
        (Decode.field "playState" playStateDecoder)
