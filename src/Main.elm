module Main exposing (..)

import Browser
import Browser.Navigation as Nav exposing (Key)
import Global
import Html exposing (..)
import Html.Attributes as HA
import Pages.Pages as Pages
import Ports
import Route as Route exposing (Route)
import Url exposing (Url)



---- MODEL ----


type alias Model =
    { key : Key
    , url : Url
    , global : Global.Model
    , page : Pages.Model
    }


type alias Flags =
    { windowWidth : Int, windowHeight : Int, gameId : Maybe String, url : String, playerName : Maybe String }



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | PagesMsg Pages.Msg
    | GlobalMsg Global.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                ( page, pageCmd, globalCmd ) =
                    Pages.init (fromUrl url) model.global
            in
            ( { model | url = url, page = page }
            , Cmd.batch
                [ Cmd.map PagesMsg pageCmd
                , Cmd.map GlobalMsg globalCmd
                ]
            )

        GlobalMsg globalMsg ->
            let
                ( global, globalCmd ) =
                    Global.update globalMsg model.global
            in
            ( { model | global = global }
            , Cmd.map GlobalMsg globalCmd
            )

        PagesMsg pageMsg ->
            let
                ( page, pageCmd, globalCmd ) =
                    Pages.update pageMsg model.page model.global
            in
            ( { model | page = page }
            , Cmd.batch
                [ Cmd.map PagesMsg pageCmd
                , Cmd.map GlobalMsg globalCmd
                ]
            )


view : Model -> Browser.Document Msg
view model =
    let
        documentMap :
            (msg1 -> msg2)
            -> Browser.Document msg1
            -> Browser.Document msg2
        documentMap fn doc =
            { title = doc.title
            , body = List.map (Html.map fn) doc.body
            }
    in
    Global.view
        { page = Pages.view model.page model.global |> documentMap PagesMsg
        , global = model.global
        , toMsg = GlobalMsg
        }



-- Html.div
--     [ HA.class "main" ]
--     [ Html.div [ HA.class "piles" ]
--         (List.map (viewPile model) playState.piles)
--     , Html.div [ HA.class "players" ]
--         (List.indexedMap (viewPlayer localState playState) playState.players)
--     ]
---- inits


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( global, globalCmd ) =
            Global.init flags url key

        ( page, pagesCmd, pageGlobalCmd ) =
            Pages.init (fromUrl url) global
    in
    ( { url = url
      , key = key
      , global = global
      , page = page
      }
    , Cmd.batch
        [ Cmd.map GlobalMsg globalCmd
        , Cmd.map GlobalMsg pageGlobalCmd
        , Cmd.map PagesMsg pagesCmd
        ]
    )



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subs
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ model.global
            |> Global.subscriptions
            |> Sub.map GlobalMsg
        , model.page
            |> (\page -> Pages.subscriptions page model.global)
            |> Sub.map PagesMsg
        ]


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound
