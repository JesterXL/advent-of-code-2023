module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, ul, li, a, b)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, href)
import Url
import Url.Parser exposing ((</>), s, parse, map, oneOf)
import Browser.Navigation as Nav

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initialModel key url (parseRoute url)
    , Cmd.none
    )

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route }

initialModel : Nav.Key -> Url.Url -> Route -> Model
initialModel key url route =
    { key = key
    , url = url
    , route = route }


type Route 
    = NotFound
    | Day1
    | Day2

routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    oneOf [
        map Day1 (s "day1")
        , map Day2 (s "day2")
    ]
    
parseRoute : Url.Url -> Route
parseRoute url =
    parse routeParser url
        |> Maybe.withDefault NotFound


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Nav.pushUrl model.key (Url.toString url) )

                        Just _ ->
                            ( model, Nav.load url.path )

                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            ( { model | url = url, route = parseRoute url }, Cmd.none )

view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2023"
    , body = 
        [ div [ class "w-full bg-gray-800 border-gray-200"] [ 
            tabs
            , case model.route of
                NotFound ->
                    div [] [ text "Not found" ]
                Day1 ->
                    div [] [ text "Day 1" ]
                Day2 ->
                    div [] [ text "Day 2" ] 
            ]
        ] 
    }

tabs =
    ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400"][
        li [ class "me-2"][
            a [ class "inline-block p-4 text-blue-600 bg-gray-100 rounded-t-lg active dark:bg-gray-800 dark:text-blue-500", href "/day1" ][ text "Day 1"]
        ]
        , li [ class "me-2"][
            a [ class "inline-block p-4 rounded-t-lg hover:text-gray-600 hover:bg-gray-50 dark:hover:bg-gray-800 dark:hover:text-gray-300", href "/day2"][ text "Day 2"]
        ]
    ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
