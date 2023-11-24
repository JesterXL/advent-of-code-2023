module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import TwoThousandFifteen exposing (day1Part1Floor, day1Part2BasementCharacter, day2Part2WrappingPaper)
import Url
import Url.Parser exposing ((</>), map, oneOf, parse, s)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initialModel key url (parseRoute url)
    , Cmd.none
    )


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , floor : Int
    , basementCharacter : Int
    , squareFeetOfWrappingPaper : Int
    }


initialModel : Nav.Key -> Url.Url -> Route -> Model
initialModel key url route =
    { key = key
    , url = url
    , route = route
    , floor = 0
    , basementCharacter = 0
    , squareFeetOfWrappingPaper = 0
    }


type Route
    = NotFound
    | Day12015Warmup
    | Day1
    | Day2


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    oneOf
        [ map Day12015Warmup (s "warmup")
        , map Day1 (s "day1")
        , map Day2 (s "day2")
        ]


parseRoute : Url.Url -> Route
parseRoute url =
    parse routeParser url
        |> Maybe.withDefault NotFound


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ParseFloors
    | ParseBasementCharacterPosition
    | ParsePresentPaperSizes


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

        ParseFloors ->
            ( { model | floor = day1Part1Floor }, Cmd.none )

        ParseBasementCharacterPosition ->
            ( { model | basementCharacter = day1Part2BasementCharacter }, Cmd.none )

        ParsePresentPaperSizes ->
            ( { model | squareFeetOfWrappingPaper = day2Part2WrappingPaper }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2023"
    , body =
        [ div [ class "w-full bg-gray-800 border-gray-200" ]
            [ tabs
            , case model.route of
                NotFound ->
                    div [] [ text "Not found" ]

                Day12015Warmup ->
                    div []
                        [ text "Day 1 & 2 2015 Warmup"
                        , button
                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                            , onClick ParseFloors
                            ]
                            [ text "Calculate Floor" ]
                        , div [] [ text ("Floor: " ++ String.fromInt model.floor) ]
                        , button
                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                            , onClick ParseBasementCharacterPosition
                            ]
                            [ text "Calculate Basement Character Position" ]
                        , div [] [ text ("Basement Position: " ++ String.fromInt model.basementCharacter) ]
                        , button
                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                            , onClick ParsePresentPaperSizes
                            ]
                            [ text "Wrapping Paper" ]
                        , div [] [ text ("Wrapping Paper Square Feet: " ++ String.fromInt model.squareFeetOfWrappingPaper) ]
                        ]

                Day1 ->
                    div [] [ text "Day 1" ]

                Day2 ->
                    div [] [ text "Day 2" ]
            ]
        ]
    }


tabs =
    ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
        [ li [ class "me-2" ]
            [ a [ class "inline-block p-4 text-blue-600 bg-gray-100 rounded-t-lg active dark:bg-gray-800 dark:text-blue-500", href "/warmup" ] [ text "Day 1 2015 Warmup" ]
            ]
        , li [ class "me-2" ]
            [ a [ class "inline-block p-4 text-blue-600 bg-gray-100 rounded-t-lg active dark:bg-gray-800 dark:text-blue-500", href "/day1" ] [ text "Day 1" ]
            ]
        , li [ class "me-2" ]
            [ a [ class "inline-block p-4 rounded-t-lg hover:text-gray-600 hover:bg-gray-50 dark:hover:bg-gray-800 dark:hover:text-gray-300", href "/day2" ] [ text "Day 2" ]
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
