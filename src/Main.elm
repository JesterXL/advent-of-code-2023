module Main exposing (main)

import Browser
import Browser.Dom exposing (Error(..))
import Browser.Navigation as Nav
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (Html, a, b, button, code, div, h5, iframe, img, li, nav, p, pre, span, text, ul)
import Html.Attributes exposing (attribute, class, height, href, src, title, width)
import Html.Events exposing (onClick)
import SyntaxHighlight exposing (elm, monokai, toBlockHtml, useTheme)
import TwoThousandFifteen exposing (day1Part1Floor, day1Part1FloorCodeString, day1Part2BasementCharacter, day1Part2BasementCharacterCodeString, day2Part1WrappingPaper, day2Part1WrappingPaperCodeString, day2Part2RibbonLength, day2Part2RibbonLengthCodeString)
import Url
import Url.Parser exposing ((</>), (<?>), map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initialModel key url (parseRoute url)
    , Cmd.none
    )


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , warmups : Warmups
    }


type alias Warmups =
    { floor : Int
    , basementCharacter : Int
    , squareFeetOfWrappingPaper : Int
    , ribbonFeet : Int
    }


initialModel : Nav.Key -> Url.Url -> Route -> Model
initialModel key url route =
    { key = key
    , url = url
    , route = route
    , warmups =
        { floor = 0
        , basementCharacter = 0
        , squareFeetOfWrappingPaper = 0
        , ribbonFeet = 0
        }
    }


type Route
    = Warmup (Maybe Int)
    | TwoThousandTwentyThree (Maybe Int)
    | About
    | NotFound


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    oneOf
        [ map Warmup (s "advent-of-code-2023" </> s "2015" <?> Query.int "day")
        , map TwoThousandTwentyThree (s "advent-of-code-2023" </> s "2023" <?> Query.int "day")
        , map About (s "advent-of-code-2023" </> s "about")
        ]


parseRoute : Url.Url -> Route
parseRoute url =
    parse routeParser url
        |> Maybe.withDefault (TwoThousandTwentyThree (Just 1))


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ParseFloors
    | ParseBasementCharacterPosition
    | ParsePresentPaperSizes
    | ParseRibbonLength


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        warmup =
            model.warmups
    in
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
            ( { model | warmups = { warmup | floor = day1Part1Floor } }, Cmd.none )

        ParseBasementCharacterPosition ->
            ( { model | warmups = { warmup | basementCharacter = day1Part2BasementCharacter } }, Cmd.none )

        ParsePresentPaperSizes ->
            ( { model | warmups = { warmup | squareFeetOfWrappingPaper = day2Part1WrappingPaper } }, Cmd.none )

        ParseRibbonLength ->
            ( { model | warmups = { warmup | ribbonFeet = day2Part2RibbonLength } }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2023"
    , body =
        [ div [ class "w-full bg-gray-900 border-gray-200 antialiased" ]
            [ navbar model.route
            , case model.route of
                Warmup day ->
                    case day of
                        Just 1 ->
                            div []
                                [ tabs2015 1
                                , div [ class "flex flex-row gap-6 p-6" ]
                                    [ div [ class "flex flex-col gap-6 w-[350px] block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                        [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Wrapping Paper" ]
                                        , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Wrapping Paper Square Feet: " ++ formatInt model.warmups.squareFeetOfWrappingPaper) ]
                                        , button
                                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                            , onClick ParsePresentPaperSizes
                                            ]
                                            [ text "Calculate Wrapping Paper" ]
                                        , elmCode day1Part1FloorCodeString
                                        ]
                                    , div [ class "flex flex-col gap-6 w-[620px] block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                        [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Ribbon Length" ]
                                        , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Ribbon Length: " ++ formatInt model.warmups.ribbonFeet) ]
                                        , button
                                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                            , onClick ParseRibbonLength
                                            ]
                                            [ text "Ribbon Length" ]
                                        , elmCode day1Part2BasementCharacterCodeString
                                        ]
                                    ]
                                ]

                        Just 2 ->
                            div []
                                [ tabs2015 2
                                , div [ class "flex flex-row gap-6 p-6" ]
                                    [ div [ class "flex flex-col gap-6 w-[460px] block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                        [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Wrapping Paper" ]
                                        , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Floor: " ++ formatInt model.warmups.floor) ]
                                        , button
                                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                            , onClick ParseFloors
                                            ]
                                            [ text "Calculate Floor" ]
                                        , elmCode day2Part1WrappingPaperCodeString
                                        ]
                                    , div [ class "flex flex-col gap-6 w-[540px] block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                        [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Wrapping Paper" ]
                                        , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Basement Position: " ++ formatInt model.warmups.basementCharacter) ]
                                        , button
                                            [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                            , onClick ParseBasementCharacterPosition
                                            ]
                                            [ text "Calculate Basement Character Position" ]
                                        , elmCode day2Part2RibbonLengthCodeString
                                        ]
                                    ]
                                ]

                        _ ->
                            span [] []

                TwoThousandTwentyThree day ->
                    case day of
                        Just 2 ->
                            div [] [ tabs2023 2, div [ class "m-6 block max-w-sm p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ] [] ]

                        _ ->
                            div [] [ tabs2023 1, div [ class "m-6 block max-w-sm p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ] [] ]

                About ->
                    div [ class "flex flex-row gap-6 p-6 justify-center" ]
                        [ div [ class "flex flex-col gap-6 w-[520px] block p-6 border rounded-lg shadow bg-gray-800 border-gray-700" ]
                            [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "What is Advent of Code?" ]
                            , p [ class "font-normal text-gray-400" ] [ a [ class "font-medium text-blue-500 hover:underline", href "https://adventofcode.com/" ] [ text "Advent of Code" ], text " is a yearly code challenge that starts Dec 1st and ends Dec 25th. The code challenges are Christmas oriented, and usually follow some dual theme; like helping \"Santa's elves save Christmas\" while at the same time you're \"building the Apollo space flight navigation computer.\"" ]
                            , p [ class "font-normal text-gray-400" ] [ text "Each day has a 2 part challenge, and each day gets progressively harder. After you complete the part 1 challenge successfully, they'll give you part 2. This makes it like the real-world where you learn new things from a client, and you may have to go back and re-think how you built part 1." ]
                            , p [ class "font-normal text-gray-400" ] [ text "I don't like leetcode style challenges at all, but I like these. Even completing just Day 1 should make you feel good about yourself. I think my record was completing Day 6 back in 2020... like 14 days into December." ]
                            , p [ class "font-normal text-gray-400" ] [ text "You can use whatever programming language you like, or even tech. I've used JavaScript, Lua for Roblox, Elm, and Roc." ]
                            , p [ class "font-normal text-gray-400" ] [ text "You can try past years by clicking on \"Events\". Also, here's all the little UI's + source code I built in 2018 in Elm: ", a [ class "font-medium text-blue-500 hover:underline", href "https://jessewarden.com/2019/01/advent-of-code-2018-in-elm-review.html" ] [ text "https://jessewarden.com/2019/01/advent-of-code-2018-in-elm-review.html" ] ]
                            ]
                        , div [ class "flex flex-col gap-6 w-[520px] block p-6 border rounded-lg shadow bg-gray-800 border-gray-700" ]
                            [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Why Elm?" ]
                            , p [ class "font-normal text-gray-400" ] [ img [ src "elm-hat.png", width 120, class "float-left m-2" ] [], text "I choose to use ", a [ class "font-medium text-blue-500 hover:underline", href "https://elm-lang.org/" ] [ text "Elm" ], text " this year because I wanted a relaxing end of year. Elm is what I enjoy writing in so even if a challenge is hard, I'm still having fun. I was hoping to attempt some more visualizations this year like I did in 2018. You do NOT need to visualize your solutions, but I find it fun to build UI's." ]
                            , p [ class "font-normal text-gray-400" ] [ text "I thought about using Excel after seeing some inspiring AoC art others had done in it on Reddit and also because it now supports Lambda functions. I debated using either ", a [ class "font-medium text-blue-500 hover:underline", href "https://www.purescript.org/" ] [ text "PureScript" ], text " + ", a [ class "font-medium text-blue-500 hover:underline", href "https://purescript-halogen.github.io/purescript-halogen/" ] [ text "Halogen" ], text " or ", a [ class "font-medium text-blue-500 hover:underline", href "https://fable.io/" ] [ text "Fable" ], text " + ", a [ class "font-medium text-blue-500 hover:underline", href "https://fsharp.org/" ] [ text "F#" ], text " this year. Maybe next year I'll try something new." ]
                            , p [ class "font-normal text-gray-400" ] [ text "To learn more, I have a talk about Happiness in Elm." ]
                            , iframe [ width 460, height 258, src "https://www.youtube.com/embed/VJCP4_zgbPQ?si=0s-zuDiBN-h1PSOm", title "YouTube video player", attribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share", attribute "allowfullscreen" "" ] []
                            ]
                        ]

                NotFound ->
                    div [] []
            ]
        ]
    }


tabs2023 day =
    case day of
        2 ->
            ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
                [ li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2023?day=1", class tabStyle ] [ text "Day 1" ]
                    ]
                , li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2023?day=2", class selectedTabStyle ] [ text "Day 2" ]
                    ]
                ]

        _ ->
            ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
                [ li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2023?day=1", class selectedTabStyle ] [ text "Day 1" ]
                    ]
                , li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2023?day=2", class tabStyle ] [ text "Day 2" ]
                    ]
                ]


selectedTabStyle : String
selectedTabStyle =
    "inline-block p-4 text-blue-600 bg-gray-100 rounded-t-lg active dark:bg-gray-800 dark:text-blue-500"


tabStyle : String
tabStyle =
    "inline-block p-4 rounded-t-lg hover:text-gray-600 hover:bg-gray-50 dark:hover:bg-gray-800 dark:hover:text-gray-300"


tabs2015 warmupDay =
    case warmupDay of
        2 ->
            ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
                [ li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2015?day=1", class tabStyle ] [ text "Warmup Day 1" ]
                    ]
                , li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2015?day=2", class selectedTabStyle ] [ text "Warmup Day 2" ]
                    ]
                ]

        _ ->
            ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
                [ li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2015?day=1", class selectedTabStyle ] [ text "Warmup Day 1" ]
                    ]
                , li [ class "me-2" ]
                    [ a [ href "/advent-of-code-2023/2015?day=2", class tabStyle ] [ text "Warmup Day 2" ]
                    ]
                ]


navbar : Route -> Html Msg
navbar route =
    nav [ class "border-gray-200 bg-gray-900" ]
        [ div [ class "max-w-screen-xl flex flex-wrap items-center justify-between mx-auto p-4" ]
            [ a [ class "flex items-center space-x-3 rtl:space-x-reverse", href "/advent-of-code-2023/" ]
                [ img [ src "elm-hat.png", class "h-8" ] []
                , span [ class "self-center text-2xl font-semibold whitespace-nowrap dark:text-white" ] [ text "Advent of Code 2023 in Elm" ]
                ]
            , div [ class "hidden w-full md:block md:w-auto" ]
                [ ul [ class "font-medium flex flex-col p-4 md:p-0 mt-4 border border-gray-100 rounded-lg bg-gray-50 md:flex-row md:space-x-8 rtl:space-x-reverse md:mt-0 md:border-0 md:bg-white dark:bg-gray-800 md:dark:bg-gray-900 dark:border-gray-700" ]
                    [ navbarLink route (TwoThousandTwentyThree Nothing) "/advent-of-code-2023/2023?day=1" "2023"
                    , navbarLink route (Warmup Nothing) "/advent-of-code-2023/2015?day=1" "Warmups"
                    , navbarLink route About "/advent-of-code-2023/about" "About"
                    , navbarLink route NotFound "https://elm-lang.org/" "What Is Elm?"
                    ]
                ]
            ]
        ]


navbarLink : Route -> Route -> String -> String -> Html Msg
navbarLink route matchingRoute link textValue =
    case route of
        Warmup _ ->
            case matchingRoute of
                Warmup _ ->
                    li [] [ a [ href link, class navbarLinkSelectedStyle ] [ text textValue ] ]

                _ ->
                    li [] [ a [ href link, class navbarLinkStyle ] [ text textValue ] ]

        TwoThousandTwentyThree _ ->
            case matchingRoute of
                TwoThousandTwentyThree _ ->
                    li [] [ a [ href link, class navbarLinkSelectedStyle ] [ text textValue ] ]

                _ ->
                    li [] [ a [ href link, class navbarLinkStyle ] [ text textValue ] ]

        About ->
            case matchingRoute of
                About ->
                    li [] [ a [ href link, class navbarLinkSelectedStyle ] [ text textValue ] ]

                _ ->
                    li [] [ a [ href link, class navbarLinkStyle ] [ text textValue ] ]

        NotFound ->
            li [] [ a [ href link, class navbarLinkStyle ] [ text textValue ] ]


navbarLinkStyle : String
navbarLinkStyle =
    "block py-2 px-3 text-gray-900 rounded hover:bg-gray-100 md:hover:bg-transparent md:border-0 md:hover:text-blue-700 md:p-0 dark:text-white md:dark:hover:text-blue-500 dark:hover:bg-gray-700 dark:hover:text-white md:dark:hover:bg-transparent"


navbarLinkSelectedStyle : String
navbarLinkSelectedStyle =
    "block py-2 px-3 text-white bg-blue-700 rounded md:bg-transparent md:text-blue-700 md:p-0 dark:text-white md:dark:text-blue-500"


formatInt : Int -> String
formatInt value =
    format { usLocale | decimals = Exact 0 } (toFloat value)


formatNumber : Float -> String
formatNumber value =
    format usLocale value


elmCode : String -> Html Msg
elmCode codeString =
    div [ class "text-xs code" ]
        [ useTheme monokai
        , elm codeString
            |> Result.map (toBlockHtml (Just 1))
            |> Result.withDefault
                (pre [] [ code [] [ text codeString ] ])
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
