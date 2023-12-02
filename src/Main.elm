module Main exposing (main)

import Browser
import Browser.Dom exposing (Error(..))
import Browser.Navigation as Nav
import Day1 exposing (day1Part1, day1Part1CodeString, day1Part2, day1Part2CodeString)
import Day2 exposing (day2Part1, day2Part1CodeString, day2Part2, day2Part2CodeString)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (Html, a, b, br, button, code, div, h5, iframe, img, li, nav, p, pre, span, text, ul)
import Html.Attributes exposing (attribute, class, height, href, src, title, width)
import Html.Events exposing (onClick)
import SyntaxHighlight exposing (elm, monokai, toBlockHtml, useTheme)
import TwoThousandFifteen exposing (day1Part1Floor, day1Part1FloorCodeString, day1Part2BasementCharacter, day1Part2BasementCharacterCodeString, day2Part1WrappingPaper, day2Part1WrappingPaperCodeString, day2Part2RibbonLength, day2Part2RibbonLengthCodeString, day3Part1HousePresents, day3Part1HousePresentsCodeString, day3Part2HousePresentsRobot)
import Url
import Url.Parser exposing ((</>), (<?>), map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


type alias Model =
    { page : Page
    , warmups : Warmups
    , day1 : Day1Model
    , day2 : Day2Model
    }


type Page
    = WarmupsDay1
    | WarmupsDay2
    | WarmupsDay3
    | TwoThousandTwentyThreeDay1
    | TwoThousandTwentyThreeDay2
    | About


type alias Warmups =
    { floor : Int
    , basementCharacter : Int
    , squareFeetOfWrappingPaper : Int
    , ribbonFeet : Int
    , housePresents : Int
    , housePresentsRobot : Int
    }


type alias Day1Model =
    { trebuchetConfig : Int
    , trebuchetConfigEnhanced : Int
    }


type alias Day2Model =
    { thresholdGames : Int
    , minimumCubeSetPower : Int
    }


initialModel : Model
initialModel =
    { page = TwoThousandTwentyThreeDay1
    , warmups =
        { floor = 0
        , basementCharacter = 0
        , squareFeetOfWrappingPaper = 0
        , ribbonFeet = 0
        , housePresents = 0
        , housePresentsRobot = 0
        }
    , day1 =
        { trebuchetConfig = 0
        , trebuchetConfigEnhanced = 0
        }
    , day2 =
        { thresholdGames = 0
        , minimumCubeSetPower = 0
        }
    }


type Msg
    = GoToPage Page
    | ParseFloors
    | ParseBasementCharacterPosition
    | ParsePresentPaperSizes
    | ParseRibbonLength
    | ParseHousePresents
    | ParseHousePresentsRobot
    | Day1Part1
    | Day1Part2
    | Day2Part1
    | Day2Part2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        warmup =
            model.warmups

        day1 =
            model.day1

        day2 =
            model.day2
    in
    case msg of
        GoToPage page ->
            ( { model | page = page }, Cmd.none )

        ParseFloors ->
            ( { model | warmups = { warmup | floor = day1Part1Floor } }, Cmd.none )

        ParseBasementCharacterPosition ->
            ( { model | warmups = { warmup | basementCharacter = day1Part2BasementCharacter } }, Cmd.none )

        ParsePresentPaperSizes ->
            ( { model | warmups = { warmup | squareFeetOfWrappingPaper = day2Part1WrappingPaper } }, Cmd.none )

        ParseRibbonLength ->
            ( { model | warmups = { warmup | ribbonFeet = day2Part2RibbonLength } }, Cmd.none )

        ParseHousePresents ->
            ( { model | warmups = { warmup | housePresents = day3Part1HousePresents } }, Cmd.none )

        ParseHousePresentsRobot ->
            ( { model | warmups = { warmup | housePresentsRobot = day3Part2HousePresentsRobot } }, Cmd.none )

        Day1Part1 ->
            ( { model | day1 = { day1 | trebuchetConfig = day1Part1 } }, Cmd.none )

        Day1Part2 ->
            ( { model | day1 = { day1 | trebuchetConfigEnhanced = day1Part2 } }, Cmd.none )

        Day2Part1 ->
            ( { model | day2 = { day2 | thresholdGames = day2Part1 } }, Cmd.none )

        Day2Part2 ->
            ( { model | day2 = { day2 | minimumCubeSetPower = day2Part2 } }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2023"
    , body =
        [ div [ class "w-full bg-gray-900 border-gray-200 antialiased" ]
            [ navbar model.page
            , case model.page of
                WarmupsDay1 ->
                    div []
                        [ tabs2015 model.page
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

                WarmupsDay2 ->
                    div []
                        [ tabs2015 model.page
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

                WarmupsDay3 ->
                    div []
                        [ tabs2015 model.page
                        , div [ class "flex flex-row gap-6 p-6" ]
                            [ div [ class "flex flex-col gap-6 w-[510px] block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Santa Present Delivery" ]
                                , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("House Presents: " ++ formatInt model.warmups.housePresents) ]
                                , button
                                    [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                    , onClick ParseHousePresents
                                    ]
                                    [ text "Calculate Presents in Houses" ]
                                , elmCode day3Part1HousePresentsCodeString
                                ]
                            , div [ class "flex flex-col gap-6 w-[460px] block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Santa + Robot" ]
                                , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("House Presents: " ++ formatInt model.warmups.housePresentsRobot) ]
                                , button
                                    [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                    , onClick ParseHousePresentsRobot
                                    ]
                                    [ text "Calculate Robot Help" ]
                                , elmCode "???"
                                ]
                            ]
                        ]

                TwoThousandTwentyThreeDay1 ->
                    div []
                        [ tabs2023 model.page
                        , div [ class "flex flex-row gap-6 p-6" ]
                            [ div [ class "flex flex-col gap-6 block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Trebuchet Configs" ]
                                , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Trebuchet Value: " ++ formatInt model.day1.trebuchetConfig) ]
                                , button
                                    [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                    , onClick Day1Part1
                                    ]
                                    [ text "Calculate Trebuchet Config" ]
                                , elmCode day1Part1CodeString
                                ]
                            , div [ class "flex flex-col gap-6 block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Trebuchet Enhanced Configs" ]
                                , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Trebuchet Enhanced: " ++ formatInt model.day1.trebuchetConfigEnhanced) ]
                                , button
                                    [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                    , onClick Day1Part2
                                    ]
                                    [ text "Ribbon Length" ]
                                , elmCode day1Part2CodeString
                                ]
                            ]
                        ]

                TwoThousandTwentyThreeDay2 ->
                    div []
                        [ tabs2023 model.page
                        , div [ class "flex flex-row gap-6 p-6" ]
                            [ div [ class "flex flex-col gap-6 block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Threshold Games" ]
                                , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Threshold Games: " ++ formatInt model.day2.thresholdGames) ]
                                , button
                                    [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                    , onClick Day2Part1
                                    ]
                                    [ text "Calculate Threshold Games" ]
                                , elmCode day2Part1CodeString
                                ]
                            , div [ class "flex flex-col gap-6 block p-6 bg-white border border-gray-200 rounded-lg shadow hover:bg-gray-100 dark:bg-gray-800 dark:border-gray-700 dark:hover:bg-gray-700" ]
                                [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Minimum Cube Set Power" ]
                                , div [ class "font-normal text-gray-700 dark:text-gray-400" ] [ text ("Minimum Cube Set Power: " ++ formatInt model.day2.minimumCubeSetPower) ]
                                , button
                                    [ class "ext-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 dark:bg-blue-600 dark:hover:bg-blue-700 focus:outline-none dark:focus:ring-blue-800"
                                    , onClick Day2Part2
                                    ]
                                    [ text "Calculate Minimum Cube Set Power" ]
                                , elmCode day2Part2CodeString
                                ]
                            ]
                        ]

                About ->
                    div [ class "flex flex-row gap-6 p-6 justify-center" ]
                        [ div [ class "flex flex-col gap-6 w-[420px] min-w-[420px] block p-6 border rounded-lg shadow bg-gray-800 border-gray-700" ]
                            [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "What is Advent of Code?" ]
                            , p [ class "font-normal text-gray-400" ] [ a [ class "font-medium text-blue-500 hover:underline", href "https://adventofcode.com/" ] [ text "Advent of Code" ], text " is a yearly code challenge that starts Dec 1st and ends Dec 25th. The code challenges are Christmas oriented, and usually follow some dual theme; like helping \"Santa's elves save Christmas\" while at the same time you're \"building the Apollo space flight navigation computer.\"" ]
                            , p [ class "font-normal text-gray-400" ] [ text "Each day has a 2 part challenge, and each day gets progressively harder. After you complete the part 1 challenge successfully, they'll give you part 2. This makes it like the real-world where you learn new things from a client, and you may have to go back and re-think how you built part 1." ]
                            , p [ class "font-normal text-gray-400" ] [ text "I don't like leetcode style challenges at all, but I like these. Even completing just Day 1 should make you feel good about yourself. I think my record was completing Day 6 back in 2020... like 14 days into December." ]
                            , p [ class "font-normal text-gray-400" ] [ text "You can use whatever programming language you like, or even tech. I've used JavaScript, Lua for Roblox, Elm, and Roc." ]
                            , p [ class "font-normal text-gray-400" ] [ text "You can try past years by clicking on \"Events\". Also, here's all the little UI's + source code I built in 2018 in Elm: ", a [ class "font-medium text-blue-500 hover:underline", href "https://jessewarden.com/2019/01/advent-of-code-2018-in-elm-review.html" ] [ text "https://jessewarden.com/2019/01/advent-of-code-2018-in-elm-review.html" ] ]
                            ]
                        , div [ class "flex flex-col gap-6 w-[520px] min-w-[520px] block p-6 border rounded-lg shadow bg-gray-800 border-gray-700" ]
                            [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Why Elm?" ]
                            , p [ class "font-normal text-gray-400" ] [ img [ src "elm-hat.png", width 120, class "float-left m-2" ] [], text "I choose to use ", a [ class "font-medium text-blue-500 hover:underline", href "https://elm-lang.org/" ] [ text "Elm" ], text " this year because I wanted a relaxing end of year. Elm is what I enjoy writing in so even if a challenge is hard, I'm still having fun. I was hoping to attempt some more visualizations this year like I did in 2018. You do NOT need to visualize your solutions, but I find it fun to build UI's." ]
                            , p [ class "font-normal text-gray-400" ] [ text "I thought about using Excel after seeing some inspiring AoC art others had done in it on Reddit and also because it now supports Lambda functions. I debated using either ", a [ class "font-medium text-blue-500 hover:underline", href "https://www.purescript.org/" ] [ text "PureScript" ], text " + ", a [ class "font-medium text-blue-500 hover:underline", href "https://purescript-halogen.github.io/purescript-halogen/" ] [ text "Halogen" ], text " or ", a [ class "font-medium text-blue-500 hover:underline", href "https://fable.io/" ] [ text "Fable" ], text " + ", a [ class "font-medium text-blue-500 hover:underline", href "https://fsharp.org/" ] [ text "F#" ], text " this year. Maybe next year I'll try something new." ]
                            , p [ class "font-normal text-gray-400" ] [ text "To learn more, I have a talk about Happiness in Elm." ]
                            , iframe [ width 460, height 258, src "https://www.youtube.com/embed/VJCP4_zgbPQ?si=0s-zuDiBN-h1PSOm", title "YouTube video player", attribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share", attribute "allowfullscreen" "" ] []
                            ]
                        , div [ class "w-[250px] min-w-[250px] block p-6 self-baseline border rounded-lg shadow bg-gray-800 border-gray-700" ]
                            [ h5 [ class "mb-2 text-2xl font-bold tracking-tight text-gray-900 dark:text-white" ] [ text "Who" ]
                            , img [ src "jxl-helmet.jpg", class "float-left m-1" ] []
                            , p [ class "font-normal text-gray-400" ] [ text "I'm Jesse Warden, a software developer. I love building UI's and API's in AWS Serverless." ]
                            , span []
                                [ img [ class "inline mr-1", src "jxl-logo.png", width 16 ] []
                                , a [ class "font-medium text-blue-500 hover:underline", href "https://www.jessewarden.com/" ] [ text "Blog" ]
                                ]
                            , br [] []
                            , span []
                                [ img [ class "inline mr-1", src "youtube-logo.png", width 16 ] []
                                , a [ class "font-medium text-blue-500 hover:underline", href "https://www.youtube.com/@JesseWarden" ] [ text "YouTube Programming" ]
                                ]
                            , br [] []
                            , span []
                                [ img [ class "inline mr-1", src "youtube-logo.png", width 16 ] []
                                , a [ class "font-medium text-blue-500 hover:underline", href "https://www.youtube.com/@JesseWardenAdventures" ] [ text "YouTube Outdoors" ]
                                ]
                            , br [] []
                            , span []
                                [ img [ class "inline mr-1", src "linkedin-logo.png", width 16 ] []
                                , a [ class "font-medium text-blue-500 hover:underline", href "https://www.linkedin.com/in/jessewarden/" ] [ text "LinkedIn" ]
                                ]
                            , br [] []
                            , span []
                                [ img [ class "inline mr-1", src "x-logo.png", width 16 ] []
                                , a [ class "font-medium text-blue-500 hover:underline", href "https://twitter.com/jesterxl" ] [ text "X (aka Twitter)" ]
                                ]
                            , br [] []
                            , span []
                                [ img [ class "inline mr-1", src "github-logo.png", width 16 ] []
                                , a [ class "font-medium text-blue-500 hover:underline", href "https://github.com/JesterXL" ] [ text "GitHub" ]
                                ]
                            , br [] []
                            , span []
                                [ span [ class "mr-1" ] [ text "📧" ]
                                , a [ class "font-medium text-blue-500 hover:underline", href "mailto:jesse.warden@gmail.com" ] [ text "Email" ]
                                ]
                            ]
                        ]
            ]
        ]
    }


tabs2023 : Page -> Html Msg
tabs2023 page =
    ul [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
        [ li [ class "me-2" ]
            [ a [ href "#", onClick (GoToPage TwoThousandTwentyThreeDay1), class (getSelectedTabStyle page TwoThousandTwentyThreeDay1) ] [ text "Day 1" ]
            ]
        , li [ class "me-2" ]
            [ a [ href "#", onClick (GoToPage TwoThousandTwentyThreeDay2), class (getSelectedTabStyle page TwoThousandTwentyThreeDay2) ] [ text "Day 2" ]
            ]
        ]


getSelectedTabStyle : Page -> Page -> String
getSelectedTabStyle pageA pageB =
    if pageA == pageB then
        selectedTabStyle

    else
        tabStyle


selectedTabStyle : String
selectedTabStyle =
    "inline-block p-4 text-blue-600 bg-gray-100 rounded-t-lg active dark:bg-gray-800 dark:text-blue-500"


tabStyle : String
tabStyle =
    "inline-block p-4 rounded-t-lg hover:text-gray-600 hover:bg-gray-50 dark:hover:bg-gray-800 dark:hover:text-gray-300"


tabs2015 warmupDay =
    ul
        [ class "flex flex-wrap text-sm font-medium text-center text-gray-500 border-b border-gray-200 dark:border-gray-700 dark:text-gray-400" ]
        [ li [ class "me-2" ]
            [ a [ href "#", onClick (GoToPage WarmupsDay1), class (getSelectedTabStyle warmupDay WarmupsDay1) ] [ text "Warmup Day 1" ]
            ]
        , li [ class "me-2" ]
            [ a [ href "#", onClick (GoToPage WarmupsDay2), class (getSelectedTabStyle warmupDay WarmupsDay2) ] [ text "Warmup Day 2" ]
            ]
        , li [ class "me-2" ]
            [ a [ href "#", onClick (GoToPage WarmupsDay3), class (getSelectedTabStyle warmupDay WarmupsDay3) ] [ text "Warmup Day 3" ]
            ]
        ]


navbar : Page -> Html Msg
navbar page =
    nav [ class "border-gray-200 bg-gray-900" ]
        [ div [ class "max-w-screen-xl flex flex-wrap items-center justify-between mx-auto p-4" ]
            [ a [ class "flex items-center space-x-3 rtl:space-x-reverse", href "/advent-of-code-2023/" ]
                [ img [ src "elm-hat.png", class "h-8" ] []
                , span [ class "self-center text-2xl font-semibold whitespace-nowrap dark:text-white" ] [ text "Advent of Code 2023 in Elm" ]
                ]
            , div [ class "hidden w-full md:block md:w-auto" ]
                [ ul [ class "font-medium flex flex-col p-4 md:p-0 mt-4 border border-gray-100 rounded-lg bg-gray-50 md:flex-row md:space-x-8 rtl:space-x-reverse md:mt-0 md:border-0 md:bg-white dark:bg-gray-800 md:dark:bg-gray-900 dark:border-gray-700" ]
                    [ li [] [ a [ href "#", class (getSelectedStyle2023 page), onClick (GoToPage TwoThousandTwentyThreeDay1) ] [ text "2023" ] ]
                    , li [] [ a [ href "#", class (getSelectedStyleWarmups page), onClick (GoToPage WarmupsDay1) ] [ text "Warmups" ] ]
                    , navbarLink page About "About"
                    , li [] [ a [ href "https://elm-lang.org/", class navbarLinkStyle ] [ text "What Is Elm?" ] ]
                    ]
                ]
            ]
        ]


navbarLink : Page -> Page -> String -> Html Msg
navbarLink page matchingPage textValue =
    li [] [ a [ href "#", class (getSelectedStyle page matchingPage), onClick (GoToPage matchingPage) ] [ text textValue ] ]


getSelectedStyle : Page -> Page -> String
getSelectedStyle pageA pageB =
    if pageA == pageB then
        navbarLinkSelectedStyle

    else
        navbarLinkStyle


getSelectedStyleWarmups : Page -> String
getSelectedStyleWarmups page =
    if page == WarmupsDay1 || page == WarmupsDay2 || page == WarmupsDay3 then
        navbarLinkSelectedStyle

    else
        navbarLinkStyle


getSelectedStyle2023 : Page -> String
getSelectedStyle2023 page =
    if page == TwoThousandTwentyThreeDay1 || page == TwoThousandTwentyThreeDay2 then
        navbarLinkSelectedStyle

    else
        navbarLinkStyle


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
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
