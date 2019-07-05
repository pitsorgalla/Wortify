module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, pre, section, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode exposing (Decoder, andThen, at, field, index, int, list, map, map2, map3, string, succeed)
import SelectableText exposing (defaultOptions)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure Http.Error
    | Loading
    | Success Article
    | Show String


type alias Article =
    { title : String
    , text : String
    , displayModel : SelectableText.Model
    }


type alias UrlLog =
    { title : String
    , pageId : Int
    }


type Msg
    = NoOp
    | GotText (Result Http.Error Article)
    | GotUrl (Result Http.Error UrlLog)
    | SelectableTextMessage SelectableText.Msg
    | FinishedSelectingText Article String
    | RefreshSite
    | GotDefinition (Result Http.Error String)
    | GetDefinition String Article


type DefMsg
    = NoDef
    | GotDef


globalStringTuple =
    { urlString = ""
    , pageDef = ""
    }



-- Wikipedia API um Wiki-Artikel zu laden (durch den Namen des Artikles der mit API2 geladen wurde)


api : String -> String
api apiString =
    "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exlimit=max&explaintext&exintro&titles=" ++ apiString ++ "&redirects=1&origin=*"



{-
   "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exlimit=max&explaintext&exintro&titles=German_battleship_Tirpitz&redirects=1&origin=*"
   "https://api.coindesk.com/v1/bpi/currentprice.json"
   "https://en.wikipedia.org/api/rest_v1/page/summary/Stack_Overflow"
-}
-- Wiki Rest API um zufälligen Title (bzw den Namen) zu laden


api2 : String
api2 =
    "https://en.wikipedia.org/api/rest_v1/page/random/title"


getApiString : Cmd Msg
getApiString =
    Http.get
        { url = api2
        , expect = Http.expectJson GotUrl randomDecoder
        }


randomDecoder : Decoder UrlLog
randomDecoder =
    map2 UrlLog
        ranTitleDecoder
        ranPageDecoder


ranTitleDecoder : Decoder String
ranTitleDecoder =
    field "items" (index 0 (field "title" string))


ranPageDecoder : Decoder Int
ranPageDecoder =
    field "items" (index 0 (field "page_id" int))


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = api2
        , expect = Http.expectJson GotUrl randomDecoder
        }
    )


refresh : Cmd Msg
refresh =
    Http.get
        { url = api2
        , expect = Http.expectJson GotUrl randomDecoder
        }


callWikiApi : String -> Int -> Cmd Msg
callWikiApi apiString pageNr =
    Http.get
        { url = api apiString
        , expect = Http.expectJson GotText (articleDecoder pageNr)
        }


articleDecoder : Int -> Decoder Article
articleDecoder pageNr =
    map3 Article
        (titleDecoder (String.fromInt pageNr))
        (textDecoder (String.fromInt pageNr))
        (displayModelDecoder (String.fromInt pageNr))


titleDecoder : String -> Decoder String
titleDecoder pageNr =
    at [ "query", "pages", pageNr, "title" ] string


textDecoder : String -> Decoder String
textDecoder pageNr =
    at [ "query", "pages", pageNr, "extract" ] string


displayModelDecoder : String -> Decoder SelectableText.Model
displayModelDecoder pageNr =
    at [ "query", "pages", pageNr, "extract" ] string
        |> map
            (\text ->
                let
                    options =
                        { defaultOptions | id = "text-area", maxSelectionLength = Just 3 }
                in
                SelectableText.initialModel options
            )



-- field "query" (field "pages" (field "31124517" (field "title" string)))
-- UPDATE


message : Msg -> Cmd Msg
message msg =
    Task.perform identity (Task.succeed msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotText result ->
            case result of
                Ok article ->
                    let
                        messageToDisplayModel =
                            SelectableTextMessage <| SelectableText.RenderText article.text

                        cmd =
                            message messageToDisplayModel
                    in
                    ( Success article, cmd )

                Err e ->
                    ( Failure e, Cmd.none )

        GotUrl result ->
            case result of
                Ok fullUrl ->
                    ( Loading, callWikiApi fullUrl.title fullUrl.pageId )

                Err e ->
                    ( Failure e, Cmd.none )

        RefreshSite ->
            ( Loading, refresh )

        SelectableTextMessage subMsg ->
            case model of
                Success article ->
                    let
                        newDisplayModel =
                            SelectableText.update subMsg article.displayModel

                        newArticle =
                            { article | displayModel = newDisplayModel }
                    in
                    ( Success newArticle, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FinishedSelectingText article selectedText ->
            ( Success article, Cmd.none )

        GetDefinition selected article ->
            ( Success article, callWordApi selected )

        GotDefinition result ->
            case result of
                Ok fullDef ->
                    ( Show fullDef, Cmd.none )

                Err e ->
                    ( Failure e, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure e ->
            text "I was unable to load your text."

        Loading ->
            div []
                [ applicationHeader
                , section [ class "hero" ]
                    [ div [ class "hero-body" ]
                        [ div [ class "container" ]
                            [ h3 [ class "title" ]
                                [ text "loading title..." ]
                            , div [ class "content" ]
                                [ --div [ class "button is-info is-rounded" ]
                                  --  [ text "info" ]
                                  --, text " "
                                  div [ class "button is-white" ]
                                    [ text "nothing selected" ]
                                ]
                            , div [ class "box" ]
                                [ text "loading content..." ]
                            ]
                        ]
                    ]
                ]

        Success article ->
            let
                selectedText =
                    Maybe.withDefault "nothing selected" article.displayModel.selectedPhrase

                textView =
                    SelectableText.view article.displayModel
                        |> Debug.log (Debug.toString article)
                        |> Html.map SelectableTextMessage
            in
            div []
                [ applicationHeader
                , section [ class "hero" ]
                    [ div [ class "hero-body" ]
                        [ div [ class "container" ]
                            [ h3 [ class "title" ]
                                [ text article.title ]
                            , div [ class "content" ]
                                [ -- button [ class "button is-info is-rounded", onClick (GetDefinition selectedText article) ]
                                  --    [ text "show defnition of" ]
                                  --, text " "
                                  button [ class "button is-white", onClick (GetDefinition selectedText article) ]
                                    [ text selectedText ]
                                ]
                            , div [ class "box" ]
                                [ textView ]

                            {-
                               , div [class "box" ]
                                   [ viewDefinition selectedText ]
                            -}
                            ]
                        ]
                    ]
                ]

        Show definition ->
            text definition



{-
   viewDefinition : String -> Html Msg
   viewDefinition word =



   updateDef : Msg -> Model -> ( Model, Cmd Msg )
   updateDef msg model =
       case msg of
           NoDef ->
               ( model, Cmd.none )

           GotDef result ->
               case result of
                   Ok fullDefinition ->
                       ( Success fullDefinition, Cmd.none )

                   Err e ->
                       ( Failure e, Cmd.none )
-}


definitionDecoder : Decoder String
definitionDecoder =
    field "definitions" (index 0 (field "definition" string))


api3 : String -> String
api3 input =
    "https://wordsapiv1.p.rapidapi.com/words/" ++ input ++ "/definitions"


callWordApi : String -> Cmd Msg
callWordApi input =
    Http.request
        { method = "GET"
        , headers = headers3
        , url = api3 input
        , body = Http.emptyBody
        , expect = Http.expectJson GotDefinition definitionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


headers3 : List Header
headers3 =
    [ header "X-RapidAPI-Host" "wordsapiv1.p.rapidapi.com"
    , header "X-RapidAPI-Key" "820b1c05d6msh6ce2ee05c73291fp132ba2jsn72892ae44c16"
    ]



-- pretty header


applicationHeader : Html Msg
applicationHeader =
    section [ class "hero is-info" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ]
                    [ text applicationTitle ]
                , h1 [ class "subtitle" ]
                    [ text applicationSubTitle ]
                , button [ class "button is-important is-rounded", onClick RefreshSite ]
                    [ text "new text" ]
                ]
            ]
        ]


applicationTitle : String.String
applicationTitle =
    "Wortify"


applicationSubTitle : String.String
applicationSubTitle =
    "Ein Projekt von Pit, Paul und Nina"
