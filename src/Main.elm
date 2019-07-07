module Main exposing (Model(..), Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, pre, section, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode exposing (Decoder, andThen, at, field, index, int, list, map, map2, map4, string, succeed)
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


type alias Article =
    { title : String
    , text : String
    , displayModel : SelectableText.Model
    , definitionText : Maybe String
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
    | RefreshSite
    | GotDefinition (Result Http.Error String) Article
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



-- WikiRest API um zufälligen Title (bzw den Namen) zu laden


api2 : String
api2 =
    "https://en.wikipedia.org/api/rest_v1/page/random/title"


getApiString : Cmd Msg
getApiString =
    Http.get
        { url = api2
        , expect = Http.expectJson GotUrl randomDecoder
        }



-- Decoder der Apis


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



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = api2
        , expect = Http.expectJson GotUrl randomDecoder
        }
    )



-- neue Seite laden


refresh : Cmd Msg
refresh =
    Http.get
        { url = api2
        , expect = Http.expectJson GotUrl randomDecoder
        }



-- ruft random WikiAPI auf


callWikiApi : String -> Int -> Cmd Msg
callWikiApi apiString pageNr =
    Http.get
        { url = api apiString
        , expect = Http.expectJson GotText (articleDecoder pageNr)
        }



-- Decoder von Article
-- um Json zu verarbeiten


articleDecoder : Int -> Decoder Article
articleDecoder pageNr =
    map4 Article
        (titleDecoder (String.fromInt pageNr))
        (textDecoder (String.fromInt pageNr))
        (displayModelDecoder (String.fromInt pageNr))
        (succeed Nothing)


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



-- message wandelt msg in cmd um


message : Msg -> Cmd Msg
message msg =
    Task.perform identity (Task.succeed msg)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- macht nichts
        NoOp ->
            ( model, Cmd.none )

        -- Article geladen
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

        -- ruft APIs auf
        GotUrl result ->
            case result of
                Ok fullUrl ->
                    ( Loading, callWikiApi fullUrl.title fullUrl.pageId )

                Err e ->
                    ( Failure e, Cmd.none )

        -- neuen Article laden
        RefreshSite ->
            ( Loading, refresh )

        -- Rueckgabe zu SelectableText um markieren des Textes zu speichern
        -- DisplayModel wird aktualisiert
        SelectableTextMessage subMsg ->
            case model of
                Success article ->
                    let
                        newDisplayModel =
                            SelectableText.update subMsg article.displayModel

                        newArticle =
                            { article | displayModel = newDisplayModel, definitionText = Nothing }

                        command =
                            --case newDisplayModel.selectedPhrase of
                            --    Just selectedPhrase ->
                            --        callWordApi selectedPhrase newArticle
                            --    Nothing ->
                            Cmd.none
                    in
                    ( Success newArticle, command )

                _ ->
                    ( model, Cmd.none )

        -- Aufruf wenn Definition gesucht
        GetDefinition selected article ->
            ( Success article, callWordApi selected article )

        -- liefert DEfinition zurueck
        GotDefinition definition article ->
            let
                definitionText =
                    case definition of
                        Ok fullDef ->
                            Just fullDef

                        Err e ->
                            Just "error"

                newArticle =
                    { article | definitionText = definitionText }
            in
            ( Success newArticle, Cmd.none )



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
                                [ text "loading title.." ]
                            , div [ class "content" ]
                                [ div []
                                    [ text "nothing selected"
                                    ]
                                ]
                            , div [ class "content" ]
                                [ button [ class "button is-info" ]
                                    [ text "Load Definition" ]
                                ]
                            , div [ class "box" ]
                                [ text "loading text.." ]
                            ]
                        ]
                    ]
                ]

        -- erfolgreich Article geladen
        Success article ->
            let
                -- markierter Text
                selectedText =
                    Maybe.withDefault "nothing selected" article.displayModel.selectedPhrase

                -- Edfinition des markierten Textes
                definitionText =
                    case article.definitionText of
                        Just definition ->
                            " - " ++ definition

                        Nothing ->
                            ""

                -- Article als Html
                textView =
                    SelectableText.view article.displayModel
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
                                [ div []
                                    [ text (selectedText ++ definitionText)
                                    ]
                                ]
                            , div [ class "content" ]
                                [ button [ class "button is-info", onClick (GetDefinition selectedText article) ]
                                    [ text "Load Definition" ]
                                ]
                            , div [ class "box" ]
                                [ textView ]
                            ]
                        ]
                    ]
                ]



-- alles fuer Definitions API
-- API call


callWordApi : String -> Article -> Cmd Msg
callWordApi input article =
    Http.request
        { method = "GET"
        , headers = headers3
        , url = api3 input
        , body = Http.emptyBody
        , expect = Http.expectJson (createGotDefinition article) definitionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


headers3 : List Header
headers3 =
    [ header "X-RapidAPI-Host" "wordsapiv1.p.rapidapi.com"
    , header "X-RapidAPI-Key" "820b1c05d6msh6ce2ee05c73291fp132ba2jsn72892ae44c16"
    ]



-- DefinitionsAPI


api3 : String -> String
api3 input =
    "https://wordsapiv1.p.rapidapi.com/words/" ++ input ++ "/definitions"



-- flip GotDefinition


createGotDefinition : Article -> Result Http.Error String -> Msg
createGotDefinition article httpResult =
    GotDefinition httpResult article



-- Decoder


definitionDecoder : Decoder String
definitionDecoder =
    field "definitions" (index 0 (field "definition" string))



-- header


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
