port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Browser
import Dict
import Html exposing (Html, a, div, img, text)
import Html.Attributes as Attr
import Json.Encode
import Maybe.Extra
import QS
import Random
import Random.Char
import Random.String
import Url exposing (Url)
import Url.Builder



-- PORTS


port cache : Json.Encode.Value -> Cmd msg



-- MODEL


type alias Model =
    { publicPath : String
    , redditAuthState : Maybe String
    , queryParams : Dict.Dict String QS.OneOrMany
    , isLoggedIn : Bool
    }


getQueryParamStringValue : QS.OneOrMany -> Maybe String
getQueryParamStringValue oneOrMany =
    case oneOrMany of
        QS.One one ->
            case one of
                QS.Str str ->
                    Just str

                _ ->
                    Nothing

        QS.Many _ ->
            Nothing


getQueryParamsStringValue : Dict.Dict String QS.OneOrMany -> String -> Maybe String
getQueryParamsStringValue queryParams key =
    Dict.get
        key
        queryParams
        |> Maybe.andThen getQueryParamStringValue


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        queryParams =
            QS.parse QS.config flags.queryString

        getQueryParam =
            getQueryParamsStringValue queryParams

        maybeRedditAuthCode =
            Maybe.Extra.combine
                [ getQueryParam "code"
                , getQueryParam "state"
                , flags.redditAuthState
                ]
                |> Maybe.andThen
                    (\passedAuthCachedState ->
                        case passedAuthCachedState of
                            queryCode :: queryState :: cacheState :: _ ->
                                if queryState == cacheState then
                                    Just queryCode

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        cacheAuthCodeCmd =
            maybeRedditAuthCode
                |> Maybe.map (jsonCacheValue "redditAuthCode")
                |> Maybe.map cache
                |> Maybe.withDefault Cmd.none
    in
    ( { publicPath = flags.publicPath
      , redditAuthState = Maybe.Nothing
      , queryParams = queryParams
      , isLoggedIn = not (Maybe.Extra.isNothing maybeRedditAuthCode)
      }
    , Cmd.batch
        [ cacheAuthCodeCmd
        , Random.generate GenerateRedditAuthState
            (Random.String.string 15 Random.Char.english)
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | GenerateRedditAuthState String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateRedditAuthState randomString ->
            ( { model | redditAuthState = Maybe.Just randomString }
            , cache (jsonCacheValue "redditAuthState" randomString)
            )


jsonCacheValue : String -> String -> Json.Encode.Value
jsonCacheValue key value =
    Json.Encode.object
        [ ( key, Json.Encode.string value )
        ]



-- VIEW


buildAuthLinkQueryString randomString =
    let
        -- FIXME condition on environment
        redirectUri =
            "http://localhost:3000"

        clientId =
            "Lf7PiyAEaoSp6Q"

        params =
            [ ( "client_id", clientId )
            , ( "response_type", "code" )
            , ( "state", randomString )
            , ( "duration", "permanent" )
            , ( "scope", "history" )
            , ( "redirect_uri", redirectUri )
            ]
    in
    params
        |> List.map (\( key, val ) -> Url.Builder.string key val)
        |> Url.Builder.toQuery



-- https://github.com/reddit-archive/reddit/wiki/OAuth2
-- https://github.com/not-an-aardvark/snoowrap
-- https://www.reddit.com/dev/api/oauth#GET_user_{username}_saved


authUrl =
    "https://www.reddit.com/api/v1/authorize"


view : Model -> Html Msg
view model =
    let
        authLink =
            if model.isLoggedIn then
                text ""

            else
                model.redditAuthState
                    |> Maybe.map buildAuthLinkQueryString
                    |> Maybe.map
                        (\queryParams ->
                            a
                                [ Attr.href (authUrl ++ queryParams) ]
                                [ text "authorize site to read reddit history" ]
                        )
                    |> Maybe.withDefault (text "")
    in
    div
        []
        [ img
            [ Attr.src (model.publicPath ++ "/img/elm.png")
            , Attr.style "border"
                "1px solid black"
            ]
            []
        , text "Hello world"
        , authLink
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Flags =
    { publicPath : String
    , queryString : String
    , redditAuthState : Maybe String
    , redditAuthCode : Maybe String
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
