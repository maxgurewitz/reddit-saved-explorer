port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Browser
import Dict
import Html exposing (Html, a, div, img, text)
import Html.Attributes as Attr
import Http
import HttpBuilder
import Json.Decode exposing (field)
import Json.Encode as Encode
import Maybe.Extra
import QS
import Random
import Random.Char
import Random.String
import Url exposing (Url)
import Url.Builder



-- PORTS


port cache : Encode.Value -> Cmd msg


port initializeReddit : RedditAccess -> Cmd msg



-- MODEL


type alias Model =
    { publicPath : String
    , redditAuthState : Maybe String
    , queryParams : Dict.Dict String QS.OneOrMany
    , isLoggedIn : Bool
    , redirectUri : String
    , clientId : String
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

        -- FIXME need to translate code to auth token and refresh toke
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

        requestAccessTokenCmd =
            maybeRedditAuthCode
                |> Maybe.map (requestAccessToken flags.redirectUri flags.clientId)
                |> Maybe.withDefault Cmd.none
    in
    ( { publicPath = flags.publicPath
      , redditAuthState = Maybe.Nothing
      , queryParams = queryParams
      , redirectUri = flags.redirectUri
      , clientId = flags.clientId
      , isLoggedIn = False
      }
    , Cmd.batch
        [ requestAccessTokenCmd
        , Random.generate GenerateRedditAuthState
            (Random.String.string 15 Random.Char.english)
        ]
    )


type alias RedditAccess =
    { accessToken : String
    , refreshToken : String
    }


decodeRedditAccess : Json.Decode.Decoder RedditAccess
decodeRedditAccess =
    Json.Decode.map2 RedditAccess
        (field "accessToken" Json.Decode.string)
        (field "refreshToken" Json.Decode.string)


accessTokenUrl =
    "https://www.reddit.com/api/v1/access_token"


requestAccessToken : String -> String -> String -> Cmd Msg
requestAccessToken redirectUri clientId code =
    let
        body =
            Encode.object
                [ ( "code", Encode.string code )
                , ( "redirect_uri", Encode.string redirectUri )
                , ( "grant_type", Encode.string "authorization_code" )
                ]
    in
    HttpBuilder.post accessTokenUrl
        |> HttpBuilder.withExpectJson decodeRedditAccess
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.send
            (\result ->
                result
                    |> Result.map InitializeReddit
                    |> Result.withDefault NoOp
            )



-- UPDATE


type Msg
    = NoOp
    | InitializeReddit RedditAccess
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

        InitializeReddit redditAccess ->
            ( model, initializeReddit redditAccess )


jsonCacheValue : String -> String -> Encode.Value
jsonCacheValue key value =
    Encode.object
        [ ( key, Encode.string value )
        ]



-- VIEW


buildAuthLinkQueryString : Model -> String -> String
buildAuthLinkQueryString model randomString =
    let
        params =
            [ ( "client_id", model.clientId )
            , ( "response_type", "code" )
            , ( "state", randomString )
            , ( "duration", "permanent" )
            , ( "scope", "history" )
            , ( "redirect_uri", model.redirectUri )
            ]
    in
    params
        |> List.map (\( key, val ) -> Url.Builder.string key val)
        |> Url.Builder.toQuery



-- https://github.com/reddit-archive/reddit/wiki/OAuth2
-- https://github.com/not-an-aardvark/snoowrap
-- https://www.reddit.com/dev/api/oauth#GET_user_{username}_saved
-- https://www.oauth.com/oauth2-servers/single-page-apps/
-- Authorization: Basic client_id:


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
                    |> Maybe.map (buildAuthLinkQueryString model)
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
    , clientId : String
    , redirectUri : String
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
