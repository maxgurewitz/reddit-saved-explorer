port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Base64
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


type alias CacheEntry =
    { key : String
    , value : Encode.Value
    }


port cache : CacheEntry -> Cmd msg


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

        initializeRedditCmd =
            flags.redditAccess
                |> Maybe.map initializeReddit
                |> Maybe.withDefault Cmd.none

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
      , isLoggedIn = not (Maybe.Extra.isNothing flags.redditAccess)
      }
    , Cmd.batch
        [ requestAccessTokenCmd
        , initializeRedditCmd
        , Random.generate GenerateRedditAuthState
            (Random.String.string 15 Random.Char.english)
        ]
    )


type alias RedditAccess =
    { access_token : String
    , refresh_token : String
    }


decodeRedditAccess : Json.Decode.Decoder RedditAccess
decodeRedditAccess =
    Json.Decode.map2 RedditAccess
        (field "access_token" Json.Decode.string)
        (field "refresh_token" Json.Decode.string)


accessTokenUrl =
    "https://www.reddit.com/api/v1/access_token"


paramsToQueryPrimitives val =
    QS.One (QS.Str val)


requestAccessToken : String -> String -> String -> Cmd Msg
requestAccessToken redirectUri clientId code =
    let
        params =
            [ ( "code", code )
            , ( "redirect_uri", redirectUri )
            , ( "grant_type", "authorization_code" )
            ]

        body =
            QS.serialize QS.config
                (Dict.fromList
                    (params |> List.map (Tuple.mapSecond paramsToQueryPrimitives))
                )
                |> String.dropLeft 1

        authorization =
            "Basic " ++ Base64.encode (clientId ++ ":")
    in
    HttpBuilder.post accessTokenUrl
        |> HttpBuilder.withExpectJson decodeRedditAccess
        |> HttpBuilder.withStringBody "application/x-www-form-urlencoded" body
        |> HttpBuilder.withHeaders [ ( "Authorization", authorization ) ]
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


encodeRedditAccess : RedditAccess -> Encode.Value
encodeRedditAccess redditAccess =
    Encode.object
        [ ( "access_token", Encode.string redditAccess.access_token )
        , ( "refresh_token", Encode.string redditAccess.refresh_token )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateRedditAuthState randomString ->
            ( { model | redditAuthState = Maybe.Just randomString }
            , cache { key = "redditAuthState", value = Encode.string randomString }
            )

        InitializeReddit redditAccess ->
            ( { model | isLoggedIn = True }
            , Cmd.batch
                [ initializeReddit redditAccess
                , cache
                    { key = "redditAccess"
                    , value = encodeRedditAccess redditAccess
                    }
                ]
            )


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
            , ( "scope", "identity history" )
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
-- https://github.com/reddit-archive/reddit/wiki/API
-- https://github.com/not-an-aardvark/snoowrap/blob/cd2cc85fdc6354e641a458c192795e8784c0c675/src/objects/RedditUser.js#L217


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
    , redditAccess : Maybe RedditAccess
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
