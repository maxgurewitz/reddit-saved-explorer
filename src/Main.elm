port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Base64
import Browser
import Dict
import Html exposing (Html, a, div, img, input, label, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
import List.Extra
import Maybe.Extra
import QS
import Random
import Random.Char
import Random.String
import Set exposing (Set)
import Set.Extra
import Url exposing (Url)
import Url.Builder



-- PORTS


type alias CacheEntry =
    { key : String
    , value : Encode.Value
    }


port cache : CacheEntry -> Cmd msg


type alias PageParams =
    { after : String
    }


type alias PageRequest =
    { params : PageParams
    }


type alias InitializeRequest =
    { access : RedditAccess }


port initializeReddit : InitializeRequest -> Cmd msg


port pageReddit : PageRequest -> Cmd msg


port saved : (Encode.Value -> msg) -> Sub msg



-- MODEL


type alias Model =
    { publicPath : String
    , redditAuthState : Maybe String
    , saved : List Link
    , queryParams : Dict.Dict String QS.OneOrMany
    , isLoggedIn : Bool
    , redirectUri : String
    , clientId : String
    , selectedSubreddits : Set String
    }



{-

   sample values:

      author: "Unknownrealm"
      created_utc: 1539113941
      permalink: "/r/MMA/comments/9msb4h/khabib_pre_and_post_fight_with_past_opponents/"
      subreddit: "MMA"
      thumbnail: "https://a.thumbs.redditmedia.com/_XaaOS8cYjbkIaMzVmgE9AyZxekgQAwcCiqSm83pz18.jpg"

-}


type alias Link =
    { author : String
    , created_utc : Int
    , name : String
    , permalink : String
    , subreddit : String
    , thumbnail : Maybe String
    , title : String
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
                |> Maybe.map
                    (\redditAccess ->
                        initializeReddit
                            { access = redditAccess }
                    )
                |> Maybe.withDefault Cmd.none

        requestAccessTokenCmd =
            maybeRedditAuthCode
                |> Maybe.map (requestAccessToken flags.redirectUri flags.clientId)
                |> Maybe.withDefault Cmd.none
    in
    ( { publicPath = flags.publicPath
      , redditAuthState = Maybe.Nothing
      , queryParams = queryParams
      , saved = []
      , redirectUri = flags.redirectUri
      , clientId = flags.clientId
      , isLoggedIn = not (Maybe.Extra.isNothing flags.redditAccess)
      , selectedSubreddits = Set.empty
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


decodeRedditAccess : Decode.Decoder RedditAccess
decodeRedditAccess =
    Decode.map2 RedditAccess
        (field "access_token" Decode.string)
        (field "refresh_token" Decode.string)


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
    | ReceiveSaved (List Link)
    | InitializeReddit RedditAccess
    | GenerateRedditAuthState String
    | ToggleSelectedSubReddit String


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

        ToggleSelectedSubReddit toggled ->
            ( { model
                | selectedSubreddits = Set.Extra.toggle toggled model.selectedSubreddits
              }
            , Cmd.none
            )

        ReceiveSaved savedItems ->
            let
                pageCmd =
                    List.Extra.last savedItems
                        |> Maybe.map
                            (\lastItem ->
                                pageReddit
                                    { params = { after = lastItem.name }
                                    }
                            )
                        |> Maybe.withDefault Cmd.none
            in
            ( { model
                | saved = List.concat [ model.saved, savedItems ]
              }
            , pageCmd
            )

        GenerateRedditAuthState randomString ->
            ( { model | redditAuthState = Maybe.Just randomString }
            , cache { key = "redditAuthState", value = Encode.string randomString }
            )

        InitializeReddit redditAccess ->
            ( { model | isLoggedIn = True }
            , Cmd.batch
                [ initializeReddit
                    { access = redditAccess }
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



{-

   resources:

     https://ssl.reddit.com/prefs/apps/
     https://github.com/reddit-archive/reddit/wiki/OAuth2
     https://github.com/not-an-aardvark/snoowrap
     https://www.reddit.com/dev/api/oauth#GET_user_{username}_saved
     https://www.oauth.com/oauth2-servers/single-page-apps/
     https://github.com/reddit-archive/reddit/wiki/API
     https://github.com/not-an-aardvark/snoowrap/blob/cd2cc85fdc6354e641a458c192795e8784c0c675/src/objects/RedditUser.js#L217

-}


authUrl =
    "https://www.reddit.com/api/v1/authorize"


authLinkView : Model -> String -> Html Msg
authLinkView model authState =
    a
        [ Attr.href (authUrl ++ buildAuthLinkQueryString model authState) ]
        [ text "authorize site to read reddit history" ]


savedItemView : Model -> Link -> Html Msg
savedItemView model item =
    div []
        [ item.thumbnail
            |> Maybe.map (\src -> img [ Attr.src src ] [])
            |> Maybe.withDefault (text "")
        , a [ Attr.href ("https://reddit.com" ++ item.permalink) ]
            [ text item.title ]
        ]


subredditFilter : Model -> String -> Html Msg
subredditFilter model subreddit =
    label []
        [ input
            [ Attr.type_ "checkbox"
            , onClick (ToggleSelectedSubReddit subreddit)
            ]
            []
        , text subreddit
        ]


loggedInView : Model -> Html Msg
loggedInView model =
    let
        subreddits =
            model.saved
                |> List.map .subreddit
                |> List.Extra.unique

        displayedSaved =
            if Set.isEmpty model.selectedSubreddits then
                model.saved

            else
                model.saved
                    |> List.filter
                        (\savedItem ->
                            Set.member savedItem.subreddit model.selectedSubreddits
                        )
    in
    div []
        [ div []
            (List.map (subredditFilter model) subreddits)
        , div []
            (List.map
                (savedItemView model)
                displayedSaved
            )
        ]


view : Model -> Html Msg
view model =
    let
        maybeAuthLink =
            if model.isLoggedIn then
                loggedInView model

            else
                model.redditAuthState
                    |> Maybe.map (authLinkView model)
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
        , maybeAuthLink
        ]



-- SUBSCRIPTIONS


linkDecoder =
    Decode.map7 Link
        (field "author" Decode.string)
        (field "created_utc" Decode.int)
        (field "name" Decode.string)
        (field "permalink" Decode.string)
        (field "subreddit" Decode.string)
        (field "thumbnail" (Decode.nullable Decode.string))
        (field "title" Decode.string)


decodeSaved : Encode.Value -> List Link
decodeSaved value =
    Decode.decodeValue (Decode.list linkDecoder) value
        |> Result.withDefault []


subscriptions : Model -> Sub Msg
subscriptions model =
    saved (decodeSaved >> ReceiveSaved)


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
