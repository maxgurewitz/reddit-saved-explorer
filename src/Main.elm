port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Base64
import Browser
import Dict
import Dropdown exposing (Dropdown, Event(..))
import Element exposing (Element, alignLeft, alignTop, centerX, centerY, column, el, fill, height, image, link, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Input exposing (checkbox, labelLeft)
import Element.Keyed as Keyed
import Html exposing (Html, a, div, img, input, label)
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
    , saved : List SavedItem
    , queryParams : Dict.Dict String QS.OneOrMany
    , isLoggedIn : Bool
    , redirectUri : String
    , clientId : String
    , selectedSubreddits : Set String
    , over18Dropdown : Dropdown
    , over18Selected : Over18Filter
    }


type Over18Filter
    = OnlyOver18
    | OnlyUnder18
    | IncludeOver18



{-

   sample values:

      author: "Unknownrealm"
      created_utc: 1539113941
      permalink: "/r/MMA/comments/9msb4h/khabib_pre_and_post_fight_with_past_opponents/"
      subreddit: "MMA"
      thumbnail: "https://a.thumbs.redditmedia.com/_XaaOS8cYjbkIaMzVmgE9AyZxekgQAwcCiqSm83pz18.jpg"

-}


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
      , over18Dropdown = Dropdown.init
      , over18Selected = OnlyUnder18
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
    | Over18Selected (Dropdown.Msg Over18Filter)
    | ReceiveSaved (List SavedItem)
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

        Over18Selected dropdownMsg ->
            let
                ( updatedDropdown, event ) =
                    Dropdown.update dropdownMsg model.over18Dropdown

                updatedModel =
                    case event of
                        ItemSelected over18Selected ->
                            { model
                                | over18Dropdown = updatedDropdown
                                , over18Selected = over18Selected
                            }

                        _ ->
                            { model | over18Dropdown = updatedDropdown }
            in
            ( updatedModel, Cmd.none )

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


authLinkView : Model -> String -> Element Msg
authLinkView model authState =
    link []
        { url =
            authUrl ++ buildAuthLinkQueryString model authState
        , label = text "authorize site to read reddit history"
        }


savedItemHeight =
    100


savedItemView : Model -> SavedItem -> Element Msg
savedItemView model item =
    let
        maybeThumbnail =
            if model.over18Selected == OnlyUnder18 && item.over18 then
                Nothing

            else
                item.thumbnail

        savedImg =
            maybeThumbnail
                |> Maybe.map
                    (\src ->
                        image
                            [ width (fill |> Element.maximum savedItemHeight)
                            , height (Element.px savedItemHeight)
                            , centerX
                            , centerY
                            ]
                            { src = src, description = "" }
                    )
                |> Maybe.withDefault Element.none

        savedImgWithBackground =
            el
                [ height (Element.px savedItemHeight)
                , width (Element.px savedItemHeight)
                , Background.color (Element.rgb 0.5 0.5 0.5)
                ]
                savedImg
    in
    row
        [ height (Element.px savedItemHeight)
        ]
        [ savedImgWithBackground
        , link [] { url = "https://reddit.com" ++ item.permalink, label = text item.title }
        ]



-- [ -- Element.behindContent
--     (el
--         [ Background.color (Element.rgb 0.0 0.0 0.0)
--         , width (Element.px savedItemHeight)
--         , height (Element.px savedItemHeight)
--         ]
--         Element.none
--     )


subredditFilterIcon : Bool -> Element Msg
subredditFilterIcon isSelected =
    let
        iconClass =
            if isSelected then
                "glyphicon glyphicon-check"

            else
                "glyphicon glyphicon-unchecked"
    in
    Element.html <| Html.span [ Attr.class iconClass ] []


subredditFilter : Model -> String -> Element Msg
subredditFilter model subreddit =
    checkbox []
        { onChange = \_ -> ToggleSelectedSubReddit subreddit
        , icon = subredditFilterIcon
        , checked = Set.member subreddit model.selectedSubreddits
        , label = labelLeft [] (text subreddit)
        }


dropdownItemName : Over18Filter -> String
dropdownItemName filter =
    case filter of
        OnlyOver18 ->
            "Only NSFW."

        OnlyUnder18 ->
            "Safe for work."

        IncludeOver18 ->
            "Include NSFW."


isSavedVisible : Model -> SavedItem -> Bool
isSavedVisible model savedItem =
    let
        ageAppropriate =
            case model.over18Selected of
                OnlyOver18 ->
                    savedItem.over18

                OnlyUnder18 ->
                    not savedItem.over18

                IncludeOver18 ->
                    True

        inSelectedSubreddit =
            Set.isEmpty model.selectedSubreddits
                || Set.member savedItem.subreddit model.selectedSubreddits
    in
    ageAppropriate && inSelectedSubreddit


paddingStandard =
    20


loggedInView : Model -> Element Msg
loggedInView model =
    let
        -- FIXME should only display subreddits if they have the correct over 18 status
        subreddits =
            model.saved
                |> List.map .subreddit
                |> List.Extra.unique

        displayedSaved =
            List.filter (isSavedVisible model) model.saved

        dropdownView =
            Element.html <|
                Html.map
                    Over18Selected
                    (Dropdown.view
                        [ OnlyUnder18, IncludeOver18, OnlyOver18 ]
                        (Just model.over18Selected)
                        dropdownItemName
                        model.over18Dropdown
                    )

        savedColumn =
            Keyed.column []
                (List.map
                    (\s -> ( s.permalink, savedItemView model s ))
                    displayedSaved
                )

        subredditColumn =
            Keyed.column []
                (List.map
                    (\s -> ( s, subredditFilter model s ))
                    subreddits
                )
    in
    column [ width fill, centerX ]
        [ row [ padding paddingStandard ] [ el [] dropdownView ]
        , row []
            [ el
                [ alignTop
                , alignLeft
                , padding paddingStandard
                ]
                subredditColumn
            , el [ centerX ] savedColumn
            ]
        ]


view : Model -> Html Msg
view model =
    let
        authView =
            if model.isLoggedIn then
                loggedInView model

            else
                model.redditAuthState
                    |> Maybe.map (authLinkView model)
                    |> Maybe.withDefault (Element.text "")
    in
    Element.layout [] authView



-- SUBSCRIPTIONS


type alias SavedItem =
    { author : String
    , createdUtc : Int
    , name : String
    , over18 : Bool
    , permalink : String
    , subreddit : String
    , thumbnail : Maybe String
    , title : String
    }


savedItemDecoder =
    Decode.map8 SavedItem
        (field "author" Decode.string)
        (field "createdUtc" Decode.int)
        (field "name" Decode.string)
        (field "over18" Decode.bool)
        (field "permalink" Decode.string)
        (field "subreddit" Decode.string)
        (field "thumbnail" (Decode.nullable Decode.string))
        (field "title" Decode.string)


decodeSaved : Encode.Value -> List SavedItem
decodeSaved value =
    Decode.decodeValue (Decode.list savedItemDecoder) value
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
