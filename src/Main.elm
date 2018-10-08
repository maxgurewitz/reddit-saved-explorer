module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict
import Html exposing (Html, a, div, img, text)
import Html.Attributes as Attr
import Random
import Random.Char
import Random.String
import Url.Builder



-- MODEL


type alias Model =
    { publicPath : String
    , redditAuthState : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { publicPath = flags.publicPath
      , redditAuthState = Maybe.Nothing
      }
    , Random.generate GenerateRedditAuthState (Random.String.string 15 Random.Char.english)
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
            ( { model | redditAuthState = Maybe.Just randomString }, Cmd.none )



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
            , ( "duration", "temporary" )
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
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
