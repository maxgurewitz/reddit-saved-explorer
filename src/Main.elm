module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, img, text)
import Html.Attributes as Attr



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init flags =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW
-- should be reddit-saved-explorer/


view : Model -> Html Msg
view model =
    div
        []
        [ img
            [ Attr.src "/img/elm.png"
            , Attr.style "border"
                "1px solid black"
            ]
            []
        , text "Hello world"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
