module Main exposing (main)

import Browser
import Html exposing (..)
 

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { property : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "hehe", Cmd.none )


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "Hello world!" ]
