module Main exposing (..)

import Html exposing (Html, text, div)


main =
    Html.program
        { init = createModel
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    {}


createModel : ( Model, Cmd Msg )
createModel =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "Hello Micah!" ]
