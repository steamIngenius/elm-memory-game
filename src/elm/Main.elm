module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class, classList)


main =
    Html.program
        { init = createModel
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }



-- Model


type Model
    = Playing Deck


type alias Deck =
    List Card


type alias Card =
    { id : String
    , group : Group
    , flipped : Bool
    }


type Group
    = A
    | B


cards : List String
cards =
    [ "dinosaur"
    , "8-ball"
    , "baked-potato"
    , "kronos"
    , "rocket"
    , "skinny-unicorn"
    , "that-guy"
    , "zeppelin"
    ]


initCard : Group -> String -> Card
initCard group name =
    { id = name
    , group = group
    , flipped = True
    }


deck : Deck
deck =
    let
        groupA =
            List.map (initCard A) cards

        groupB =
            List.map (initCard B) cards
    in
        List.concat [ groupA, groupB ]


createModel : ( Model, Cmd Msg )
createModel =
    ( Playing deck, Cmd.none )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            div [ class "wrapper" ] ( List.map createCard deck )


createCard : Card -> Html Msg
createCard card =
    div [ class "container" ]
        [ div [ classList [ ( "card", True ), ( "flipped", card.flipped ) ] ]
            [ div [ class "card-back" ] []
            , div [ class ("front " ++ cardClass card) ] []
            ]
        ]


cardClass : Card -> String
cardClass { id } =
    "card-" ++ id
