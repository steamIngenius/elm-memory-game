module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class, classList)
import Random


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
    let
        model =
            Playing deck

        cmd =
            randomList Shuffle (List.length deck)
    in
        ( model, cmd )



-- Update


type Msg
    = NoOp
    | Shuffle (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle xs ->
            let
                newDeck =
                    shuffleDeck deck xs
            in
                ( Playing newDeck, Cmd.none )


shuffleDeck : Deck -> List comparable -> Deck
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first



-- View


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            div [ class "wrapper" ] (List.map createCard deck)


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



-- Utility


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 0 100
        |> Random.list len
        |> Random.generate msg
