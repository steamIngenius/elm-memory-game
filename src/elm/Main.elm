module Main exposing (..)

import Html exposing (Html, text, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import Random


main : Program Never Model Msg
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
    | Guessing Deck Card
    | MatchCard Deck Card Card


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
    , flipped = False
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
    | Flip Card


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

        Flip card ->
            if card.flipped then
                model ! []
            else
                checkIfCorrect card model


flip : Bool -> Card -> Card -> Card
flip isFlipped a b =
    if (a.id == b.id) && (a.group == b.group) then
        { b | flipped = isFlipped }
    else
        b


shuffleDeck : Deck -> List comparable -> Deck
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


checkIfCorrect : Card -> Model -> ( Model, Cmd Msg )
checkIfCorrect card model =
    case model of
        Playing deck ->
            let
                newDeck =
                    List.map (flip True card) deck
            in
                Guessing newDeck card ! []

        Guessing deck guess ->
            let
                newDeck =
                    List.map (flip True card) deck

                newModel =
                    MatchCard newDeck guess card
            in
                newModel ! []

        MatchCard deck guess1 guess2 ->
            if guess1.id == guess2.id then
                update (Flip card) (Playing deck)
            else
                let
                    flipGuess =
                        flip False guess1 >> flip False guess2

                    newDeck =
                        List.map flipGuess deck
                in
                    Playing newDeck ! []



-- View


view : Model -> Html Msg
view model =
    case model of
        Playing deck ->
            game deck

        Guessing deck _ ->
            game deck

        MatchCard deck _ _ ->
            game deck


game : Deck -> Html Msg
game deck =
    div [ class "wrapper" ] (List.map createCard deck)


createCard : Card -> Html Msg
createCard card =
    div [ class "container" ]
        [ div
            [ classList
                [ ( "card", True )
                , ( "flipped", card.flipped )
                ]
            , onClick (Flip card)
            ]
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
