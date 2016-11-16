module Main exposing (..)

import Html exposing (Html, button, div, text, table, tr, th, td, img)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Random exposing (Generator, int)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Types


type Throw
    = Rock
    | Paper
    | Scissors


type Outcome
    = Win
    | Loss
    | Tie


type Msg
    = Reset
    | CompThrow Throw
    | HumanThrow Throw


type alias Turn =
    { human : Throw
    , computer : Throw
    }



-- Model & Init


type alias Model =
    { humanThrow : Throw
    , compThrow : Throw
    , wins : Int
    , ties : Int
    , losses : Int
    }


init : ( Model, Cmd Msg )
init =
    ( (Model Rock Rock 0 0 0), Cmd.none )



-- Update


randomThrow : Generator Throw
randomThrow =
    Random.map
        (\n ->
            if n == 0 then
                Rock
            else if n == 1 then
                Paper
            else
                Scissors
        )
        (int 0 2)



-- Notice how a HumanThrow msg triggers a CompThrow msg in update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        HumanThrow humanThrow ->
            ( { model
                | humanThrow = humanThrow
              }
              -- This triggers a new CompThrow msg
            , Random.generate CompThrow randomThrow
            )

        CompThrow compThrow ->
            let
                outcome =
                    determineOutcome model.humanThrow compThrow

                newWins =
                    if outcome == Win then
                        model.wins + 1
                    else
                        model.wins

                newTies =
                    if outcome == Tie then
                        model.ties + 1
                    else
                        model.ties

                newLosses =
                    if outcome == Loss then
                        model.losses + 1
                    else
                        model.losses
            in
                ( { model
                    | compThrow = compThrow
                    , wins = newWins
                    , ties = newTies
                    , losses = newLosses
                  }
                , Cmd.none
                )


determineOutcome : Throw -> Throw -> Outcome
determineOutcome human computer =
    case human of
        Rock ->
            (case computer of
                Rock ->
                    Tie

                Paper ->
                    Loss

                Scissors ->
                    Win
            )

        Paper ->
            (case computer of
                Rock ->
                    Win

                Paper ->
                    Tie

                Scissors ->
                    Loss
            )

        Scissors ->
            (case computer of
                Rock ->
                    Loss

                Paper ->
                    Win

                Scissors ->
                    Tie
            )



-- View


view : Model -> Html Msg
view model =
    table [ style tableStyle ]
        [ tr []
            [ th [] [ text "Player" ]
            , th [] [ text " " ]
            , th [] [ text "Computer" ]
            ]
        , tr []
            [ td [ style [ ( "width", "110px" ), ( "height", "110px" ) ] ]
                [ img
                    ([ src (imgSrc model model.humanThrow) ]
                        ++ [ (style imgStyle) ]
                    )
                    []
                ]
            , td [] [ text " " ]
            , td [ style [ ( "width", "110px" ), ( "height", "110px" ) ] ]
                [ img
                    ([ src (imgSrc model model.compThrow) ]
                        ++ [ (style imgStyle) ]
                    )
                    []
                ]
            ]
        , tr []
            [ th [] [ text "Wins" ]
            , th [] [ text "Ties" ]
            , th [] [ text "Wins" ]
            ]
        , tr []
            [ td [ style tdStyle ] [ text (toString model.wins) ]
            , td [ style tdStyle ] [ text (toString model.ties) ]
            , td [ style tdStyle ] [ text (toString model.losses) ]
            ]
        , tr []
            [ td []
                [ button
                    [ onClick (HumanThrow Rock) ]
                    [ text "Rock" ]
                ]
            , td []
                [ button
                    [ onClick (HumanThrow Paper) ]
                    [ text "Paper" ]
                ]
            , td []
                [ button
                    [ onClick (HumanThrow Scissors) ]
                    [ text "Scissors" ]
                ]
            ]
        , tr []
            [ td [] []
            , td []
                [ button
                    [ onClick Reset ]
                    [ text "Reset" ]
                ]
            , td [] []
            ]
        ]


imgSrc : Model -> Throw -> String
imgSrc model throw =
    if (model.wins + model.ties + model.losses) == 0 then
        "blank.png"
    else
        case throw of
            Rock ->
                "rock.png"

            Paper ->
                "paper.png"

            Scissors ->
                "scissors.png"


tableStyle : List ( String, String )
tableStyle =
    [ ( "border", "2px solid black" )
    , ( "text-align", "center" )
    , ( "padding", "2px" )
    , ( "margin", "4px" )
    ]


imgStyle : List ( String, String )
imgStyle =
    [ ( "border", "1px solid black" )
    , ( "width", "105px" )
    , ( "height", "106px" )
    , ( "padding", "2px" )
    ]


tdStyle : List ( String, String )
tdStyle =
    [ ( "border", "1px solid black" )
    , ( "width", "64px" )
    , ( "height", "16px" )
    , ( "padding", "2px" )
    ]
