module Main exposing (..)

import Array exposing (..)
import Debug exposing (log)
import Html
    exposing
        ( Html
        , button
        , div
        , text
        , table
        , tr
        , th
        , td
        , img
        , input
        , label
        , span
        )
import Html.Attributes exposing (style, src, type_, checked)
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



-- The outcome of the game refers to the human players perspective not
-- the computer.


type Outcome
    = Win
    | Loss
    | Tie


type Prediction
    = R
    | P
    | S
    | RP
    | RS
    | PS
    | RPS


type Msg
    = Reset
    | HumanThrow Throw
    | CompThrow Throw
    | ToggleMarkovView


{-| Keep track of all the turns so we can use a Markov chain to
predict the human players next move.
-}
type alias Turn =
    { human : Throw
    , computer : Throw
    }


{-| A Markov matrix keeps track of how many times the human player
throws y after throwing x. There are three choices for both x and y,
namely rock, paper or scissors, thus the matrix needs to be 3x3. Thus,
the matrix or grid has 9 numbers in it.

Technically, a real Markov matrix should just store the probability (a
number between 0 and 1) of transitioning from one state to the next,
such as from say rock to paper, but just storing counts is simpler.
-}
type alias MarkovMatrix =
    { rock : Array Int
    , paper : Array Int
    , scissors : Array Int
    }



-- Model & Init


type alias Model =
    { humanThrow : Throw
    , compThrow : Throw
    , turns : List Turn
    , markov : MarkovMatrix
    , showMarkovView : Bool
    , wins : Int
    , ties : Int
    , losses : Int
    }


initMarkov : MarkovMatrix
initMarkov =
    { rock = fromList [ 0, 0, 0 ]
    , paper = fromList [ 0, 0, 0 ]
    , scissors = fromList [ 0, 0, 0 ]
    }


init : ( Model, Cmd Msg )
init =
    ( (Model Rock Rock [] initMarkov False 0 0 0), Cmd.none )



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


throwToInt : Throw -> Int
throwToInt throw =
    case throw of
        Rock ->
            0

        Paper ->
            1

        Scissors ->
            2


getMarkov : Throw -> Throw -> MarkovMatrix -> Int
getMarkov last current markov =
    case last of
        Rock ->
            Maybe.withDefault 0 (Array.get (throwToInt current) markov.rock)

        Paper ->
            Maybe.withDefault 0 (Array.get (throwToInt current) markov.paper)

        Scissors ->
            Maybe.withDefault 0 (Array.get (throwToInt current) markov.scissors)


setMarkov : Throw -> Throw -> Int -> MarkovMatrix -> MarkovMatrix
setMarkov last current i markov =
    let
        newArray =
            case last of
                Rock ->
                    Array.set (throwToInt current) i markov.rock

                Paper ->
                    Array.set (throwToInt current) i markov.paper

                Scissors ->
                    Array.set (throwToInt current) i markov.scissors
    in
        case last of
            Rock ->
                { markov
                    | rock = newArray
                }

            Paper ->
                { markov
                    | paper = newArray
                }

            Scissors ->
                { markov
                    | scissors = newArray
                }


incrementMarkov : Throw -> Throw -> MarkovMatrix -> MarkovMatrix
incrementMarkov last current markov =
    let
        count =
            getMarkov last current markov
    in
        setMarkov last current (count + 1) markov


updateMarkovMatrix : Throw -> Model -> MarkovMatrix
updateMarkovMatrix current model =
    let
        numTurns =
            List.length model.turns
    in
        -- If the game has just begun, then there is nothing to do yet.
        if numTurns < 1 then
            model.markov
        else
            let
                last =
                    case (List.head model.turns) of
                        Just turn ->
                            turn.human

                        Nothing ->
                            Rock
            in
                incrementMarkov last current model.markov


predictNextThrow : Model -> Prediction
predictNextThrow model =
    let
        last =
            case (List.head model.turns) of
                Just turn ->
                    turn.human

                -- It's the first turn of the game, thus the entire
                -- Markov Matrix is empty, so just use the first row,
                -- which will have all zeros (just like all the other
                -- rows).
                Nothing ->
                    Rock

        ( r, p, s ) =
            ( getMarkov last Rock model.markov
            , getMarkov last Paper model.markov
            , getMarkov last Scissors model.markov
            )
    in
        if (r > p) && (r > s) then
            R
        else if (p > r) && (p > s) then
            P
        else if (s > r) && (s > p) then
            S
            -- At this point we are guaranteed of no single greatest number
        else if (r == p) && (r > s) then
            RP
        else if (r == s) && (r > p) then
            RS
        else if (p == s) && (p > r) then
            PS
        else
            -- All throws are equally likely
            RPS


computerThrow : Throw -> Prediction -> Throw
computerThrow randThrow prediction =
    case prediction of
        R ->
            Paper

        P ->
            Scissors

        S ->
            Rock

        RP ->
            Paper

        RS ->
            Rock

        PS ->
            Scissors

        RPS ->
            randThrow



-- Notice how a HumanThrow Msg triggers a CompThrow Msg in update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Reset ->
            init

        HumanThrow humanThrow ->
            ( { model
                | humanThrow = humanThrow
              }
              -- This triggers a new CompThrow msg
            , Random.generate CompThrow randomThrow
            )

        ToggleMarkovView ->
            ( { model
                | showMarkovView = not model.showMarkovView
              }
            , Cmd.none
            )

        CompThrow randThrow ->
            let
                cThrow =
                    computerThrow randThrow (predictNextThrow model)

                outcome =
                    determineOutcome model.humanThrow cThrow

                ( w, t, l ) =
                    case outcome of
                        Win ->
                            ( model.wins + 1, model.ties, model.losses )

                        Tie ->
                            ( model.wins, model.ties + 1, model.losses )

                        Loss ->
                            ( model.wins, model.ties, model.losses + 1 )
            in
                ( { model
                    | compThrow = cThrow
                    , turns = (Turn model.humanThrow cThrow) :: model.turns
                    , markov = (updateMarkovMatrix model.humanThrow model)
                    , wins = w
                    , ties = t
                    , losses = l
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
    div []
        [ viewGame model
        , div []
            [ checkbox model ToggleMarkovView "Show Markov Matrix" ]
        , div []
            [ viewMarkovMatrix model ]
          --, div []
          --    [ text (toString model.turns) ]
        ]


checkbox : Model -> Msg -> String -> Html Msg
checkbox model msg name =
    label
        []
        [ input
            [ type_ "checkbox"
            , checked model.showMarkovView
            , (onClick msg)
            ]
            []
        , text name
        ]


viewMarkovMatrix : Model -> Html Msg
viewMarkovMatrix model =
    if model.showMarkovView then
        table [ style tableStyle ]
            [ tr []
                [ th [] []
                , th [] [ text "Rock" ]
                , th [] [ text "Paper" ]
                , th [] [ text "Scissors" ]
                ]
            , tr []
                [ th [] [ text "Rock" ]
                , td [] [ text (toString (getMarkov Rock Rock model.markov)) ]
                , td [] [ text (toString (getMarkov Rock Paper model.markov)) ]
                , td [] [ text (toString (getMarkov Rock Scissors model.markov)) ]
                ]
            , tr []
                [ th [] [ text "Paper" ]
                , td [] [ text (toString (getMarkov Paper Rock model.markov)) ]
                , td [] [ text (toString (getMarkov Paper Paper model.markov)) ]
                , td [] [ text (toString (getMarkov Paper Scissors model.markov)) ]
                ]
            , tr []
                [ th [] [ text "Scissors" ]
                , td [] [ text (toString (getMarkov Scissors Rock model.markov)) ]
                , td [] [ text (toString (getMarkov Scissors Paper model.markov)) ]
                , td [] [ text (toString (getMarkov Scissors Scissors model.markov)) ]
                ]
            ]
    else
        span [] []


viewGame : Model -> Html Msg
viewGame model =
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
