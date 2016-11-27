import Html exposing (Html, text, div)
import Time exposing (Time)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (KeyCode)
import Platform.Sub exposing (batch)
import AnimationFrame


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Player  =
    { pos : Float
    , left : Bool
    , right : Bool
    , space : Bool
    , velocity : Float
    , width : Float
    , height : Float
    }

type alias Ball =
    { x : Float
    , y : Float
    , velocityX : Float
    , velocityY : Float
    , size : Float
    }

type alias Game =
    { player : Player
    , ball : Ball
    , screenWidth : Float
    , screenHeight : Float
    }

defaultPlayer = Player 0 False False False 0.4 100 10
defaultBall = Ball 300 300 0.3 0.4 6

defaultGame : Game
defaultGame =
    { player = defaultPlayer
    , ball = defaultBall
    , screenWidth = 500
    , screenHeight = 680
    }


init : (Game, Cmd Msg)
init =
  (defaultGame, Cmd.none)

  -- UPDATE

type Msg = Tick Time | KeyDown KeyCode | KeyUp KeyCode


update : Msg -> Game -> (Game, Cmd Msg)
update msg model =
  case msg of
    Tick diff ->
      ({model
        | player = (playerMovement diff model)
        , ball = (ballMovement diff (ballCollision model))}, Cmd.none)
    KeyDown keyCode ->
      ({model | player = (keyDown keyCode model.player) }, Cmd.none)
    KeyUp keyCode ->
      ({model | player = (keyUp keyCode model.player) }, Cmd.none)


ballMovement : Time -> Ball -> Ball
ballMovement diff ball =
        {ball
            | x = ball.x + ball.velocityX * diff
            , y = ball.y + ball.velocityY * diff}




ballCollision : Game -> Ball
ballCollision game =
    let
        ball = game.ball
    in
        if ball.x >= game.screenWidth || ball.x <= 0  then
            {ball | velocityX = -ball.velocityX }
        else if (ball.y >= game.screenHeight || ball.y <= 0) then
            {ball | velocityY = -ball.velocityY }
        else
            ball

playerMovement : Time -> Game -> Player
playerMovement diff game=
    let
        player = game.player
        tmp = diff * player.velocity
    in
        if ((player.left && player.right) || (not player.left && not player.right)) then
            player
        else if player.left then
            if (player.pos - tmp <= 0) then { player | pos = 0}
            else { player | pos = player.pos - (diff * player.velocity)}
        else
             if (player.pos + tmp >= game.screenWidth - player.width) then { player | pos = game.screenWidth - player.width}
             else { player | pos = player.pos + (diff * player.velocity)}


keyDown : KeyCode -> Player -> Player
keyDown code player =
    case code of
        -- left
        37 ->
            {player | left = True}
        -- right
        39 ->
            {player | right = True}
        _ -> player


keyUp : KeyCode -> Player -> Player
keyUp code player =
    case code of
        -- left
        37 ->
            {player | left = False}
        -- right
        39 ->
            {player | right = False}
        _ -> player


-- SUBSCRIPTIONS

subscriptions : Game -> Sub Msg
subscriptions model = batch
    [ AnimationFrame.diffs Tick
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]

-- VIEW

playerSvg : Game -> Svg Msg
playerSvg game =
        let
            player = game.player
        in
            rect [ x (toString (player.pos)), y (toString (game.screenHeight - player.height - 10)), width (toString player.width), height (toString player.height)] []


rectBorder : Game -> Svg Msg
rectBorder game = rect [ x "0", y "0", width (toString game.screenWidth), height (toString game.screenHeight), fillOpacity "0", stroke "black", strokeWidth "5"] []

ball : Game -> Svg Msg
ball game = circle [ cx (toString game.ball.x), cy (toString game.ball.y), r (toString game.ball.size)] []


view : Game -> Html Msg
view game =
        svg
            [ viewBox "0 0 600 900", width "1000px", height "900px", preserveAspectRatio "xMidYmid meet"]
            [ playerSvg game
            , rectBorder game
            , ball game
            ]


