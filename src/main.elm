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

(gameWidth, gameHeight) = (600, 900)

type alias Player  =
    { pos : Float
    , left : Bool
    , right : Bool
    , space : Bool
    , velocity : Float
    }

type alias Game =
    { player : Player
    }

defaultPlayer = Player 300 False False False 0.7

defaultGame : Game
defaultGame =
    { player = defaultPlayer
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
      ({model | player = (playerMovement diff model.player) }, Cmd.none)
    KeyDown keyCode ->
      ({model | player = (keyDown keyCode model.player) }, Cmd.none)
    KeyUp keyCode ->
      ({model | player = (keyUp keyCode model.player) }, Cmd.none)


playerMovement : Time -> Player -> Player
playerMovement diff player =
     if ((player.left && player.right) || (not player.left && not player.right)) then
        player
    else if player.left then
        { player | pos = player.pos - (diff * player.velocity)}
    else
        { player | pos = player.pos + (diff * player.velocity)}





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

playerSvg : Player -> Svg Msg
playerSvg player = rect [ x (toString player.pos), y "880", width "80", height "20"] []


rectBorder : Svg Msg
rectBorder = rect [ x "0", y "0", width "600", height "900", fillOpacity "0.2"] []

view : Game -> Html Msg
view game =
        svg [ viewBox "0 0 600 900", width "600px", height "900px" ]
            [ playerSvg game.player
            , rectBorder
            ]


