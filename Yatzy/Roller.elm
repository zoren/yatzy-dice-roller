module Yatzy.Roller where
import Color exposing(red)
import Graphics.Element exposing (..)
import Random
import Keyboard

type Action = Roll | Hold Int | Reset

-- Main

main = Signal.map view model

-- Model
initialModel = ([], initialSeed)

-- View
view1 : (Int, Bool) -> Element
view1 (eyes, isHeld) =
  let
    src = "../assets/" ++ (toString eyes) ++ ".png"
  in
    if isHeld
    then color red <| image 50 50 src
    else image 50 50 src

view (eyeList, _) =
  flow right (List.map view1 eyeList)

-- Update
roll (eyeList, seed) =
  List.foldl (\(_, held) (al, s) ->
          let
            (eye, s') = randomSide s
          in
            ((eye, held) :: al, s'))
          ([], seed)
          eyeList

update action (eyeList, seed) =
  case action of
    Roll ->
      case eyeList of
        [] -> roll (List.repeat 5 (0, False), seed)
        _ -> roll (eyeList, seed)
    Reset -> (eyeList, seed)
    Hold i ->
      let el = List.indexedMap (\i' (e, ih) -> (e, (i == i') /= ih)) eyeList in
      (el, seed)


-- Signals
digit = Signal.filter (\val-> val > 48 && val <= 57 || val == 32) 0 Keyboard.presses
intToAction d =
  case d of
    32 -> Roll
    48 -> Reset
    d -> Hold <| d - 48 - 1

inputs = Signal.map intToAction digit
model =
  Signal.foldp update initialModel inputs
-- Ports


initialSeed : Random.Seed
initialSeed = Random.initialSeed 1

-- Remember this is a pure function, same input seed yields the same output (Int, Seed)
randomSide : Random.Seed -> (Int, Random.Seed)
randomSide seed =
  Random.generate (Random.int 1 6) seed
