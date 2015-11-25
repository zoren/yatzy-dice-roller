module Yatzy.Roller where

import Graphics.Element exposing (..)
import Random
import Keyboard

type Action = Roll

-- Main

main = Signal.map view model

-- Model
initialModel = ([1,1,1,1,1], initialSeed)

-- View
view1 : Int -> Element
view1 eyes =
  let
    src = "../assets/" ++ (toString eyes) ++ ".png"
  in
    image 50 50 src

view (eyeList, _) =
  flow right (List.map view1 eyeList)

-- Update
update action (eyeList, seed) =
  case action of
    Roll -> List.foldl (\x (al, s) ->
                    let (eye, s') = randomSide s in (eye :: al, s'))
                    ([], seed)
                    eyeList

-- Signals
inputs = Signal.map (\_ -> Roll) <| Keyboard.isDown 32
model =
  Signal.foldp update initialModel inputs
-- Ports


initialSeed : Random.Seed
initialSeed = Random.initialSeed 1

-- Remember this is a pure function, same input seed yields the same output (Int, Seed)
randomSide : Random.Seed -> (Int, Random.Seed)
randomSide seed =
  Random.generate (Random.int 1 6) seed
