module BasicAPIExamples

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import Objects.Element
import Objects.Train
import State
import Objects.DrawObject
import TrainGui.MakeTrainMove
import TrainGui.ChooseRole

state :: Shared State
state = sharedStore "sharedState" {elements = [], trains = [], elementSelected = Nothing, paramTestX = 0.0, paramTestY = 0.0}

Start :: *World -> *World
Start world = startEngine [
		publish "/" (WebApp []) (\_ -> resetAreTrainsMoving >>| makeTrainMove state >>| chooseRole state)
	] world