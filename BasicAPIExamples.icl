module BasicAPIExamples

import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import Element
import Train
import State
import DrawObject

Start :: *World -> *World
Start world = startEngine [
		publish "/" (WebApp []) (\_ -> chooseRole)
	] world

chooseRole :: Task State
chooseRole = trackController
// chooseRole = enterChoice "Select the role you want to play" [] ["Track Designer", "Track Controller", "Train Driver"] >>= routeRoles

routeRoles "Track Designer" 	= trackDesinger
routeRoles "Track Controller" 	= trackController
routeRoles "Train Driver" 		= trainDriver


state :: Shared State
state = sharedStore "sharedState" {elements = [], trains = [], elementSelected = Nothing, paramTestX = 0.0, paramTestY = 0.0}

loopNothing :: Task State
loopNothing = viewInformation "xx" [] "ddd" >>| loopNothing

trackDesinger	= loopNothing
trainDriver		= loopNothing

// trackController	= imageTask
trackController	= makeTrainMove ||- (updateSharedInformation "xxx" [] state ||- imageTask)


makeTrainMove :: Task Time
makeTrainMove = get currentTime >>- \start -> watch currentTime >>* [
		OnValue (ifValue (\now -> now >= nextSecond start) (\_ . saa))
	]
where
	nextSecond t = t + {Time|hour=0,min=0,sec=1}
	saa :: Task Time
	saa = upd (\s . {s & trains = updateTrains s.trains s.elements}) state ||- makeTrainMove
	updateTrains [train:tail] elements = [
			case train.tMoving of
				True = updateMovingTrain train elements
				False = train
			:updateTrains tail elements]
	updateTrains [] _ = []
	updateMovingTrain train elements
		| elem == Nothing 		= {train & tMoving = False, tDelta = 25}
		| train.tDelta >= 100 	= {train & tDelta = 0, tPosition = nextTrainPos (getSignFromDir train.tDirection)}
							   	= {train & tDelta = train.tDelta + 25}
		where
			nextTrainPos d = case elem of
				Nothing = train.tPosition
				Just (Section s) = {train.tPosition & x = train.tPosition.x+d}
				Just (Point p) = case p.pOrientation of
					NE = {train.tPosition & x = train.tPosition.x+d, y = train.tPosition.y-d}
					_ = {train.tPosition & x = train.tPosition.x+d, y = train.tPosition.y+d}
			elem = findElementFromPosition train.tPosition train.tDirection elements


// display the shared state as an image and update it
imageTask :: Task State
imageTask =
	updateSharedInformation
		(Title "Image")
		[imageUpdate
			id							// server state (share) and view are identical
			(\s v tags -> displayMap s)	// generate image
			(\s v -> v)					// update view when state changes
			(\s v -> s)					// update state when view changes
			(\_ s -> Nothing)			// no conflict handling
			(\o n.n)					// always select the new state
		]
		state


displayMap :: State -> Image State
displayMap s =
	collage []
	[	drawObjects s.elements s defaultGlobalVisualStyle []
	,	drawObjects s.trains s defaultGlobalVisualStyle []
	] Nothing