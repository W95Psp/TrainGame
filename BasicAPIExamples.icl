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
state = sharedStore "sharedState" {elements = [], trains = [], elementSelected = Nothing, stateTime = 0}

loopNothing :: Task State
loopNothing = viewInformation "xx" [] "ddd" >>| loopNothing

trackDesinger	= loopNothing
trainDriver		= loopNothing

// trackController	= imageTask
trackController	= makeTrainMove ||- (updateSharedInformation "xxx" [] state ||- imageTask)

// makeTrainMove :: Task Timestamp
// makeTrainMove = watch currentTimestamp >^* [
// 		OnValue (ifValue saa)
// 	]
// 	where
// 		addSeconds n t = t + {Time|hour=0,min=0,sec=n}

		

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
		| elem == Nothing 		= {train & tMoving = False, tDeltaX = 25}
		| train.tDeltaX >= 100 	= {train & tDeltaX = 0, tPosition = nextTrainPos}
							   	= {train & tDeltaX = train.tDeltaX + 25}
		where
			nextTrainPos = case elem of
				Nothing = train.tPosition
				Just (Section s) = {train.tPosition & x = train.tPosition.x+1}
				Just (Point p) = case p.pOrientation of
					NE = {train.tPosition & x = train.tPosition.x+1, y = train.tPosition.y-1}
					_ = {train.tPosition & x = train.tPosition.x+1, y = train.tPosition.y+1}
			elem = getElementByPositions train.tPosition elements




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

elementWidth_i = 50.0
elementWidth = px elementWidth_i
elementHeight_i = 35.0
elementHeight= px elementHeight_i

marginWidth	= (px 1.0)
marginHeight= (px 1.0)
pBar 		= (px 12.0)

absPosition :: Real Real (Image State) (Image State) -> (Image State)
absPosition x y img over =	collage [(px x, px y)] [img] (Just over)

whiteRect :: Span Span -> (Image State)
whiteRect w h =	rect w h <@< {fill = toSVGColor ("white")} <@< {strokewidth = (px 0.0)}
nbRect :: Span Span String -> (Image State)
nbRect w h fill = rect w h <@< {fill = toSVGColor (fill)} <@< {strokewidth = (px 0.0)}

isPlaceFree pos [current:tail] = if (current==pos) False (isPlaceFree pos tail)
isPlaceFree pos [] = True


getElementHeight (Point _) = 2
getElementHeight _ = 1

lth [] = 0
lth [a:b] = (lth b) + 1

tripleMe [a:b] = [a:a:a:tripleMe b]
tripleMe [] = []

getTileLocationOfPosition {x = x, y = y} = (
				(px (toReal x)) * (elementWidth + marginWidth),
				(px (toReal y)) * (elementHeight + marginHeight)
			)

getTileLocationOfPosition {x = x, y = y} = (
				(px (toReal x)) * (elementWidth + marginWidth),
				(px (toReal y)) * (elementHeight + marginHeight)
			)

convertPositionsToImageOffsets [c:tail] = [getTileLocationOfPosition c:convertPositionsToImageOffsets tail]
convertPositionsToImageOffsets [] = []

displayMap :: State -> Image State
displayMap s =
	collage []
	[	drawObjects s.elements s defaultGlobalVisualStyle []
	,	drawObjects s.trains s defaultGlobalVisualStyle []
	] Nothing

font = normalFontDef "Arial" 9.0