module BasicAPIExamples

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

import StdDebug

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

:: Section =
	{ sLabel 		:: String
	, sPosition 	:: Position
	, sLeftSignal 	:: Maybe Bool
	, sRightSignal 	:: Maybe Bool
	}
:: Point =
	{ pLabel 		:: String
	, pPosition 	:: Position
	, pOrientation 	:: Orientation
	}
:: Position = {x :: Int, y :: Int}
:: Orientation = NE | SE | SW | NW

:: Element = Section Section | Point Point

:: Train = 
	{ 
		usedElement :: Element,
		name :: String
	}
derive class iTask Section
derive class iTask Point
derive class iTask Position
derive class iTask Orientation
derive class iTask Element
derive class iTask Train

:: State = {elements :: [Element], trains :: [Train], elementSelected :: (Maybe Element)}
derive class iTask State
// We suppose State.elements is ordered (in rows then in columns)


// instance < Element
// where
// 	(<) oa ob = (a.y < b.y) || (a.y == b.y && a.x < b.y)
// 	where
// 		a = getPos oa
// 		b = getPos ob

getPos	(Point p) 	= p.pPosition
getPos	(Section s)	= s.sPosition
getLabel (Point p)	= p.pLabel
getLabel (Section s)= s.sLabel


instance == Position
where
	(==) a b = a.x == b.x && a.y == b.y

instance == Element
where
	(==) (Point a) (Point b) 		= a.pPosition == b.pPosition
	(==) (Section a) (Section b) 	= a.sPosition == b.sPosition
	(==) _ _ 	= False


mkPos x y = {x = x, y = y}

exmp = Section { sLabel= "String"
		, sPosition 	= (mkPos 10 10)
		, sLeftSignal 	= Just True
		, sRightSignal 	= Just True
		}

state :: Shared State
state = sharedStore "sharedState" {elements = [], trains = [], elementSelected = Nothing}

loopNothing :: Task State
loopNothing = viewInformation "xx" [] "ddd" >>| loopNothing

trackDesinger	= loopNothing
trainDriver		= loopNothing

trackController	= imageTask
// trackController	= updateSharedInformation "xxx" [] state ||- imageTask


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

drawElementSpecific :: Element -> (Host State) -> (Image State)
drawElementSpecific (Section s) =
	collage [
		(px 0.0, elementHeight - pBar),
		(px 4.0, px 4.0),
		(elementWidth - (px 10.0), px 4.0)
	]
	[
		line Nothing Slash elementWidth zero,
		showSignal s.sLeftSignal  <@< {onclick = update (\_ . Section {s & sLeftSignal  = if (isJust s.sLeftSignal)  Nothing (Just True)}), local = False},
		showSignal s.sRightSignal <@< {onclick = update (\_ . Section {s & sRightSignal = if (isJust s.sRightSignal) Nothing (Just True)}), local = False}
	]
	where
		showSignal Nothing	= circle (px 9.0) <@< {fill = toSVGColor "cyan"} <@< {stroke = toSVGColor "cyan"}
		showSignal (Just s)	= circle (px 6.0) <@< strokeSty <@< (if s green red)
					 // <@< {onclick = update, local = False}
		strokeSty	= {strokewidth = (px 0.4)}
		green		= {fill = toSVGColor "green"}
		red			= {fill = toSVGColor "red"}
		update = updateElementInState (Section s)

drawElementSpecific (Point p) = 
	collage [
		(px 0.0, elementHeight - pBar),
		(px 0.0, elementHeight - pBar)
	]
	[
		line Nothing Slash elementWidth zero <@< { dash = [2, 3] },
		polyline Nothing orient
	]
	where
		orient = case p.pOrientation of
			NE -> up
			SE -> down
			NW -> down
			SW -> up
		where
			down = [(px 0.0, px 0.0), (elementWidth, elementHeight)]
			up = [(px 0.0, px 0.0), (elementWidth, zero-elementHeight)]
getElementHeight (Point _) = 2
getElementHeight _ = 1

drawElement s elem backRectangle = 
			overlay [(AtMiddleX,AtBottom)] [] [text font (getLabel elem)]
			(Just (
				(drawElementSpecific elem)
				(Just ((backRectangle (getElementHeight elem) "cyan") <@< {strokewidth = (px (if selected 1.0 0.0))}))
			))
			<@< {onclick = \i s . {s & elementSelected = if selected Nothing (Just elem)}, local = False}
			where
				selected = (isJust s.elementSelected) && (fromJust s.elementSelected) == elem

// same (Section a) (Section b) = a.sLabel == b.sLabel && a.sPosition.x == b.sPosition.x && a.sPosition.y == b.sPosition.y
// same (Point a) (Point b) = a.sLabel == b.sLabel && a.sPosition.x == b.sPosition.x && a.sPosition.y == b.sPosition.y
// same _ _  = False

updateElementInState elem w = \i s . {s & elements = (copyOrAlter s.elements w elem)}
	where
	copyOrAlter [current:tail] w elem = [if (current == elem) (w elem) current:copyOrAlter tail w elem]
	copyOrAlter [] _ _ = []

lth [] = 0
lth [a:b] = (lth b) + 1

tripleMe [a:b] = [a:a:a:tripleMe b]
tripleMe [] = []

getTileLocationOfPosition {x = x, y = y} = (
				(px (toReal x)) * (elementWidth + marginWidth),
				(px (toReal y)) * (elementHeight + marginHeight)
			)

displayMap :: State -> Image State
displayMap s = 
		collage ((convertPositionsToImageOffsets [getPos o \\ o <- s.elements]) ++ (getArrowsPosition s.elementSelected))
				([drawElement s item eBackground \\ item <- s.elements] ++ arrows)
				Nothing
		where
			eBackground :: Int String -> Image State
			eBackground n fill = rect elementWidth (
						elementHeight * (px (toReal n))
						+ (marginHeight * (px (toReal (n-1))))
					) <@< {fill = toSVGColor (fill)} <@< {strokewidth = (px 0.0)}
			arrows = if (isJust s.elementSelected) [
					rotate (deg -90.0)	(if (isPlaceFree) arrow),
					rotate (deg 90.0)	(if (isPlaceFree) arrow),
										(if (isPlaceFree) arrow),
					rotate (deg 180.0)	(if (isPlaceFree) arrow),
					
					
					(scale 10.0 10.0 (polygon Nothing [
									(px 0.30, px 0.0),
									(px 0.65, px 0.0),
									(px 0.65, px 0.15),
									(px 0.97, px 0.15),
									(px 0.90, px 0.15),
									(px 0.85, px 1.20),
		
									(px 0.70, px 1.20),
									(px 0.70, px 0.34),
									(px 0.70, px 1.20),
		
									(px 0.50, px 1.20),
									(px 0.50, px 0.34),
									(px 0.50, px 1.20),
		
									(px 0.27, px 1.20),
									(px 0.27, px 0.34),
									(px 0.27, px 1.20),
		
									(px 0.13, px 1.20),
									(px 0.09, px 0.15),
									(px 0.0, px 0.15),
									(px 0.65, px 0.15),
									(px 0.28, px 0.15),
									(px 0.30, px 0.0)
								])) <@< {strokewidth = (px 0.08)}
									<@< {fill = (toSVGColor "white")}
									<@< {stroke = (toSVGColor "black")}
				] []
			ifFree x y = case s.elementSelected of
				Just (elem) = case elem of
					Point p		-> 
					Section s	-> add (getTileLocationOfPosition s.sPosition) (get 1)
			arrow = scale arrowScale arrowScale (polygon Nothing [(px 0.0, px 0.0), (px 1.0, px 1.0), (px 0.0, px 2.0)])
					<@< {stroke = (toSVGColor "gray")} <@< {strokewidth = px 0.12}
			getArrowsPosition s = case s of
					Just elem -> case elem of
									Point p		-> add (getTileLocationOfPosition p.pPosition) (get 2)
									Section s	-> add (getTileLocationOfPosition s.sPosition) (get 1)
					Nothing   -> []
				where
					add loc [current:tail] = [loc + current:add loc tail]
					add loc [] = []
					get h = [	(middleWidth							, zero - (px 6.0)),
								(middleWidth + arrowWidth - (px 1.5)	, height + (px 4.5)),
								(elementWidth + (px 6.0)				, (height - arrowWidth) / (px 2.0)),
								(zero - (px 6.0)						, (height + arrowWidth) / (px 2.0) - (px 2.0)),

								(elementWidth + (px 3.0)				, zero - (px 10.0))
							]
						where
							middleWidth = (elementWidth - arrowWidth) / (px 2.0)
							height = elementHeight * (px (toReal h))
			middle a = a / (px 2.0)
			arrowWidth = px (arrowScale * 2.0)
			arrowScale = 10.0
			// <@< {onclick = \i s.{s & clicks = i + s.clicks, red = not s.red}, local = False}

font = normalFontDef "Arial" 9.0

