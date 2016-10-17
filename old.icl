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

:: State = {elements :: [Element], trains :: [Train]}
derive class iTask State
// We suppose State.elements is ordered (in rows then in columns)


// instance < Element
// where
// 	(<) oa ob = (a.y < b.y) || (a.y == b.y && a.x < b.y)
// 	where
// 		a = getPos oa
// 		b = getPos ob

// instance == Element
// where
// 	(==) oa ob = a.y == b.y && a.x == b.y
// 	where
// 		a = getPos oa
// 		b = getPos ob


mkPos x y = {x = x, y = y}

exmp = Section { sLabel= "String"
		, sPosition 	= (mkPos 10 10)
		, sLeftSignal 	= Just True
		, sRightSignal 	= Just True
		}

state :: Shared State
state = sharedStore "sharedState" {elements = [exmp], trains = []}

loopNothing :: Task State
loopNothing = viewInformation "xx" [] "ddd" >>| loopNothing

trackDesinger	= loopNothing
trainDriver		= loopNothing

trackController	= updateSharedInformation "xxx" [] state ||- imageTask


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

getPos (Point p) 	= p.pPosition
getPos (Section s)	= s.sPosition
getLabel (Point p)	= p.pLabel
getLabel (Section s)= s.sLabel

absPosition :: Real Real (Image State) (Image State) -> (Image State)
absPosition x y img over =	collage [(px x, px y)] [img] (Just over)

whiteRect :: Span Span -> (Image State)
whiteRect w h =	rect w h <@< {fill = toSVGColor ("white")} <@< {strokewidth = (px 0.0)}
nbRect :: Span Span String -> (Image State)
nbRect w h fill = rect w h <@< {fill = toSVGColor (fill)} <@< {strokewidth = (px 0.0)}

drawElementSpecific (Section s) =
	collage [
		(px 0.0, elementHeight - pBar),
		(px 4.0, px 4.0),
		(elementWidth - (px 10.0), px 4.0)
	]
	[
		line Nothing Slash elementWidth zero,
		showSignal s.sLeftSignal (\i s.{s & s.sLeftSignal}),
		showSignal s.sRightSignal (\i s.{s & clicks = i + s.clicks, red = not s.red})
	]
	where
		showSignal Nothing			= empty zero zero
		showSignal (Just s) onclick	= circle (px 6.0) <@< strokeSty <@< (if s green red)
				// <@< {onclick = \i s . {s & elements:copyOrAlter}, local = False}
		strokeSty	= {strokewidth = (px 0.4)}
		green		= {fill = toSVGColor "green"}
		red			= {fill = toSVGColor "red"}
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

drawElement elem backRectangle = 
			overlay [(AtMiddleX,AtBottom)] [] [text font (getLabel elem)]
			(Just (
				(drawElementSpecific elem)
				(Just (backRectangle (getElementHeight elem) "cyan"))
			))

lth [] = 0
lth [a:b] = (lth b) + 1

tripleMe [a:b] = [a:a:a:tripleMe b]
tripleMe [] = []

convertPositionsToImageOffsets [{x = x, y = y}:tail] = [(
		(px (toReal x)) * (elementWidth + marginWidth),
		(px (toReal y)) * (elementHeight + marginHeight)
	):convertPositionsToImageOffsets tail]
convertPositionsToImageOffsets [] = []

displayMap :: State -> Image State
displayMap s = 
		collage (convertPositionsToImageOffsets [getPos o \\ o <- s.elements])
				[drawElement item eBackground \\ item <- s.elements]
				Nothing
		where
			eBackground :: Int String -> Image State
			eBackground n fill = rect elementWidth (
						elementHeight * (px (toReal n))
						+ (marginHeight * (px (toReal (n-1))))
					) <@< {fill = toSVGColor (fill)} <@< {strokewidth = (px 0.0)}

			// <@< {onclick = \i s.{s & clicks = i + s.clicks, red = not s.red}, local = False}

font = normalFontDef "Arial" 9.0

