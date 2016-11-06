implementation module Element

import iTasks
import State
import Train
import globalVisualStyle
import iTasks.API.Extensions.SVG.SVGlet
import DrawObject

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
	, pIsUp			:: Bool
	}
:: Position = {x :: Int, y :: Int}
:: Orientation = NE | NW

:: Element = Section Section | Point Point

derive class iTask Section
derive class iTask Point
derive class iTask Position
derive class iTask Orientation
derive class iTask Element

getPos	 :: Element	-> Position
getPos	(Point p) 	= p.pPosition
getPos	(Section s)	= s.sPosition

getLabel :: Element	-> String
getLabel (Point p)	= p.pLabel
getLabel (Section s)= s.sLabel

instance == Position
where
	(==) a b = a.x == b.x && a.y == b.y

// instance < Element
// where
// 	(<) oa ob = (a.y < b.y) || (a.y == b.y && a.x < b.y)
// 	where
// 		a = getPos oa
// 		b = getPos ob

instance == Element
where
	(==) (Point a) (Point b) 		= a.pPosition == b.pPosition
	(==) (Section a) (Section b) 	= a.sPosition == b.sPosition
	(==) _ _ 	= False

findElementFromPosition :: Position TrainDirection [Element] -> Maybe Element
findElementFromPosition pos dir lst = case findElementFromPositionSub pos dir lst of
	Just (Point p) = case p.pIsUp of
		True = Just (Section {sLabel= p.pLabel
					, sPosition 	= p.pPosition
					, sLeftSignal 	= Just True
					, sRightSignal 	= Just True
					})
		False= Just (Point p)
	anything = anything

findElementFromPositionSub :: Position TrainDirection [Element] -> Maybe Element
findElementFromPositionSub pos trainDirection [elem:tail] = if samePos (Just elem) (findElementFromPositionSub pos trainDirection tail)
	where
		samePos = case elem of
			(Section s) = eq
			(Point p) = case p.pIsUp of
				True  = eq
				False = if accOrient (posElem == {pos & y = pos.y-1}) eq
					where
						accOrient = ((not td) && isNE) || (td && (not isNE)) 
						isNE = case p.pOrientation of
							NE = True
							NW = False
						td = case trainDirection of
							GoLeft = True
							GoRight = False
		eq = (posElem == pos)
		posElem = getPos elem
findElementFromPositionSub pos trainDirection [] = Nothing

drawElementContent :: Element State GlobalVisualStyle (Events Element) -> (Host State) -> (Image State)
drawElementContent elem state style events = case elem of
		Section s 	-> 
			collage [
				(px 0.0, style.vEHeight * (px 0.75)),
				(px 4.0, px 4.0),
				(style.vEWidth - (px 10.0), px 4.0)
			]
			[
				line Nothing Slash style.vEWidth zero,
				showSignal s.sLeftSignal,  //<@< {onclick = update ctx.ceEvents.ee_onclickSignal "L", local = False},
				showSignal s.sRightSignal //<@< {onclick = update ctx.ceEvents.ee_onclickSignal "R", local = False}
			]
			where
				showSignal Nothing	= circle (px 9.0) <@< {fill = style.vEBackgroundColor} <@< {stroke = style.vEBackgroundColor}
				showSignal (Just s)	= circle (px 6.0) <@< strokeSty <@< (if s green red)
							 // <@< {onclick = update, local = False}
				strokeSty	= {strokewidth = (px 0.4)}
				green		= {fill = toSVGColor "green"}
				red			= {fill = toSVGColor "red"}
				// update = updateElementInState (Section s)
		Point p 	-> 
			collage [
				(px 0.0, style.vEHeight * (px 0.75)),
				(px 0.0, style.vEHeight * (px 0.75))
			]
			(
				if p.pIsUp
				[l1, polyline Nothing orient <@< { dash = [2, 3] }]
				[l1 <@< { dash = [2, 3] }, polyline Nothing orient]
			)
			where
				l1 = line Nothing Slash style.vEWidth zero
				orient = case p.pOrientation of
					NE -> up
					NW -> down
				where
					down = [(px 0.0, px 0.0), (style.vEWidth, style.vEHeight)]
					up = [(px 0.0, px 0.0), (style.vEWidth, zero-style.vEHeight)]
				// update = updateElementInState (Point p)




instance DrawableObject Element
where
	getImageOffset elem state style events = case getPos elem of
		{x = x, y = y} -> ((toPx x) * (style.vEWidth + m), (toPx y) * (style.vEHeight + m))
		where
			m = style.vEMargin
			toPx = px o toReal
	drawObject elem state style events = 
		overlay [(AtMiddleX,AtBottom)] [] [text font (getLabel elem)]
		(Just (
			(
				(drawElementContent elem state style events)
				(Just background)// <@< {strokewidth = (px (if selected 1.0 0.0))})
			)
		))
		where
			background = rect style.vEWidth height <@< {fill = style.vEBackgroundColor} <@< {strokewidth = px 0.0}
			height = style.vEHeight + case elem of
										Point p	-> style.vEHeight + style.vEMargin
										_		-> zero

// updateElementInState :: Element (Maybe ElementEventHandler) -> ElementEventHandlerParam -> Int -> State -> State 
// updateElementInState elem Nothing = \_ . \i s . s
// updateElementInState elem (Just w) = \param . \i s . {s & elements = (copyOrAlter s.elements (w param) elem)}
// 	where
// 		copyOrAlter [current:tail] w elem = [if (current == elem) (w elem) current:copyOrAlter tail w elem]
// 		copyOrAlter [] _ _ = []

font = normalFontDef "Arial" 9.0

