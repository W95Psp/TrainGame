implementation module Objects.Element

import iTasks
import State
import Objects.Train
import globalVisualStyle
import iTasks.API.Extensions.SVG.SVGlet
import Objects.DrawObject

// define stuctures
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
	(==) (Point a) (Point b) 		= a.pPosition == b.pPosition// && a.pLabel == b.pLabel
	(==) (Section a) (Section b) 	= a.sPosition == b.sPosition// && a.sLabel == b.sLabel
	(==) _ _ 	= False

findElementFromPosition :: Position TrainDirection [Element] -> Maybe Element
findElementFromPosition pos dir lst = case findElementFromPositionSub pos dir lst of
	Just (Point p) = case p.pIsUp of // if the Point is Up, then it can be seen as a section, hence, we return a section (already handled in collision and stuff)
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

drawElementContent :: Element State GlobalVisualStyle (Events Element) SVGColor -> (Host State) -> (Image State)
drawElementContent elem state style events backColor = case elem of
		Section s 	-> 
			collage [
				(px 0.0, style.vEHeight * (px 0.75)),
				(px 4.0, px 4.0),
				(style.vEWidth - (px 10.0), px 4.0)
			]
			[
				line Nothing Slash style.vEWidth zero,
				showSignal s.sLeftSignal <@< {onclick = update (fetchEvent events ONCLICK_LEFT_SIGNAL) "", local = False},
				showSignal s.sRightSignal <@< {onclick = update (fetchEvent events ONCLICK_RIGHT_SIGNAL) "", local = False}
			]
			where
				showSignal Nothing	= circle (px 9.0) <@< {fill = backColor} <@< {stroke = backColor}
				showSignal (Just s)	= circle (px 6.0) <@< strokeSty <@< (if s green red)
							 // <@< {onclick = update, local = False}
				strokeSty	= {strokewidth = (px 0.4)}
				green		= {fill = toSVGColor "green"}
				red			= {fill = toSVGColor "red"}
				update = updateElemInState (Section s)
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



// implements DrawableObject for Element
instance DrawableObject Element
where
	getImageOffset elem state style events = case getPos elem of
		{x = x, y = y} -> ((toPx x) * (style.vEWidth + m), (toPx y) * (style.vEHeight + m))
		where
			m = style.vEMargin
			toPx = px o toReal
	drawObject elem state style events param = 
		case isGlobalEventDefined of
			True  = all <@< (
						if (isSelected && isPoint)
						{onclick = update (fetchEvent events ONCLICK_POINT) "", local = False}
						{onclick = (\i s . {s & elementSelected = if isSelected Nothing (Just elem)}), local = False}
					)
			False = if (isPoint) (all <@< {onclick = update (fetchEvent events ONCLICK_POINT) "", local = False}) all
		where
			all = overlay [(AtMiddleX,AtBottom)] [] [text font (getLabel elem)]
				(Just (
					(
						(
							drawElementContent elem state style events backColor
						)
						(Just background)// <@< {strokewidth = (px (if selected 1.0 0.0))})
					)
				))
			background = rect style.vEWidth height <@< {fill = backColor} <@< {strokewidth = px 0.0}
			height = style.vEHeight + case elem of
										Point p	-> style.vEHeight + style.vEMargin
										_		-> zero
			update = updateElemInState elem
			backColor = if (isSelected && isGlobalEventDefined) (toSVGColor "#2ecc71") style.vEBackgroundColor
			isSelected = case state.elementSelected of
					Just x = x==elem
					_ 	   = False
			isPoint = case elem of
					Point _= True
					_ 	   = False
			isGlobalEventDefined = case fetchEvent events ONCLICK_ELEMENT of
				Just _ = True
				_ 		= False

updateElemInState :: Element (Maybe (EventHandler Element)) -> EventHandlerParam -> Int -> State -> State
updateElemInState item Nothing = \_ . \i s . s
updateElemInState item (Just w) = \param . \i s . {s & elements = map (\j . if (j==item) (w item param) j) s.elements}

font = normalFontDef "Arial" 9.0

