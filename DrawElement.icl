implementation module DrawElement

import State

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

import StdArray


:: ElementEventHandlerParam :== String
:: ElementEventHandler :== ElementEventHandlerParam -> Element -> Element

:: EventsElements =
	{ ee_onclickSignal 	:: Maybe ElementEventHandler
	, ee_onclick 		:: Maybe ElementEventHandler
	}
:: DrawElementCtx	= 
	{ cElement		:: Element
	, cStyle		:: GlobalVisualStyle
	, cEvents		:: EventsElements
	}
:: DrawElementsCtx	= 
	{ csElements	:: [Element]
	, csState		:: State
	, csStyle		:: GlobalVisualStyle
	, csEvents		:: EventsElements
	}



defaultEventsElements =
	{ ee_onclickSignal 	= Just onclickSignal
	, ee_onclick 		= Nothing
	}
	where
		onclickSignal param (Section s)
			| param=="L" = 	Section {s & sLeftSignal = inv s.sLeftSignal}
			| otherwise	 = Section {s & sRightSignal = inv s.sRightSignal}
		onclickSignal _ x = x
		inv (Just _)	= Nothing
		inv Nothing		= Just True

defaultEventsElements =
	{ ee_onclickSignal 	= Nothing
	, ee_onclick 		= Nothing
	}

buildDrawElementsCtx:: [Element]	State	-> DrawElementsCtx
buildDrawElementsCtx elements s =
	{ csStyle		= defaultGlobalVisualStyle
	, csElements	= elements
	, csState		= s
	, csEvents		= defaultEventsElements
	}

buildDrawElementCtx element ctx =
	{ cStyle 	= ctx.csStyle
	, cElement 	= element
	, cEvents 	= ctx.csEvents
	}

drawElementContent :: DrawElementCtx -> (Host State) -> (Image State)
drawElementContent ctx = case ctx.cElement of
		Section s 	-> 
			collage [
				(px 0.0, ctx.cStyle.vEHeight - pBar),
				(px 4.0, px 4.0),
				(ctx.cStyle.vEWidth - (px 10.0), px 4.0)
			]
			[
				line Nothing Slash ctx.cStyle.vEWidth zero,
				showSignal s.sLeftSignal  <@< {onclick = update ctx.cEvents.ee_onclickSignal "L", local = False},
				showSignal s.sRightSignal <@< {onclick = update ctx.cEvents.ee_onclickSignal "R", local = False}
			]
			where
				showSignal Nothing	= circle (px 9.0) <@< {fill = ctx.cStyle.vEBackgroundColor} <@< {stroke = ctx.cStyle.vEBackgroundColor}
				showSignal (Just s)	= circle (px 6.0) <@< strokeSty <@< (if s green red)
							 // <@< {onclick = update, local = False}
				strokeSty	= {strokewidth = (px 0.4)}
				green		= {fill = toSVGColor "green"}
				red			= {fill = toSVGColor "red"}
				update = updateElementInState (Section s)
		Point p 	-> 
			collage [
				(px 0.0, ctx.cStyle.vEHeight - pBar),
				(px 0.0, ctx.cStyle.vEHeight - pBar)
			]
			[
				(line Nothing Slash ctx.cStyle.vEWidth zero <@< { dash = [2, 3] })
					 // <@< {draggable = Just(
					 // 			\u x y s . s
					 // 		)}
				,
				polyline Nothing orient
			]
			where
				orient = case p.pOrientation of
					NE -> up
					SE -> down
					NW -> down
					SW -> up
				where
					down = [(px 0.0, px 0.0), (ctx.cStyle.vEWidth, ctx.cStyle.vEHeight)]
					up = [(px 0.0, px 0.0), (ctx.cStyle.vEWidth, zero-ctx.cStyle.vEHeight)]
				update = updateElementInState (Point p)


drawElement :: DrawElementCtx -> Image State
drawElement ctx = //empty zero zero 
	overlay [(AtMiddleX,AtBottom)] [] [text font (getLabel ctx.cElement)]
	(Just (
		(
			drawElementContent
			ctx 
			(Just background)// <@< {strokewidth = (px (if selected 1.0 0.0))})
		)
	))
	where
		background = rect ctx.cStyle.vEWidth height <@< {fill = ctx.cStyle.vEBackgroundColor} <@< {strokewidth = px 0.0}
		height = ctx.cStyle.vEHeight + case ctx.cElement of
									Point p	-> ctx.cStyle.vEHeight + ctx.cStyle.vEMargin
									_		-> px 0.0
	// <@< {onclick = \i s . {s & elementSelected = if selected Nothing (Just elem)}, local = False}
	// where
	// 	selected = (isJust ctx.cState.elementSelected) && (fromJust ctx.cState.elementSelected) == ctx.cElement

drawElements :: DrawElementsCtx -> Image State
drawElements ctx = 
	collage positions elements (Just background)
	where
		elements = [drawElement (buildDrawElementCtx element ctx) \\ element <- ctx.csElements]
		positions = convertElementsIntoPositions ctx.csElements
		convertElementsIntoPositions [element:tail] = [getLocation element:convertElementsIntoPositions tail]
		convertElementsIntoPositions [] = []
		getLocation element = case getPos element of
			{x = x, y = y} -> ((toPx x) * (ctx.csStyle.vEWidth + m), (toPx y) * (ctx.csStyle.vEHeight + m))
			where
				m = ctx.csStyle.vEMargin
		toPx = px o toReal
		background = empty (px 600.0) (px 600.0)

font = normalFontDef "Arial" 9.0
pBar = px 10.0

updateElementInState :: Element (Maybe ElementEventHandler) -> ElementEventHandlerParam -> Int -> State -> State 
updateElementInState elem Nothing = \_ . \i s . s
updateElementInState elem (Just w) = \param . \i s . {s & elements = (copyOrAlter s.elements (w param) elem)}
	where
		copyOrAlter [current:tail] w elem = [if (current == elem) (w elem) current:copyOrAlter tail w elem]
		copyOrAlter [] _ _ = []