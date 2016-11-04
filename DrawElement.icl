implementation module DrawElement

import State
import globalVisualStyle

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

import StdArray


	// <@< {onclick = \i s . {s & elementSelected = if selected Nothing (Just elem)}, local = False}
	// where
	// 	selected = (isJust ctx.cState.elementSelected) && (fromJust ctx.cState.elementSelected) == ctx.ceElement

drawElements :: DrawElementsCtx -> Image State
drawElements ctx = 
	collage positions elements (Just background)
	where
		elements = [drawElement (buildDrawElementCtx element ctx) \\ element <- ctx.cesElements]
		positions = convertElementsIntoPositions ctx.cesElements
		convertElementsIntoPositions [element:tail] = [getLocation element:convertElementsIntoPositions tail]
		convertElementsIntoPositions [] = []
		getLocation element = case getPos element of
			{x = x, y = y} -> ((toPx x) * (ctx.cesStyle.vEWidth + m), (toPx y) * (ctx.cesStyle.vEHeight + m))
			where
				m = ctx.cesStyle.vEMargin
		toPx = px o toReal
		background = empty (px 600.0) (px 600.0)

