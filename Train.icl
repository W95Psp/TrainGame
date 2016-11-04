implementation module Train

import globalVisualStyle
import Element
import DrawObject
import State
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import StdArray

:: Train = 
	{
		tDeltaX		:: Int,
		tDeltaY		:: Int,
		tPosition	:: Position,
		tName 		:: String,
		tMoving		:: Bool
	}
derive class iTask Train





getRealFromSpan (PxSpan r) = r
getRealFromSpan _ = 1.0

conv r = (getRealFromSpan r) * 0.7

getRotationTrain train elements = 
		case getElementByPositions train.tPosition elements of
			Nothing = 12.0
			Just (Section s) = 0.0
			Just (Point p) = case p.pOrientation of
					NE = -42.5
					SE = 42.5
					_  = 0.0

getElementByPositions :: Position [Element] -> Maybe Element
getElementByPositions pos [elem:tail] = if samePos (Just elem) (getElementByPositions pos tail)
	where
		samePos = case elem of
			(Section s) = eq
			(Point p) = case p.pOrientation of
				NE = posElem == {pos & y = pos.y-1}
				_  = eq
		eq = (posElem == pos)
		posElem = getPos elem
getElementByPositions pos [] = Nothing

drawTrainContent train state style events =
	scale (conv style.vEWidth) (conv style.vEHeight)
	(collage [
		(px 0.0, px 0.42),

		(px 0.04, px 0.69),(px 0.26, px 0.69),(px 0.48, px 0.69),
		(px 0.70, px 0.765),(px 0.84, px 0.765),

		case calc of
			0 = (px 0.32, px 0.01)
			_ = (px 0.58, px 0.11)
	] [
		polygon Nothing [
			(px 0.04, px 0.42),(px 0.24, px 0.42),(px 0.24, px 0.49),
			(px 0.43, px 0.49),(px 0.43, px 0.45),(px 0.45, px 0.43),(px 0.53, px 0.43),(px 0.55, px 0.45),(px 0.55, px 0.49),
			(px 0.80, px 0.49),(px 0.80, px 0.45),(px 0.79, px 0.45),(px 0.79, px 0.43),(px 0.91, px 0.43),(px 0.91, px 0.45),
			(px 0.90, px 0.45),(px 0.90, px 0.49),(px 0.95, px 0.49),(px 0.95, px 0.70),(px 0.99, px 0.70),(px 0.99, px 0.77),
			(px 0.00, px 0.77),(px 0.00, px 0.62),(px 0.11, px 0.62),(px 0.11, px 0.50),(px 0.04, px 0.50),(px 0.04, px 0.42)
		],
		bigCircle,bigCircle,bigCircle,
		smallCircle,smallCircle,
		case calc of
			0 = (polygon Nothing [
					(px 0.51, px 0.4),(px 0.57, px 0.4),(px 0.56, px 0.35),(px 0.56, px 0.32),
					(px 0.55, px 0.29),(px 0.54, px 0.25),(px 0.5, px 0.18),(px 0.47, px 0.15),
					(px 0.44, px 0.13),(px 0.4, px 0.12),(px 0.39, px 0.09),(px 0.37, px 0.08),
					(px 0.36, px 0.06),(px 0.34, px 0.05),(px 0.32, px 0.04),(px 0.24, px 0.04),
					(px 0.21, px 0.0),(px 0.18, px 0.0),(px 0.13, px -0.01),(px 0.08, px 0.0),
					(px 0.05, px 0.02),(px 0.03, px 0.04),(px 0.01, px 0.07),(px 0.0, px 0.1),
					(px 0.0, px 0.13),(px 0.0, px 0.17),(px 0.0, px 0.2),(px 0.02, px 0.23),
					(px 0.06, px 0.26),(px 0.08, px 0.28),(px 0.12, px 0.28),(px 0.16, px 0.28),
					(px 0.18, px 0.28),(px 0.2, px 0.29),(px 0.23, px 0.31),(px 0.28, px 0.32),
					(px 0.32, px 0.31),(px 0.34, px 0.32),(px 0.38, px 0.34),(px 0.4, px 0.34),
					(px 0.43, px 0.33),(px 0.46, px 0.31),(px 0.49, px 0.31),(px 0.5, px 0.32),
					(px 0.51, px 0.35),(px 0.51, px 0.39),(px 0.51, px 0.4),(px 0.57, px 0.4)
				] <@< {fill = toSVGColor "gray"})
			1 = (polygon Nothing [
					(px 0.32, px 0.31),(px 0.32, px 0.25),(px 0.3, px 0.19),(px 0.27, px 0.12),
					(px 0.24, px 0.08),(px 0.2, px 0.05),(px 0.18, px 0.03),(px 0.16, px 0.02),
					(px 0.13, px 0.0),(px 0.09, px 0.0),(px 0.04, px 0.0),(px 0.01, px 0.03),
					(px 0.0, px 0.05),(px 0.0, px 0.09),(px 0.0, px 0.15),(px 0.03, px 0.16),
					(px 0.04, px 0.18),(px 0.06, px 0.19),(px 0.09, px 0.19),(px 0.12, px 0.19),
					(px 0.14, px 0.2),(px 0.17, px 0.21),(px 0.19, px 0.22),(px 0.21, px 0.21),
					(px 0.23, px 0.21),(px 0.25, px 0.23),(px 0.25, px 0.25),(px 0.25, px 0.31),
					(px 0.31, px 0.31)
				] <@< {fill = toSVGColor "gray"})
			_ = empty zero zero
	] Nothing)
	where
		bigCircle = circle (px 0.20) <@< sw <@< fb
		smallCircle = circle (px 0.12) <@< sw <@< fb
		sw = {strokewidth = zero}
		fb = {fill = toSVGColor "black"}
		calc = if ((calcTmp == 0) || (calcTmp == 2)) 0 1
		calcTmp = ((train.tDeltaX + train.tDeltaY) / 25)



instance DrawableObject Train
where
	getImageOffset train state style events = case train.tPosition of
		{x = x, y = y} -> ((px cx) * (wx + m) + mx, (px cy) * (wy + m) + my)
		where
			cx = (toReal x) - isPoint * 0.3
			cy = (toReal y) + (abs 0.0) * 1.1
			// cx = (toReal x) + ((toReal train.tDeltaX) / 100.0)
			// cy = (toReal y) + ((toReal train.tDeltaY) / 100.0)
			// mx = (px 0.15) * wx
			mx = (px ((cos (toRad angle)) * w)) * wx
			my = (px ((sin (toRad angle)) * w)) * wy + (px 0.15) * wy
			w = (toReal train.tDeltaX) / 100.0
			isPoint = if ((abs angle)<30.0) 0.0 (0.0 - ((angle) / (abs angle)))
			// angle = getRotationTrain train state.elements
			angle = 3.0
			wx = style.vEWidth
			wy = style.vEHeight
			m = style.vEMargin
			toRad d = d * (pi/180.0)
			toDeg d = d * (180.0/pi)
	drawObject train state style events = 
		rotate (deg (
				getRotationTrain train state.elements
			)) (drawTrainContent train state style events)