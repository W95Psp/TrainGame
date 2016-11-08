implementation module Objects.Train

import globalVisualStyle
import Objects.Element
import Objects.DrawObject
import State
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import StdArray

:: TrainDirection = GoLeft | GoRight
:: TrainState = TrainStill | TrainMoving | TrainDestroyed
:: Train = 
	{
		tDelta		:: Int,
		tPosition	:: Position,
		tName 		:: String,
		tState		:: TrainState,
		tDirection	:: TrainDirection
	}

instance == TrainState
where
	(==) TrainStill TrainStill			= True
	(==) TrainMoving TrainMoving		= True
	(==) TrainDestroyed TrainDestroyed	= True
	(==) _ _							= False

getSignFromDir :: TrainDirection -> Int
getSignFromDir GoLeft = -1
getSignFromDir GoRight = 1

derive class iTask TrainState
derive class iTask TrainDirection
derive class iTask Train

getRealFromSpan (PxSpan r) = r
getRealFromSpan _ = 1.0

conv r = (getRealFromSpan r) * 0.7

getRotationTrain element = case element of
	Nothing = 12.0
	Just (Section s) = 0.0
	Just (Point p) = case p.pOrientation of
			NE = -42.5
			NW = 42.5

bigSmoke = polygon Nothing [
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
	]
smallSmoke = polygon Nothing [
		(px 0.32, px 0.31),(px 0.32, px 0.25),(px 0.3, px 0.19),(px 0.27, px 0.12),
		(px 0.24, px 0.08),(px 0.2, px 0.05),(px 0.18, px 0.03),(px 0.16, px 0.02),
		(px 0.13, px 0.0),(px 0.09, px 0.0),(px 0.04, px 0.0),(px 0.01, px 0.03),
		(px 0.0, px 0.05),(px 0.0, px 0.09),(px 0.0, px 0.15),(px 0.03, px 0.16),
		(px 0.04, px 0.18),(px 0.06, px 0.19),(px 0.09, px 0.19),(px 0.12, px 0.19),
		(px 0.14, px 0.2),(px 0.17, px 0.21),(px 0.19, px 0.22),(px 0.21, px 0.21),
		(px 0.23, px 0.21),(px 0.25, px 0.23),(px 0.25, px 0.25),(px 0.25, px 0.31),
		(px 0.31, px 0.31)
	]

trainSVG state op =
	(collage [
		(px 0.0, px 0.42),

		(px 0.04, px 0.69),(px 0.26, px 0.69),(px 0.48, px 0.69),
		(px 0.70, px 0.765),(px 0.84, px 0.765),

		case state of
			0 = (px 0.32, px 0.01)
			_ = (px 0.58, px 0.11)
	] [
		polygon Nothing [
					(px 0.04, px 0.42),(px 0.24, px 0.42),(px 0.24, px 0.49),
					(px 0.43, px 0.49),(px 0.43, px 0.45),(px 0.45, px 0.43),(px 0.53, px 0.43),(px 0.55, px 0.45),(px 0.55, px 0.49),
					(px 0.80, px 0.49),(px 0.80, px 0.45),(px 0.79, px 0.45),(px 0.79, px 0.43),(px 0.91, px 0.43),(px 0.91, px 0.45),
					(px 0.90, px 0.45),(px 0.90, px 0.49),(px 0.95, px 0.49),(px 0.95, px 0.70),(px 0.99, px 0.70),(px 0.99, px 0.77),
					(px 0.00, px 0.77),(px 0.00, px 0.62),(px 0.11, px 0.62),(px 0.11, px 0.50),(px 0.04, px 0.50),(px 0.04, px 0.42)
				] <@< opp,
		bigCircle <@< opp,bigCircle <@< opp,bigCircle <@< opp,
		smallCircle <@< opp,smallCircle <@< opp,
		case state of
			0 = (bigSmoke	<@< {fill = toSVGColor "gray"}) <@< opp
			1 = (smallSmoke	<@< {fill = toSVGColor "gray"}) <@< opp
			2 = empty zero zero
			3 = collage [(px -0.3, px 0.2), (px -0.3, px 0.4)]
				[rotate (deg 120.0) bigSmoke, rotate (deg -55.0) smallSmoke] Nothing <@< {fill = SVGRGB 255 204 0} <@< {opacity = op/2.0}
	] Nothing)
	where
		opp = {opacity = op}
		bigCircle = circle (px 0.20) <@< sw
		smallCircle = circle (px 0.12) <@< sw
		sw = {strokewidth = zero}


drawTrainContent train state style events param =
	scale (conv style.vEWidth) (conv style.vEHeight) case train.tDirection of
			GoRight = drawn
			GoLeft 	= flipx drawn
	where
		drawn = trainSVG calc case param of
				"" = 1.0
				_  = if(param==train.tName) 1.0 0.2
		calc = case train.tState of
			TrainMoving		= if ((calcTmp == 0) || (calcTmp == 2)) 0 1
			TrainStill		= 2
			TrainDestroyed	= 3
		calcTmp = train.tDelta / 25
	
instance + (Real,Real) where (+) (a,b) (c,d) = (a+c,b+d)
instance * ImageOffset where (*) (a,b) (c,d) = (a*c,b*d)

instance DrawableObject Train
where
	getImageOffset train state style events = case train.tPosition of
		{x = x, y = y} -> 
			  (m + style.vEWidth, m + style.vEHeight) * (toPx (
			  		  (toReal x, toReal y)
			  		+ case element of
			  			Just (Point p) = case p.pOrientation of
			  				NE = case train.tDirection of
			  					GoRight	= (-0.3, 0.3)
			  					GoLeft	= (-0.2, 1.2)
			  				NW = case train.tDirection of
			  					GoRight	= (zero,zero)
			  					GoLeft	= (0.4,-0.65)
			  			Just (Section s) = (zero, 0.15)
			  			Nothing = (zero, zero)
			  		+ ((cos angle) * w, (sin angle) * w)
			  ))
		where
			w			= case train.tDirection of
							GoLeft  = 0.5 -	howFar
							GoRight = 		howFar
			howFar		= (toReal train.tDelta) / 100.0
			angle		= (pi/180.0) * (getRotationTrain element)
			element		= findElementFromPosition train.tPosition train.tDirection state.elements
			m			= style.vEMargin
			toPx (a,b) 	= (px a, px b)
	drawObject train state style events param = 
		rotate (deg (
				getRotationTrain (findElementFromPosition train.tPosition train.tDirection state.elements)
			)) (drawTrainContent train state style events param)