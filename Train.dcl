definition module Train

import iTasks
import Element
import DrawObject


:: Train = 
	{
		tDeltaX		:: Int,
		tDeltaY		:: Int,
		tPosition	:: Position,
		tName 		:: String,
		tMoving		:: Bool
	}
derive class iTask Train

getElementByPositions :: Position [Element] -> Maybe Element

instance DrawableObject Train