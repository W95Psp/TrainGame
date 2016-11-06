definition module Train

import iTasks
import Element
import DrawObject

:: TrainDirection = GoLeft | GoRight
:: Train = 
	{
		tDelta		:: Int,
		tPosition	:: Position,
		tName 		:: String,
		tMoving		:: Bool,
		tDirection	:: TrainDirection
	}
derive class iTask TrainDirection
derive class iTask Train
getSignFromDir :: TrainDirection -> Int

instance DrawableObject Train