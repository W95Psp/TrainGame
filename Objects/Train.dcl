definition module Objects.Train

import iTasks
import Objects.Element
import Objects.DrawObject

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
derive class iTask TrainState
derive class iTask TrainDirection
derive class iTask Train
getSignFromDir :: TrainDirection -> Int

instance DrawableObject Train
instance == TrainState