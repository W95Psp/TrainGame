implementation module TrainGui.DriveTrain

import State
import Objects.Train
import Objects.Element
import TrainGui.MakeTrainMove
import ImageTask
import iTasks

:: DriverTrainView = {trainPosition :: Position}
derive class iTask DriverTrainView

driveTrain :: (Shared State) String -> Task State
driveTrain state trainName = 
	(
		updateSharedInformation ("You are driving the train \"" +++ trainName +++ "\"") [
				UpdateWith 
					((\t . {trainPosition = t.tPosition}) o getTrainFromState)
					(\s trainViewInfo . updateTrain (\t . {t & tPosition = trainViewInfo.trainPosition}) s)
			] state
		>^* [
			  OnAction (Action "Move left" [ActionIcon "previous", ActionKey (unmodified KEY_LEFT)])
			  			((ifState TrainStill) (\_ .
			  				upd (updateTrain (setTrainDirection GoLeft)) state >>= \x . makeTrainMove state >>| (return x)
			  			))
			, OnAction (Action "||" [ActionKey (unmodified KEY_BACKSPACE)])
						((ifState TrainMoving) (\_ .
							upd (updateTrain stopTrain) state
						))
			, OnAction (Action "Move right" [ActionIcon "next", ActionKey (unmodified KEY_RIGHT)])
						((ifState TrainStill) (\_ .
							upd (updateTrain (setTrainDirection GoRight)) state >>= \x . makeTrainMove state >>| (return x)
						))
			, OnAction (Action "Repair" [])
						((ifState TrainDestroyed) (\_ .
							upd (updateTrain (\t . {t &
																tState  = TrainStill
															})) state >>= \x . makeTrainMove state >>| (return x)
						))
		]
	) ||- (imageTask state [] [] "" trainName True)
	where
		ifState st = ifValue (\s . (getTrainFromState s).tState==st)
		// update current train from state
		updateTrain :: (Train -> Train) State -> State
		updateTrain replaceFunction s = {s & trains = map (\t . if (isMe t) (replaceFunction t) t) s.trains}
		// fetch current train from state
		getTrainFromState :: State -> Train
		getTrainFromState s = case filter isMe s.trains of
			[h:t] = h
			[]	  = {
					tDelta		= 0,
					tPosition	= {x = -1, y = -1},
					tName 		= "",
					tState		= TrainStill,
					tDirection	= GoRight
				}
		// check if current train is t
		isMe t = t.tName==trainName
		//Alter train
		stopTrain t = {t &  
						tState  = TrainStill
					}
		setTrainDirection dir t = {t & 
						tDelta = if (isTrainDirEq dir t) t.tDelta (100-t.tDelta),
						tDirection = dir, 
						tState  = TrainMoving
					}
		// Dirty equality
		isTrainDirEq dir b = case dir of
			GoLeft = case b.tDirection of
				GoLeft = True
				GoRight = False
			GoRight = case b.tDirection of
				GoLeft = False
				GoRight = True
