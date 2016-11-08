implementation module TrainGui.MakeTrainMove

import State
import Objects.Train
import Objects.Element
import iTasks

AreTrainsMoving :: Shared Bool
AreTrainsMoving = sharedStore "AreTrainsMoving" True

resetAreTrainsMoving :: Task Void
resetAreTrainsMoving = set False AreTrainsMoving >>| return Void

makeTrainMove :: (Shared State) -> Task Void
makeTrainMove state = get AreTrainsMoving >>- \areTrainsMoving . case areTrainsMoving of
	True=	return Void
	False=	get currentTime >>- \start . watch currentTime >>* [
				OnValue (ifValue (\now -> now > start) (\_ . 
						get state >>- (
							\s . case filter (\t . t.tState==TrainMoving) s.trains of
								[] = return Void
								_  = (
										upd (\s . {s & trains = updateTrains s.trains s.elements}) state
									) ||- makeTrainMove state
						)
					))
			]
where
	updateTrains trains elements = map (\t . case t.tState of
				TrainMoving = updateMovingTrain t elements trains
				_ 			= t
			) trains
	updateMovingTrain train elements trains
		| elem == Nothing || colision	= {train & tState  = TrainDestroyed, tDelta = 25}
		| train.tDelta >= maxV 			= {train & tDelta = 0, tPosition = nextTrainPos}
							   			= {train & tDelta = train.tDelta + 25}
		where
			maxV = 75
			nextTrainPos = nextTrainPosCalc (getSignFromDir train.tDirection)
			nextTrainPosCalc d = case elem of
				Nothing = train.tPosition
				Just (Section s) = {train.tPosition & x = train.tPosition.x+d}
				Just (Point p) = case p.pOrientation of
					NE = {train.tPosition & x = train.tPosition.x+d, y = train.tPosition.y-d}
					_ = {train.tPosition & x = train.tPosition.x+d, y = train.tPosition.y+d}
			elem = findElementFromPosition train.tPosition train.tDirection elements
			colision = lookforTrain trains
				where
					lookforTrain [h:t]	= (h.tPosition==train.tPosition && (not (h.tName==train.tName))) || lookforTrain t
					lookforTrain []		= False