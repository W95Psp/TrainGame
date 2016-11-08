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
makeTrainMove state = get AreTrainsMoving >>- \areTrainsMoving . case areTrainsMoving of	//check if this task run already
	True=	return Void
	False=	get currentTime >>- \start . watch currentTime >>* [	//watch time
				OnValue (ifValue (\now -> now > start) (\_ . 		//if it's at least a second later
						get state >>- (
							\s . case filter (\t . t.tState==TrainMoving) s.trains of	//compute list of moving trains
								[] = return Void 	// no any train moving, we stop this task
								_  = (				// some train(s) moving, we move trains and we loop
										upd (\s . {s & trains = updateTrains s.trains s.elements}) state
									) ||- makeTrainMove state
						)
					))
			]
where
	// for each train, if it is moving, the we move it, else, we let it intact
	updateTrains trains elements = map (\t . case t.tState of
				TrainMoving = updateMovingTrain t elements trains
				_ 			= t
			) trains
	// alter a train to make it move
	updateMovingTrain :: Train [Element] [Train] -> Train
	updateMovingTrain train elements trains
		| elem == Nothing || colision	= {train & tState  = TrainDestroyed, tDelta = 25}
		| train.tDelta >= maxV 			= {train & tDelta = 0, tPosition = nextTrainPosCalc (getSignFromDir train.tDirection)}
							   			= {train & tDelta = train.tDelta + 25}
		where
			maxV = 75	// threshold
			nextTrainPosCalc :: Int -> Position
			nextTrainPosCalc d = case elem of	// next position (according train's direction)
				Nothing = train.tPosition
				Just (Section s) = {train.tPosition & x = train.tPosition.x+d}
				Just (Point p) = case p.pOrientation of // if it is a point, then the train go either up or down
					NE = {train.tPosition & x = train.tPosition.x+d, y = train.tPosition.y-d}
					_ = {train.tPosition & x = train.tPosition.x+d, y = train.tPosition.y+d}
			elem :: Maybe Element
			elem = findElementFromPosition train.tPosition train.tDirection elements 	//element under current train 
			colision :: Bool
			colision = lookforTrain trains
				where
					lookforTrain [h:t]	= (h.tPosition==train.tPosition && (not (h.tName==train.tName))) || lookforTrain t
					lookforTrain []		= False