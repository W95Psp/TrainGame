implementation module TrainGui.ChooseRole

import State
import Objects.Train
import Objects.Element
import iTasks
import Text

import TrainGui.MakeTrainMove
import TrainGui.TrackController
import TrainGui.TrackDesigner
import TrainGui.DriveTrain

chooseRole :: (Shared State) -> Task State
chooseRole state = 
	(enterChoice "Select the role you want to play" [ChooseWith (ChooseFromList getNth)] [0,1,2,3]
		>>* [OnValue (hasValue routeRoles)]
	)
	where
		optionsName = ["Design railway track", "Controller railway track", "Drive a new train", "Drive a existing train"]
		getNth	  n = sub optionsName n
			where
				sub [h:t] 0 = h
				sub [_:t] n = sub t (n-1)
				sub [] 	  n = ""
		routeRoles 0 	= upd (\s . {s & elementSelected = Nothing}) state >>| trackDesigner state True
		routeRoles 1 	= trackController state True
		routeRoles 2	= (
					get state 
				>>- \s .	enterInformation
								(	"Choose the name of your train " +++ 
									case map (\t . "\"" +++ t.tName +++ "\"") s.trains of
										[]  = "(There is no any train, currently)"
										lst = "(Already taken: " +++ (join ", " lst) +++ ")"
								) []
				>>* [
						OnAction (Action "Drive my new train!" [ActionIcon "ok"]) (
							ifValue (
								\name . case filter (\t . t.tName == name) s.trains of
									[] = True
									_  = False
							) (\n . return n)
						)
					]
				>>= \name . upd (\s . {s & trains = [newTrain name : s.trains]}) state >>| (driveTrainWithQuitButton name)
			)
			where
				newTrain name = {
					  tDelta		= 0,
					  tPosition		= P,
					  tName 		= name,
					  tState 		= TrainStill,
					  tDirection	= GoRight
					}
					where
						P = {x = 0, y = 0}
		routeRoles 3	= get state >>- \s . enterChoice "Select the train you want to control" [] [item.tName \\ item <- s.trains]
									>>* [OnValue (hasValue driveTrainWithQuitButton)]
		driveTrainWithQuitButton name = driveTrain state name >>* [
							OnAction (Action "Quit" [ActionIcon "close"])
							(always (upd (\s . {s & trains = filter (\t . not (t.tName==name)) s.trains}) state))
						]
