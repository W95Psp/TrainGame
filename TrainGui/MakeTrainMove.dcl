definition module TrainGui.MakeTrainMove

import State
import Objects.Train
import Objects.Element
import iTasks

resetAreTrainsMoving :: Task Void
makeTrainMove :: (Shared State) -> Task Void