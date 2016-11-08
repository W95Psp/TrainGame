definition module ImageTask

import State
import Objects.Train
import Objects.Element
import iTasks

imageTask :: (Shared State) (Events Element) (Events Train) String String Bool -> Task State
