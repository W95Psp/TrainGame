implementation module ImageTask

import State
import Objects.Train
import Objects.Element
import Objects.DrawObject
import iTasks

// display the shared state as an image and update it
imageTask :: (Shared State) (Events Element) (Events Train) String String Bool -> Task State
imageTask state eventsElements eventsTrains paramElements paramTrains dispTrains =
	updateSharedInformation
		(Title "Image")
		[imageUpdate
			id							// server state (share) and view are identical
			(\s v tags -> displayMap state s eventsElements eventsTrains paramElements paramTrains dispTrains)
										// generate image
			(\s v -> v)					// update view when state changes
			(\s v -> s)					// update state when view changes
			(\_ s -> Nothing)			// no conflict handling
			(\o n.n)					// always select the new state
		]
		state



displayMap :: (Shared State) State (Events Element) (Events Train) String String Bool -> Image State
displayMap state s eventsElements eventsTrains paramElements paramTrains dispTrains =
	collage [] (if dispTrains [dE, dT] [dE]) (Just (empty w h))
	where
		dE = drawObjects s.elements s defaultGlobalVisualStyle eventsElements paramElements
		dT = drawObjects s.trains s defaultGlobalVisualStyle eventsTrains paramTrains
		w = (px (toReal (max.x+2))) * (defaultGlobalVisualStyle.vEWidth + defaultGlobalVisualStyle.vEMargin)
		h = (px (toReal (max.y+2))) * (defaultGlobalVisualStyle.vEHeight + defaultGlobalVisualStyle.vEMargin)
		max = {x = maxLst [i.x \\ i <- allPos] 0, y = maxLst [i.y \\ i <- allPos] 0}
		allPos = [getPos item \\ item <- s.elements] ++ [item.tPosition \\ item <- s.trains]
		maxLst [a:b] current = maxLst b (if (a > current) a current)
		maxLst [] current = current