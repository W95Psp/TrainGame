definition module DrawObject

import State
import globalVisualStyle

:: EventHandlerParam 	:== String
:: EventHandler a		:== a EventHandlerParam -> a 
:: Events a				:== [(Int, EventHandler a)]
MakeEventsList :: [(Int, EventHandler a)] -> Events a
fetchEvent :: (Events a) Int -> Maybe (EventHandler a)
getEvent :: (Events a) Int -> EventHandler a
class DrawableObject a
where
	getImageOffset :: a State GlobalVisualStyle (Events a) -> ImageOffset
	drawObject :: a State GlobalVisualStyle (Events a) -> Image State
drawObjects	:: [a] State GlobalVisualStyle (Events a) -> Image State | DrawableObject a
