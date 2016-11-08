definition module Objects.DrawObject

import State
import globalVisualStyle

:: EventHandlerParam 	:== String
:: EventHandler a		:== a EventHandlerParam -> a 
:: Events a				:== [(Int, EventHandler a)]

ONCLICK_ELEMENT 					:: Int
ONCLICK_POINT 						:: Int
ONCLICK_SECTION 					:: Int
ONCLICK_TRAIN						:: Int
ONCLICK_LEFT_SIGNAL 				:: Int
ONCLICK_RIGHT_SIGNAL 				:: Int

MakeEventsList :: [(Int, EventHandler a)] -> Events a
fetchEvent :: (Events a) Int -> Maybe (EventHandler a)
getEvent :: (Events a) Int -> EventHandler a
class DrawableObject a
where
	getImageOffset :: a State GlobalVisualStyle (Events a) -> ImageOffset
	drawObject :: a State GlobalVisualStyle (Events a) String -> Image State
drawObjects	:: [a] State GlobalVisualStyle (Events a) String -> Image State | DrawableObject a
