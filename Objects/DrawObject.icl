implementation module Objects.DrawObject

import StdOrdList
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import State
import globalVisualStyle

:: EventHandlerParam 	:== String
:: EventHandler a		:== a EventHandlerParam -> a 
:: Events a				:== [(Int, EventHandler a)]

ONCLICK_ELEMENT 					:: Int
ONCLICK_ELEMENT 					= 0
ONCLICK_POINT 						:: Int
ONCLICK_POINT 						= 1
ONCLICK_SECTION 					:: Int
ONCLICK_SECTION 					= 2
ONCLICK_TRAIN						:: Int
ONCLICK_TRAIN						= 3
ONCLICK_LEFT_SIGNAL 				:: Int
ONCLICK_LEFT_SIGNAL 				= 4
ONCLICK_RIGHT_SIGNAL 				:: Int
ONCLICK_RIGHT_SIGNAL 				= 5
// CLICK_ON_ 						= 0
// CLICK_ON_ 						= 0

MakeEventsList :: [(Int, EventHandler a)] -> Events a
MakeEventsList list = sortBy f list
	where
		f (i, h1) (j, h2) = i > j
fetchEvent :: (Events a) Int -> Maybe (EventHandler a)
fetchEvent [(cName, cHandle):t] name
	| cName == name = Just cHandle
	| otherwise		= fetchEvent t name
fetchEvent [] _ = Nothing

getEvent :: (Events a) Int -> EventHandler a
getEvent events name = case fetchEvent events name of
	Just f 	= f
	Nothing = \x _ . x 

class DrawableObject a
where
	getImageOffset :: a State GlobalVisualStyle (Events a) -> ImageOffset
	drawObject :: a State GlobalVisualStyle (Events a) String -> Image State


drawObjects	:: [a] State GlobalVisualStyle (Events a) String -> Image State | DrawableObject a
drawObjects	items state style events param =
	collage positions drawnItems (Just background)
	where
		drawnItems = [drawObject item state style events param \\ item <- items]
		positions = convertItemsIntoImageOffset items
		convertItemsIntoImageOffset [item:tail] = [getImageOffset item state style events:convertItemsIntoImageOffset tail]
		convertItemsIntoImageOffset [] = []
		background = empty (px 600.0) (px 600.0)