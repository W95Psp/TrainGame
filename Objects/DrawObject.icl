implementation module Objects.DrawObject

import StdOrdList
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import State
import globalVisualStyle

//Define event related stuff
:: EventHandlerParam 	:== String
:: EventHandler a		:== a EventHandlerParam -> a 
:: Events a				:== [(Int, EventHandler a)]

//Define constant to identify different possible events
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

// take a list of tuple and sort it
MakeEventsList :: [(Int, EventHandler a)] -> Events a
MakeEventsList list = sortBy f list
	where
		f (i, h1) (j, h2) = i > j
// fetch (Maybe EventHandler a) from an Events value and an integer
fetchEvent :: (Events a) Int -> Maybe (EventHandler a)
fetchEvent [(cName, cHandle):t] name
	| cName == name = Just cHandle
	| otherwise		= fetchEvent t name
fetchEvent [] _ = Nothing

// fetch event or identity
getEvent :: (Events a) Int -> EventHandler a
getEvent events name = case fetchEvent events name of
	Just f 	= f
	Nothing = \x _ . x 

class DrawableObject a
where
	getImageOffset :: a State GlobalVisualStyle (Events a) -> ImageOffset
	drawObject :: a State GlobalVisualStyle (Events a) String -> Image State

// draw grid of objects
drawObjects	:: [a] State GlobalVisualStyle (Events a) String -> Image State | DrawableObject a
drawObjects	items state style events param =
	collage positions drawnItems Nothing
	where
		drawnItems = [drawObject item state style events param \\ item <- items]
		positions = convertItemsIntoImageOffset items
		convertItemsIntoImageOffset [item:tail] = [getImageOffset item state style events:convertItemsIntoImageOffset tail]
		convertItemsIntoImageOffset [] = []
