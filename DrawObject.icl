implementation module DrawObject

import StdOrdList
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import State
import globalVisualStyle

:: EventHandlerParam 	:== String
:: EventHandler a		:== a EventHandlerParam -> a 
:: Events a				:== [(Int, EventHandler a)]
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
	drawObject :: a State GlobalVisualStyle (Events a) -> Image State

DrawObjects	:: [a] State GlobalVisualStyle (Events a) -> Image State | DrawableObject a
DrawObjects	items state style events =
	collage positions drawnItems (Just background)
	where
		drawnItems = [drawObject item state style events \\ item <- items]
		positions = convertItemsIntoImageOffset items
		convertItemsIntoImageOffset [item:tail] = [getImageOffset item state style events:convertItemsIntoImageOffset tail]
		convertItemsIntoImageOffset [] = []
		background = empty (px 600.0) (px 600.0)