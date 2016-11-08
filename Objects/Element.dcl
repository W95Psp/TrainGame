definition module Objects.Element

import iTasks
import Objects.Train
import Objects.DrawObject

:: Section =
	{ sLabel 		:: String
	, sPosition 	:: Position
	, sLeftSignal 	:: Maybe Bool
	, sRightSignal 	:: Maybe Bool
	}
:: Point =
	{ pLabel 		:: String
	, pPosition 	:: Position
	, pOrientation 	:: Orientation
	, pIsUp			:: Bool
	}
:: Position = {x :: Int, y :: Int}
:: Orientation = NE | NW
:: Element = Section Section | Point Point

derive class iTask Section
derive class iTask Point
derive class iTask Position
derive class iTask Orientation
derive class iTask Element

getPos	 :: Element	-> Position
getLabel :: Element	-> String

instance == Position
instance == Element

instance DrawableObject Element

findElementFromPosition :: Position TrainDirection [Element] -> Maybe Element