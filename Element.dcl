definition module Element

import iTasks
import DrawObject

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
	}
:: Position = {x :: Int, y :: Int}
:: Orientation = NE | SE | SW | NW
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