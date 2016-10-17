implementation module Element

import iTasks

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

:: Train = 
	{ 
		usedElement :: Element,
		name :: String
	}
derive class iTask Section
derive class iTask Point
derive class iTask Position
derive class iTask Orientation
derive class iTask Element
derive class iTask Train

getPos	 :: Element	-> Position
getPos	(Point p) 	= p.pPosition
getPos	(Section s)	= s.sPosition

getLabel :: Element	-> String
getLabel (Point p)	= p.pLabel
getLabel (Section s)= s.sLabel

instance == Position
where
	(==) a b = a.x == b.x && a.y == b.y

// instance < Element
// where
// 	(<) oa ob = (a.y < b.y) || (a.y == b.y && a.x < b.y)
// 	where
// 		a = getPos oa
// 		b = getPos ob

instance == Element
where
	(==) (Point a) (Point b) 		= a.pPosition == b.pPosition
	(==) (Section a) (Section b) 	= a.sPosition == b.sPosition
	(==) _ _ 	= False
