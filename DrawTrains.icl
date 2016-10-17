definition module DrawElement

import Element
import State
import iTasks
import iTasks.API.Extensions.SVG.SVGlet

import StdArray

:: TrainEventHandlerParam :== String
:: TrainEventHandler :== TrainEventHandlerParam -> Element -> Element

:: EventsTrains =
	{ ee_onclickSignal 	:: Maybe ElementEventHandler
	, ee_onclick 		:: Maybe ElementEventHandler
	}
:: DrawElementCtx	= 
	{ tElement		:: Element
	, tStyle		:: globalVisualStyle
	, tEvents		:: EventsElements
	}
:: DrawElementsCtx	= 
	{ tsElements	:: [Element]
	, tsState		:: State
	, tsStyle		:: globalVisualStyle
	, tsEvents		:: EventsElements
	}