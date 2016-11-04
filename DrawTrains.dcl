definition module DrawTrains

import globalVisualStyle
import Element
import State
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import DrawObject

import StdArray

:: TrainEventHandlerParam :== String
:: TrainEventHandler :== TrainEventHandlerParam -> Train -> Train

:: EventsTrains =
	{ ee_onclickSignal 	:: Maybe TrainEventHandler
	, ee_onclick 		:: Maybe TrainEventHandler
	}
:: DrawTrainCtx	= 
	{ ctTrain		:: Train
	, ctStyle		:: GlobalVisualStyle
	, ctEvents		:: EventsTrains
	}
:: DrawTrainsCtx	= 
	{ ctsTrains	:: [Train]
	, ctsState		:: State
	, ctsStyle		:: GlobalVisualStyle
	, ctsEvents		:: EventsTrains
	}