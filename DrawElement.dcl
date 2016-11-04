definition module DrawElement

import Element
import State
import globalVisualStyle
import iTasks
import iTasks.API.Extensions.SVG.SVGlet


:: ElementEventHandlerParam :== String
:: ElementEventHandler :== ElementEventHandlerParam -> Element -> Element

:: EventsElements =
	{ ee_onclickSignal 	:: Maybe ElementEventHandler
	, ee_onclick 		:: Maybe ElementEventHandler
	}
:: DrawElementCtx	= 
	{ ceElement		:: Element
	, ceStyle		:: GlobalVisualStyle
	, ceEvents		:: EventsElements
	}
:: DrawElementsCtx	= 
	{ cesElements	:: [Element]
	, cesState		:: State
	, cesStyle		:: GlobalVisualStyle
	, cesEvents		:: EventsElements
	}

drawElement 		:: DrawElementCtx	-> Image State
drawElements 		:: DrawElementsCtx	-> Image State

buildDrawElementsCtx:: [Element]	State	-> DrawElementsCtx
