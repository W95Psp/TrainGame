definition module DrawElement

import Element
import State
import globalVisualStyle
import iTasks
import iTasks.API.Extensions.SVG.SVGlet

:: EventHandlerParam :== String
:: EventHandler a :== ElementEventHandlerParam -> a -> a

:: ElementVisualStyle 		=
	{ vEBackgroundColor	:: SVGColor
	, vEWidth			:: Span
	, vEHeight			:: Span
	, vEMargin			:: Span
	}
:: EventsElements =
	{ ee_onclickSignal 	:: Maybe ElementEventHandler
	, ee_onclick 		:: Maybe ElementEventHandler
	}
:: DrawElementCtx	= 
	{ cElement		:: Element
	, cStyle		:: ElementVisualStyle
	, cEvents		:: EventsElements
	}
:: DrawElementsCtx	= 
	{ csElements	:: [Element]
	, csState		:: State
	, csStyle		:: ElementVisualStyle
	, csEvents		:: EventsElements
	}

drawElement 		:: DrawElementCtx	-> Image State
drawElements 		:: DrawElementsCtx	-> Image State

buildDrawElementsCtx:: [Element]	State	-> DrawElementsCtx
