definition module globalVisualStyle

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

:: GlobalVisualStyle 		=
	{ vEBackgroundColor	:: SVGColor
	, vEWidth			:: Span
	, vEHeight			:: Span
	, vEMargin			:: Span
	}

defaultGlobalVisualStyle :: GlobalVisualStyle