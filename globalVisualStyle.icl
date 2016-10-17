implementation module globalVisualStyle

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

:: GlobalVisualStyle 		=
	{ vEBackgroundColor	:: SVGColor
	, vEWidth			:: Span
	, vEHeight			:: Span
	, vEMargin			:: Span
	}

defaultGlobalVisualStyle :: GlobalVisualStyle
defaultGlobalVisualStyle = 
	{ vEBackgroundColor	= SVGRGB 26 188 156
	, vEWidth			= px 50.0
	, vEHeight			= px 40.0
	, vEMargin			= px 0.5
	}