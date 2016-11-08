implementation module TrainGui.TrackController

import State
import Objects.Train
import Objects.Element
import iTasks
import ImageTask

:: ControllerElemView = {backtrackItem :: Hidden Element, backtrackType :: Hidden Int, itemLabel :: Display String, itemState :: Bool}
derive class iTask ControllerElemView
trackController :: (Shared State) Bool -> Task State
trackController state showTrains	= 
	(
		(
			(
				(viewSharedInformation "Track Controller" [
								ViewWith  (\s . getAllControls s.elements)// (\s list . {s & elements = s.elements})
							] state
				)
				-||
				(
					viewInformation "Help" [] "You role is to control the railway traffic. To do so, you can click on the signals on the map to set them to green or red. You can also switch section by clicking on them."
					
				)
			) <<@ ForceLayout <<@ AfterLayout (tweakUI (fixedWidth 250))
		)
		-||
			displayImage showTrains <<@ ArrangeHorizontal
	)
	>>* [
		case showTrains of
			True = OnAction (Action "Hide trains" []) (always (trackController state False))
			False= OnAction (Action "Show trains" []) (always (trackController state True))
	]
	where
		getAllControls [a:b] = case a of
			Section s = 
				(if (isJust s.sRightSignal)
					[{backtrackItem = Hidden a, backtrackType = Hidden 0, itemLabel = Display (s.sLabel +++ "_rightSignal"),
						itemState = fromJust s.sRightSignal}] []) ++ 
				(if (isJust s.sLeftSignal)
					[{backtrackItem = Hidden a, backtrackType = Hidden 0, itemLabel = Display (s.sLabel +++ "leftSignal"),
						itemState = fromJust s.sLeftSignal}] []) ++ getAllControls b
			Point p = [
					{backtrackItem = Hidden a, backtrackType = Hidden 0, itemLabel = Display (p.pLabel), itemState = p.pIsUp}:
						getAllControls b
					]
		getAllControls [] = []
		displayImage showTrains = (
				imageTask state (MakeEventsList [eSigLeft, eSigRight, (ONCLICK_POINT, eClickSection)])
				[] "" "" showTrains
			)
		eSigLeft = (ONCLICK_LEFT_SIGNAL, \e _ . case e of
				Point p = Point p
				Section s = Section {s & sLeftSignal = case s.sLeftSignal of
								Just x = Just (not x)
								_ = Nothing
							})
		eSigRight = (ONCLICK_RIGHT_SIGNAL, \e _ . case e of
				Point p = Point p
				Section s = Section {s & sRightSignal = case s.sRightSignal of
								Just x = Just (not x)
								_ = Nothing
							})
		eClickSection e _ = case e of
			Point p   = Point {p & pIsUp = not p.pIsUp}
			Section e = Section e
