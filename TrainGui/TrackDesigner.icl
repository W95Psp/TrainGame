implementation module TrainGui.TrackDesigner

import State
import Objects.Train
import Objects.Element
import ImageTask
import iTasks

:: DesignerElemView = {elementName :: String, devPosition :: Position}
derive class iTask DesignerElemView
trackDesigner :: (Shared State) Bool -> Task State
trackDesigner state showTrains = get state >>- \startState . 
	(
		watch state
		-||
		(
			(
				(
					viewInformation "Help" [] "You role is to design the railway. You can select an element (section or point) by clicking on it. Then, you'll be able to edit its label, position and signals. You also can click on section's top left and right upper corners to toggle signals."
					 <<@ ForceLayout <<@ AfterLayout (tweakUI (fixedWidth 200))
				)
				-||
				(
					manageElement startState
					<<@ AfterLayout (tweakUI fillWidth)
				)
				<<@ ArrangeHorizontal
				<<@ (Title "Designer mode")
			)
		)
		-|| displayImage
	) >>* [
		OnAction (Action "Delete selected" [ActionIcon "delete"]) (
				ifValue (\s . isJust s.elementSelected) (\_ .
					upd (\s . {s & elements = filter (\x . not ((Just x)==s.elementSelected)) s.elements, elementSelected = Nothing}) state
						>>| trackDesigner state showTrains
				)
			),
		OnAction (Action "Add section" [ActionIcon "add"]) (addNew (Section {
			  sLabel 		= "New section"
			, sPosition 	= findEmptyLocation startState.elements False
			, sLeftSignal 	= Nothing
			, sRightSignal 	= Nothing 
			})),
		OnAction (Action "Add point" [ActionIcon "add"]) (addNew (Point {
			  pLabel 		= "New point"
			, pPosition 	= findEmptyLocation startState.elements True
			, pOrientation 	= NW
			, pIsUp 	= False 
			})),
		OnValue (ifValue (\ns . different startState ns) (\_ . trackDesigner state showTrains)),
		case showTrains of
			True = OnAction (Action "Hide trains" [ActionIcon "no"]) (always (trackDesigner state False))
			False= OnAction (Action "Show trains" [ActionIcon "yes"]) (always (trackDesigner state True))
	]
	where
		addNew w = always (upd (\s . {s & elements = [w:s.elements], elementSelected = Just w}) state >>| trackDesigner state showTrains)
		// check if elementSelected was updated
		different stateBefore stateNow = not (stateBefore.elementSelected == stateNow.elementSelected)
		manageElement s = case s.elementSelected of
			Just (Section section) = updateSharedInformation "Manage selection" [
						UpdateWith (\s . case (filter (\x . (Just x)==s.elementSelected) s.elements) of
								[Section sect:_] = sect
								_ = { sLabel 		= ""
									, sPosition 	= {x = -1, y = -1}
									, sLeftSignal 	= Nothing
									, sRightSignal 	= Nothing 
									}
							)
							(\s n . specialUpdate s (Section n))
					] state ||- return Void
			Just (Point point) = updateSharedInformation "Manage selection" [
						UpdateWith (\s . case (filter (\x . (Just x)==s.elementSelected) s.elements) of
								[Point pt:_] = pt
								_ = { pLabel 		= ""
									, pPosition 	= {x = -1, y = -1}
									, pOrientation 	= NW
									, pIsUp			= True
									}
							)
							(\s n . specialUpdate s (Point n))
					] state ||- return Void
			Nothing = return Void
		specialUpdate s n = if ((getLabel n)=="" && (getPos n)=={x = -1, y = -1})
								s
								{s & elementSelected = (Just n) , elements = copy s.elements s.elementSelected n}
					where
						copy [a:b] elemSelected n = if ((Just a)==elemSelected) [n:b] [a:copy b elemSelected n]
						copy [] _ _ = []
		displayImage :: Task State
		displayImage = (imageTask state (MakeEventsList [eSigLeft, eSigRight, (ONCLICK_ELEMENT, eClickSection), (ONCLICK_POINT, eClickSection)]) [] "" "" showTrains)
		findEmptyLocation elems forPoint = subSearch 0 0
				where
					subSearch :: Int Int -> Position
					subSearch n m
						| n <= (m+1) = if (sub m n) {x = m, y = n} (subSearch (n+1) m)
						| n <= (m+m+2) = if (sub (n-(m+2)) (m+2)) {x = n-(m+2), y = (m+2)} (subSearch (n+1) m)
						| otherwise = subSearch 0 (m+1)
					sub :: Int Int -> Bool
					sub x y = case find {x = x, y = y} of
							Nothing = case find {x = x, y = y-1} of
								Just (Point _) = False
								_			   = (not forPoint) || (isNothing (find {x = x, y = y+1}))
							_		= False
					find pos = findSub pos elems
						where
							findSub pos [a:b] = if ((getPos a)==pos) (Just a) (findSub pos b)
							findSub _ [] = Nothing
		eSigLeft = (ONCLICK_LEFT_SIGNAL, \e _ . case e of
				Point p = Point p
				Section s = Section {s & sLeftSignal = case s.sLeftSignal of
								Just _ = Nothing
								_ = Just True
							})
		eSigRight = (ONCLICK_RIGHT_SIGNAL, \e _ . case e of
				Point p = Point p
				Section s = Section {s & sRightSignal = case s.sRightSignal of
								Just _ = Nothing
								_ = Just True
							})
		// allow to change Point orientation
		eClickSection e _ = case e of
			Point p   = Point {p & pOrientation = case p.pOrientation of
						NW = NE
						NE = NW
					}
			Section e = Section e
