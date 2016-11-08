definition module State

import Objects.Element
import Objects.Train

:: State = {elements :: [Element], trains :: [Train], elementSelected :: (Maybe Element), paramTestX :: Real, paramTestY :: Real}
derive class iTask State