definition module State

import Element
import Train

:: State = {elements :: [Element], trains :: [Train], elementSelected :: (Maybe Element), paramTestX :: Real, paramTestY :: Real}
derive class iTask State