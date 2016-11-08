definition module State

import Objects.Element
import Objects.Train

// paramTestX and paramTestY are used only for testing purposes, it should be removed
:: State = {elements :: [Element], trains :: [Train], elementSelected :: (Maybe Element), paramTestX :: Real, paramTestY :: Real}
derive class iTask State