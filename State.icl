implementation module State

import Element
import Train

:: State = {elements :: [Element], trains :: [Train], elementSelected :: (Maybe Element), stateTime :: Int}
derive class iTask State