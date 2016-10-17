definition module State

import Element
:: State = {elements :: [Element], trains :: [Train], elementSelected :: (Maybe Element)}
derive class iTask State