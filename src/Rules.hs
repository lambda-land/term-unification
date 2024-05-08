module Rules
    ( Rule (..), Rules
    ) where

import Common

data Rule = Rule
  { conclusion :: !Judgement
  , premises   :: ![Judgement]
  }

type Rules = [( String -- name
              , Rule )]
