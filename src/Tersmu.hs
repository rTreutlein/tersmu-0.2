module Tersmu
( module Tersmu
, module ParseText
, module JboParse
, module JboSyntax
, module ParseM
, module JboShow
, module JboProp
, module Logic
, module Bindful
, module Morph
)
where

import ParseText
import JboParse
import JboSyntax hiding (Term, Connective)
import ParseM
import JboShow
import JboProp
import Logic
import Bindful
import Morph
import Control.Monad.State
import Control.Monad.Identity


lojbanToJboText :: String -> IO (Maybe JboText)
lojbanToJboText x = case morph x of
    Left errpos -> putStrLn "Morphology error" >> return Nothing
    Right text ->  case parseText text of
        Left err -> putStrLn "Parse error" >> return Nothing
        Right text -> do
            let txt   = mapStateT (return.runIdentity) $ evalText text :: ParseStateT IO JboText
            text1 <- evalParseStateT txt
            return $ Just text1

