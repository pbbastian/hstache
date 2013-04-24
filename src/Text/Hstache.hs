{-# LANGUAGE OverloadedStrings #-}

module Text.Hstache where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.Text           ((.*>), (<*.))
import qualified Data.Attoparsec.Text           as A
import qualified Data.Text                      as T

data Event = EventVariable T.Text
           | EventUnescapedVariable T.Text
           | EventBeginSection T.Text
           | EventEndSection T.Text
           | EventBeginInvertedSection T.Text
           | EventPartial T.Text
           | EventDelimiterSet T.Text T.Text
           | EventText T.Text

data HstacheDocument = HstacheVariable T.Text
                     | HstacheUnescapedVariable T.Text
                     | HstacheSection T.Text HstacheDocument
                     | HstacheInvertedSection T.Text HstacheDocument
                     | HstachePartial T.Text
                     | HstacheSetDelimiters T.Text T.Text
                     | HstacheText T.Text

data DelimiterSet = DelimiterSet T.Text T.Text

type EventParser = StateT (T.Text, T.Text) A.Parser Event

tagParser :: EventParser
tagParser = do
  (opening, closing) <- get
  lift $ A.string opening
  result <- A.choice [ unescapedVariableParser1
                     , unescapedVariableParser2
                     , beginSectionParser
                     , endSectionParser
                     , beginInvertedSectionParser
                     , partialParser
                     , delimiterSetParser
                     , variableParser ]
  lift $ A.string closing
  return result

tillText :: T.Text -> A.Parser T.Text
tillText closing = T.pack <$> (A.manyTill A.anyChar $ A.string closing)

variableParser :: EventParser
variableParser = do
  (_, closing) <- get
  EventVariable <$> lift (tillText closing)

unescapedVariableParser1 :: EventParser
unescapedVariableParser1 = do
  (_, closing) <- get
  EventUnescapedVariable <$> lift ("& " .*> tillText closing)

unescapedVariableParser2 :: EventParser
unescapedVariableParser2 =
  EventUnescapedVariable <$> lift (A.char '{' *> A.takeWhile1 (/= '}') <* A.char '}')

beginSectionParser :: EventParser
beginSectionParser = do
  (_, closing) <- get
  EventBeginSection <$> lift (A.char '#' *> tillText closing)

endSectionParser :: EventParser
endSectionParser = do
  (_, closing) <- get
  EventEndSection <$> lift (A.char '/' *> tillText closing)

beginInvertedSectionParser :: EventParser
beginInvertedSectionParser = do
  (_, closing) <- get
  EventBeginInvertedSection <$> lift (A.char '^' *> tillText closing)

partialParser :: EventParser
partialParser = do
  (_, closing) <- get
  EventPartial <$> lift ("> " .*> tillText closing)

delimiterSetParser :: EventParser
delimiterSetParser = do
  opening' <- lift $ A.char '=' *> A.takeWhile1 (/= ' ')
  closing' <- lift $ A.space *> A.takeWhile1 (/= '=') <* A.char '='
  put (opening', closing')
  return $ EventDelimiterSet opening' closing'

textParser :: EventParser
textParser = do
  (opening, _) <- get
  EventText <$> lift (tillText opening)
