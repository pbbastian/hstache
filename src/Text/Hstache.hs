{-# LANGUAGE OverloadedStrings #-}

module Text.Hstache where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Attoparsec.Text           ((.*>), (<*.))
import qualified Data.Attoparsec.Text           as A
import           Data.Char                      (digitToInt)
import qualified Data.Text                      as T

data HstacheToken = HstacheStartDelimiter
                  | HstacheEndDelimiter
                  | HstachePound
                  | HstacheSlash
                  | HstacheCaret
                  | HstacheBang
                  | HstacheGT

data HstacheTag = HstacheVariable T.Text
                | HstacheUnescapedVariable T.Text
                | HstacheSection T.Text HstacheTag
                | HstacheInvertedSection T.Text HstacheTag
                | HstachePartial T.Text
                | HstacheSetDelimiters T.Text T.Text
                | HstacheText T.Text

data DelimiterSet = DelimiterSet T.Text T.Text

hstacheTextP :: StateT DelimiterSet A.Parser HstacheTag
hstacheTextP = do
  DelimiterSet opening _ <- get
  HstacheText <$> T.pack <$> lift (A.manyTill A.anyChar $ A.string opening)

hstacheSetDelimiterP :: StateT DelimiterSet A.Parser HstacheTag
hstacheSetDelimiterP = do
  DelimiterSet opening closing <- get
  opening' <- lift $ opening .*> A.char '=' *> A.takeWhile1 (/= ' ')
  closing' <- lift $ fmap T.pack $ A.char ' ' *> (A.manyTill A.anyChar $ A.string closing)
  put $ DelimiterSet opening' closing'
  return $ HstacheSetDelimiters opening' closing'

-- hstacheTextP (DelimiterSet opening _) = HstacheText <$> T.pack <$> (A.manyTill A.anyChar $ A.string opening)

--hstacheTagP :: (A.Parser a) -> DelimiterSet -> A.Parser a
--hstacheTagP p (DelimiterSet opening closing) = opening .*>

sumP :: StateT Int (A.Parser) Int
sumP = do
  n' <- lift A.digit
  let n = digitToInt n'
  get >>= put . (+n)
  return n
