{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module HttpTypesAttoparsec where

import ABNF.ClassyParser.Gen.Attoparsec
import HttpTypesParser
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Data.ByteString (ByteString)


pRequest :: Parser Request
pRequest = $(runGenAttoparsec request_parser)

simpleRequest :: ByteString
simpleRequest =
    "GET / HTTP/1.1\r\nHost: localhost\r\nAccept: */*\r\n\r\n"


test = parseOnly pRequest simpleRequest