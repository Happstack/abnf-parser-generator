{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module HttpParser where

import Classes

import ABNF.Types                (Rule(..), RuleName(..), RuleMap(..), Alternation(..), ruleMap)
import ABNF.Parser               (abnf,abnfRule)
import ABNF.Printer              (ppElements, ppRuleList)
import ABNF.CoreRules            (core_ruleList)
import Control.Applicative       (Applicative(pure), (<$>))
import Control.Monad.Reader      (Reader(..), runReader)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Text as AT
import Data.Text                 (Text)
import qualified Data.Map        as Map
import Data.Maybe                (fromJust)
import Data.Monoid               ((<>))
import Language.Haskell.TH.Lift  (deriveLift)
import GenAttoparsec             (GenAttoparsec(..))
import GenABNF                   (GenABNF(..), normalizeAlternation)

------------------------------------------------------------------------------
-- Abstract HTTP Parser
------------------------------------------------------------------------------

method_rule =
    [abnfRule|Method = "OPTIONS"                ; Section 9.2
                      | "GET"                    ; Section 9.3
                      | "HEAD"                   ; Section 9.4
                      | "POST"                   ; Section 9.5
                      | "PUT"                    ; Section 9.6
                      | "DELETE"                 ; Section 9.7
                      | "TRACE"                  ; Section 9.8
                      | "CONNECT"                ; Section 9.9
;                       | extension-method
|]

data Method
    = OPTIONS
    | GET
    | HEAD
    | POST
    | PUT
    | DELETE
    | TRACE
    | CONNECT
      deriving (Eq, Ord, Read, Show)
$(deriveLift ''Method)

method_parser :: ABNFParser repr => repr Method
method_parser =
    pEnumerate [ ("OPTIONS", OPTIONS)
               , ("GET"    , GET)
               , ("HEAD"   , HEAD)
               , ("POST"   , POST)
               , ("PUT"    , PUT)
               , ("DELETE" , DELETE)
               , ("TRACE"  , TRACE)
               , ("CONNECT", CONNECT)
               ]

-- has intentional typo
method_parser_2 :: ABNFParser repr => repr Method
method_parser_2 =
    pEnumerate [ ("OPTIONS", OPTIONS)
               , ("GET"    , GET)
               , ("HEAD"   , HEAD)
               , ("POST"   , POST)
               , ("PUT"    , PUT)
               , ("DELETE" , DELETE)
               , ("TRACE"  , TRACE)
               , ("CONECT" , CONNECT)
               ]

sp_rule :: Rule
sp_rule = [abnfRule|SP             =  %x20|]

sp_parser :: ABNFParser repr => repr Char
sp_parser = pHexChar "20"

sp_parser_2 :: ABNFParser repr => repr Char
sp_parser_2 = pHexChar "21"

uri_rule :: Rule
uri_rule = [abnfRule|URI = / |]

uri_parser :: ABNFParser repr => repr Text
uri_parser = pCharVal "/"

httpVersion_rule :: Rule
httpVersion_rule =
    [abnfRule|HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT |]

httpVersion_parser :: (ABNFParser repr) => repr ([Char], [Char])
httpVersion_parser =
    (pCharVal "HTTP")  `appR` (pCharVal "/") `appR` (pair `app` (pMany1 pDigit) `appL` pCharVal "." `app` (pMany1 pDigit))

crlf_parser :: (ABNFParser repr) => repr ()
crlf_parser = pHexChar "0D" `appR` pHexChar "0A" `appR` pureR ()

{-
       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
-}

separators = "()<>@,;:\\\"/[]?={} \t"
ctl = '\127':['\0'..'\31']

token_parser :: (ABNFParser repr) => repr Text
token_parser =
    pTakeWhile1 (NotInClass (separators++ctl))

fieldName_rule :: Rule
fieldName_rule = [abnfRule|field-name     = token|]

fieldName_parser :: (ABNFParser repr) => repr Text
fieldName_parser = token_parser

fieldValue_parser :: (ABNFParser repr) => repr Text
fieldValue_parser =
    pTakeWhile1 (NotInClass ctl)

messageHeader_Rule :: Rule
messageHeader_Rule = [abnfRule|message-header = field-name ":" [ field-value ] |]

messageHeader_parser :: (ABNFParser repr) => repr (Text, Maybe Text)
messageHeader_parser =
    pair `app` fieldName_parser `appL` pCharVal ":" `app` (pOptional fieldValue_parser)

messageHeaders_parser :: (ABNFParser repr) => repr [(Text, Maybe Text)]
messageHeaders_parser =
    pMany (messageHeader_parser  `appL` crlf_parser)


requestLine_rule = [abnfRule|Request-Line   = Method SP Request-URI SP HTTP-Version CRLF|]

requestLine =
    [abnf|
Request-Line   = Method SP Request-URI SP HTTP-Version CRLF

HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT

Method         = "OPTIONS"                ; Section 9.2
               | "GET"                    ; Section 9.3
               | "HEAD"                   ; Section 9.4
               | "POST"                   ; Section 9.5
               | "PUT"                    ; Section 9.6
               | "DELETE"                 ; Section 9.7
               | "TRACE"                  ; Section 9.8
               | "CONNECT"                ; Section 9.9
CRLF = CR LF
         |]

{-
Request       = Request-Line              ; Section 5.1
                  *(( general-header        ; Section 4.5
                    | request-header         ; Section 5.3
                    | entity-header ) CRLF)

-}
request_rule =
    [abnf|
Request       = Request-Line              ; Section 5.1
                *(message-header CRLF)
                CRLF

Request-Line   = Method SP Request-URI SP HTTP-Version CRLF

Request-URI    = "/"

HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT

Method         = "OPTIONS"                ; Section 9.2
               | "GET"                    ; Section 9.3
               | "HEAD"                   ; Section 9.4
               | "POST"                   ; Section 9.5
               | "PUT"                    ; Section 9.6
               | "DELETE"                 ; Section 9.7
               | "TRACE"                  ; Section 9.8
               | "CONNECT"                ; Section 9.9
CRLF = CR LF

message-header = field-name ":" [ field-value ]

field-name     = token

token = 1*("!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / "A" / "B" / "C" / "D" / "E" / "F" / "G" / "H" / "I" / "J" / "K" / "L" / "M" / "N" / "O" / "P" / "Q" / "R" / "S" / "T" / "U" / "V" / "W" / "X" / "Y" / "Z" / "^" / "_" / "`" / "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" / "k" / "l" / "m" / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" / "v" / "w" / "x" / "y" / "z" / "|" / "~")

field-value = 1*(" " / "!" / %x22 / "#" / "$" / "%" / "&" / "'" / "(" / ")" / "*" / "+" / "," / "-" / "." / "/" / "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / ":" / ";" / "<" / "=" / ">" / "?" / "@" / "A" / "B" / "C" / "D" / "E" / "F" / "G" / "H" / "I" / "J" / "K" / "L" / "M" / "N" / "O" / "P" / "Q" / "R" / "S" / "T" / "U" / "V" / "W" / "X" / "Y" / "Z" / "[" / "\" / "]" / "^" / "_" / "`" / "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" / "k" / "l" / "m" / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" / "v" / "w" / "x" / "y" / "z" / "{" / "|" / "}" / "~")

         |]


data Request = Request
    { method  :: Method
    , uri     :: Text
    , version :: ([Char], [Char])
    , headers :: [(Text, Maybe Text)]
    }
    deriving Show
$(deriveLift ''Request)

class RequestC repr where
    request :: repr (Method -> Text -> ([Char], [Char]) -> [(Text, Maybe Text)] -> Request)

instance RequestC GenAttoparsec where
    request = GA [| pure Request |]

instance RequestC GenABNF where
    request = GenABNF (Alternation [])

request_parser :: (ABNFParser repr, RequestC repr) => repr Request
request_parser =
    request `app` method_parser         `appL` sp_parser
            `app` uri_parser            `appL` sp_parser
            `app` httpVersion_parser    `appL` crlf_parser
            `app` messageHeaders_parser `appL` crlf_parser


testGen =
    do {- print $ ppElements $ runGenABNF $  pCharVal "foo"
       print $ ppElements $ runGenABNF $ method_parser
       print $ ppElements $ runGenABNF $ uri_parser
       print $ ppElements $ runGenABNF $ httpVersion_parser
       print $ ppElements $ runGenABNF $ messageHeaders_parser -}
       print $ ppElements $ runGenABNF $ request_parser

-- check :: RuleMap -> Rule -> p -> IO ()
check :: RuleMap -> Rule -> GenABNF a -> IO ()
check rulemap rule parser =
    do let (Rule _ alternation) = rule
           method_n   = runReader (normalizeAlternation alternation)         rulemap
           method_gen = runReader (normalizeAlternation (runGenABNF parser)) rulemap
       putStrLn "normalized rules from spec"
       print $ ppElements $ method_n
--       print $ method_n
       putStrLn "normalized generated rules"
       print $ ppElements $ method_gen
       print (method_n == method_gen)

-- test_method = check Map.empty method_rule method_parser
{-
test_request =
    check (ruleMap (core_ruleList <> request_rule))
          (Rule (RuleName "Request") (fromJust $ Map.lookup (RuleName "Request") (ruleMap request_rule)))
          request_parser
-}
--    check requestLine requestLine_rule


test_rule rulelist rulename parser =
    case Map.lookup (RuleName rulename) (ruleMap rulelist) of
      Nothing -> error $ "rulename " ++ show rulename ++ "not found."
      (Just elements) ->
          check (ruleMap (rulelist <> core_ruleList))
                (Rule (RuleName rulename) elements)
                parser

test_method         = test_rule request_rule "Method"         method_parser
test_request_uri    = test_rule request_rule "Request-URI"    uri_parser
test_httpVersion    = test_rule request_rule "HTTP-Version"   httpVersion_parser
test_fieldName      = test_rule request_rule "field-name"     fieldName_parser
test_fieldValue     = test_rule request_rule "field-value"    fieldValue_parser
test_messageHeader  = test_rule request_rule "message-header" messageHeader_parser
test_request        = test_rule request_rule "Request"        request_parser