{-# LANGUAGE GADTs, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes
, OverloadedStrings #-}
module Classes where

import ABNF.Types as ABNF
import ABNF.Parser (abnf,abnfRule)
import ABNF.Printer
import Control.Applicative (Applicative(pure, (<*>),(<*), (*>)))
import Control.Monad.Reader (Reader, ask, runReader)
import Data.ByteString.Char8
import Data.Char (chr)
import Data.List ((\\), sort)
-- import Data.Text (Text, pack, unpack)
import qualified Data.Map as Map
import Language.Haskell.TH        (Exp, ExpQ, Q, appE, varE)
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift   (deriveLift)

------------------------------------------------------------------------------
-- ApplicativeRepr Class
------------------------------------------------------------------------------

class ApplicativeRepr repr where
    pureR :: Lift a => a -> repr a
    app  :: repr (a -> b) -> repr a -> repr b
    appL :: repr a -> repr b -> repr a
    appR :: repr a -> repr b -> repr b


infixl 4 `app`
infixl 4 `appL`
infixl 4 `appR`

------------------------------------------------------------------------------
-- Pair Class (tuple)
------------------------------------------------------------------------------

class ApplicativeRepr repr => Pair repr where
    pair :: repr (a -> b -> (a, b))
    prj1 :: repr ((a, b) -> a)
    prj2 :: repr ((a, b) -> b)

------------------------------------------------------------------------------
-- ABNF Parser class
------------------------------------------------------------------------------

instance Lift ByteString where
    lift t =
        let s = unpack t
        in [| pack s |]

data Predicate =
    NotInClass String
    deriving (Eq, Ord, Read, Show)
$(deriveLift ''Predicate)

class (Pair repr, ApplicativeRepr repr) => ABNFParser repr where
    pCharVal    :: ByteString -> repr ByteString
    pHexChar    :: [Char] -> repr Char
--    pAnyChar    :: repr Char
    pEnumerate  :: (Lift a) => [(ByteString, a)] -> repr a
    pDigit      :: repr Char
    pTakeWhile1 :: Predicate -> repr ByteString
    pMany       :: (Lift a) => repr a -> repr [a]
    pMany1      :: (Lift a) => repr a -> repr [a]
    pOptional   :: (Lift a) => repr a -> repr (Maybe a)
    digitsToInt :: repr [Char] -> repr Int

