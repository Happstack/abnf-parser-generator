{-# LANGUAGE GADTs, NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes
, OverloadedStrings #-}
{-| Core classes used by ClassyParsers.
-}
module ABNF.ClassyParser.Classes where

import Data.ByteString.Char8      (ByteString, pack, unpack)
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift   (deriveLift)

------------------------------------------------------------------------------
-- ApplicativeRepr Class
------------------------------------------------------------------------------

-- | 'Applicative' class representation
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

-- | tuple '(,)' representation
class ApplicativeRepr repr => Pair repr where
    pair :: repr (a -> b -> (a, b))
    prj1 :: repr ((a, b) -> a)
    prj2 :: repr ((a, b) -> b)

------------------------------------------------------------------------------
-- ABNF Parser class
------------------------------------------------------------------------------

-- | filter predicates for use with 'pTakeWhile1', etc.
data Predicate =
    NotInClass String
    deriving (Eq, Ord, Read, Show)
$(deriveLift ''Predicate)


-- | A simple set of parsing combinators. This class needs to be
-- implement for each 'repr' that you wish to work with.
class (Pair repr, ApplicativeRepr repr) => ClassyParser repr where
    pCharVal    :: ByteString -> repr ByteString
    pHexChar    :: [Char] -> repr Char
--    pAnyChar    :: repr Char
    pEnumerate  :: (Lift a) => [(ByteString, a)] -> repr a
    pDigit      :: repr Char
    pTakeWhile1 :: Predicate -> repr ByteString
    pMany       :: repr a -> repr [a]
    pMany1      :: repr a -> repr [a]
    pOptional   :: repr a -> repr (Maybe a)
    digitsToInt :: repr [Char] -> repr Int
