{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-| ClassyParser instance which generates 'ABNF'
-}
module ABNF.ClassyParser.Gen.ABNF
       ( GenABNF(..)
       , normalizeAlternation
       ) where

import ABNF.Types
import ABNF.Printer
import ABNF.ClassyParser.Classes
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as C
import Data.List ((\\), sort)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

newtype GenABNF a = GenABNF { runGenABNF :: Elements }

instance ApplicativeRepr GenABNF where
    pureR a  = GenABNF (Alternation [])
    app  (GenABNF (Alternation  [Concatenation f])) (GenABNF (Alternation [Concatenation x])) = GenABNF (Alternation [Concatenation (f ++ x)])
    app  (GenABNF (Alternation  [])) (GenABNF (Alternation x)) = GenABNF (Alternation x)
    appL (GenABNF (Alternation  [Concatenation f])) (GenABNF (Alternation [Concatenation x])) = GenABNF (Alternation [Concatenation (f ++ x)])
    appL (GenABNF (Alternation  [])) (GenABNF (Alternation [Concatenation x])) = GenABNF (Alternation [Concatenation x])
    appL (GenABNF (Alternation  [Concatenation f])) (GenABNF (Alternation [])) = GenABNF (Alternation [Concatenation f])
    appL (GenABNF f) (GenABNF x) = GenABNF (Alternation [Concatenation [Repetition Nothing (Group f), Repetition Nothing (Group x)]])
    appR (GenABNF (Alternation  [Concatenation f])) (GenABNF (Alternation [Concatenation x])) = GenABNF (Alternation [Concatenation (f ++ x)])
    appR (GenABNF (Alternation  [])) (GenABNF (Alternation [Concatenation x])) = GenABNF (Alternation [Concatenation x])
    appR (GenABNF (Alternation  [Concatenation f])) (GenABNF (Alternation [])) = GenABNF (Alternation [Concatenation f])
    appR (GenABNF f) (GenABNF x) = GenABNF (Alternation [Concatenation [Repetition Nothing (Group f), Repetition Nothing (Group x)]])

instance Pair GenABNF where
    prj1  = GenABNF (Alternation [])
    prj2  = GenABNF (Alternation [])
    pair  = GenABNF (Alternation [])

instance ClassyParser GenABNF where
    pCharVal t
        | t == "\"" = GenABNF (Alternation [Concatenation [Repetition Nothing (NV (HexVal (NumSpec [HexDigit '2', HexDigit '2'] Nothing)))]])
        | otherwise = GenABNF (Alternation [Concatenation [Repetition Nothing (CV (CharVal (Text.decodeUtf8 t)))]])
    pEnumerate keyVals =
        GenABNF (Alternation [Concatenation [Repetition Nothing (CV (CharVal (Text.decodeUtf8 key)))] | (key, _) <- keyVals])
    pHexChar hc = GenABNF (Alternation [Concatenation [Repetition Nothing (NV (HexVal (NumSpec (map HexDigit hc) Nothing)))]])
    pDigit = GenABNF (Alternation [Concatenation [Repetition Nothing (RN (RuleName "DIGIT"))]])
    pMany e = GenABNF (Alternation [Concatenation [Repetition (Just (Variable Nothing Nothing)) (Group (runGenABNF e))]])
    pMany1 e = GenABNF (Alternation [Concatenation [Repetition (Just (Variable (Just 1) Nothing)) (Group (runGenABNF e))]])
    pOptional e = GenABNF (Alternation [Concatenation [Repetition Nothing (Option (runGenABNF e))]])
    pTakeWhile1 (NotInClass notInClass) = GenABNF (Alternation [Concatenation [Repetition (Just (Variable (Just 1) Nothing)) (Group (Alternation [Concatenation [Repetition Nothing (if c == '\x22' then (NV (HexVal (NumSpec [HexDigit '2', HexDigit '2'] Nothing))) else (CV (CharVal (Text.singleton c))))] | c <- ['\0'..'\127'] \\ notInClass]))]])
    digitsToInt p = GenABNF (runGenABNF p)



type NormalM = Reader RuleMap

normalizeElement :: Element -> NormalM Element
normalizeElement (RN ruleName) =
    do rulemap <- ask
       case Map.lookup ruleName rulemap of
         Nothing -> fail $ "Invalid rule name: "++ show ruleName
         (Just elements) ->
             do elements' <- normalizeAlternation elements
                return (Group elements')
normalizeElement (Group alternation) =
    do a <- normalizeAlternation alternation
       case a of
         (Alternation [Concatenation [Repetition Nothing (Group a)]]) -> return (Group a)
         x -> return (Group x)
normalizeElement (Option alternation) =
    Option <$> (normalizeAlternation alternation)
normalizeElement x = return x -- FIXME: normalize CV, NV, and PV

normalizeRepetition :: Repetition -> NormalM Repetition
normalizeRepetition (Repetition mRepeat element) =
    do let mRepeat' =
            case mRepeat of
              (Just (Specific 1)) -> Nothing
--              (Just 1) -> Nothing -- FIXME: normalize other Repeat ?
              x -> x
       element' <- normalizeElement element
       return (Repetition mRepeat' element')

-- should normalize "HTTP" "/" to "HTTP/"
normalizeConcatenation :: Concatenation -> NormalM Concatenation
normalizeConcatenation (Concatenation rs) =
    do rs' <- mapM normalizeRepetition rs
       return $ Concatenation (flattenReps rs')

flattenReps :: [Repetition] -> [Repetition]
flattenReps [] = []
flattenReps (Repetition Nothing (Group (Alternation [Concatenation reps])) : rest) = flattenReps reps ++ flattenReps rest
flattenReps (r:rest) = r : flattenReps rest

normalizeAlternation:: Alternation -> NormalM Alternation
normalizeAlternation (Alternation concatenations) =
    do cs <- mapM normalizeConcatenation concatenations
       return $ Alternation $ sort cs
