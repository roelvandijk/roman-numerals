{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-| Parsing and pretty printing of Roman numerals.

This module provides functions for parsing and pretty printing Roman
numerals. Because the notation of Roman numerals has varied through
the centuries this package allows for some customisation using a
configuration that is passed to the conversion functions. Exceptions
are dealt with by wrapping the results of conversions in the error
monad.

Example:

@
  \> 'toRoman' 1729 &#x2237; Either String String
  Right \"MDCCXXIX\"
@

@
  \> 'fromRoman' \"MDCCXXIX\" &#x2237; Either String Integer
  Right 1729
@

@
  \> 'convertTo' 'simpleRoman' 1729 &#x2237; Either String String
  Right \"MDCCXXVIIII\"
@

@
  \> 'fromRoman' \"Bla\" &#x2237; Either String Integer
  Left \"Roman.convertFrom: can't parse\"
@

-}
module Text.Numeral.Roman
  ( -- * Types
    NumeralConfig(..)

    -- * Standard configurations
  , modernRoman
  , simpleRoman

    -- * Pretty printing
  , convertTo
  , unsafeConvertTo
  , toRoman
  , unsafeToRoman

    -- * Parsing
  , StripPrefix
  , convertFrom
  , fromRoman
  , unsafeConvertFrom
  , unsafeFromRoman
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Data.Bool     ( Bool(False, True), otherwise )
import Data.Char     ( String )
import Data.Either   ( either )
import Data.Eq       ( Eq, (==) )
import Data.Function ( ($), id, const )
import Data.Maybe    ( Maybe(Nothing, Just), maybe )
import Data.Monoid   ( Monoid )
import Data.Ord      ( Ord, (>), (<) )
import Data.String   ( IsString, fromString )
import qualified Data.List as L ( null, stripPrefix )
import Control.Monad ( (>>=), (>>), return, fail, liftM2 )
import Prelude       ( Num, (+), (-), Integer, fromInteger, error )
import Text.Show     ( show )

-- base-unicode-symbols
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≥) )
import Data.Monoid.Unicode   ( (∅), (⊕) )

-- bytestring
import qualified Data.ByteString as BS

-- mtl
import Control.Monad.Error ( MonadError, throwError )


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A configuration with which the 'convertTo' and 'convertFrom'
--  functions can be parameterized.
data NumeralConfig s n = NC 
    { -- |The largest value that can be represented using this
      -- configuration.
      ncMax ∷ n
      -- |Symbol to represent the value 0. The Romans did not have a
      -- symbol for zero. If set to Nothing a 'convertFrom' 0 will
      -- throw an error.
    , ncZero ∷ Maybe s
      -- |A table of symbols and their numerical values. The table
      -- must be ordered in descending order of the value of the
      -- symbols. If any symbol is the empty string then 'convertFrom'
      -- will be &#x22a5;. If any symbol in this table is associated
      -- with the value 0 both the 'convertTo' and 'convertFrom'
      -- function will be &#x22a5;.
    , ncTable ∷ [(s, n)]
    }

-------------------------------------------------------------------------------
-- Default tables
-------------------------------------------------------------------------------

-- |Configuration for Roman numerals as they are commonly used
-- today. The value 0 is represented by the empty string. It can be
-- interpreted as not writing down a number. This configuration is
-- limited to the range [1..3999]. Larger numbers can be represented
-- using Roman numerals but you will need notations that are hard or
-- impossible to express using strings.
modernRoman ∷ (IsString s, Ord n, Num n) ⇒ NumeralConfig s n
modernRoman = NC { ncMax      = 3999
                 , ncZero     = Just ""
                 , ncTable    = [ ("M",  1000)
                                , ("CM",  900)
                                , ("D",   500)
                                , ("CD",  400)
                                , ("C",   100)
                                , ("XC",   90)
                                , ("L",    50)
                                , ("XL",   40)
                                , ("X",    10)
                                , ("IX",    9)
                                , ("V",     5)
                                , ("IV",    4)
                                , ("I",     1)
                                ]
                 }

-- |Configuration for Roman numerals that do not use the rule that a
-- lower rank symbol can be placed before a higher rank symbol to
-- denote the difference between them. Thus a numeral like \"IV\" will
-- not be accepted or generated by this configuration.
simpleRoman ∷ (IsString s, Ord n, Num n) ⇒ NumeralConfig s n
simpleRoman = NC { ncMax      = 3999
                 , ncZero     = Just ""
                 , ncTable    = [ ("M", 1000)
                                , ("D",  500)
                                , ("C",  100)
                                , ("L",   50)
                                , ("X",   10)
                                , ("V",    5)
                                , ("I",    1)
                                ]
                 }

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

-- |Converts a number to a Roman numeral according to the given
-- configuration. Numbers which are out of bounds will cause
-- exceptions to be thrown. An exception will also be raised if no
-- representation is possible with the given configuration. If the
-- value of any symbol in the configuration is equal to 0 or a symbol
-- is the empty string this function is undefined.
convertTo ∷ (Monoid s, Ord n, Num n, MonadError String m)
          ⇒ NumeralConfig s n → n → m s
convertTo nc n 
    | n < 0     = errMsg $ "can't represent negative numbers"
    | n > maxN  = errMsg $ "too large (max = " ⊕ (show maxN) ⊕ ")"
    | n ≡ 0     = maybe (errMsg "no symbol for zero")
                        return
                        $ ncZero nc
    | otherwise = go n $ ncTable nc
    where maxN = ncMax nc

          go 0 _  = return (∅)
          go _ [] = errMsg "out of symbols"
          go n tab@(~(sym, val) : ts) 
              | n ≥ val   = liftM2 (⊕) (return sym) $ go (n - val) tab
              | otherwise = go n ts

          errMsg = throwError ∘ ("Roman.convertTo: " ⊕)

-- |Like 'convertTo', but exceptions are promoted to errors.
unsafeConvertTo ∷ (Monoid s, Ord n, Num n) ⇒ NumeralConfig s n → n → s
unsafeConvertTo nc = either error id ∘ convertTo nc

-- |Converts a number to a modern Roman numeral. See 'convertTo' for
--  possible exceptions.
toRoman ∷ (IsString s, Monoid s, Ord n, Num n, MonadError String m) ⇒ n → m s
toRoman = convertTo modernRoman

-- |Like 'toRoman', but exceptions are promoted to errors.
unsafeToRoman ∷ (IsString s, Monoid s, Ord n, Num n) ⇒ n  → s
unsafeToRoman = unsafeConvertTo modernRoman

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- |Class used to overload the input of the parsing functions.
class StripPrefix s where
    null        ∷ s → Bool
    stripPrefix ∷ s → s → Maybe s

instance Eq α ⇒ StripPrefix [α] where
    null        = L.null
    stripPrefix = L.stripPrefix

instance StripPrefix BS.ByteString where
    null = BS.null
    stripPrefix p s = let (h, t) = BS.breakSubstring p s
                      in if null h
                         then Just $ BS.drop (BS.length p) t
                         else Nothing

-- |Parses a string as a Roman numeral according to the given
-- configuration. An exception will be raised if the input is not a
-- valid numeral.
convertFrom ∷ (Monoid s, StripPrefix s, Eq s, Ord n, Num n, MonadError String m)
            ⇒ NumeralConfig s n → s → m n
convertFrom nc s | maybe False (≡ s) (ncZero nc) = return 0
                 | otherwise = do n  ← (go 0 (ncTable nc) s)
                                  s' ← convertTo nc n
                                  if s ≡ s'
                                    then return n
                                    else errMsg "invalid Roman numeral"
    where go n _  s | null s = return n
          go _ [] s          = errMsg "can't parse"
          go n tab@((sym, val) : ts) s = maybe (go n ts s)
                                               (go (n + val) tab)
                                               $ stripPrefix sym s

          errMsg = throwError ∘ ("Roman.convertFrom: " ⊕)

-- |Like 'convertFrom', but exceptions are promoted to errors.
unsafeConvertFrom ∷ (Monoid s, StripPrefix s, Eq s, Ord n, Num n)
                  ⇒ NumeralConfig s n → s → n
unsafeConvertFrom nc = either error id ∘ convertFrom nc

-- |Parses a string as a modern Roman numeral. See 'convertFrom' for
-- possible exceptions.
fromRoman ∷ ( IsString s, Monoid s, StripPrefix s, Eq s
            , Ord n, Num n, MonadError String m
            ) ⇒ s → m n
fromRoman = convertFrom modernRoman

-- |Like 'fromRoman', but exceptions are promoted to errors.
unsafeFromRoman ∷ ( IsString s, Monoid s, StripPrefix s, Eq s
                  , Ord n, Num n
                  ) ⇒ s → n
unsafeFromRoman = unsafeConvertFrom modernRoman

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

prop_printParseIsId ∷ NumeralConfig String Integer → String → Bool
prop_printParseIsId nc xs = either (const True) id
                                   $ do n   ← convertFrom nc xs
                                        xs' ← convertTo   nc n
                                        return $ xs' ≡ xs

prop_parsePrintIsId ∷ NumeralConfig String Integer → Integer → Bool
prop_parsePrintIsId nc n = either (const True) id
                                  $ do xs ← convertTo   nc n
                                       n' ← convertFrom nc xs
                                       return $ n' ≡ n
