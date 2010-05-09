{-# LANGUAGE FlexibleContexts
           , NoImplicitPrelude
           , OverloadedStrings
           , UnicodeSyntax
  #-}

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
    NumeralConfig
  , mkNumConfig

    -- * Pretty printing
  , convertTo

    -- * Parsing
  , StripPrefix(..)
  , convertFrom

    -- * Default Configurations
  , modernRoman
  , simpleRoman

    -- * Utility
  , toRoman
  , fromRoman
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool, otherwise )
import Data.Char     ( String )
import Data.Eq       ( Eq )
import Data.Function ( ($), flip, on )
import Data.List     ( sortBy, null, stripPrefix )
import Data.Maybe    ( Maybe(Nothing, Just), maybe )
import Data.Monoid   ( Monoid )
import Data.Ord      ( Ord, compare )
import Data.String   ( IsString, fromString )
import Data.Tuple    ( snd )
import Control.Monad ( (>>=), (>>), return, fail )
import Prelude       ( Num, (+), (-), fromInteger, error )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )
import Data.Ord.Unicode      ( (≥), (≤) )
import Data.Monoid.Unicode   ( (∅), (⊕) )

-- from bytestring:
import qualified Data.ByteString as BS

-- from monads-fd:
import Control.Monad.Error ( MonadError, throwError )


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A configuration with which the 'convertTo' and 'convertFrom'
--  functions can be parameterized.
data NumeralConfig s n = NC
    { -- |Symbol to represent the value 0.
      ncZero ∷ s
      -- |A table of symbols and their numerical values. The table
      -- must be ordered in descending order of the value of the
      -- symbols. If any symbol is the empty string then 'convertFrom'
      -- will be &#x22a5;. If any symbol in this table is associated
      -- with the value 0 both the 'convertTo' and 'convertFrom'
      -- function will be &#x22a5;.
    , ncTable ∷ [(s, n)]
    }

-- |Smart constructor for a 'NumeralConfig'.
mkNumConfig ∷ (Ord n, Num n) 
            ⇒ s -- ^Symbol for zero
            → s -- ^Symbol for one
            → [(s, n)] -- ^ Symbol-value table.
            → NumeralConfig s n
mkNumConfig z o tab = 
    NC { ncZero  = z
       , ncTable = sortBy (flip compare `on` snd) ((o, 1) : tab)
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
convertTo ∷ Monoid s ⇒ Ord n ⇒ Num n ⇒ NumeralConfig s n → n → s
convertTo nc n
    | n ≡ 0     = ncZero nc
    | otherwise = go n $ ncTable nc
    where go _ [] = error "Roman.convertTo: out of symbols (BUG)"
          go i tab@(~(sym, val) : ts)
              | i ≤ 0     = (∅)
              | i ≥ val   = sym ⊕ go (i - val) tab
              | otherwise = go i ts


-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- |Class used to overload the input of the parsing functions.
class StripPrefix s where
    spNull        ∷ s → Bool
    spStripPrefix ∷ s → s → Maybe s

instance Eq α ⇒ StripPrefix [α] where
    spNull        = null
    spStripPrefix = stripPrefix

instance StripPrefix BS.ByteString where
    spNull = BS.null
    spStripPrefix p s = let (h, t) = BS.breakSubstring p s
                        in if spNull h
                           then Just $ BS.drop (BS.length p) t
                           else Nothing

-- |Parses a string as a Roman numeral according to the given
-- configuration. An exception will be thrown if the input is not a
-- valid numeral.
convertFrom ∷ (Monoid s, StripPrefix s, Eq s, Ord n, Num n, MonadError String m)
            ⇒ NumeralConfig s n → s → m n
convertFrom nc s | ncZero nc ≡ s = return 0
                 | otherwise = do n  ← (go 0 (ncTable nc) s)
                                  if s ≡ convertTo nc n
                                    then return n
                                    else errMsg "invalid Roman numeral"
    where go n _  x | spNull x = return n
          go _ [] _            = errMsg "can't parse"
          go n tab@((sym, val) : ts) x = maybe (go n ts x)
                                               (go (n + val) tab)
                                               $ spStripPrefix sym x
          errMsg = throwError ∘ ("Roman.convertFrom: " ⊕)


-------------------------------------------------------------------------------
-- Default Configurations
-------------------------------------------------------------------------------

-- |Configuration for Roman numerals as they are commonly used
-- today. The value 0 is represented by the empty string. It can be
-- interpreted as not writing down a number. This configuration is
-- practically limited to the range [1..3999]. Smaller numbers will
-- result in empty string. Larger numbers will result in repeated use
-- of the \'M\' symbol.
modernRoman ∷ (IsString s, Ord n, Num n) ⇒ NumeralConfig s n
modernRoman = 
    mkNumConfig ""
                "I"
                [ ("IV",    4)
                , ("V",     5)
                , ("IX",    9)
                , ("X",    10)
                , ("XL",   40)
                , ("L",    50)
                , ("XC",   90)
                , ("C",   100)
                , ("CD",  400)
                , ("D",   500)
                , ("CM",  900)
                , ("M",  1000)
                ]

-- |Configuration for Roman numerals that do not use the rule that a
-- lower rank symbol can be placed before a higher rank symbol to
-- denote the difference between them. Thus a numeral like \"IV\" will
-- not be accepted or generated by this configuration. This
-- configuration is practically limited to the range [1..3999]. Larger
-- numbers will result in repeated use of the \'M\' symbol.
simpleRoman ∷ (IsString s, Ord n, Num n) ⇒ NumeralConfig s n
simpleRoman = 
    mkNumConfig ""
                "I"
                [ ("V",    5)
                , ("X",   10)
                , ("L",   50)
                , ("C",  100)
                , ("D",  500)
                , ("M", 1000)
                ]


-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

-- |Converts a number to a modern Roman numeral. See 'convertTo' for
-- possible exceptions.
toRoman ∷ (IsString s, Monoid s, Ord n, Num n) ⇒ n → s
toRoman = convertTo modernRoman


-- |Parses a string as a modern Roman numeral. See 'convertFrom' for
-- possible exceptions.
fromRoman ∷ ( IsString s, Monoid s, StripPrefix s, Eq s
            , Ord n, Num n, MonadError String m
            ) ⇒ s → m n
fromRoman = convertFrom modernRoman
