{-# LANGUAGE CPP
           , FlexibleContexts
           , NoImplicitPrelude
           , OverloadedStrings
           , UnicodeSyntax
  #-}

{-| Parsing and pretty printing of Roman numerals.

This module provides functions for parsing and pretty printing Roman
numerals. Because the notation of Roman numerals has varied through
the centuries this package allows for some customisation using a
configuration that is passed to the conversion functions.

Example:

@
  \> 'toRoman' 1729 &#x2237; String
  \"MDCCXXIX\"
@

@
  \> 'fromRoman' \"MDCCXXIX\" &#x2237; Maybe Integer
  Just 1729
@

@
  \> 'convertTo' 'simpleRoman' 1729 &#x2237; String
  \"MDCCXXVIIII\"
@

@
  \> 'fromRoman' \"Bla\" &#x2237; Maybe Integer
  Nothing
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
import Control.Monad ( return, mzero )
import Data.Bool     ( Bool, otherwise )
import Data.Eq       ( Eq )
import Data.Function ( ($), flip, on )
import Data.List     ( sortBy, null, stripPrefix )
import Data.Maybe    ( Maybe(Nothing, Just), maybe )
import Data.Monoid   ( Monoid )
import Data.Ord      ( Ord, compare )
import Data.String   ( IsString )
import Data.Tuple    ( snd )
import Prelude       ( Num, (+), (-), error )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad ( (>>=), (>>), fail )
import Data.String   ( fromString )
import Prelude       ( fromInteger )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Monoid.Unicode   ( (∅), (⊕) )
import Data.Ord.Unicode      ( (≥), (≤) )

-- from bytestring:
import qualified Data.ByteString      as BS  ( ByteString
                                             , isPrefixOf
                                             , drop, length, null
                                             )
import qualified Data.ByteString.Lazy as BSL ( ByteString
                                             , isPrefixOf
                                             , drop, length, null
                                             )

-- from text:
import qualified Data.Text      as T  ( Text, null, stripPrefix )
import qualified Data.Text.Lazy as TL ( Text, null, stripPrefix )


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A configuration with which the 'convertTo' and 'convertFrom' functions can
-- be parameterized.
data NumeralConfig s n = NC
    { -- |Symbol to represent the value 0.
      ncZero ∷ s
      -- |A table of symbols and their numerical values. The table must be
      -- ordered in descending order of the value of the symbols. If any symbol
      -- is the empty string then 'convertFrom' will be &#x22a5;.
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

-- |Converts a number to a Roman numeral according to the given configuration.
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
    spStripPrefix p s = if p `BS.isPrefixOf` s
                        then Just $ BS.drop (BS.length p) s
                        else Nothing

instance StripPrefix BSL.ByteString where
    spNull = BSL.null
    spStripPrefix p s = if p `BSL.isPrefixOf` s
                        then Just $ BSL.drop (BSL.length p) s
                        else Nothing

instance StripPrefix T.Text where
    spNull        = T.null
    spStripPrefix = T.stripPrefix

instance StripPrefix TL.Text where
    spNull        = TL.null
    spStripPrefix = TL.stripPrefix


-- |Parses a string as a Roman numeral according to the given
-- configuration. Result is 'Nothing' if the input is not a valid numeral.
convertFrom ∷ ( Monoid s, StripPrefix s, Eq s
              , Ord n, Num n
              )
            ⇒ NumeralConfig s n → s → Maybe n
convertFrom nc s | ncZero nc ≡ s = return 0
                 | otherwise = do n  ← (go 0 (ncTable nc) s)
                                  if s ≡ convertTo nc n
                                    then return n
                                    else mzero
    where go n _  x | spNull x = return n
          go _ [] _            = mzero
          go n tab@((sym, val) : ts) x = maybe (go n ts x)
                                               (go (n + val) tab)
                                               $ spStripPrefix sym x

-------------------------------------------------------------------------------
-- Default Configurations
-------------------------------------------------------------------------------

-- |Configuration for Roman numerals as they are commonly used today. The value
-- 0 is represented by the empty string. It can be interpreted as not writing
-- down a number. This configuration is practically limited to the range
-- [1..3999]. Smaller numbers will result in an empty string. Larger numbers
-- will result in repeated use of the \'M\' symbol.
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

-- |Configuration for Roman numerals that do not use the rule that a lower rank
-- symbol can be placed before a higher rank symbol to denote the difference
-- between them. Thus a numeral like \"IV\" will not be accepted or generated by
-- this configuration. This configuration is practically limited to the range
-- [1..3999]. Smaller numbers will result in an empty string. Larger numbers
-- will result in repeated use of the \'M\' symbol.
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

-- |Converts a number to a modern Roman numeral.
toRoman ∷ (IsString s, Monoid s, Ord n, Num n) ⇒ n → s
toRoman = convertTo modernRoman

-- |Parses a string as a modern Roman numeral.
fromRoman ∷ ( IsString s, Monoid s, StripPrefix s, Eq s
            , Ord n, Num n
            ) ⇒ s → Maybe n
fromRoman = convertFrom modernRoman
