{-# LANGUAGE UnicodeSyntax #-}
module Text.Columnify
  ( Justify(..)
  , columnify
  ) where

import Base0

import Prelude ( (*) )

-- base --------------------------------

import Data.List ( repeat, transpose, zip, zipWith )

-- lens --------------------------------

import Control.Lens.Each ( each )
import Control.Lens.Fold ( (^..) )

-- natural -----------------------------

import Natural ( NumSign(SignMinus, SignPlus), length, replicate, unNegate,
                 (⊖) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor ( (⊳) )
import Data.MoreUnicode.Lens    ( (⊧) )

-- safe --------------------------------

import Safe ( maximumDef )

-- text --------------------------------

import Data.Text ( Text )

--------------------------------------------------------------------------------

{- Given a list of lines, each being a list of columns; pad out the columns
   to provide an aligned display.

   The columns are padded out according to the input `pads` argument.  Widths
   are set according to the widest input column.  Columns for which no justify
   value is provided are left unmolested.
-}
data Justify = JustifyLeft | JustifyRight

-- provide fixed width args, and ignore args, and centrejustify args

columnify ∷ [Justify] → [[Text]] → [[Text]]
columnify pads zs =
  let pad_t ∷ ℤ → Text → Text
      pad_t (unNegate → (SignMinus,n)) t = replicate @Text (n ⊖ length t) ' ' ⊕ t
      pad_t (unNegate → (SignPlus, n)) t = t ⊕ replicate @Text (n ⊖ length t) ' '

      col_widths = transpose zs & each ⊧ (\ ys → maximumDef 0 $ length ⊳ ys)
      xx JustifyLeft  = 1
      xx JustifyRight = (-1)
      col_widths' = (\(x,y) → fromIntegral y * xx x) ⊳ zip pads col_widths
  in
    (^.. each) ∘ zipWith pad_t (col_widths' ⊕ repeat 0) ⊳ zs

-- that's all, folks! ----------------------------------------------------------
