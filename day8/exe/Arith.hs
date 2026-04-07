{-# LANGUAGE FunctionalDependencies #-}

module Arith where

import Prelude hiding (negate, (+), (-))
import Prelude qualified (negate, (+), (-))

infixl 6 +

infixl 6 -

class Add l m n | l m -> n where
  (+) :: l -> m -> n

class Neg n m | n -> m where
  negate :: n -> m

class Sub l m n | l m -> n where
  (-) :: l -> m -> n

instance {-# OVERLAPPABLE #-} (Num n) => Add n n n where
  (+) = (Prelude.+)

instance {-# OVERLAPPABLE #-} (Num n) => Sub n n n where
  (-) = (Prelude.-)

instance {-# OVERLAPPABLE #-} (Num n) => Neg n n where
  negate = Prelude.negate
