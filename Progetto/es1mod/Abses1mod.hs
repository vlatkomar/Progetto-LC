module Abses1mod where

-- Haskell module generated by the BNF converter


data E =
   Radice E
 | Nodo E E
 | Nodi E E
 | Label Integer
  deriving (Eq,Ord,Show)

