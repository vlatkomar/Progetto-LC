-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Pares1mod where
import Abses1mod
import Lexes1mod
import ErrM

}

%name pE E
%name pE1 E1
%name pE2 E2
%name pE3 E3
%name pListE ListE

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 ' ' { PT _ (TS _ 1) }
 '(' { PT _ (TS _ 2) }
 ')' { PT _ (TS _ 3) }

L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

E :: { E }
E : E1 { Radice $1 } 
  | E1 { $1 }


E1 :: { E }
E1 : E3 '(' E2 ')' { Nodo $1 $3 } 
  | E3 { $1 }
  | E2 { $1 }


E2 :: { E }
E2 : E1 ' ' E2 { Nodi $1 $3 } 
  | E { $1 }
  | E3 { $1 }


E3 :: { E }
E3 : Integer { Label $1 } 
  | '(' E ')' { $2 }


ListE :: { [E] }
ListE : {- empty -} { [] } 
  | ListE E { flip (:) $1 $2 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
