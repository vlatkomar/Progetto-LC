module TypeCheckRuby where

import ErrM
import AbsRuby
import PrettyPrinter
import Control.Monad

type Fun = (Ident, (Type, [(Mod, Type)]))   
type Var = (Ident, Type)

-- definizione dell'ambiente
type Env = [([Fun], [Var])]
emptyEnv :: Env
emptyEnv = [([],[])]

-- definizione del programma
typecheck :: Program -> Err ()
typecheck (Prog defs) =
  do
    env <- checkDecls emptyEnv defs
    -- inserimento delle funzioni predefinite
    env <- addFun env (Ident "writeInt")    TNil [ADec MDef TInt    (Ident "x")]
    env <- addFun env (Ident "writeReal")   TNil [ADec MDef TReal   (Ident "x")]
    env <- addFun env (Ident "writeChar")   TNil [ADec MDef TChar   (Ident "x")]
    env <- addFun env (Ident "writeString") TNil [ADec MDef TString (Ident "x")]
    env <- addFun env (Ident "readInt")     TInt    []
    env <- addFun env (Ident "readReal")    TReal   []
    env <- addFun env (Ident "readChar")    TChar   []
    env <- addFun env (Ident "readString")  TString []
    checkDefs env defs

-- controlla la lista di definizioni del programma
checkDecls :: Env -> [ Def ] -> Err Env
checkDecls env [] = Ok env
checkDecls env (def:defs) =
  do
    env_ <- checkDecl env def
    checkDecls env_ defs

-- aggiunge all'ambiente le funzioni e le dichiarazioni di variabili trovate 
checkDecl :: Env -> Def -> Err Env
checkDecl env def =
  case def of
    DFun identifier args typ stmts -> addFun env identifier typ args  --aggiunge una funzione
    DVar id expr ->
     do
       tipo <- checkExpr env expr   --determina il tipo di una variabile in base a quella della sua r-expr
       addVar env id tipo   --aggiunge una variabile
    DBlock _ defs -> checkDecls env defs  --se trova un blocco, controlla le funzioni e le variabili che lo compongono
    --otherwise -> Bad ("Un programma può contenere solo funzioni o dichiarazioni di variabili")


-- controlla la lista di definizioni del programma
checkDefs :: Env -> [ Def ] -> Err ()
checkDefs env [] = Ok ()
checkDefs env (def:defs) =
  do
    env_ <- checkDef env def
    checkDefs env_ defs

checkDefsBlock :: Env -> [ Def ] -> Err Env
checkDefsBlock env [] = Ok env
checkDefsBlock env (def:defs) =
  do
    env_ <- checkDef env def
    checkDefsBlock env_ defs


-- legge il corpo delle funzioni
checkDef :: Env -> Def -> Err Env
checkDef env def =
  case def of
    DFun identifier args typ stmts  ->
      do
        env_ <- addParams env identifier typ args
        checkStmts env_ stmts typ
        env__ <- remScope env_
        Ok env__
    DVar id expr -> Ok env  --non fa niente perché le variabili sono già state registrate prima
    DBlock _ defs -> checkDefsBlock env defs  --se trova un blocco, controlla le funzioni e le variabili che lo compongono
    --otherwise -> Bad ("Un programma può contenere solo funzioni o dichiarazioni di variabili")

-- controlla la lista di statement che compone una funzione
checkStmts :: Env -> [ Stm ] -> Type -> Err Env
checkStmts env [] _ = Ok env
checkStmts env (stmt:stmts) typ =
  do
    env_ <- checkStmt env stmt typ
    checkStmts env_ stmts typ

-- controlla ogni singolo statement, e ne controlla la validità
checkStmt :: Env -> Stm -> Type -> Err Env
checkStmt env stmt typ =
  case stmt of
    SVar (VarAss identifier expr) ->     --dichiarazione di una varabile
      do
       tipo <- checkExpr env expr  --determina il tipo della variabile in base alla sua r-expr
       addVar env identifier tipo 
    SVarP identifier expr ->     --dichiarazione di una varabile con puntatore   &p=0
      do
       tipo <- checkExpr env expr  --determina il tipo della variabile in base alla sua r-expr
       addVar env identifier tipo 
    SExpr expr                 ->       
      do
        checkExpr env expr
        Ok env
    SBlock _ defs             ->
      do
        env_ <- addScope env      --viene creata una copia dell'ambiente che verrà utilizzata solo all'interno del blocco
        checkDecls env_ defs      --controlla la definizioni (funzioni e variabili) che compongono il blocco
        Ok env
    SReturn expr              ->
      do 
	if (typ /= TNil) then     --il return ci può essere solo se la funzione non è nil(void)
   	  do
           checkReturn env expr typ   --controlla se il tipo dell'espressione del return è compatibile con il tipo della funzione
           Ok env
         else
          Bad ("Una funzione Nil non può avere il return!")
    SWhile expr stmt          ->
      do 
        env_ <- addScope env      --viene creata una copia dell'ambiente che verrà utilizzata solo all'interno del ciclo
        if (checkExpr env_ expr == Ok TBool) then   
          do
            env_ <- addVar env_ (Ident "loop") TNil  --crea una variabile che serve per sapere se si è all'interno del while
            checkStmt env_ stmt typ
            Ok env
        else
          Bad ("L'espressione del ciclo while deve essere del tipo boolean!")
    SFor (VarFor id) expr stmt   ->
      do 
	env_ <- addScope env      --viene creata una copia dell'ambiente che verrà utilizzata solo all'interno del ciclo
	if (checkExpr env_ expr == Ok TRange) then
	   do
	     env_ <- addVar env_ id TInt  --viene registrata la variabile del for
             env_ <- addVar env_ (Ident "loop") TNil   --crea una variabile che serve per sapere se si è all'interno del for
             checkStmt env_ stmt typ
	     Ok env 
	else
	   Bad ("Bisogna scrivere un intervallo nel modo: 'int..int'!")
    SIf expr stmt        ->             --if semplice
      do
	env_ <- addScope env
        if (checkExpr env_ expr == Ok TBool) then
          do
            checkStmt env_ stmt typ
            Ok env
        else
          Bad ("L'espressione del if condizionale deve essere boolean!")
    SElsif expr stmt        ->          --blocco elsif
      do
	env_ <- addScope env
        if (checkExpr env_ expr == Ok TBool) then
          do
            checkStmt env_ stmt typ
            Ok env
        else
          Bad ("L'espressione del if condizionale deve essere boolean!")
    SIfElse expr1 stm1 stm2 stm3 ->     --if-elsif-else; stm2 e' la lista di elsif, stm3 e' quello del else
      do
        env_ <- addScope env
        if (checkExpr env_ expr1 == Ok TBool) then
          do
            checkStmt env_ stm1 typ
	    checkStmts env_ stm2 typ  --controlla tutti gli elsif
	    checkStmt env_ stm3 typ   --controlla l'else
            Ok env
        else
          Bad ("L'espressione del if-else condizionale deve essere boolean!")
    otherwise -> Bad ("\"" ++ printTree stmt ++ "\" non è un comando valido")

checkReturn env [] typ = Ok typ
checkReturn env (expr:exprs) typ =
  do
    checkExprType env expr typ
    checkReturn env exprs typ


-- controlla un'espressione e ne determina il tipo
checkExpr :: Env -> Expr -> Err Type
checkExpr env expr =
  case expr of
    --tipi primitivi
    ETrue                    -> Ok TBool
    EFalse                   -> Ok TBool
    EInt _                   -> Ok TInt
    EReal _                  -> Ok TReal
    EStr _                   -> Ok TString
    EChar _                  -> Ok TChar
    ERange _ _               -> Ok TRange
    EArray id _              -> 
      do
        env <- addVar env id TArray
        Ok TArray
    ECall id exprs           -> --chiamata di una funzione
      do
        (retType, types) <- lookupFun env id
        if (length exprs == length types) then  --controlla se viene passato il numero corretto di parametri
          do
            mapM (\(expr, typ) -> checkExprType env expr typ) (zip exprs (map snd types))
	    checkMods env exprs (map fst types) --controlla se viene rispettata la modalità di passaggio dei parametri
            Ok retType
        else
          Bad ("Il numero di parametri della funzione: \"" ++ printTree id ++ "\" non corrisponde alla dichiarazione della funzione!")
    EIncPre  expr          -> checkUnaryArithmeticOperator env expr  -- ++expr
    EDecPre  expr          -> checkUnaryArithmeticOperator env expr  -- --expr
    EIncPost expr          -> checkUnaryArithmeticOperator env expr  -- expr++
    EDecPost expr          -> checkUnaryArithmeticOperator env expr  -- expr--
    ENeg     expr          -> checkUnaryArithmeticOperator env expr  --  -expr
    EMul lhs rhs           -> checkArithmeticOperator env lhs rhs    --lhs*rhs
    EDiv lhs rhs           -> checkArithmeticOperator env lhs rhs    --lhs/rhs
    EAdd lhs rhs           -> checkPlusOperator env lhs rhs          --lhs+rhs
    ESub lhs rhs           -> checkArithmeticOperator env lhs rhs    --lhs-rhs
    EPow lhs rhs           -> checkArithmeticOperator env lhs rhs    --lhs**rhs
    ELt  lhs rhs           ->   -- lhs<rhs
      do
        checkExprTypeEquality env lhs rhs
        Ok TBool
    EGt  lhs rhs           ->   -- lhs>rhs
      do
        checkExprTypeEquality env lhs rhs
        Ok TBool
    ELtEq lhs rhs          ->   -- lhs<=rhs
      do
        checkExprTypeEquality env lhs rhs
        Ok TBool
    EGtEq lhs rhs          ->   -- lhs>=rhs
      do
        checkExprTypeEquality env lhs rhs
        Ok TBool
    EEq lhs rhs            ->   -- lhs==rhs
      do
        checkExprTypeEquality env lhs rhs
        Ok TBool
    ENeq lhs rhs           ->   -- lhs!=rhs
      do
        checkExprTypeEquality env lhs rhs
        Ok TBool
    ENot expr              -> checkExpr env expr     -- !expr
    EAnd lhs rhs           -> checkExprTypesAreBool env lhs rhs   -- lhs&&rhs
    EOr  lhs rhs           -> checkExprTypesAreBool env lhs rhs   -- lhs||rhs
    EAss lhs rhs           -> checkExprTypeEquality env lhs rhs   -- lhs=rhs
    EIncAss lhs rhs        -> checkExprTypeEquality env lhs rhs   -- lhs+=rhs
    EDecAss lhs rhs        -> checkExprTypeEquality env lhs rhs   -- lhs-=rhs
    EDeRef expr            -> checkExpr env expr     -- *expr
    EId id                 -> lookupVar env id 
    -- per break e continue si controlla se esiste la variabile loop (si è all'interno di un ciclo); altrimenti è errore
    EBreak                 -> if((lookupVar env (Ident "loop")) == Ok TNil) then Ok TNil else Bad("Non siamo in un ciclo: l'istruzione break non può venir usata al di fuori di un ciclo!")
    ECont                  -> if((lookupVar env (Ident "loop")) == Ok TNil) then Ok TNil else Bad("Non siamo in un ciclo: l'istruzione continue non può venir usata al di fuori di un ciclo!")

--controlla la lista di modalità di passaggio dei parametri di una funzione
checkMods :: Env -> [Expr] -> [Mod] -> Err Env
checkMods env [] [] = Ok env
checkMods env (expr:exprs) (mod:mods) =
  do
    env_ <- checkMod env expr mod
    checkMods env_ exprs mods

--verifica che sia corretto il passaggio del parametro attuale
checkMod :: Env -> Expr -> Mod -> Err Env
checkMod env expr mod =
  case expr of
    ETrue      -> if (mod == MValRes) || (mod == MRef) then Bad ("con \"" ++ printTree mod ++ "\" bisogna passare un'espressione") else Ok env
    EFalse     -> if (mod == MValRes) || (mod == MRef) then Bad ("con \"" ++ printTree mod ++ "\" bisogna passare un'espressione") else Ok env
    EInt _     -> if (mod == MValRes) || (mod == MRef) then Bad ("con \"" ++ printTree mod ++ "\" bisogna passare un'espressione") else Ok env
    EReal _    -> if (mod == MValRes) || (mod == MRef) then Bad ("con \"" ++ printTree mod ++ "\" bisogna passare un'espressione") else Ok env
    EStr _     -> if (mod == MValRes) || (mod == MRef) then Bad ("con \"" ++ printTree mod ++ "\" bisogna passare un'espressione") else Ok env
    EChar _    -> if (mod == MValRes) || (mod == MRef) then Bad ("con \"" ++ printTree mod ++ "\" bisogna passare un'espressione") else Ok env	
    otherwise  -> Ok env


-- verifica che expr sia di tipo typ
checkExprType :: Env -> Expr -> Type -> Err Type
checkExprType env expr typ =
  case checkExpr env expr of
    Ok exprTyp ->
      if exprTyp == typ then
        Ok exprTyp
      else
        if((typ == TReal) && (exprTyp == TInt)) then Ok typ else    --int può essere visto come real
          Bad ("I tipi nella chiamata di funzione non corrispondono. L'espressione: " ++ printTree expr ++ " dovrebbe essere del tipo " ++ printTree typ ++ ", invece è del tipo " ++ printTree exprTyp ++ "!")
    Bad s -> Bad s

-- controlla se due espressioni sono dello stesso tipo
checkExprTypeEquality :: Env -> Expr -> Expr -> Err Type
checkExprTypeEquality env lhs rhs =
  do
    typ1 <- checkExpr env lhs
    typ2 <- checkExpr env rhs
    if (typ1 == typ2) then
      Ok typ1  --viene ritornato il tipo di entrambe
    else       
      if (((typ1 == TReal) && (typ2 == TInt)) || ((typ1 == TInt) && (typ2 == TReal))) then Ok TReal else  --int può essere visto come real
        Bad ("I tipi delle espressioni non corrispondono. lhs: " ++ printTree lhs ++ " del tipo " ++ printTree typ1 ++ " rhs: " ++ printTree rhs ++ " del tipo " ++ printTree typ2 ++ "!")

-- ccontrolla se due espressioni sono booleane
checkExprTypesAreBool :: Env -> Expr -> Expr -> Err Type
checkExprTypesAreBool env lhs rhs =
      if (checkExpr env lhs == Ok TBool && checkExpr env rhs == Ok TBool) then
        Ok TBool
      else
        Bad ("Nella congiunzione e nella disgiunzione i tipi devono essere boolean!")

-- verifica che un operatore aritmetico unario venga usato solo con interi o real
checkUnaryArithmeticOperator :: Env -> Expr -> Err Type
checkUnaryArithmeticOperator env expr =
  do
    typ <- checkExpr env expr
    if (typ == TInt || typ == TReal) then
      Ok typ
    else
      Bad ("Gli operatori unari sono definiti solo per i tipi int e real!")

-- verifica che un operatore aritmetico binario venga usato solo con interi o real
checkArithmeticOperator :: Env -> Expr -> Expr -> Err Type
checkArithmeticOperator env lhs rhs =
  do
    lhsTyp <- checkExpr env lhs
    if (lhsTyp == TInt || lhsTyp == TReal) then
      checkExprType env rhs lhsTyp
    else
      Bad ("Gli operatori aritmetici sono definiti solo per i tipi int e real!")

-- verifica che il + binario venga usato solo con interi, real o stringhe(concatenazione)
checkPlusOperator :: Env -> Expr -> Expr -> Err Type
checkPlusOperator env lhs rhs =
  do
    lhsTyp <- checkExpr env lhs
    if (lhsTyp == TInt || lhsTyp == TReal || lhsTyp == TString) then
      checkExprType env rhs lhsTyp
    else
      Bad ("L'operatore aritmetico è definito solo per i tipi int e real!")







-- aggiunge uno scope vuoto all'ambiente
addScope :: Env -> Err Env
addScope env = Ok (([],[]):env)

-- rimuove uno scope vuoto dall'ambiente
remScope :: Env -> Err Env
remScope [] = Bad []
remScope (scope:rest) = Ok rest

-- aggiunge più variabili all'ambiente
addVars :: Env -> [Ident] -> Type -> Err Env
addVars env [] typ = Ok env
addVars env (top:rest) typ =
  do
    env_ <- addVar env top typ
    addVars env_ rest typ

-- aggiunge una variablie nell'ambiente dato
addVar :: Env -> Ident -> Type -> Err Env
addVar [] _ _ = Bad ("Impossibile aggiungere una variabile all'ambiente vuoto!")
addVar (scope:rest) identifier typ =
  case lookup identifier (snd scope) of  --verifica che tale variabile non sia già stata dichiarata altrove
    Nothing -> Ok ((fst scope, (identifier, typ):(snd scope)):rest)
    Just _ -> Bad ("La variabile " ++ printTree identifier ++ " è già stata dichiarata!")  

-- aggiunge una funzione nell'ambiente dato
addFun :: Env -> Ident -> Type -> [ Arg ] -> Err Env
addFun [] _ _ _ = Bad ("Impossibile aggiungere una funzione all'ambiente vuoto!")
addFun env@(scope:rest) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing ->
      do
        env_ <- addScope env   -- si aggiunge uno scope per la funzione
        env__ <- foldM (\env (ADec mod typ identifier) -> addVar env identifier typ) env_ args
        let sig_ = (map (\(ADec mod typ identifier) -> typ) args)
	let modality = (map (\(ADec mod typ identifier) -> mod) args)
	let sig = (typ, zip modality sig_)
	remScope ((\(scope:outer:rest) -> scope:(((identifier, sig):(fst outer), snd outer):rest)) env__)
    Just _ -> Bad ("La funzione " ++ printTree identifier ++ " è già stata dichiarata!")

-- aggiunge i parametri della funzione in un nuovo scope
addParams :: Env -> Ident -> Type -> [ Arg ] -> Err Env
addParams [] _ _ _ = Bad ("Impossibile aggiungere i parametri della funzione all'ambiente vuoto!")
addParams env@(scope:rest) identifier typ args =
  case lookup identifier (fst scope) of
    Nothing -> Bad ("La funzione " ++ printTree identifier ++ " non è stata dichiarata!")
    Just _  ->
      do
        env_ <- addScope env    -- si aggiunge uno scope per la funzione
        env__ <- foldM (\env (ADec mod typ identifier) -> addVar env identifier typ) env_ args
        Ok env__




-- Cerca la variabile data nell'ambiente attuale
lookupVar :: Env -> Ident -> Err Type
lookupVar [] identifier = Bad ("Variabile sconosciuta: " ++ printTree identifier ++ " non è stata ancora dichiarata.")
lookupVar (scope:rest) identifier =
  case lookup identifier (snd scope) of
    Nothing -> lookupVar rest identifier
    Just typ -> Ok typ     --restituisce il tipo della variabile trovata

-- Cerca la funzione data nell'ambiente attuale
lookupFun :: Env -> Ident -> Err (Type, [(Mod, Type)])
lookupFun [] identifier = Bad ("Funzione sconosciuta: " ++ printTree identifier ++ ".")  --la funzione  non è stata ancora dichiarata
lookupFun (scope:rest) identifier =
  case lookup identifier (fst scope) of
    Nothing -> lookupFun rest identifier
    Just sig -> Ok sig     --restituisce il tipo della funzione e quello dei parametri, oltre alle modalità di passaggio

