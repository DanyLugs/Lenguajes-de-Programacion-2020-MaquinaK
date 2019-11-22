module BAE.Sintax where


{--
Importamos el paquete de funciones sobre listas  
Usamos delete y union de listas para esta práctica
delete: recibe un elemento y una lista
return: la lista sin el elemento
union: recibe dos listas
return: una lista aplicando la operacion de UNION sobre listas
--}
import Data.List
import Data.Char
import Data.String

--Sinonimo para el identificador como un string
type Identifier = String

--Sinonimo para definir una sustitución
type Substitution = (Identifier, Expr)

--Sinonimo para definir una pila
type Stack = [Frame]


--Tipo de dato para EAB (aritmetico y booleanas)
data Expr = V Identifier | I Int | B Bool
          | Fix Identifier Expr
          | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
          | Not Expr | And Expr Expr | Or Expr Expr
          | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | Fn Identifier Expr | App Expr Expr
          | L Int
          | Assig Expr Expr -- Poner algo
          | Alloc Expr -- Crear un registro
          | Deref Expr -- Sacar algo
          | Seq Expr Expr
          | Void
          | While Expr Expr 
          | Raise Expr
          | Handle Expr Identifier Expr
          | LetCC Identifier Expr
          | Continue Expr Expr
          | Cont Stack deriving (Eq)

--Instancia de Show para imprimir expresiones
instance Show Expr where
        show e = case e of
              (L x) -> "L "++ (show x)
              (V x) -> "V[" ++ x ++ "]"
              (I n) -> "I[" ++ (show n) ++ "]"
              (B b) -> "B[" ++ (show b) ++ "]"
              (Add n m) -> "Add(" ++ (show n) ++ "," ++ (show m) ++ ")" 
              (Mul n m) -> "Mul(" ++ (show n) ++ "," ++ (show m) ++ ")" 
              (Succ n) -> "Succ(" ++ (show n) ++ ")"
              (Pred n) -> "Pred(" ++ (show n) ++ ")"
              (Not n) -> "Not(" ++ (show n) ++ ")"
              (And n m) -> "And(" ++ (show n) ++ "," ++ (show m) ++ ")"
              (Or n m) -> "Or(" ++ (show n) ++ "," ++ (show m) ++ ")"
              (Lt n m) -> "Lt(" ++ (show n) ++ "," ++ (show m) ++ ")"
              (Gt n m) -> "Gt(" ++ (show n) ++ "," ++ (show m) ++ ")"
              (Eq n m) -> "Eq(" ++ (show n) ++ "," ++ (show m) ++ ")"
              (If n m o) -> "If(" ++ (show n) ++ "," ++ (show m) ++ "," ++ (show o) ++ ")"
              (Let i n m) -> "Let(" ++ (show n) ++ "," ++ i ++ "." ++ (show m) ++ ")"
              (Fn i e) -> "Fn(" ++ i ++ "." ++ (show e) ++ ")"
              (Fix i e) -> "Fix(" ++ i ++ "." ++ (show e) ++ ")"
              (App e1 e2) -> "App(" ++ (show e1) ++ "," ++ (show e2) ++ ")"
              (Assig e1 e2) -> "Assig(" ++ (show e1) ++ "," ++ (show e2) ++ ")"
              (Alloc e1) -> "Alloc(" ++ (show e1) ++ ")"
              (Deref e1) -> "Deref(" ++ (show e1) ++ ")"
              (Seq e1 e2) -> "Seq(" ++ (show e1) ++ "," ++ (show e2) ++ ")"
              (Void) -> "Void"
              (While e1 e2) -> "While(" ++ (show e1) ++ "," ++ (show e2) ++ ")"
              (Raise e) -> "Raise(" ++ (show e) ++ ")"
              (Handle e1 x e2) -> "Handle(" ++ (show e1) ++ "," ++ x ++ "," ++ (show e2) ++ ")"
              (LetCC x e) -> "LetCC(" ++ (show x) ++ "." ++ (show e) ++ ")"
              (Continue e1 e2) -> "Continue(" ++ (show e1) ++ "," ++ (show e2)
              (Cont s) -> "Cont(" ++ (show s) ++ ")"


--Sinonimo para representar el computo pendiente
type Pending = ()

--Tipo de dato para representar las expresiones EAB y adicionales
--con computo pendiente
data Frame = SuccF Pending
           | PredF Pending
           | NotF Pending
           | AddFL Pending Expr
           | AddFR Expr Pending
           | MulFL Pending Expr
           | MulFR Expr Pending
           | AndFL Pending Expr
           | AndFR Expr Pending
           | OrFL Pending Expr
           | OrFR Expr Pending
           | LtFL Pending Expr
           | LtFR Expr Pending
           | GtFL Pending Expr
           | GtFR Expr Pending
           | EqFL Pending Expr
           | EqFR Expr Pending
           | IfF Pending Expr Expr
           | LetF Identifier Pending Expr
           | FnF Identifier Pending
           | AppFL Pending Expr
           | AppFR Expr Pending
           | AllocF Pending
           | DerefF Pending
           | AssigFL Pending Expr
           | AssigFR Expr Pending
           | SeqFL Pending Expr
           | SeqFR Expr Pending
           | WhileF Pending Expr
           | RaiseF Pending
           | HandleF Pending Identifier Expr
           | LetCCF Identifier Pending
           | ContinueFL Pending Expr
           | ContinueFR Expr Pending
           | ContF Stack
           | Error deriving (Eq)


--Instancia show para visualizar operaciones con marcos
instance Show Frame where
        show e = case e of
              (SuccF x) -> "SuccF(-)"
              (PredF x) -> "PredF(-)"
              (NotF e) -> "NotF(-)"
              (AddFL p e) -> "AddFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (AddFR e p) -> "AddFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (MulFL p e) -> "MulFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (MulFR e p) -> "MulFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (AndFL p e) -> "AndFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (AndFR e p) -> "AndFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (OrFL p e) -> "OrFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (OrFR e p) -> "OrFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (LtFL p e2) -> "LtFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (LtFR e1 p) -> "LtFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (GtFL p e2) -> "GtFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (GtFR e1 p) -> "GtFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (EqFL p e2) -> "EqFL(" ++ "-" ++ "," ++ (show e) ++ ")"
              (EqFR e1 p) -> "EqFR(" ++ (show e) ++ ","++ "-" ++ ")"
              (IfF p e1 e2) -> "IfF(" ++ "-" ++ "," ++ (show e1) ++ "," ++ (show e2) ++ ")"
              (LetF x p e) -> "LetF(" ++ "-" ++ "," ++ "x" ++ (show e) ++ ")"
              (FnF x e) -> "Fnf(" ++ "x" ++ "," ++ "- )"
              (AppFL p e) -> "AppFL(" ++ "-" ++ (show e) ++ ")"
              (AppFR e p) -> "AppFL(" ++ (show e) ++ "-" ++ ")"
              (Error) -> "Error"
              (AllocF e) -> "AllocF(-)"
              (DerefF e) -> "DerefF(-)"
              (AssigFL e1 e2) -> "AssigFL(" ++ "-" ++ "," ++ (show e2) ++ ")"
              (AssigFR e1 e2) -> "AssigFR(" ++ (show e1) ++ "," ++ "-" ++ ")"
              (SeqFL e1 e2) -> "SeqFL(" ++ "-" ++ "," ++ (show e2) ++ ")"
              (SeqFR e1 e2) -> "SeqFR(" ++ (show e1) ++ "," ++ "-" ++ ")"
              (WhileF e1 e2) -> "WhileF(" ++ "-" ++ "," ++ (show e2) ++ ")" 
              (RaiseF e) -> "RaiseF(-)"
              (HandleF e1 x e2) -> "HandleF(" ++ "-" ++ "," ++ x ++ "." ++ (show e2) ++ ")"
              (LetCCF x e) -> "LetCCF(" ++ x ++ "," ++ "-" ++ ")"
              (ContinueFL e1 e2) -> "ContinueFL(" ++ "-" ++ "," ++ (show e2) ++ ")"
              (ContinueFR e1 e2) -> "ContinueFR(" ++ (show e1) ++ "," ++ "-" ++ ")"
              (ContF s) -> "ContF(" ++ (show s) ++ ")"
 

{--
Función frVars: dada una expresión, calcula las variables libres de la misma
return: una lista con las variables libres de la Expr
return: [] si no tiene variables libres la Epxpr
--}
frVars :: Expr -> [Identifier]
frVars (V x) = [x]
frVars (L x) = []
frVars (I n) =  []
frVars (B b) = []
frVars (Add n m) = union (frVars n) (frVars m)
frVars (Mul n m) = union (frVars n) (frVars m)
frVars (Succ n) = frVars (n)
frVars (Pred n) = frVars (n)
frVars (Not n) = frVars (n)
frVars (And n m) = union (frVars n) (frVars m)
frVars (Or n m) = union (frVars n) (frVars m)
frVars (Lt n m) = union (frVars n) (frVars m)
frVars (Gt n m) = union (frVars n) (frVars m)
frVars (Eq n m) = union (frVars n) (frVars m)
frVars (If n m o) = union (frVars n) (union(frVars m) (frVars o))
frVars (Let i n m) = union (frVars n) (delete i (frVars m))
frVars (Fn i e) = delete i (frVars e)
frVars (Fix i e) = delete i (frVars e)
frVars (App e1 e2) = union (frVars e1) (frVars e2)
frVars (Assig e1 e2) = union (frVars e1) (frVars e2)
frVars (Alloc e1) = (frVars e1)
frVars (Deref e1) = (frVars e1)
frVars (Seq e1 e2) = union (frVars e1) (frVars e2)
frVars (Void) = []
frVars (While e1 e2) = union (frVars e1) (frVars e2)
frVars (Raise e) = frVars (e)
frVars (Handle e1 x e2) = union (frVars e1) (delete x (frVars e2))
frVars (LetCC i e) = delete i (frVars e)
frVars (Continue e1 e2) = union (frVars e1) (frVars e2)
frVars (Cont s) = []

{--
Función incrVar: dado un identificador renombra la cadena de caracteres
si termina en cualquier char agrega un "1" al final de la cadena
suma 1 si el ultimo caracter de la cadena termina en numero
si string = char -> return: char1 
si string = char1 -> return: char2
--}
incrVar :: Identifier -> Identifier
incrVar x 
    | (last x == '9') = incrVar(take ((length x) -1) x) ++ '0':[]
    | (isDigit(last x)) = let x1 = ((getInt(last x))+1) in take ((length x) -1) x ++ (intToDigit x1):[]
    | otherwise = x ++ "1"

{--
Función auxiliar getInt: dado un char lo convierte a integer
return: un entero si su entrada en un char
--}
getInt :: Char -> Int
getInt x = digitToInt x


{--
Función alphaExpr: dada una expresión devuelve otra que es 
alfa-equivalente, usa la funcion subst e incrVar cuando la sustitucion
encuentra variables ligadas con el mismo nombre
return: una expresión alfa-equivalente
--}
alphaExpr :: Expr -> Expr
alphaExpr (V i) = (V (incrVar i))
alphaExpr (I n) = I n
alphaExpr (B b) = B b
alphaExpr (L i) = L i
alphaExpr (Add e1 e2) = Add (alphaExpr e1) (alphaExpr e2)
alphaExpr (Mul e1 e2) = Mul (alphaExpr e1) (alphaExpr e2)
alphaExpr (Succ e) = Succ (alphaExpr e)
alphaExpr (Pred e) = Pred (alphaExpr e)
alphaExpr (Not e) = Not (alphaExpr e)
alphaExpr (And e1 e2) = And (alphaExpr e1) (alphaExpr e2)
alphaExpr (Or e1 e2) = Or (alphaExpr e1) (alphaExpr e2)
alphaExpr (Lt e1 e2) = Lt (alphaExpr e1) (alphaExpr e2)
alphaExpr (Gt e1 e2) = Gt (alphaExpr e1) (alphaExpr e2)
alphaExpr (Eq e1 e2) = Eq (alphaExpr e1) (alphaExpr e2)
alphaExpr (If e1 e2 e3) = If (alphaExpr e1) (alphaExpr e2) (alphaExpr e3) 
alphaExpr (Let i e1 e2) = let x = (find' i e2) in Let x e1 (subst e2 (i, (V x)))
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)
alphaExpr (Fn x e) = let x' = (find' x e) in Fn x' (subst e (x, (V x'))) 
alphaExpr (Fix x e) = let x' = (find' x e) in Fix x' (subst e (x, (V x'))) 
alphaExpr (Assig e1 e2) = Assig (alphaExpr e1) (alphaExpr e2)
alphaExpr (Alloc e) = Alloc (alphaExpr e)
alphaExpr (Deref e) = Deref (alphaExpr e)
alphaExpr (Seq e1 e2) = Seq (alphaExpr e1) (alphaExpr e2)
alphaExpr (Void) = Void
alphaExpr (While e1 e2) = While (alphaExpr e1) (alphaExpr e2)
alphaExpr (Raise e) = Raise (alphaExpr e)
alphaExpr (Handle e1 i e2) = let x = (find' i e2) in Handle e1 i (subst e2 (i, (V x)))
alphaExpr (LetCC i e) = let x' = (find' i e) in LetCC x' (subst e (i, (V x'))) 
alphaExpr (Continue e1 e2) = Continue (alphaExpr e1) (alphaExpr e2)
alphaExpr (Cont s) = Cont s

{--
Funcion find', dado un indetifier y una expresion, devuelve un
identificador nuevo, que no se encuentre en la expresion
--}
find' :: Identifier -> Expr -> Identifier
find' x e = let x' = (incrVar x)
            in 
              if elem x' (frVars e)
              then find' x' e
              else x' 


{--
Función subst: dada una expresión y un tipo de dato Substitution
da la sustitución sobre la expresión
return: una expresión con la sustitución dada aplicada
--}
subst :: Expr -> Substitution -> Expr
subst (L x) s = L x
subst (V x) (i,e) = if (x == i)
                    then (e)
                    else (V x)
subst (I n) s = I n 
subst (B b) s = B b                      
subst (Add n m) s = Add (subst n s) (subst m s)
subst (Mul n m) s = Mul (subst n s) (subst m s)
subst (Succ n) s = Succ (subst n s) 
subst (Pred n) s = Pred (subst n s)
subst (Not n) s = Not (subst n s)
subst (And n m) s = And (subst n s) (subst m s)
subst (Or n m) s = Or (subst n s) (subst m s)                          
subst (Lt n m) s = Lt (subst n s) (subst m s)
subst (Gt n m) s = Gt (subst n s) (subst m s)
subst (Eq n m) s = Eq (subst n s) (subst m s)
subst (If n m o) s = If (subst n s) (subst m s) (subst o s)
subst (Let x e1 e2) (i,e) = if (x == i) || (elem x (frVars e)) 
                            then (Let x e1 e2)
                            else if elem x (frVars e) 
                                 then subst (alphaExpr (Let x e1 e2)) (i,e)
                                 else (Let x (subst e1 (i,e)) (subst e2 (i,e)))                    
subst (App e1 e2) s = App (subst e1 s) (subst e2 s)
subst (Fn i e1) (id,e) = if ((id == i) || (elem i (frVars e))) 
                        then (Fn (incrVar i) (subst e1 (id,e))) 
                        else (Fn i (subst e1 (id,e)))
subst (Fix i e1) (id,e) = if ((id == i) || (elem i (frVars e))) 
                        then (Fix (incrVar i) (subst e1 (id,e))) 
                        else (Fix i (subst e1 (id,e))) 
subst (Assig e1 e2) s = Assig (subst e1 s) (subst e2 s)
subst (Alloc e1) s = Alloc (subst e1 s)
subst (Deref e1) s = Deref (subst e1 s)
subst (Seq e1 e2) s = Seq (subst e1 s) (subst e2 s)
subst (Void) s = error "Can't substitute, Void is value"
subst (While e1 e2) s = While (subst e1 s) (subst e2 s) 
subst (Raise e) s = Raise (subst e s)
subst (Handle e1 x e2) (i,e) = if (x == i) || (elem x (frVars e)) 
                                then (Handle e1 x e2)
                                else if elem x (frVars e) 
                                     then subst (alphaExpr (Handle e1 x e2)) (i,e)
                                     else (Handle (subst e1 (i,e)) x (subst e2 (i,e)))
subst (LetCC i e1) (id,e) = if ((id == i) || (elem i (frVars e))) 
                            then (LetCC (incrVar i) (subst e1 (id,e))) 
                            else (LetCC i (subst e1 (id,e)))
subst (Continue e1 e2) s = Continue (subst e1 s) (subst e2 s)
subst (Cont p) s = error "Can't substitute stack with expression"


{--
Función alphaEq: verifica si dos expresiones dadas son alfa equivalentes
return: True si las dos expresiones son alfa-equivalentes
return: False si las dos expresiones no son alfa-equivalentes
--}
alphaEq :: Expr -> Expr -> Bool
alphaEq (V a) (V b) = (a == b)
alphaEq (I a) (I b) = (a == b)
alphaEq (B a)(B b) = (a == b)
alphaEq (Add a b)(Add c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Mul a b)(Mul c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (And a b)(And c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Or a b)(Or c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Lt a b)(Lt c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Gt a b)(Gt c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Eq a b)(Eq c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Succ a)(Succ b) = alphaEq a b
alphaEq (Pred a)(Pred b) = alphaEq a b
alphaEq (Not a)(Not b) = alphaEq a b
alphaEq (If a b c)(If d e f) = (alphaEq a d) && (alphaEq b e) && (alphaEq c f)
alphaEq (Let i1 a1 b1)(Let i2 a2 b2) = (alphaEq a1 a2) && (alphaEq b2 (subst b1 (i1,V i2)))
alphaEq (Fn x1 e1) (Fn x2 e2) = alphaEq e1 (subst e2 (x2, V x1))
alphaEq (Fix x1 e1) (Fix x2 e2) = alphaEq e1 (subst e2 (x2, V x1))
alphaEq (Assig e1 e2) (Assig e1' e2') = (alphaEq e1 e1') && (alphaEq e2 e2')  
alphaEq (Alloc e1) (Alloc e2) = alphaEq e1 e2
alphaEq (Deref e) (Deref e') = alphaEq e e' 
alphaEq (Seq e1 e2) (Seq e1' e2') = (alphaEq e1 e1') && (alphaEq e2 e2')
alphaEq (While e1 e2) (While e1' e2') =(alphaEq e1 e1') && (alphaEq e2 e2')
alphaEq (Raise e) (Raise e') = alphaEq e e'
alphaEq (Handle e1 i e2) (Handle e1' i' e2') = (alphaEq e1 e1') && (alphaEq e2' (subst e2 (i, V i')))
alphaEq (LetCC i e) (LetCC i' e') = alphaEq e (subst e' (i', V i))
alphaEq (Continue e1 e2) (Continue e1' e2') = (alphaEq e1 e1') && (alphaEq e2 e2')
alphaEq (Cont s) (Cont s') = s == s'
alphaEq _ _ = False