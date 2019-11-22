module BAE.Dynamic where

--import qualified Sintax as Sintax
import BAE.Sintax
import BAE.Memory
import BAE.Type
import BAE.Static

--Sinonimo de contexto
type Ctx = [Decl]

--Sinonimo de declaracion de tipo con un identificador
type Decl = (BAE.Sintax.Identifier,Type)

--Tipo de dato para representar:
--E = Evaluar
--R = Devuelve
--P = Propagar (error)
data State = E (Memory, Stack, Expr)
           | R (Memory, Stack, Expr)
           | P (Memory, Stack, Expr) deriving (Show)


{--
Funcion eval1: funcion que evalua un solo paso de la ejecución
return: devuelve el siguiente paso de la expresion que reciba
return: devuelve error si tenemos valores tales como: Boolean o Integer
--}
eval1 :: State -> State
eval1 (E (m,s,e)) = case e of
                    (V _) -> R (m,s,e)
                    (I _) -> R (m,s,e)
                    (B _) -> R (m,s,e)
                    (Fn x e) -> E (m,(FnF x ()) : s,e) 
                    (Fix x e) -> E (m,s, BAE.Sintax.subst e (x,Fix x e))
                    (Succ e) -> E (m,((SuccF ()) : s), e)
                    (Pred e) -> E (m,((PredF ()) : s), e)
                    (Add e1 e2) -> E (m,((AddFL () e2) : s), e1)
                    (Mul e1 e2) -> E (m,((MulFL () e2) : s), e1)
                    (Not e) -> E (m,((NotF ()) : s), e)
                    (And e1 e2) -> E (m,((AndFL () e2) : s), e)
                    (Or e1 e2) -> E (m,((OrFL () e2) : s), e)
                    (Lt e1 e2) -> E (m,((LtFL () e2) : s), e)
                    (Gt e1 e2) -> E (m,((GtFL () e2) : s), e)
                    (Eq e1 e2) -> E (m,((EqFL () e2) : s), e)
                    (App e1 e2) -> E (m,((AppFL () e2) : s), e1)
                    (If ec e1 e2) -> E (m,((IfF () e1 e2) : s), ec)
                    (Let x e1 e2) -> E (m,((LetF x () e2) : s), e1)
                    (Assig e1 e2) ->  E (m,((AssigFL () e2) : s), e1)
                    (Alloc e) -> E (m,((AllocF ()): s), e)
                    (Deref e) -> E (m,((DerefF ()): s), e)
                    (Seq e1 e2) -> E (m,((SeqFL () e2): s), e1)
                    (Void) -> P (m,s,e)
                    (While e1 e2) -> E (m,((WhileF () e2): s), e1)
                    (Raise e) -> E (m,((RaiseF ()): s), e)
                    (Handle e1 x e2) -> E (m,((HandleF () x e2):s),e1)
                    (LetCC x e) -> E (m,s, BAE.Sintax.subst e (x, (Cont s)))
                    (Continue e1 e2) -> E (m,((ContinueFL () e2): s), e1)
eval1 (R (m,s,e)) = case e of
                    (V x) -> case s of
                                ((FnF _ _) : s') -> P (m,s, Raise e)
                                ((SuccF _) : s') -> P (m,s, Raise e) 
                                ((PredF _) : s') -> P (m,s, Raise e)
                                ((AddFL _ _) : s') -> P (m,s, Raise e)
                                ((MulFL _ _) : s') -> P (m,s, Raise e)
                                ((NotF _) : s') -> P (m,s, Raise e)
                                ((AndFL _ _) : s') -> P (m,s, Raise e)
                                ((OrFL _ _) : s') -> P (m,s, Raise e)
                                ((LtFL _ _) : s') -> P (m,s, Raise e)
                                ((GtFL _ _) : s') -> P (m,s, Raise e)
                                ((EqFL _ _) : s') -> P (m,s, Raise e)
                                ((IfF _ _ _) : s') -> P (m,s, Raise e)
                                ((AppFL _ _) : s') -> P (m,s, Raise e)
                                ((AssigFL _ e2) : s') -> E (m, ((AssigFR e () ) : s'), e2)
                                ((AllocF _) : s') -> let (L n) = newAddress m in E (((n,e):m), s,(L n))
                                ((DerefF _) : s') -> P (m,s, Raise e)
                                ((SeqFL _ _) : s' ) -> P (m,s, Raise e)
                                ((WhileF _ _) : s') -> P (m,s, Raise e)
                                ((RaiseF _) : s') -> P (m,s, Raise e)
                                ((HandleF _ x e2) : s') -> R (m,s,e)
                                ((ContinueFL _ e2) : s') -> E (m, ((ContinueFR e ()) : s'), e2)
                                ((LetCCF x _) : s') -> P (m,s, Raise e)
                                ((LetF x _ e2) : s') -> E (m,s', BAE.Sintax.subst e2 (x,e))
                                _ -> P (m,s, Raise e)
                    (I m') -> case s of 
                                ((FnF x _) : s') -> R (m,s', Fn x e) 
                                ((SuccF _) : s') -> R (m,s', Succ e)
                                ((PredF _) : s') -> R (m,s', Pred e)
                                ((AddFL _ e2) : s') -> E (m,(AddFR e () : s'), e2)
                                ((AddFR (I n) _) : s') -> R (m,s', I (n + m'))
                                ((MulFL _ e2) : s') -> E (m,(MulFR e () : s'), e2)
                                ((MulFR (I n) _) : s') -> R (m,s',I (n * m'))
                                ((AndFL _ _) : s') -> P (m,s, Raise e)
                                ((OrFR _ _) : s') -> P (m,s, Raise e)
                                ((AndFR _ _) : s') -> P (m,s, Raise e)
                                ((OrFL _ _) : s') -> P (m,s, Raise e)
                                ((LtFL _ e2) : s') -> E (m,(LtFR e () : s'), e2)
                                ((LtFR (I n) _) : s') -> R (m,s', B (n < m'))
                                ((GtFL _ e2) : s') -> E (m,(GtFR e () : s'), e2)
                                ((GtFR (I n) _) : s') -> R (m,s',B (n > m'))
                                ((EqFL _ e2) : s') -> E (m,(EqFR e () : s'), e2)
                                ((EqFR (I n) _) : s') -> R (m,s',B (n == m'))
                                ((IfF _ _ _) : s') -> P (m,s, Raise e)
                                ((LetF x _ e2) : s') -> E (m,s', BAE.Sintax.subst e2 (x,e))
                                ((AppFL _ _) : s') -> P (m,s, Raise e)
                                ((AssigFL _ e2) : s') -> E (m, ((AssigFR e () ) : s'), e2)
                                ((AppFR _ _) : s') -> P (m,s, Raise e)
                                ((AllocF _) : s') -> let (L n) = newAddress m in E (((n,e):m), s,(L n))
                                ((DerefF _) : s') -> P (m,s, Raise e)
                                ((SeqFL _ e2) : s' ) -> E (m,((SeqFR e ()):s),e2)
                                ((SeqFR _ _) : s') -> P (m,s, Raise e)
                                ((WhileF _ _) : s') -> P (m,s, Raise e)
                                ((RaiseF _) : s') -> P (m,s, Raise e)
                                ((HandleF _ x e2) : s') -> R (m,s,e)
                                ((ContinueFL _ e2) : s') -> E (m, ((ContinueFR e ()) : s'), e2)
                                ((LetCCF x _) : s') -> P (m,s, Raise e)
                                _ -> P (m,s, Raise e)    
                    (B q) -> case s of
                                ((FnF x _) : s') -> R (m,s', Fn x e)
                                ((SuccF _) : s') -> P (m,s, Raise e) 
                                ((PredF _) : s') -> P (m,s, Raise e)
                                ((NotF _) : s') -> R (m,s', B (not q))
                                ((AddFL _ e2) : s') -> P (m,s, Raise e)
                                ((MulFL _ e2) : s') -> P (m,s, Raise e)
                                ((AndFL _ e2) : s') -> E (m,(AndFR e () : s'), e)
                                ((AndFR (B q') _) : s') -> R ((m,s', B (q && q')))
                                ((OrFL _ e2) : s') -> E (m,(OrFR e () : s'), e)
                                ((OrFR (B q') _) : s') -> R ((m,s', B (q || q')))
                                ((LtFL _ _) : s') -> P (m,s, Raise e)
                                ((GtFL _ _) : s') -> P (m,s, Raise e)
                                ((EqFL _ _) : s') -> P (m,s, Raise e)
                                ((IfF _ e1 e2) : s') -> E (m,s', if q then e1 else e2) 
                                ((LetF x _ e2) : s') -> E (m,s', BAE.Sintax.subst e2 (x,e))
                                ((AppFL _ _) : s') -> P (m,s, Raise e)
                                ((AssigFL _ e2) : s') -> E (m, ((AssigFR e () ) : s'), e2)
                                ((AppFR _ _) : s') -> P (m,s, Raise e)
                                ((AllocF _) : s') -> let (L n) = newAddress m in E (((n,e):m), s,(L n))
                                ((DerefF _) : s') -> P (m,s, Raise e)
                                ((SeqFL _ e2) : s' ) -> E (m,((SeqFR e ()):s),e2)
                                ((SeqFR _ _) : s') -> P (m,s, Raise e)
                                ((WhileF _ _) : s') -> P (m,s, Raise e)
                                ((RaiseF _) : s') -> P (m,s, Raise e)
                                ((HandleF _ x e2) : s') -> R (m,s,e)
                                ((ContinueFL _ e2) : s') -> E (m, ((ContinueFR e ()) : s'), e2)
                                ((LetCCF x _) : s') -> P (m,s, Raise e)
                                _ -> P (m,s, Raise e) 
                    (Fn x e1) -> case s of
                                ((FnF x _) : s') -> R (m,s', Fn x e)
                                ((AppFL _ e2) : s') -> E (m,s', BAE.Sintax.subst e1 (x,e2))
                                ((AppFR (Fn x e1) _) : s') -> E (m,s,BAE.Sintax.subst e (x,e1))
                                ((LetF x _ e2) : s') -> E (m,s', BAE.Sintax.subst e2 (x,e))                                                    
                                _ -> P (m,s, Raise e)
                    (L n) -> case s of
                                ((DerefF _) : s') -> case access n m of
                                                         Just e' -> R (m,s,e')
                                ((AssigFL _ e2) : s') -> E (m, ((AssigFR e ()):s), e2) 
                                ((AssigFR e1 _) : s') -> case update (n,e1) m of
                                                            Just m' -> R (m',s,Void)
                                _ -> P (m,s, Raise e)
eval1 (P (m, s, Raise e)) = case s of 
                            (f : s') -> P (m,s', Raise e)
                            _ -> R ([],[],e)


{--
Funcion evals: recibe un estado de la maquina K
return: devuelve el un estado derivado de evaluar varias veces hasta 
obtener la pila vacia
--}
evals :: State -> State
evals (E (m,s,e)) = evals (eval1 (E (m,s,e)))
evals (R (m,[],e)) = R (m,[],e)
evals (R (m,s,e)) = evals (eval1 (R (m,s,e)))
evals (P (m,[],e)) = P(m,[],e)
evals (P (m,s,e)) = evals (eval1 (P (m,s,e)))      


{--
Funcion evale: recibe una expresion EAB, la evalua con la maquina K y
devuelve un valor si, iniciando con la pila vacia esta devuelve un valor
con la pila vacia
--}
evale :: Expr -> Expr
evale e = case evals (E ([],[],e)) of
                R (m',[],e') -> if BAE.Memory.isValue' e'
                                then e'
                                else error "Error"
                _ -> error "Error"     


{--
Funcion locked: nos dice si una expresion esta bloqueada
return False: si la Expr puede seguir siendo evaluada o si no es aplicada a un valor
return True: si la Expr llega a un valor o es un valor
--}
locked :: Expr -> Bool
locked (V x) = True
locked (I n) = True
locked (B False) = True
locked (B True) = True
locked (Add (I n) (I m)) = False
locked (Add (I n) e2) = (locked e2)
locked (Add e1 e2) = (locked e1)
locked (Mul (I n) (I m)) = False
locked (Mul (I n) e2) = (locked e2)
locked (Mul e1 e2) = (locked e1)
locked (Succ (I n)) = False
locked (Succ n) = (locked n)
locked (Pred (I n)) = False
locked (Pred n) = (locked n)
locked (Not (B b)) = False
locked (Not n) = (locked n)
locked (And (B b1) (B b2)) = False
locked (And (B b) e2) = (locked e2) 
locked (And b1 b2) = (locked b1)
locked (Or (B b1) (B b2)) = False
locked (Or (B True) e2) = (locked e2)
locked (Or b1 b2) = (locked b1)
locked (Lt (I n) (I m)) = False
locked (Lt (I n) m) = (locked m)
locked (Lt n m) = (locked n)
locked (Gt (I n) (I m)) = False
locked (Gt (I n) m) = (locked m)
locked (Gt n m) = (locked n)
locked (Eq (I n) (I m)) = False
locked (Eq (I n) m) = (locked m)
locked (Eq n m) = (locked n)
locked (If (B True) n m) = False
locked (If (B False) n m) = False
locked (If n m o) = (locked n)
locked (Let x (I n) e2) = False
locked (Let x (V n) e2) = False
locked (Let x (B b) e2) = False
locked (Let x (Fix _ _) _) = False
locked (Let x e1 e2) = False
locked (Fn i e) = (locked e)
locked (Fix x e) = True
locked (App (Fn x e) e2) = False 
locked (App e1 e2) = (locked e1) && (locked e2)
locked (Assig e1 e2) = (locked e1) && (locked e2)
locked (Alloc e) = (locked e)
locked (Deref e) = (locked e)
locked (Seq e1 e2) = False
locked (While e1 e2) = False

