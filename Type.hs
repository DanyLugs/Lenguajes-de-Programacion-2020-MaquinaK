module BAE.Type where

{--
Importamos la biblioteca List de Haskell    
--}
import qualified Data.List as List

--Sinonimo para el identificador como un entero
type Identifier = Int

infix :->

--Tipo de dato para definir nuestro propio Tipo llamado Type
data Type = T Identifier | Integer | Boolean  
          | Type :-> Type deriving (Show,Eq)


{--
Función vars: devuelve el conjunto de variables de tipo
return: una lista con las variables de tipo
--}
vars :: Type -> [Identifier]
vars (T t) = [t]
vars (t1 :-> t2) = List.union (vars t1) (vars t2)
vars _ = []

--Sinonimo para la sustitucion como una lista de duplas
--de identificadores con su tipo
type Substitution = [(Identifier,Type)]

{--
Función subst: aplica la sustitucion a un tipo dado
return: el tipo resultante despues de hacer la respectiva sustitucion
--}
subst :: Type -> Substitution -> Type
subst (T t) s = case s of
                    [] -> T t
                    ((x,t') : ss) -> if x == t then
                                        t'
                                        else
                                        subst (T t) ss 
subst (t1 :-> t2) s = (subst t1 s) :-> (subst t2 s)
subst t s = t


{--
Función comp: funcion que realiza la compisicion de dos sustituciones
return: una sustitucion compuesta con otra sustitucion
--}
comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = [(x, subst t s2) | (x,t) <- s1] `List.union` [(x,t) | (x,t) <- s2, List.notElem x [y | (y, t) <- s1]]

{--
Función simpl: elimina sustituciones redundantes
return: una lista sin las sustituciones redundantes
--}
simpl :: Substitution -> Substitution
simpl [] = []
simpl ((i,x):xs) = case x of
                     (T x') -> if (i /= x')
                               then (i,x):(simpl xs)
                               else simpl xs
                     (t1 :-> t2) -> (i,x) : (simpl xs)     
                     (x') -> (i,x) : (simpl xs)