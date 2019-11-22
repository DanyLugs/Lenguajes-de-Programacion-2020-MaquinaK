module BAE.Static where

{--
Importamos los modulos necesarios para implemenar Static.hs
Importamos funciones del paquete de List de Haskell
--}
import qualified Data.List as List
import BAE.Sintax
import BAE.Type
import BAE.Unifier

--Sinonimo para el contexto como una dupla de
--un identificador con su tipo
type Ctxt = [(BAE.Sintax.Identifier, BAE.Type.Type)]

{--
Función subst: funcion que aplica una sustitucion (de variables de tipo)
a un contexto dado
return: el contexto resultante de aplicar la susticucion
--}
subst :: Ctxt -> BAE.Type.Substitution -> Ctxt
subst [] _ = []
subst ((x, t) : cs) s = (x, BAE.Type.subst t s) : BAE.Static.subst cs s

{--
Función find: funcion que busca el tipo de una variable en un contexto dado.
return: devuelve el tipo en caso de encontrarlo
return: NOthing en otro caso
--}
find :: BAE.Sintax.Identifier -> Ctxt -> Maybe Type
find _ [] = Nothing
find x ((y, t): cs) = if x == y then
                        Just t
                        else
                        find x cs

--Sinonimo para definir restricciones que es una lista de duplas de tipos con tipos
type Constraint = [(BAE.Type.Type, BAE.Type.Type)]


{--
Función fresh': funcion auxiliar para ser usada posteriormente por la funcion fresh
Verifica si el tipo ya existe en las variables
return: si si existe, crea un nuevo tipo (fresco)
return: si no existe, lo deja en las variables
--}
fresh' :: BAE.Type.Type -> [BAE.Type.Type] -> BAE.Type.Type
fresh' (T n) vars = if List.elem (T n) vars
                    then fresh' (T $ succ n) vars
                    else (T n)

{--
Función fresh: dado un conjunto de variables de tipo, obtiene una variable
de tipo fresca, es decir, que no aparece en este conjunto.
return: una variable fresca usando la funcion auxiliar fresh'
--}
fresh :: [BAE.Type.Type] -> BAE.Type.Type
fresh vars = fresh' (T 0) vars                      

{--
Función infer': dada una expresion, infiere su tipo implementando las reglas 
anteriormente
Utiliza el conjunto de variables de tipo apra crear variables de tipo frescas
durante la ejecucion.
return: el contexto y el conjunto de restricciones donde es valido.
--}
infer' :: ([BAE.Type.Type], BAE.Sintax.Expr) -> ([BAE.Type.Type], Ctxt, BAE.Type.Type, Constraint)
infer' (nv, (BAE.Sintax.V x)) = let t = fresh nv
                                    nv' = nv `List.union` [t]
                            in 
                                (nv' , [(x, t)],  t,  [])                              
infer' (nv, (BAE.Sintax.I n)) = (nv, [] , Integer, [])
infer' (nv, (BAE.Sintax.B b)) = (nv, [] , Boolean, [])
infer' (nv, (BAE.Sintax.Add e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                          (nv2, g2, t2, r2) = infer' (nv1,e2)
                                          s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                          g = g1 `List.union` g2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                   in 
                                      (nv2,g,Integer,r)       
infer' (nv, (BAE.Sintax.Mul e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                          (nv2, g2, t2, r2) = infer' (nv1,e2)
                                          s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                          g = g1 `List.union` g2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                   in 
                                      (nv2,g,Integer,r)
infer' (nv, (BAE.Sintax.Succ e)) = let (nv',g,t,re) = infer'(nv,e)
                                       r = re `List.union` [(t,Integer)]
                                in 
                                    (nv',g,Integer,r)
infer' (nv, (BAE.Sintax.Pred e)) = let (nv',g,t,re) = infer'(nv,e)
                                       r = re `List.union` [(t,Integer)]
                                in 
                                    (nv',g,Integer,r)
infer' (nv, (BAE.Sintax.Not e)) = let (nv' , g , t , re) = infer'(nv,e)
                                      r = re `List.union` [(t,Boolean)]
                               in (nv',g,Boolean,r)
infer' (nv, (BAE.Sintax.And e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                          (nv2, g2, t2, r2) = infer' (nv1,e2)
                                          s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                          g = g1 `List.union` g2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Boolean),(t2,Boolean)]
                                    in 
                                      (nv2,g,Boolean,r)
infer' (nv, (BAE.Sintax.Or e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                         (nv2, g2, t2, r2) = infer' (nv1,e2)
                                         s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                         g = g1 `List.union` g2
                                         r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Boolean),(t2,Boolean)]
                                   in 
                                      (nv2,g,Boolean,r)
infer' (nv, (BAE.Sintax.Gt e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                         (nv2, g2, t2, r2) = infer' (nv1,e2)
                                         s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                         g = g1 `List.union` g2
                                         r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                   in 
                                     (nv2,g,Boolean,r)
infer' (nv, (BAE.Sintax.Lt e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                         (nv2, g2, t2, r2) = infer' (nv1,e2)
                                         s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                         g = g1 `List.union` g2
                                         r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                   in 
                                      (nv2,g,Boolean,r)
infer' (nv, (BAE.Sintax.Eq e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                         (nv2, g2, t2, r2) = infer' (nv1,e2)
                                         s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                         g = g1 `List.union` g2
                                         r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                   in 
                                     (nv2,g,Boolean,r)
infer' (nv, (BAE.Sintax.If i e1 e2)) = let (nvi, gi, ti, ri) = infer' (nv,i)
                                           (nv1, g1, t1, r1) = infer' (nvi,e1)
                                           (nv2, g2, t2, r2) = infer' (nv1,e2)
                                           s1s2 = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                           si' = [(s1,s2)|(x,s1) <- g1, (y,s2) <- gi, x==y]
                                           si'' = [(s1,s2)|(x,s1) <- gi, (y,s2) <- g2, x==y]
                                           g = gi `List.union` g1 `List.union` g2
                                           r = ri `List.union` r1 `List.union` r2 `List.union` s1s2 `List.union` si' `List.union` si'' `List.union` [(t1,t2),(ti,Boolean)]
                                   in (nv2,g,t1,r)
infer' (nv, (BAE.Sintax.Let x e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                            (nv2, g2, t2, r2) = infer' (nv1,e2)
                                            s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                            g = g1 `List.union` g2
                                            r = r1 `List.union` r2 `List.union` s
                                   in case find x g2 of
                                              Just x' -> let r' = r `List.union` [(t1,x')]
                                                             g' = g List.\\  [(x,x')]
                                                         in (nv2,g',t2,r')
                                              Nothing -> let x' = fresh nv2
                                                             nv' = nv2 `List.union` [x']
                                                             r' = r `List.union` [(t1,x')]
                                                         in (nv',g,t2,r')                                                                     
infer' (nv ,(BAE.Sintax.Fix x e)) = let (nv', g, t, c) = infer' (nv, e)
                                in case find x g of
                                  Just t' -> let g' = filter (\d -> x /= fst d) g
                                                 c' = c `List.union` [(t',t)]
                                             in   
                                              (nv', g', t, c')  
                                  Nothing -> let z = fresh nv' 
                                                 nv'' = nv' `List.union` [z]
                                             in
                                            (nv', g, t, c)
infer' (nv, (BAE.Sintax.Fn x e)) = let (nv', g, t, r) = infer' (nv, e)
                               in case find x g of
                                  Just t' -> (nv', g List.\\ [(x, t')], t' :-> t, r)
                                  Nothing -> let t' = fresh nv' 
                                                 nv'' = nv' `List.union` [t']
                                              in
                                                 (nv'', g, t' :-> t, r) 
infer' (nv, (BAE.Sintax.App e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv, e1)
                                          (nv2, g2, t2, r2) = infer' (nv1, e2)
                                          s = [(s1,s2) | (x,s1) <- g1, (y,s2) <- g2, x == y]
                                          z = fresh nv2
                                          nv' = nv2 `List.union` [z]
                                   in 
                                      (nv', (g1 `List.union` g2), z, r1 `List.union` r2 `List.union` s `List.union` [(t1,t2 :-> z)])


{--
Función infer: dada una expresion, infiere su tipo devolviendo el contexto donde es valido.
Utiliza el algoritmo de Martelli Montanari (Unificado mas general)
return: el contexto donde es valida esa expresion
--}
infer :: BAE.Sintax.Expr -> (Ctxt, BAE.Type.Type)
infer e = let (_, g, t, r) = infer' ([], e)
              [umg] = µ r --unificador
          in
              (BAE.Static.subst g umg, BAE.Type.subst t umg)                          
