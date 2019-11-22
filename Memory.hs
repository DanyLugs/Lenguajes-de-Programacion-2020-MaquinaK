module BAE.Memory where

import BAE.Sintax   
import Data.List 
import Data.Char
import Data.String

--Sinonimo para direccion de memoria a un entero
type Address = Int

--Sinonimo para valores
type Value = Expr

--Sinonimo para una celda contenida en memoria
type Cell = (Address, Value)

--Sinonimo de memoria a una lista de celdas
type Memory = [Cell]

{--
Función newAddress: dada una memoria, genera una nueva dirección de memoria
que no este contenida en esta
return: una expresión con la nueva direccion de memoria (fresh)
--}
newAddress :: Memory -> Value
newAddress m =  newAddressAux 0 (newAddressError m)  

{--
Función newAddressAux: dada una dirección y una lista de direcciones,
crea una nueva direccion en memoria que no este contenida
return: un valor, que es el valor fresco de la lista de direcciones
--}
newAddressAux :: Address -> [Address] -> Value
newAddressAux _ [] = L 0
newAddressAux n l = if n `elem` l
                    then newAddressAux (n+1) l
                    else L n    

{--
Función newAddressError: dada una memoria, regresa una lista de direcciones,
verifica si esa direccion esta contenida en la lista de direcciones
return: error si la direccion si esta contenida en la lista de direcciones
--}
newAddressError :: Memory -> [Address]
newAddressError [] = []
newAddressError ((a,v):xs) = if elem a (newAddressError xs)
                             then error "Corrupted Memory"
                             else a:(newAddressError xs)

{--
Función access: dada una direccion de memoria, devuelve el valor contenido
en la celda con tal direccion, en caso de no encontrarla devuelve Nothing
return: Nothing si no encuentra la celda con tal direccion
--}
access :: Address -> Memory -> Maybe Value
access a [] = Nothing
access a m@((ad,v):xs) = if a == ad
                         then Just v
                         else access a (tail m) 

{--
Función update: dada una celda de memoria, actualiza el valor de esta
misma en la memoria. En caso de no existir debe devolver Nothing
return: Nothing si no existe dicha celda de memoria para poder actualizarla
--}
update :: Cell -> Memory -> Maybe Memory
update c [] = Nothing
update (a1,v1) ((a2,v2):xs)
    | (isValue' v1) && (a1 == a2) = Just ((a2,v1):xs)
    | (isValue' v1) = update (a1,v1) xs
    | otherwise = error "Memory can only store values"

{--
Funcion auxiliar isValue': verifica si una expresion es valor
return: True si se verifica un Integer o un Boolean o una dirección de memoria ya que Address = Int
return: False si es de cualquier otra forma que no sea Integer o Boolean (valores)
--}
isValue' :: Expr -> Bool
isValue' (I n) = True
isValue' (B b) = True
isValue' (Fix x e) = True
isValue' (L n) = True
isValue' e = False
