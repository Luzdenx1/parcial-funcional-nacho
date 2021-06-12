module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--- Punto #1 ---
type Id = String

type Intercambio = Number -> Cuenta -> Cuenta
type Mineria = Cuenta -> Cuenta

type Bloque = [(Id,Transaccion)]

-- i
data Cuenta =
    Cuenta{
        identificador :: Id,
        saldo :: Number 
    }deriving Show

instance Eq Cuenta where
    (==) cuenta cuenta' = correspondeId (identificador cuenta) cuenta' && (saldo cuenta == saldo cuenta')


data Transaccion = Intercambio | Mineria


-- ii

-- a
pagar :: Intercambio
pagar saldoAPagar cuenta = cuenta { saldo = saldo cuenta - saldoAPagar}  

-- b
cobrar :: Intercambio
cobrar saldoACobrar = \cuenta -> cuenta { saldo = saldoACobrar + saldo cuenta }


-- c
minar :: Mineria
minar = cobrar 25


--- Punto #2 ---

--1
correspondeId :: Id -> Cuenta -> Bool
correspondeId identificacion = (== identificacion).identificador 

--2

obtenerLaPrimeraSegun :: (c -> Bool) -> [c] -> c
obtenerLaPrimeraSegun condicion = head . filter condicion
 
--3

eliminarLaPrimeraSegun :: (Cuenta -> Bool) -> [Cuenta] -> [Cuenta]
eliminarLaPrimeraSegun condicion cuentas = 
    
    filter (not . (==) primeraQueCumple) cuentas
    
    where   primeraQueCumple = obtenerLaPrimeraSegun condicion cuentas
            

--- Punto #3 ---

modificarSegun :: Id -> [Cuenta] -> (Cuenta -> Cuenta) -> [Cuenta]
modificarSegun id cuentas trasformacion = 
    map trasformacion (filter (correspondeId id) cuentas) ++ filter (not . correspondeId id) cuentas


--- Punto #4 ---

--- Punto #5 ---

sonEstables :: [Cuenta] -> Bool
sonEstables = all ((>=0).saldo) 

--- Punto #6 ---


--- Punto #7 ---

funcionSinPudor :: [[Number]] -> (Number -> Number, a -> a) -> a -> a
funcionSinPudor x y 
    | (length . filter even . map (fst y) $ head x) > 10 =  id 
    | otherwise = snd y

-- Procedimiento de inferencia --
{-
    Primero se acomodo la funcion para tener un mejor "panorama". Al observar lo que retorna la funcion podemos notar que "x" 
    es una lista ya que se le esta aplicando la funcion head, mientras que "y" es una tupla ya que se le aplica la funcion fst.
    
    Dicho esto, se puede identificar a "y" como una tupla de funciones ya que luego de que se extrae el primer elemento de la 
    tupla, se utiliza como "transformacion" en un map.

    Luego se hace uso de un filter con la condicion de que los elementos sean pares, por lo que se puede deducir que "x" es una
    lista de listas de numeros.
    
    Ademas, se puede observar que ese filtro se aplica solo a la primer lista de numeros de x (por el head), obviamente
    luego de hacerle la transformacion, para luego medir la longitud de la misma. En el caso que la longitud de esa lista sea menor a 10
    entonces se aplica la identidad y en caso contrario se devuelve el segundo elemento 
    de "y" que es una funcion. 

    En base a esto ultimo, podemos ver tambien que la "funcionsSinPudor" se esta aplicando parcialmente a un elemento cuyo tipo no se
    puede determinar ya que en ambos casos de las guardas, se le aplica una funcion que no tiene un tipo preestablecido. En el caso de 
    la identidad, devuelve el mismo elemento. Y en el otherwise, devuelve la segunda funcion de "y" que tampoco tiene un tipo definido ni
    ninguna operacion que lo haga. 
-}
