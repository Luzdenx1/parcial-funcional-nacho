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

data Transaccion = Intercambio | Mineria


--tests

cuenta1=
    Cuenta{
        identificador="1",
        saldo = 1200
    }


cuenta2=
    Cuenta{
        identificador="2",
        saldo = 1500
    }


cuenta3=
    Cuenta{
        identificador="3",
        saldo = 180000
    }

cuenta4=
    Cuenta{
        identificador="3",
        saldo = 180000000
    }

transaccion1 = minar cuenta1

transaccion2 = cobrar 400 cuenta2 

cuentas :: [Cuenta]
cuentas=[cuenta1,cuenta2,cuenta3,cuenta4]

bloque1 = [ (identificador cuenta1, minar cuenta1), (identificador cuenta2, cobrar 200 cuenta2), (identificador cuenta3, pagar 400 cuenta3) ]


------------------------------------------------------------------------------------------------

-- ii

-- a
pagar :: Intercambio
pagar saldoAPagar cuenta = cuenta { saldo = saldo cuenta - saldoAPagar}  

-- b
cobrar :: Intercambio
cobrar saldoACobrar cuenta = cuenta { saldo = saldoACobrar + saldo cuenta }

-- c
minar :: Mineria
minar = cobrar 25


--- Punto #2 ---

--1
correspondeId :: Id -> Cuenta -> Bool
correspondeId identificacion = (== identificacion).identificador 

--2

-- obtenerLaPrimeraSegun (correspondeId "2") cuentas ------------- Para testear
obtenerLaPrimeraSegun :: (c -> Bool) -> [c] -> c
obtenerLaPrimeraSegun condicion = head . filter condicion
 
--3

esLaMisma::Cuenta -> Cuenta -> Bool
esLaMisma cue cue' = correspondeId (identificador cue) cue' && (saldo cue == saldo cue')

eliminarLaPrimeraSegun :: (Cuenta -> Bool) -> [Cuenta] -> [Cuenta]
eliminarLaPrimeraSegun condicion cuentas = 
    
    filter (not . esLaMisma aux) cuentas
    
    where   aux = obtenerLaPrimeraSegun condicion cuentas


--- Punto #3 ---

modificarSegun :: Id -> [Cuenta] -> (Cuenta -> Cuenta) -> [Cuenta]
modificarSegun id cuentas trasformacion = 
    map trasformacion (filter (correspondeId id) cuentas) ++ filter (not . correspondeId id) cuentas

--- Punto #5 ---

sonEstables :: [Cuenta] -> Bool
sonEstables = all ((>=0).saldo) 

--- Punto #7 ---

---------------------------------------------------------------- Tests

lista1=[1..20]
lista2=[1..50]

tupla :: (Number -> Number -> Number, Number -> Number -> Number)
tupla=( (+) , (*) )

listaDeListas=[lista1,lista2]

listaPalabras=["hoal","chau"]



----------------------------------------------------------------------

--funcionSinPudor :: [[Number]] -> ( Number -> Number , String -> String ) -> String -> String

funcionSinPudor :: [[Number]] -> (Number -> Number, b -> b) -> b -> b
funcionSinPudor x y 
    | (length . filter even . map (fst y) $ head x) > 10 =  id 
    | otherwise = snd y

-- map::(a->a) -> [a] -> [a]

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
    puede determinar 

-}
