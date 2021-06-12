module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--- Punto #1 ---
type Id = String

-- type Intercambio = Number -> Cuenta -> Cuenta
-- type Mineria = Cuenta -> Cuenta

type Bloque = [(Id,Transaccion)]

-- i
data Cuenta =
    Cuenta{
        identificador :: Id,
        saldo :: Number 
    }deriving Show

data Transaccion= --Intercambio | Mineria
        Intercambio {
            operacion :: Intercambio
        } | Mineria {
            operacion :: Mineria
        }

--data Bloque = Bloque { pares :: [Par] }

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

transaccion1::Transaccion
transaccion1 = Transaccion { minar cuenta1 }

transaccion2 = cobrar 400 cuenta2 

cuentas :: [Cuenta]
cuentas=[cuenta1,cuenta2,cuenta3,cuenta4]

------------------------------------------------------------------------------------------------

-- ii

-- a
--pagar :: Intercambio
pagar saldoAPagar cuenta = cuenta { saldo = saldo cuenta - saldoAPagar}  

-- b
--cobrar :: Intercambio
cobrar saldoACobrar cuenta = cuenta { saldo = saldoACobrar + saldo cuenta }

-- c
--minar :: Mineria
minar = cobrar 25


--- Punto #2 ---

--1
correspondeId identificacion = (== identificacion).identificador 

--2

-- obtenerLaPrimeraSegun (correspondeId "2") cuentas ------------- Para testear
obtenerLaPrimeraSegun condicion = head . filter condicion
 
--3

esLaMisma::Cuenta -> Cuenta -> Bool
esLaMisma cue cue' = correspondeId (identificador cue) cue' && (saldo cue == saldo cue')

eliminarLaPrimeraSegun condicion cuentas = 
    
    filter (not . esLaMisma aux) cuentas
    
    where   aux = obtenerLaPrimeraSegun condicion cuentas


--- Punto #3 ---

modificarSegun id cuentas trasformacion = 
    map trasformacion (filter (correspondeId id) cuentas) ++ filter (not . correspondeId id) cuentas



