module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--- Punto #1 ---

type Intercambio = Number -> Cuenta -> Cuenta
type Mineria = Cuenta -> Cuenta

-- i
data Cuenta =
    Cuenta{
        identificador :: String,
        saldo :: Number 
    }deriving Show

data Transaccion= Intercambio | Mineria

--data Bloque = Bloque { transacciones :: [Transaccion] }

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

cuentas=[cuenta1,cuenta2,cuenta3,cuenta4]


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
correspondeId identificacion = (== identificacion).identificador 

--2

-- obtenerLaPrimeraSegun (correspondeId "2") cuentas ------------- Para testear
obtenerLaPrimeraSegun condicion = head . filter condicion
 
--3
eliminarLaPrimeraSegun condicion cuentas = 
    
    aux2
    where   aux = obtenerLaPrimeraSegun condicion cuentas
            aux2 = filter (esLaMisma aux) cuentas
            esLaMisma c c' = correspondeId c c' && (saldo cuenta == saldo cuenta')