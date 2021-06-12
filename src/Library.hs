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

data Transaccion=
    Intercambio{
        
    }
    | 
    Mineria{

    }

-- data Transaccion=
--                 Mineria {
--                     --minar :: Cuenta -> Cuenta
--                 }
--                 |
--                 Cobro { 
--                     --cobrar :: Number -> Cuenta -> Cuenta
--                 } 
--                 | 
--                 Pago { 
--                     --pagar :: Number -> Cuenta -> Cuenta
--                     --pagar :: Pago
--                 }
--     deriving (Show, Eq)


data Bloque = Bloque { transacciones :: [] } deriving (Show, Eq)

--tests

cuenta1=
    Cuenta{
        identificador="1",
        saldo = 1200
    }


-- ii

-- a
pagar :: Pago
pagar cuenta saldoAPagar = cuenta{ saldo = saldoAPagar + saldo cuenta}  

