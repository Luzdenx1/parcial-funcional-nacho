module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--- Punto #1 ---

-- i
data Cuenta =
    Cuenta{
        identificador :: String,
        saldo :: Number 
    }deriving Show

data Transaccion=
                Mineria {
                    minar :: Cuenta -> Cuenta
                }
                |
                Cobro { 
                    cobrar :: Number -> Cuenta -> Cuenta
                } 
                | 
                Pago { 
                    pagar :: Number -> Cuenta -> Cuenta
                }
    deriving (Show, Eq)


data Bloque = Bloque { transacciones :: [Transaccion] } deriving (Show, Eq)

--tests

cuenta1=
    Cuenta{
        identificador="1",
        saldo = 1200
    }


-- ii

-- a
--      pago :: Number -> Cuenta -> Cuenta
paga cuenta saldoAPagar = cuenta{ saldo = saldoAPagar + saldo cuenta}  

