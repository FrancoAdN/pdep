import Data.List (isPrefixOf)
import Distribution.PackageDescription (BuildInfo(otherExtensions))

type Actor = String;
data Filmacion = Filmacion { titulo :: String, puntaje :: Int, lanzamiento :: Int, duracion :: Int, actores :: [String]} deriving Show 

armaMortal = Filmacion "Arma Mortal" 7 1987 109 ["Mel Gibson", "Danny Glover", "Gary Busey"]
reinas = Filmacion "9 Reinas" 8 2000 114 ["Gastón Pauls", "Ricardo Darín", "Leticia Bredice","Pochi Ducasse"]
odisea = Filmacion "La odisea de los giles" 8 2019 116 ["Ricardo Darín", "Luis Brandoni", "Verónica Llinás", "Daniel Aráoz", "Rita Cortese"]
flor = Filmacion "La Flor" 7 2018 840 ["Pilar Gamboa"]
speed = Filmacion "Speed" 7 1994 116 ["Keanu Reeves", "Sandra Bullock", "Dennis Hopper", "Jeff Daniels", "Alan Ruck"]
indianaIV = Filmacion "Indiana Jones IV" 6 2007 125 ["Harrison Ford"]
indianaI =  Filmacion "Indiana Jones I" 8 1981 115 ["Harrison Ford"]

filmaciones = [armaMortal, reinas, odisea, flor, speed, indianaIV, indianaI]





esDarin :: Actor -> Bool
esDarin act = act == "Ricardo Darín"

esDarinesca :: Filmacion -> Bool
esDarinesca film = (esDarin.head) (actores film)

pintaBuena :: Filmacion -> Bool
pintaBuena film = length (actores film) >= 5

excedentes :: Filmacion -> Int
excedentes film = abs ((duracion film) - 115)


esVieja :: Filmacion -> Bool
esVieja film  = (lanzamiento film) < 1990

precioBase:: Filmacion -> Int
precioBase film | pintaBuena(film) = 200
                | esVieja(film) = length (titulo film) * 2
                | otherwise = 100 + ((puntaje film) * 3)


extraFilmacionLarga :: Filmacion -> Int
extraFilmacionLarga film | (excedentes(film) * 10) < 100 = excedentes(film) * 10
                         | otherwise = 100

precioExtra :: Filmacion -> Int
precioExtra film | (duracion film) > 115 = extraFilmacionLarga(film)
                 | (not.esVieja) film = 50
                 | otherwise = 0



aplicarDescuento :: Int -> Int
aplicarDescuento num
    | num < 200 = num
    | otherwise = round (fromIntegral num * 0.9)


precioTotal :: Filmacion -> Int
precioTotal film = aplicarDescuento (precioBase(film) + precioExtra(film)) 


type Nombre = String;
data Persona = Persona { 
  nombre :: Nombre,
  satisfaccion :: Int,
  edad :: Int, 
  cantidadVistas :: Int,
  credito :: Int
} deriving Show 

 


actualizarCredito :: Int -> Int -> Int
actualizarCredito credito precio | precio > credito = 0
                                 | otherwise = credito - precio

aplicarPelicula :: Persona -> Filmacion -> Persona
aplicarPelicula p f = p {cantidadVistas = 1 + cantidadVistas p, credito = actualizarCredito (credito p) (precioTotal f)}


type LtsSangre = Int;
terror :: Persona -> LtsSangre -> Persona
terror p lts = p { satisfaccion = (satisfaccion p) - lts}


aplicarTerror :: Persona -> Filmacion -> LtsSangre -> Persona
aplicarTerror p f lts = aplicarPelicula (terror p lts) f


comedia :: Persona -> Persona
comedia p = p { nombre = nombre p ++ " muy alegre", satisfaccion = 2 * satisfaccion p }

aplicarComedia :: Persona -> Filmacion -> Persona
aplicarComedia p f = (aplicarPelicula.comedia) p f


type EscenasFelices = Int
drama :: Persona -> EscenasFelices -> Persona
drama p escenas = p { edad = edad p + 1, satisfaccion = (min 3 escenas) + satisfaccion p}

aplicarDrama :: Persona -> EscenasFelices -> Filmacion -> Persona
aplicarDrama p escenas f = aplicarPelicula (drama p escenas) f

accion :: Persona -> Filmacion -> Persona
accion p film | pintaBuena(film) = p { satisfaccion = 100 + satisfaccion p}
              | otherwise = p

aplicarAccion :: Persona -> Filmacion -> Persona
aplicarAccion p f = aplicarPelicula (accion p f) f


tragicomico :: Persona -> Persona
tragicomico p = (drama.comedia) p 4


aplicarTragicomico :: Persona -> Filmacion -> Persona
aplicarTragicomico p f = aplicarPelicula (aplicarPelicula (tragicomico p) f) f



terminaConVersion :: VersionMala -> String -> Bool
terminaConVersion v titulo = v == drop (length titulo - length v) titulo

type VersionMala = String
aventura :: Persona -> Filmacion -> VersionMala -> Persona
aventura p f v | terminaConVersion v (titulo f) = p
               | otherwise = comedia p

aplicarAventura :: Persona -> Filmacion -> VersionMala -> Persona
aplicarAventura p f v = aplicarPelicula (aventura p f v) f

pepe = Persona "Pepe" 20 30 3 1500





