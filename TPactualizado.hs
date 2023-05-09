-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([],_,_) = []
nombresDeUsuarios ((x:xs),rs,ps) = [nombreDeUsuario x] ++ nombresDeUsuarios (xs,rs,ps)

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_,[],_) _ = []
amigosDe (us,((x,y):rs),ps) u
  | u == x = y : friends
  | u == y = x : friends
  | otherwise = friends
  where friends = amigosDe (us, rs, ps) u

--let a = ([(1,"usuario1"),(2,"usuario2"),(3,"usuario3"),(4,"usuario4")],[((1,"usuario1"),(2,"usuario2")),((1,"usuario1"),(3,"usuario3")),((2,"usuario2"),(4,"usuario4"))],[])


-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos res u = longitud (amigosDe res u)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([x,y],rs,ps) = if cantidadDeAmigos ([x,y],rs,ps) x >= cantidadDeAmigos ([x,y],rs,ps) y then x else y
usuarioConMasAmigos ((x:y:us),rs,ps) = if cantidadDeAmigos ((x:y:us),rs,ps) x >= cantidadDeAmigos ((x:y:us),rs,ps) y then usuarioConMasAmigos ((x:us),rs,ps) else usuarioConMasAmigos ((y:us),rs,ps)

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red
  | longitud (usuarios red) <= 1000000 = False
  | otherwise = if cantidadDeAmigos red (usuarioConMasAmigos red) <= 1000000 then False else True

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u
  | publicaciones red == []
  | otherwise = if usuarioDePublicacion (head (publicaciones red)) == u then head (publicaciones red) : publicacionesDe (usuarioDePublicacion (tail (publicaciones red))) u else publicacionesDe (usuarioDePublicacion (tail (publicaciones red))) u

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
