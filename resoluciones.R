# Teoría de la Decisión || Trabajo 01 Grupo || Parte 2, Resolución -------------



# PROBLEMAS TABLA DE DECISIÓN --------------------------------------------------


## PROBLEMA I ------------------------------------------------------------------

### Enunciado I ----------------------------------------------------------------

# Teniendo la matriz:       e1  e2  e3  e4  e5 
#                      d1    9  11   6   3  12
#                      d2    4   2   8   7  13
#                      d3   10   1   4   8   7
# 
# Resuelve el problema con cada uno de los métodos o funciones individuales de
# Incertidumbre por separado (tanto en situación favorable como desfavorable).


### Solución I -----------------------------------------------------------------

source("teoriadecision_funciones_incertidumbre.R")

tabla_1 <- crea.tablaX(c( 9, 11, 6, 3, 12,
                          4,  2, 8, 7, 13, 
                         10,  1, 4, 8,  7),
                       numalternativas = 3,
                       numestados = 5)
tabla_1

## Hacemos la función que escribe la solución dependiendo del método, el criterio,
## el número de alternativas óptimas y, obviamente, la alternativa óptima.

solucion <- function (x) {
  
  # Creamos las variables que se irán necesitando.
  crit_sol = x$criterio
  met_sol = x$metodo
  opt_sol = x$AlternativaOptima
  n_sol = length(opt_sol)
  
  # Hacemos la primera parte de la frase de la solución (método y criterio).
  cat("En situación", met_sol, "y mediante el criterio de", crit_sol, 
      if (n_sol == 1) {
        "la solución es "
      } else {
        "las soluciones son "
      })
  
  # Hacemos la parte final de la frase de la solución (alternativas óptimas).
  if(n_sol == 1){
    cat(opt_sol, ".")
  }
  else {
    if(n_sol==2){
      cat(opt_sol[1]," y ",opt_sol[2],".")
    } else {
      for (i in 1:(n_sol-2)){
        cat(opt_sol[i],", ")}
    cat(opt_sol[n_sol-1], " y ",opt_sol[n_sol],".")
    }
    }
  }

#### Favorables ----------------------------------------------------------------

##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_1))

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_1))

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_1))

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_1))

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_1))

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_1))


#### Desfavorables -------------------------------------------------------------


##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_1, favorable = F))

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_1, favorable = F))

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_1, favorable = F))

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_1, favorable = F))

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_1, favorable = F))

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_1, favorable = F))



## PROBLEMA II ------------------------------------------------------------------

### Enunciado II ----------------------------------------------------------------

# Sea la tabla de decisión con 4 estados de la naturaleza y 5 alternativas de
# la siguiente forma:
#                           e1  e2  e3  e4   
#                      d1    6   9   4   6
#                      d2    5   5   6   5
#                      d3    6   6   6   8
#                      d4    6   8   6   6
#                      d5    5   8   9  15
# 
# Resolverla tanto para situación favorable como desfavorable, con cada uno de los 
# criterios por separado.


### Solución II -----------------------------------------------------------------

source("teoriadecision_funciones_incertidumbre.R")

tabla_2=crea.tablaX(c(6,9,4,6,
                 5,5,6,5,
                 6,6,6,8,
                 6,8,6,6,
                 5,8,9,15),
               numalternativas = 5,
               numestados = 4)
tabla_2

## Utilizaremos la función solucion creada para el problema 1

#### Favorables ----------------------------------------------------------------

##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_2))

# En situación favorable y mediante el criterio de Hurwicz la solución es la alternativa 5 .

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_2))

# En situación favorable y mediante el criterio de Hurwicz General la solución es la alternativa 5 .

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_2))

# En situación favorable y mediante el criterio de Laplace la solución es la alternativa 5 .

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_2))

# En situación favorable y mediante el criterio optimista la solución es la alternativa 5 .

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_2))

# En situación favorable y mediante el criterio del punto ideal la solución es la alternativa 5 .

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_2))

# En situación favorable y mediante el criterio de Savage la solución es la alternativa 5 .

##### Wald -------------------------------------------------------------------

solucion(criterio.Wald(tabla_2))

# En situación favorable y mediante el criterio de Wald la solución es la alternativa 3.


#### Desfavorables -------------------------------------------------------------


##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_2, favorable = F))

# En situación desfavorable y mediante el criterio de Hurwicz la solución es la alternativa 2 .

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_2, favorable = F))

# En situación desfavorable y mediante el criterio de Hurwicz general la solución es la alternativa 2 .

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_2, favorable = F))

# En situación desfavorable y mediante el criterio de Laplace la solución es la alternativa 2 .

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_2, favorable = F))

# En situación desfavorable y mediante el criterio optimista la solución es la alternativa 2 .

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_2, favorable = F))

# En situación desfavorable y mediante el criterio del punto ideal la solución es la alternativa 2 .

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_2, favorable = F))

# En situación desfavorable y mediante el criterio de Savage la solución es la alternativa 2 .

##### Wald -------------------------------------------------------------------

solucion(criterio.Wald(tabla_2,favorable=F))

# En situación desfavorable y mediante el criterio de Wald la solución es la alternativa 2 .



## PROBLEMA III ----------------------------------------------------------------

### Enunciado III --------------------------------------------------------------

# A partir de la siguiente tabla de decisión, aplicar los métodos de decisión bajo 
# incertidumbre por separados, tanto para el caso favorable (beneficios) como para 
# el caso desfavorable (costos).

#                                      e1  e2  e3
#                                 d1 | 30  50  20
#                                 d2 | 40  60  10
#                                 d3 | 10  70  30
#                                 d4 | 50  40  40


### Solución III ---------------------------------------------------------------

source("teoriadecision_funciones_incertidumbre.R")

tabla_3 = crea.tablaX(c(30,50,20,
                        40,60,10,
                        10,70,30,
                        50,40,40), numalternativas = 4, numestados = 3)
tabla_3

## Utilizaremos la función solución creada para el problema 1


#### Favorables ----------------------------------------------------------------

##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_3))
# En situación favorable y mediante el criterio de Hurwicz la solución es 4 .

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_3))
# En situación favorable y mediante el criterio de Hurwicz la solución es 4 .

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_3))
# En situación favorable y mediante el criterio de Laplace la solución es 4 .

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_3))
# En situación favorable y mediante el criterio de Optimista la solución es 3 .

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_3))
# En situación favorable y mediante el criterio de Punto Ideal la solución es 4 .

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_3))
# En situación favorable y mediante el criterio de Savage la solución es 1 .


#### Desfavorables -------------------------------------------------------------

##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_3, favorable = F))
# En situación desfavorable y mediante el criterio de Hurwicz la solución es 1 .

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_3, favorable = F))
# En situación desfavorable y mediante el criterio de Hurwicz la solución es 1 .

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_3, favorable = F))
# En situación desfavorable y mediante el criterio de Laplace la solución es 1 .

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_3, favorable = F))
# En situación desfavorable y mediante el criterio de Optimista las soluciones son 2 y 3 .

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_3, favorable = F))
# En situación desfavorable y mediante el criterio de Punto Ideal la solución es 1 .

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_3, favorable = F))
# En situación desfavorable y mediante el criterio de Savage la solución es 1 .




## PROBLEMA IV ----------------------------------------------------------------

### Enunciado IV --------------------------------------------------------------

# Se tiene la siguiente matriz de decisión:

#                                      e1  e2  e3
#                                 d1 | 15  25  35
#                                 d2 | 10  30  50
#                                 d3 | 20  28  30
#                                 d4 | 12  22  27

# Aplicar los  distintos métodos de decisión bajo 
# incertidumbre para resolver el problema, tanto para el caso favorable (beneficios) como para 
# el caso desfavorable (costos).


### Solución IV ---------------------------------------------------------------

source("teoriadecision_funciones_incertidumbre.R")

tabla_4 = crea.tablaX(c(15, 25, 35,  
                        10, 30, 50,  
                        20, 28, 30,  
                        18, 22, 27) , numalternativas = 4, numestados = 3)
tabla_4

## Utilizaremos la función solución creada para el problema 1

#### Favorables ----------------------------------------------------------------

##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_4))

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_4))

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_4))

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_4))

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_4))

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_4))


#### Desfavorables -------------------------------------------------------------


##### Hurwicz ------------------------------------------------------------------

solucion(criterio.Hurwicz(tabla_4, favorable = F))

##### Hurwicz General ----------------------------------------------------------

solucion(criterio.Hurwicz.General(tabla_4, favorable = F))

##### Laplace ------------------------------------------------------------------

solucion(criterio.Laplace(tabla_4, favorable = F))

##### Optimista ----------------------------------------------------------------

solucion(criterio.Optimista(tabla_4, favorable = F))

##### PuntoIdeal ---------------------------------------------------------------

solucion(criterio.PuntoIdeal(tabla_4, favorable = F))

##### Savage -------------------------------------------------------------------

solucion(criterio.Savage(tabla_4, favorable = F))



# PROBLEMAS SITUACIÓN REAL -----------------------------------------------------


## PROBLEMA V -----------------------------------------------------------------

### Enunciado V ---------------------------------------------------------------

# Supongamos que un inversor deportivo debe decidir entre tres posibles
# estrategias a la hora de destinar su dinero en la temporada 24/25 de Dallas
# Mavericks, el equipo de la liga estadounidense de baloncesto, la NBA.
#
# Estas estrategias de inversión son marketing (estrategia A), infraestructura (B)
# o apostar por jugadores jóvenes (C).
#
# Pueden generar diferentes beneficios dependiendo del rendimiento del equipo
# texano. Las cuatro posibles situaciones y los beneficios según los escenarios
# son las siguientes:
#
# - Si el equipo no clasifica a play-off: la estrategia A da 25 millones de
# dólares; la B da 15 y la C, 20.
#
# - Si Dallas clasifica a play-off pero no a las finales, con las estrategias se 
# obtiene un beneficio de 35, 45 y 40 millones ($) respectivamente.
# 
# - Si pierden la final los beneficios serán de 50, 55 y 45 (en millones de $).
#
# - Por último, si Dallas Mavericks resulta ganador del anillo, los beneficios se
# irían a 70 millones con la estrategia A, 65 millones con la B y 80 con la C.
#
#
# Resuelve este problema mediante todos los criterios conocidos y explica las
# conclusiones obtenidas.


### Solución V ----------------------------------------------------------------

# Primero, creamos la matriz del problema.

tabla_5 <- crea.tablaX(c( 25, 35, 50, 70,
                          15, 45, 55, 65, 
                          20, 40, 45, 80),
                       numalternativas = 3,
                       numestados = 4)
tabla_5

# Resolvemos mediante todos los métodos a la vez.

criterio.Todos(tabla_5, alfa = 0.5)

# Vemos que las alternativas óptimas son la 1 ó la 3.
#
# Para Wald es la estrategia A la que hay que tomar, ya que en el caso más
# pesimista (caer eliminados antes de play-off), es la que más beneficio reporta.
#
# Para los criterios Optimista, Hurwicz, Laplace y Punto Ideal es la estrategia 
# C la que se debe adoptar, siendo esta la más beneficiosa en el caso de quedar 
# campeones, la peor en caso de perder la final y la más equilibrada en los otros
# dos escenarios.
#
# Para Savage, hay un empate entre las estrategias A y C.
#
# La segunda estrategia, la de la inversión en infraestructura, queda descartada
# por todos los métodos.



## PROBLEMA VI -----------------------------------------------------------------

### Enunciado VI ---------------------------------------------------------------

# José Ignacio está decidiendo con qué empresa firmar su primer contrato.
# Deloitte le paga 1300€ al mes y si se queda dos años, le costean un máster que
# desearía hacer; si lo comienza cuando lleve solo un año, le costean solo la mitad.
# Por otra parte, PWC le paga 1250€ al mes, y le costea la mitad del máster desde el
# primer momento. Una consultora pequeña le paga 1600€ pero no le costea nada del máster.
# EY le paga 1150€ y le costea el máster al completo si lo comienza cuando lleve al
# menos un año en la empresa. Finalmente, KPMG le paga 1400€ y le costea el 75% del
# máster si lo empieza tras llevar dos años en la empresa.
#
# Sabiendo que es posible que lo empiece a la vez que el trabajo, al llevar un año
# en el trabajo, dos años o que finalmente no lo haga, y que el coste del máster es
# 6000€, plantea un problema de decisión bajo incertidumbre utilizando todos los criterios que conoces
# de forma que haya obtenido el máximo dinero posible tras 3 años trabajando (14 pagas por año).

### Solución VI ----------------------------------------------------------------

# Primero, creamos la matriz del problema.

tabla_6=crea.tablaX(c(67200,61200,61200,61200,
                      54600,48600,51600,54600,
                      48300,42300,48300,48300,
                      58800,52800,52800,57300,
                      52500,49500,49500,49500),
                    numalternativas = 5,numestados = 4)
colnames(tabla_6)=c('sin máster','máster año 0','máster año 1','máster año 2')
rownames(tabla_6)=c('pequeña consultora','Deloitte','EY','KPMG','PWC')
tabla_6

# Resolvemos mediante todos los métodos a la vez.

criterio.Todos(tabla_6, alfa = 0.5)

# Según todos los criterios, José Ignacio debería elegir la pequeña consultora, por
# lo que no hay duda de que esa debe ser la alternativa tomada.



## PROBLEMA VII -----------------------------------------------------------------

### Enunciado VII ---------------------------------------------------------------

# Una empresa manufacturera necesita seleccionar un proveedor de materiales para un 
# proyecto importante. Existen tres proveedores (A, B y C), y la elección afectará 
# tanto el costo como la calidad de los materiales recibidos, lo que influirá en la 
# rentabilidad total del proyecto. Sin embargo, debido a la incertidumbre en el 
# mercado y factores externos como la disponibilidad de materiales y fluctuaciones 
# de precios, el rendimiento de cada proveedor puede variar según tres posibles 
# escenarios económicos: favorable, moderado o desfavorable.
# 
# - En un escenario favorable, el proveedor A ofrece el mayor rendimiento en términos 
#   de rentabilidad.
# 
# - En un escenario moderado, el proveedor B es más confiable en términos de calidad 
#   y tiempo de entrega, lo que minimiza los costos de producción.
# 
# - En un escenario desfavorable, el proveedor C, aunque menos rentable en otros 
#   escenarios, logra mantener precios y tiempos de entrega estables, por lo que es 
#   el de menos riesgo.
# 
# La empresa ha estimado el rendimiento (en miles de euros) que cada proveedor 
# ofrecería en cada uno de los escenarios económicos:
#
#                    Esc.favorable  Esc.moderado  Esc.desfavorable
#       Proveedor A |    80             40              10
#       Proveedor B |    70             50              20
#       Proveedor C |    60             30              25
#
# La empresa busca maximizar su rendimiento, pero también desea evitar pérdidas 
# significativas en caso de que se presente un escenario económico desfavorable.
# Entonces, ¿cuál proveedor debería elegir la empresa para obtener el mejor 
# rendimiento en condiciones de incertidumbre?
#
# Resuelve este problema haciendo uso de la función R que devuelve la resolución
# de todos los métodos en una única tabla.

### Solución VII ----------------------------------------------------------------

# Primero, creamos la matriz del problema.

tabla_7 = crea.tablaX(c(80,40,10,
                        70,50,20,
                        60,30,25), numalternativas = 3, numestados = 3)
tabla_7

# Resolvemos mediante todos los métodos a la vez.

criterio.Todos(tabla_7, favorable = T)

# Podemos ver que según el criterio utilizado aparecen distintas alternativas 
# óptimas. Para los criterios de Wald y Hurwicz la mejor alternativa será optar por 
# el Proveedor C, mientras que el criterio optimista indica que la mejor alternativa 
# sería el Proveedor A. 

# Por otro lado, el resto de criterios (Savage, Laplace y Punto Ideal) coinciden en
# que la empresa debería elegir el Proveedor B para obtener el mejor rendimiento
# en un escenario moderado.



## PROBLEMA VIII -----------------------------------------------------------------

### Enunciado VIII ---------------------------------------------------------------

# Supongamos que una empresa debe decidir en cuál de tres proyectos (P1, P2, P3) invertir, 
# pero las condiciones futuras del mercado son inciertas. Existen cuatro posibles escenarios económicos: 
# Recesión, Estabilidad, Crecimiento moderado y Crecimiento elevado. 
# Los beneficios estimados para cada proyecto, dependiendo del escenario económico, 
# están dados en millones de dólares en la siguiente tabla:

#                     Recesión  Estabilidad  Crecimiento moderado  Crecimiento elevado
#       Proyecto 1. |    2           6                10.                  12
#       Proyecto 2  |    4           8                 6                    8
#       Proyecto 3  |   -1           5                15                   20
#


### Solución VIII ----------------------------------------------------------------

# Primero, creamos la matriz del problema.

tabla_8 = crea.tablaX(c(2, 6, 10, 12,  
                       4, 8, 6, 8,     
                       -1, 5, 15, 20) , numalternativas = 3, numestados = 4)
tabla_8

# Resolvemos mediante todos los métodos a la vez.

criterio.Todos(tabla_8, favorable = T)

# Solución:
  
#  -   El Proyecto 3 es la alternativa óptima según los criterios Optimista, Hurwicz, Savage, Laplace y Punto Ideal.

#  -   El Proyecto 2 es la alternativa óptima únicamente según el criterio de Wald.

#  -   El Proyecto 1 no es óptima según ninguno de los criterios aplicados.

# Dado que el Proyecto 3ves la alternativa óptima en la mayoría de los criterios de decisión 
# (Optimista, Hurwicz, Savage, Laplace y Punto Ideal), 
# se recomienda a la empresa optar por invertir en este proecto. 
# Dicho proyecto maximiza las expectativas de beneficios bajo diversas perspectivas de incertidumbre.
