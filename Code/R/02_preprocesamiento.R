

# -------------------------------------------------------------------------

# Manipulacion de la data para posterior analisis


# Despues de las explicaciones vistas en las sesiones y con el profesor David,
# parece natural analizar nuestro conjunto de datos por grupos separados
# la division que se habia propuesto era por estado, sin embargo despues
# de nuestra exploracion parece que no es suficiente con solo esa division,
# una division mas adecuada parece que es:

# estado > tienda > dia > hora del dia

# esto con base en algunos articulos que lei en donde la division que hacen
# es por horario, y en cada rango de horario se hace su propio analisis, ya
# que es muy probable que los datos se comporten diferente de acuerdo 
# a la hora del dia

# igualmete hare una conversion a hora sexagesimal para poder entender 
# de mejor manera los horarios, ya que aunque en el analisis no influya el 
# como esten representadas estas horas, para el entendimiento humano si hara
# mas sentido si los tiempos los tenemos en formal natural.


library(tidyverse)
library(hms)

# dfSample <- dfRaw |> 
#   sample_n(500000)
# 
# dfSample



dfCoppel <- dfRaw |> 
  mutate(
    hora_llegada_sexagesimal = 
      round_hms(as_hms(hora_llegada*60*60*24), digits = 0),
    hora_llamado_sexagesimal = 
      round_hms(as_hms(hora_llamado*60*60*24), digits = 0),
    hora_salida_sexagesimal = 
      round_hms(as_hms(hora_salida*60*60*24), digits = 0),
    SEGMENTO_FISICO = case_when(
      str_starts(str_to_lower(caja), 'caja') ~ 'retail',
      str_starts(str_to_lower(caja), 'p') ~ 'afiliacion',
      str_starts(str_to_lower(caja), 'ventanilla') ~ 'banco',
      .default = 'error'),
    dia_semana = wday(Fecha, label = T, abbr = F),
    dias_finde = 
      if_else(dia_semana %in% c('viernes', 'sábado', 'domingo'),
              'fin de semana', 'entre semana'))|> 
  rename(Id=...1) |> 
  relocate(estado, tienda, Fecha, dia_semana, dias_finde,
           hora_llegada_sexagesimal, hora_llamado_sexagesimal,
           hora_salida_sexagesimal,
           Segmento, SEGMENTO_FISICO, caja, status, hora_llegada, 
           hora_llamado, hora_salida, Id)




dfCoppel |> glimpse()

dfCoppel |> 
  slice_sample(n = 1000) |> 
  View()



# cuales son las observaciones en donde el cliente inicia el proceso
# en un segmento, pero termina en otro
dfCoppel |> 
  filter(Segmento != SEGMENTO_FISICO) 
# son 1,229,113, talvez sea necesario estudiar estos datos por aparte
# PREGUNTAR


# -------------------------------------------------------------------------

# estudio rapido de los datos

dfCoppel |> 
  ggplot(aes(x = hora_llegada_sexagesimal)) +
  geom_histogram()

dfCoppel |> 
  ggplot(aes(x = hora_salida_sexagesimal)) +
  geom_histogram()

dfCoppel |> 
  ggplot(aes(x = hora_llamado_sexagesimal)) +
  geom_histogram()

# parece que hay algunas pocas observaciones fuera del rango del horario de 
# la tienda



# Limpieza  ---------------------------------------------------------------

# Aqui hay que terminar de ver las observaciones que no tienen sentido,
# es decir, aquellas en donde la hora de llegada es despues de la hora de 
# llamado o de salida, asi como algun posible outlier, como por ejemplo, 
# horas de llegada en donde la tienda ni siquiera esta abierta



# CONTAR CUANTOS ATENDIDOS Y AUSENTES HAY

# Pendiente









# -------------------------------------------------------------------------



# -------------------------------------------------------------------------

# particionar datos

dfClean <- dfCoppel |> 
  # slice_sample(prop = 0.1) |>
  group_by(estado, tienda, Fecha) |> 
  arrange(hora_llegada_sexagesimal, .by_group = T) |> 
  mutate(interarribo =
           hora_llegada - lag(hora_llegada),
         .after = hora_llegada_sexagesimal) |> 
 mutate(
    tiempo_espera = hora_llamado - hora_llegada,
    tiempo_servicio = hora_salida - hora_llamado,
    .after = hora_salida_sexagesimal)

dfClean <- dfClean |> 
  ungroup() |>
  mutate(interarribo_sexagesimal = as_sexagesimal(interarribo),
         tiempo_espera_sexagesimal = as_sexagesimal(tiempo_espera),
         tiempo_servicio_sexagesimal = as_sexagesimal(tiempo_servicio))



glimpse(dfClean)
 
View(dfClean |> head(10000))



dfRaw |> 
  filter(Fecha == '2023-01-04') |> 
  count(estado)
  View()
# no todos los dias tenemos registros de todas las tienas


dfClean |> 
  arrange(Fecha) |> 
  head()
# vemos que las observaciones del 2023-01-01 solo corresponden a 
# unas pocas tiendas, es decir, ese dia casi no tenemos datos (446)

dfRaw |> 
  filter(Fecha == '2023-01-01') |> 
  View()

dfClean |> 
  head(n = 1000) |> 
  View()



dfCoppel |> 
  arrange(desc( hora_salida_sexagesimal)) |> 
  head(n = 10000) |> 
  View()

dfCoppel |> 
  arrange(hora_llegada_sexagesimal) |> 
  head(n = 10000) |> 
  View()



# veamos una grafica por dias

dfRaw |> 
  ggplot(aes(x = Fecha)) + 
  geom_histogram()

# parece que la afluencia de clientes es menor en los primeros dias del 
# a;no, aumentando alrededor de marzo




dfAgrupado <- dfClean |> 
  nest(.by = c(estado, tienda, Fecha)) 


View(dfAgrupado)

# promedio de atencion al cliente global
dfClean |> 
  summarise(promedio_servicio = mean(tiempo_servicio_sexagesimal))
# 197 segundos







# -------------------------------------------------------------------------

# Otra forma de agrupar los datos
# Considero que esta es la mejor forma, sin embargo, nos quedamos con
# muy pocas observaciones en cada caso, y no si si esto influya 

# save(dfCoppel, file = 'dfCoppel.RData')

dfClean2 <- dfCoppel |> 
  # slice_sample(prop = 0.1) |>
  group_by(estado, tienda, Fecha, SEGMENTO_FISICO) |> 
  arrange(hora_llegada_sexagesimal, .by_group = T) |> 
  mutate(interarribo =
           hora_llegada - lag(hora_llegada),
         .after = hora_llegada_sexagesimal) |> 
  mutate(
    tiempo_espera = hora_llamado - hora_llegada,
    tiempo_servicio = hora_salida - hora_llamado,
    .after = hora_salida_sexagesimal) |> 
  relocate(SEGMENTO_FISICO,.after = Fecha)

dfClean2 <- dfClean2 |> 
  ungroup() |>
  mutate(interarribo_sexagesimal = as_sexagesimal(interarribo),
         tiempo_espera_sexagesimal = as_sexagesimal(tiempo_espera),
         tiempo_servicio_sexagesimal = as_sexagesimal(tiempo_servicio)) |> 
  relocate(estado, tienda, Fecha, dia_semana, dias_finde, Segmento, SEGMENTO_FISICO, 
           hora_llegada_sexagesimal, interarribo_sexagesimal,
           hora_llamado_sexagesimal, tiempo_espera_sexagesimal,
           hora_salida_sexagesimal, tiempo_servicio_sexagesimal,
           caja, status, hora_llegada, 
           hora_llamado, hora_salida, Id)



glimpse(dfClean2)

View(dfClean2 |> head(10000))

dfAgrupado2 <- dfClean2 |> 
  nest(.by = c(estado, tienda, Fecha, SEGMENTO_FISICO)) 


dfAgrupado2 |> 
  View()



