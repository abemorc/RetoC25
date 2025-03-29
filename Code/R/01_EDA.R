
library(tidyverse)


# -------------------------------------------------------------------------

# Un analisis rapido para ver como es la data que tenemos

glimpse(dfRaw)
# los tipos de datos son correctos, aunque las horas de llegada, llamado y salida
# vienen en double
# falta transformar a la hora en sitema sxagesimal para una mejor apreciacion
# de los datos



dfRaw |> 
  summarise(across(everything(), \(x)sum(is.na(x)))) |> 
  View()
# veo que no hay ningun NA, y aparentemente la data esta limpia, es decir,
# no hay errores de captura o correcciones neesarias


# conteo de observaciones por tienda y por estado
dfRaw |> 
  count(estado, tienda)


dfRaw |> 
  count(estado)
# vemos que hay informacion de 23 estados


# numero de tiendas por estados
dfRaw |> 
  count(estado,tienda) |> 
  count(estado) |> 
  arrange(desc(n)) 
# vemos que nuevo leon, puebla, sinaloa y veracruz 
# son los que tienen mas tiendas, teniendo 4



# ver los diferentes valores de cada variable
map(.x = dfRaw,.f = \(x) x |> as_tibble() |> distinct())

# vemos que hay 50 tiendas, 77 cajas aunque esas cajas en realidad no 
# son unicas
# 3 segmentos
# tenemos informacion de 396 dias



dfRaw |> 
  count(caja) |> 
  View()
# como era de esperarse, no todas las tiendas tienen el mismo numero de cajas
# y en la mayoria de las tiendas se observa que hay mas cajas de retail y bancos



# Conclusion --------------------------------------------------------------

# Parece que es mejor explorar la informacion dividida esta por grupos,
# la division natural que se me ocurre es:
# estado > tienda > fecha (dia)




# veamos solo una tienda
# seleccionemos la mas grande

dfRaw |> 
  filter(estado == 'Puebla') |> 
  group_by(tienda) |> 
  summarise(n())

dfRaw |> 
  filter(estado == 'Puebla') |> 
  group_by(tienda, caja) |> 
  summarise(n=n()) |> 
  count(tienda) |> View()
# como era de esperase, la tienda mas grande 'tienda_BA' tiene mayor numero de 
# cajas

dfRaw |> 
  filter(estado == 'Puebla') |>
  count(caja, tienda) |> 
  count(caja) |> 
  View()
# no todas las tiendas en puebla tienen promotoria, algunas solo
# cajas y ventanillas



dfRaw |> 
  filter(estado == 'Puebla', tienda == 'Tienda_BA') |> 
  count(caja) |> 
  View()
# en esta tienda solo hay 37 cajas


dfRaw |> 
  filter(estado == 'Puebla', tienda == 'Tienda_BA') |> View()
# vemos que hay algunas observaciones que el segmento es diferente al 
# caja en la que fue atendido como se habia explicado en las sesiones 
# de teams






# cajas --------------------------------------------------------------------
# exploracion cjas
View(dfRaw)

# hay unas cajas que tienen su nombre en mayusculas, causa intriga que no son
# tantas observaciones, veamos

dfRaw |> 
  filter(caja %in% paste0('Caja_', letters[1:5])) |> 
  View()
  

dfRaw |> 
  filter(caja %in% paste0('Caja_', letters[1:5])) |>
  count(estado)

dfRaw |> 
  filter(caja %in% paste0('Caja_', letters[1:5])) |>
  count(tienda)


dfRaw |> 
  filter(estado == 'Ciudad de México') |> 
  distinct(caja) |> 
  View()

dfRaw |> 
  filter(estado == 'Ciudad de México') |> 
  count(Segmento, caja) |> 
  View()


# parece que en realidad todas las observaciones pertenecen a un solo estado, 
# ay dos observaciones que son de otros dos estados, podria deberse a un error
# debido a la forma en como se generaron los datos

# y tambien parece que todas pertenecen a la misma tienda, supongo que fueron 
# los datos que en la simulacion se generaron primero y luego en los siguientes
# se cambio la instruccion para que las cajas sean con minuscula.

# parece que las observaciones las generaron por tienda por el patron que se
# tiene

# Creo recomendable estudiar esta tienda por separado, ya que no parece seguir 
# el mismo comportamiento de las otras tiendas
# Tiena_AA ciudad de mexico







