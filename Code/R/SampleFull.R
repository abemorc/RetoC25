
library(fitdistrplus)
library(tidyverse)
library(hms)

# observacion general de los datos
dfRaw |> 
  count(estado) |> 
  View()


# -------------------------------------------------------------------------
# LIMPIEZA DE LOS DATOS

# EMPEZAMOS PRIMERO POR LA VARIABLE DE HORA LLEGADA
# limpieza de hora llegada (interarribo por consiguiente)
dfRaw |> 
  mutate(hora_llegada_sexagesimal = as_sexagesimal(hora_llegada),
         hora_llamado_sexagesimal = as_sexagesimal(hora_llamado),
         hora_salida_sexagesimal = as_sexagesimal(hora_salida)) |> 
  ggplot(aes(x = hora_llegada_sexagesimal)) + 
  geom_histogram() 
  

# veamos donde estan la mayoria de las observaciones
dfRaw |> 
  mutate(hora_llegada_sexagesimal = as_sexagesimal(hora_llegada),
         hora_llamado_sexagesimal = as_sexagesimal(hora_llamado),
         hora_salida_sexagesimal = as_sexagesimal(hora_salida)) |> 
  mutate(hora_bin = hour(hora_llegada_sexagesimal)) |> 
  count(hora_bin) |> 
  View()

dfRaw |> 
  mutate(hora_llegada_sexagesimal = as_sexagesimal(hora_llegada),
         hora_llamado_sexagesimal = as_sexagesimal(hora_llamado),
         hora_salida_sexagesimal = as_sexagesimal(hora_salida)) |> 
  mutate(hora_bin = hour(hora_llegada_sexagesimal)) |> 
  nest(.by = c(estado,tienda)) |> 
  View()


  
# hare las pruebas solo con 2 estados para poder manejar facil los datos
dfSample <- dfRaw |> 
  filter(estado == 'Puebla' | estado == 'Sonora') |> 
  mutate(hora_llegada_sexagesimal = as_sexagesimal(hora_llegada),
         hora_llamado_sexagesimal = as_sexagesimal(hora_llamado),
         hora_salida_sexagesimal = as_sexagesimal(hora_salida)) |> 
  mutate(hora_llegada_bin = hour(hora_llegada_sexagesimal))

# nos quedamos con las observaciones que tienen logica,
# escribir aqui la justificacion de porque retiramos estas
# IMPORTANTE

# dfSample |> 
#   mutate(hora_llegada_sexagesimal = as_sexagesimal(hora_llegada),
#          hora_llamado_sexagesimal = as_sexagesimal(hora_llamado),
#          hora_salida_sexagesimal = as_sexagesimal(hora_salida)) |> 
#   filter(! (hora_llegada<=hora_llamado & hora_llamado<=hora_salida & hora_llegada<=hora_salida) )  |> 
#   View()

# ver cuantas observaciones voy a quitar
# En el caso de hora llegada, la mayoria de estas observaciones
# hora llegada fuera de horario de tienda, tienen una hora de salida
# casi justo al horario de apertura de la tienda, por lo cual
# se puede deducir que al inicio del dia, se revisa qe no haya
# tickets en espera y se cancelan esos
dfSample |> 
  filter(!(hora_llegada_bin >= 8 & hora_llegada_bin<=22)) |>
  View()

dfSample <- dfSample |> 
  filter( (hora_llegada<=hora_llamado & hora_llamado<=hora_salida & hora_llegada<=hora_salida) ) |> 
  filter(hora_llegada_bin >= 8 & hora_llegada_bin<=22) |> 
  mutate(
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

dfSample |> View()




dfSampleClean <- dfSample |> 
  group_by(estado, tienda, Fecha, SEGMENTO_FISICO) |> 
  arrange(hora_llegada, .by_group = T) |> 
  mutate(interarribo =
           hora_llegada - lag(hora_llegada)) |> 
  mutate(
    tiempo_espera = hora_llamado - hora_llegada,
    tiempo_servicio = hora_salida - hora_llamado) |> 
  ungroup() |>
  mutate(interarribo_sexagesimal = as_sexagesimal(interarribo),
         tiempo_espera_sexagesimal = as_sexagesimal(tiempo_espera),
         tiempo_servicio_sexagesimal = as_sexagesimal(tiempo_servicio)) |> 
  relocate(estado, tienda, Fecha, dia_semana, dias_finde, Segmento, SEGMENTO_FISICO, 
           hora_llegada_sexagesimal, interarribo_sexagesimal,
           hora_llamado_sexagesimal, tiempo_espera_sexagesimal,
           hora_salida_sexagesimal, tiempo_servicio_sexagesimal,
           caja, status, hora_llegada, 
           hora_llamado, hora_salida, , hora_llegada_bin, Id)



glimpse(dfSampleClean)


dfSampleClean |> View()

dfSampleClean |> 
  filter(is.na(interarribo_sexagesimal))


# JUSTIFICACION DE PORQUE SE DEJAN LAS OTRAS VARIABLES INTACTAS
# PARA LLAS SALIDAS, PUEDE QUE SEAN VERDADERAS O FALSAS, YA QUE 
# ALGUN CLIEMTE PUDO QUEDARSE EN LA TIENDA HASTA QUE LE RESOLVIERAN SU
# cSITUACION Y GENUINAMENTE ESAS OBSERVACIONES REPRESENTEN A ESOS
# VLIENTES, SIN EMBARGO LO MAS PROBABLE ES QUE SEAN TICKETS QUE A
# LOS CAJEROS SE LES OLVIDO MARCAR SU HORA DE SALIDA, SIENDA HASTA HORAS
# DESPUES QUE SE 'CIERRAN' AQUELLOS TICKETS, LAMENTABLEMENTE UNICAMENTE 
# CON LA INFORMACION DE LOS DATOS NO HAY FORMA DE DETERMINAR CUALES SI Y 
# CUALES NO PERTENECEN A CADA UNO, SIN EMBARGO ESTAS OBSERVACIONES
# SI DAN INFORMACION ACERCA DE LOS TIEMPO DE LLEGADA, INTERARRIVO
# cDE LOS CLIETES, ASI COMO DEL TIEMPO EN ESPERA EN LA FILA,
# POR TANTO, ESTAS OBSERVACIONES SE DECIDE DEJARLAS TAL CUAL ESTAN




# aqui es conveniente hacer algunos graficoas descriptivos
# para justificar el porque de los analisis que haremos

dfSampleClean |> 
  ggplot(aes(x = dia_semana)) + 
  geom_bar()


dfSampleClean |> 
  ggplot(aes(x = hora_llegada_bin)) +
  geom_bar()

# veamos al final con cuanto nos quedamos por grupo
dfSampleClean |> 
  count(hora_llegada_bin)


dfSampleClean |> 
  group_by(estado, tienda, Fecha, SEGMENTO_FISICO) |> 
  summarise(promedio_espera = as_sexagesimal( mean(tiempo_espera))) |> 
  View()

dfSampleClean |> 
  View()
# -------------------------------------------------------------------------


# Particion datos
dfSampleAgrupado <- dfSampleClean |> 
  nest(.by = c(estado, tienda, Fecha, SEGMENTO_FISICO)) 

dfSampleAgrupado |> 
  View()

# ahora creare algunas variables nuevas para usarlas en las modelos


fun_pvalue_serv <- function(df) {
  ajExp <- fitdist(as.numeric(na.omit(df$tiempo_servicio)), 'exp', method = 'mle')
  test <-  ks.test(as.numeric(na.omit(df$tiempo_servicio)), 'pexp', rate = ajExp$estimate)
  
  return(test$p.value)
}
fun_rate_serv <- function(df) {
  ajExp <- fitdist(as.numeric(na.omit(df$tiempo_servicio)), 'exp', method = 'mle')
  return(ajExp$estimate)
}


fun_pvalue_inter <- function(df) {
  ajExp <- fitdist(as.numeric(na.omit(df$interarribo)), 'exp', method = 'mle')
  test <-  ks.test(as.numeric(na.omit(df$interarribo)), 'pexp', rate = ajExp$estimate)
  
  return(test$p.value)
}
fun_rate_inter <- function(df) {
  ajExp <- fitdist(as.numeric(na.omit(df$interarribo)), 'exp', method = 'mle')
  return(ajExp$estimate)
}

df1 <- 
dfSampleAgrupado |> 
  slice_sample(prop = 0.05) |> 
  mutate(
    rate_interarribo =
      map(data,
          possibly(fun_rate_inter)),
    rate_servicio =
      map(data, possibly(fun_rate_serv)),
    p_value_interarribo = 
      map(data, 
          possibly(fun_pvalue_inter)),
    # p_value_servicio = 
    #   map(data, 
    #       possibly(fun_pvalue_serv)),
    n_cajas = 
      map(data, possibly(\(x) {x |> distinct(caja) |> nrow() }) )
  ) 


dfSampleAgrupado |> 


# asi ya creamos el dataset con los valores adecuados
df1 |> glimpse()
df1 <- df1 |> 
  unnest(n_cajas, p_value_interarribo, rate_servicio, rate_interarribo, data)
  

df22 <- df1 |> 
  group_by(tienda, dia_semana, hora_llegada_bin) |> 
  summarise(rate_interarribo = mean(rate_interarribo, na.rm=T, trim=0.05),
            rate_servicio = mean(rate_servicio, na.rm=T, 0.05),
            n_servers = round(mean(n_cajas, na.rm=T)))

df22 |> 
  ungroup() |> 
  nest(.by = c(tienda, dia_semana, hora_llegada_bin)) |> 
  View()

df33 <- df22 |>
  ungroup() |> 
  nest(.by = c(tienda, dia_semana, hora_llegada_bin)) |> 
  mutate(modelMMC= map(data, possibly(\(x) {
    input <- NewInput.MMC(lambda = x$rate_interarribo, mu = x$rate_servicio, c = x$n_servers)
    model <- QueueingModel(input)
    return(model)
    
  } ))) |> 
  mutate(Resumen_model = map(data, possibly(\(x) {
    input <- NewInput.MMC(lambda = x$rate_interarribo, mu = x$rate_servicio, c = x$n_servers)
    model <- QueueingModel(input)
    resumen <- summary(model)
    
    return(as_tibble(resumen$el))
    
  } )))  |> 
  unnest(Resumen_model)
  
View(df33)





# -------------------------------------------------------------------------

# Ahora reportar algunos de estos resultados

185
479

inp <- NewInput.MMC(lambda = 2150, mu = 601, c = 6)
CheckInput(inp)
model1 <- QueueingModel(inp)

# con esto para ponerlo en el reporte y decir como se interpreta
Report(model1)
summary(model1)


# -------------------------------------------------------------------------

# Ahora para lo de ver si se puede mejorar

# acceder a un elemento
df33$modelMMC[[1]] |> summary()

modeltemp <- df33$modelMMC[[6]]
modeltemp$Inputs$lambda
modeltemp$Inputs$mu

summary(modeltemp)



# OPTIMIZAR UN MODELO
servers <- 1:10

ltemp <- map_vec(servers,possibly(\(x) {
  inp <- NewInput.MMC(lambda = modeltemp$Inputs$lambda,
                      mu = modeltemp$Inputs$mu,
                      c = x)
  model22 <- QueueingModel(inp)
  return(Wq(model22))
}, otherwise = NA) )

str(ltemp)

View(ltemp)

df55 <- tibble(
  servers=servers,
  Wq=ltemp
)

df55 |> 
  mutate(aver=map(servers, \(x) x + 10)) |> 
  unnest()

df55 |> 
  na.omit() |> 
  ggplot(aes(x = servers, y = as_sexagesimal(Wq))) + 
  geom_line()


Wq(modeltemp)

# 
# W_f_aux <- function(x){W (QueueingModel(NewInput.MMC(lambda=modeltemp$Inputs$lambda, mu=modeltemp$Inputs$mu, c=x)))}
# Wq_f_aux <- function(x){Wq (QueueingModel(NewInput.MMC(lambda=modeltemp$Inputs$lambda, mu=modeltemp$Inputs$mu, c=x)))}
# 
# W_f <- function(v){sapply(v, W_f_aux)}
# Wq_f <- function(v){sapply(v, Wq_f_aux)}
# 
# 
# gt <- "W and Wq"
# ylab <- "W, Wq"
# xlab <- "lambda, c"
# n <- 10
# ty<- "l"
# leg<- c("W", "Wq")
# lty<- c(1, 1, 1)
# cols <- c("blue", "red")
# 
# curve(W_f, from=5, to=n, n=n, ylab=ylab, xlab=xlab , col=cols[1], type=ty, main=gt)
# curve(Wq_f, from=1, to=n, n=n, col=cols[2], add=T, type=ty)
# 
# 
# glimpse(df33)


df33$modelMMC[[1]]
# eee <- summary(o_mm1)
# str(eee)


df1 <- dfSampleAgrupado$data[[5]]

# veamos si en verdad sigue la exp
aj <- fitdist(df1$interarribo |> na.omit() |> as.numeric(),
              distr = 'exp', method = 'mle' )
aj$estimate
ks.test(df1$interarribo |> na.omit() |> as.numeric(), 'pexp', rate=aj$estimate)
1/mean(df1$interarribo, na.rm=T)


aj <- fitdist(df1$tiempo_servicio,
              distr = 'exp', method = 'mle' )
plot(aj)

aj$estimate

1/mean(df1$tiempo_servicio, na.rm =T)

ks.test(df1$tiempo_servicio, 'pexp', rate=0.00202)


condicion <- !(abs(df1$tiempo_servicio - median(df1$tiempo_servicio)) > 2*sd(df1$tiempo_servicio))
condicion

1/mean(df1$tiempo_servicio[condicion])
1/mean(df1$tiempo_servicio)

ks.test(df1$tiempo_servicio[condicion], 'pexp', rate=571.0688)



aver <- NewInput.MMC(lambda = 880.383, mu = 493.4854,c = 4 )
CheckInput(aver)
model1 <- QueueingModel(aver)

summary(model1)

dfSampleClean |> 
  group_by(tienda, Fecha, SEGMENTO_FISICO) |> 
  count(caja) |> 
  View()


dfSampleClean |> 
  group_by(tienda, Fecha, SEGMENTO_FISICO, caja) |> 
  summarise(n_cajas=n()) |> 
  View()

# ver la distribucion del numero de cajas por dia en una tienda
dfSampleClean |> 
  group_by(tienda, Fecha, SEGMENTO_FISICO) |> 
  summarise(n_cajas = n_distinct(caja)) |> 
  ggplot(aes(x = n_cajas)) + 
  geom_bar()



dfSampleClean |> 
  count(dia_semana)








# Esto nos da el dat frame completo con numero de servers por dia
dfSampleClean |> 
  group_by(tienda, Fecha, SEGMENTO_FISICO) |> 
  summarise(n_cajas = n_distinct(caja)) |>
  inner_join(dfSampleClean, by = join_by(tienda, Fecha, SEGMENTO_FISICO)) |> 
  View()

# Prueba de como lo agrupo
dfSampleClean |> 
  mutate(mes = month(Fecha)) |> 
  nest(.by = c (estado, tienda, dia_semana)) |> 
  View()


dfSampleClean |> 
  mutate(mes = month(Fecha)) |> 
  group_by(tienda, dia_semana, hora_llegada_bin) |> 
  summarise()


df1 <- 
dfSampleClean |> 
  nest(.by = c(estado,tienda, dia_semana, hora_llegada_bin)) 

df11 <- df1$data[[2]]


View(df11)

ajex <- fitdist( as.numeric(na.omit(df11$interarribo)), distr = 'exp', method = 'mle')
ajex$estimate


1/mean(df11$interarribo, na.rm = T)

ks.test(as.numeric(na.omit(df11$interarribo)), 'pexp', rate = ajex$estimate)
# ok parece que no se puede hacer sobre todo agrupado,
# ya que no sigue una exp