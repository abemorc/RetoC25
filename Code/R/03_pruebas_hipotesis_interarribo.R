
library(tidyverse)
library(fitdistrplus)

# haremos pruebas con unas pocas observaciones
# para despues hacer todo esto sobre cada grupo (tienda, fecha)
dftemp <- dfAgrupado |> 
  slice_head(n=100)

dftemp

df1 <- dfAgrupado |> 
  slice(6) 

str(df1)

dfPruebas <- df1$data[[1]]



# -------------------------------------------------------------------------

# pruebas con un solo dataframe (los datos de una tienda en un solo dia)
# para tiempo de interarribo

dfPruebas |> View()

# promedio de interarribo
lambda <- mean(dfPruebas$interarribo, na.rm = T)

# veo que es lo mismo en los valores en sexagesimal
mean(dfPruebas$interarribo_sexagesimal, na.rm = T)
lambda |> as_sexagesimal()


numBreaks <- nclass.Sturges(dfPruebas$interarribo)
numBreaks


# ver si tiene una forma exponencial 
hist(dfPruebas$interarribo, probability = T, breaks = 11, col = 'steelblue2')
lines(density(dfPruebas$interarribo |> na.omit()), col = 'red')
curve(dexp(x, rate = 1/lambda), add = T)



boxplot(dfPruebas$interarribo |> na.omit())
# veo que si hay demasiados outliers, entonces no se exactamente como tratarlos

dfPruebas |> View()


# bondad de ajuste
# pruebas formales

ajuste_exponencial <- fitdist(na.omit(dfPruebas$interarribo) |> as.numeric(),
                              distr = 'exp', method = 'mle')
summary(ajuste_exponencial)

ajuste_weibull <- fitdist(na.omit(dfPruebas$interarribo) |> as.numeric(),
                          distr = 'weibull', 
                          method = 'mle',
                          start = list(scale = 1, shape = 1))
summary(ajuste_weibull)

# entonces tenemos lo siguiente:

rate_data <- 1/lambda

rate_estimada <- ajuste_exponencial$estimate

rate_data; rate_estimada
# vemos que son casi iguales

ks.test(as.numeric(na.omit(dfPruebas$interarribo)), 
        'pexp', rate = 1297.576)


# parece que no siguen una distribucion exponencial

ks.test(dfPruebas$interarribo, 'pweibull', scale = 0.0006240593,
        shape = 0.7821786187)

# tampoco seguirian una dist weibull



# -------------------------------------------------------------------------

# PROBANDO CON LA OTRA PARTICION DE LOS DATOS

# ESTADO > TIENDA > FECHA > SEGMENTO REAL


# tomare como ejemplo el datset 21, porque esa dia ya hay afluencia normal
# y un numero considerable de observaciones
# Este corresponde al segmento fisico de retail

df21 <- dfAgrupado2$data[[21]]

str(df21)
glimpse(df21)


# prueba hipotesis dist exp de la variable interarribo

# separemos las variables para no escribir tanto

inter <- df21 |> 
  dplyr::select(interarribo) |> 
  filter(!is.na(interarribo)) |> 
  pull()
str(inter)  

serv <- df21 |> 
  dplyr::select(tiempo_servicio) |> 
  filter(!is.na(tiempo_servicio)) |> 
  pull()
str(serv)  



lambda <- mean(inter)

ajExp <- fitdist(inter, 'exp', method = 'mle')

# vemos que son iguales
ajExp; 1/lambda


test <-  ks.test(inter, 'pexp', rate = ajExp$estimate)
test$p.value

# aqui parece que los datos si siguen una distribucion exponencial



# asi que veamos para una muestra de 500 datasets del conjunto de datos

dfAgrupadoSample <- dfAgrupado2 |> 
  slice_head(n = 500)  

df52 <- dfAgrupadoSample[[5]][[2]]

fitdist(data = df52$hora_llegada, distr = 'exp', method = 'mle')

dfAgrupadoSample |> View()

fun_p <- function(df) {
  ajExp <- fitdist(as.numeric(na.omit(df$tiempo_servicio)), 'exp', method = 'mle')
  test <-  ks.test(as.numeric(na.omit(df$tiempo_servicio)), 'pexp', rate = ajExp$estimate)
  
  return(test$p.value)
}

dfAgrupadoSample |> 
  mutate(
    promedio_interarribo = 
      map(data,
          possibly( \(x) mean(x$interarribo, na.rm = T))),
    p_value = 
      map(data, 
          possibly(fun_p))
    ) |> 
  View()


dfAgrupadoSample |> 
  mutate(
    blabla= map(.x = data, 
                .f = ~fitdist(data = .x$hora_llegada, distr = 'exp', method = 'mle'))
    # promedio_interarribo = map(data, \(x) mean(x$interarribo, na.rm =T)),
    # p_value = map(data, \(x) c(2,4)),
    # pVvalue = map(data, 
    #               \(x) {
    #                 
    #                 fitdist(x$hora_llegada, 'exp', method = 'mle')
    #                 })
  ) 

dfAgrupadoSample |> 
  mutate(test = 
           map(data, 
               possibly(~ cor.test(.x$hora_llamado, .x$hora_llegada))
               )
  )


