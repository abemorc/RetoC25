

lambda2 <- mean(dfPruebas$tiempo_servicio)
lambda2

ajuste_exponencial2 <- fitdist(dfPruebas$tiempo_servicio,
                              distr = 'exp', method = 'mle')
summary(ajuste_exponencial2)
1/lambda2

ajuste_weibull2 <- fitdist(dfPruebas$tiempo_servicio,
                          distr = 'weibull', 
                          method = 'mle',
                          start = list(scale = 1, shape = 1))

summary(ajuste_weibull2)

ks.test(dfPruebas$tiempo_servicio, 
        'pexp', rate = ajuste_exponencial2$estimate)

# no seguirian una dist exp


ks.test(dfPruebas$tiempo_servicio, 
        'pweibull', 
        scale = ajuste_weibull2$estimate[1],
        shape= ajuste_weibull2$estimate[2])


# tampoco seguirian una weibull


