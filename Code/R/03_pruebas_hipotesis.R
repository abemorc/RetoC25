
library(tidyverse)

# haremos pruebas con unas pocas observaciones
dftemp <- dfClean |> 
  slice_head(n=1000)

df1 <- dfClean |> 
  slice(6) 

dfPruebas <- df1$data[[1]]
dfPruebas |> View()

lambda <- mean(dfPruebas$interarribo, na.rm = T)
mean(dfPruebas$interarribo_sexagesimal, na.rm = T)

numBreaks <- nclass.Sturges(dfPruebas$interarribo)
numBreaks

hist(dfPruebas$interarribo, probability = T, breaks = 20)
lines(density(dfPruebas$interarribo |> na.omit()))
curve(dexp(x, rate = 1/lambda), add = T)
# density(dfPruebas$interarribo |> na.omit()) |> plot()
# ???
# hist(dfPruebas$interarribo_sexagesimal)
boxplot(dfPruebas$interarribo |> na.omit())





dftemp |> glimpse()

dftemp |> 
  mutate(promedio)