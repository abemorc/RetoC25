
library(googledrive)
# library(tidyverse)


drive_deauth()
drive_user()


datos_coppel <- drive_get(as_id('1Z-o8eG5l8J0-rzFS2UcE1m8FwwFw_67K'))

destino <- paste0('RawData/', datos_coppel$name)
destino

if (!file.exists(destino)) {
  drive_download(datos_coppel, path = destino)
} 


dfRaw <- vroom::vroom(file = destino, delim = ',')

str(dfRaw)

# cargar funciones auxiliares
source('Code/R/utils.R')

