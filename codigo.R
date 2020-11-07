

# Comentarios iniciales ####

  'A continuacion se presenta un codigo REPLICABLE para cualquier problema
   de regresion sin importar la naturaleza del negocio, se puede ajustar cambiando
   los parametros necesarios y entendiendo la estructura, pero no deberia ser dificil para
   la persona interesada en ejecutar un solucion rapida y entrenar varios modelos en un mismo
   codigo. Los modelos se basan en la libreia caret, por tanto algunos procesos pueden ser demorados.'
  
#PD: Se haran los comentarios necesarios, no sobre todo. :)


# Intro ####

# ambiente

rm(list = ls())
gc()
options(scipen = '999')

# librerias
# install.packages('pacman') # Se utiliza pacman para agilizar la instalacion
pacman::p_load(
  data.table,
  caret,
  lubridate,
  pROC,
  ROCR,
  doParallel,
  DMwR,
  MASS,
  readxl,
  naniar,
  tidyverse
)


# datos, arreglos y demas ####
data = read_xlsx('data.xlsx') %>% as.data.table()

head(data)







