

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
  tidyverse,
  Hmisc,
  funModeling
)


# datos, arreglos y demas ####
data = read_xlsx('data.xlsx') %>% as.data.table()
names(data) = tolower(names(data))
glimpse(data)

# verificar nas
naniar::vis_miss(data)
nrow(data)
data = data[!is.na(c_calidcocina)]
nrow(data)

# visualizacion de ceros
print(status(data))

# frecuencias de categorias
freq(data)

# descriptivas de la variables numericas
print(profiling_num(data))
plot_num(data)

describe(data)

# modificacion de variables y asignacion 

data[, f_fechaavaluo := as.Date(f_fechaavaluo, '%Y/%m/%d')]

var_num =
  c('ant_avaluo_trim',
    "area_privada_valfinal",
    "area_privada_area")

#Variables numericas
df[, (var_num) := lapply(.SD, as.numeric), .SDcols = var_num]
no_factor <- c("f_fechaavaluo", var_num)

#Variables factor
factor_vars <- names(df)[!(names(df) %in% no_factor)]
df[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]








