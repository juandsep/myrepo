

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
  funModeling,
  mice
  
)

# datos, arreglos y demas ####
data = read_xlsx('data.xlsx') %>% as.data.table()
names(data) = tolower(names(data))
glimpse(data)

# modificacion de variables y asignacion 
data[, f_fechaavaluo := as.Date(f_fechaavaluo, '%Y/%m/%d')]
head(data)

# Asignar tipos de var
var_num =
  c('ant_avaluo_trim',
    'tot_banos',
    'n_deposito',
    'n_habitaciones',
    'n_totalgarajes',
    'n_pisos',
    'n_sotanos',
    'n_vetustez',
    'a_edipiso',
    'k_ascensor',
    'latitud_double',
    'longitud_double',
    "area_privada_valfinal",
    'area_privada_area')

#Variables numericas
data[, (var_num) := lapply(.SD, as.numeric), .SDcols = var_num]
no_factor = c("f_fechaavaluo", var_num)

#Variables factor
factor_vars = names(data)[!(names(data) %in% no_factor)]
data[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

numericVars <- which(sapply(data, is.numeric)) 
numericVarNames <- names(numericVars) 
cat('Hay', length(numericVars), 'variables numericas')


# verificar nas
naniar::vis_miss(data)
nrow(data)
data = data[!is.na(c_calidcocina)]
nrow(data)

temp = data[, mget(var_num)]
mean(temp$a_edipiso, na.rm = T) %>% print()

# imputacion con mice
# mice solo funciona con variables numericas
mice(temp, m=5, maxit=50, meth='pmm',seed=500, printFlag = F) %>% 
  mice::complete() -> temp

data[, a_edipiso := temp$a_edipiso]

naniar::vis_miss(data)

# visualizacion de ceros
print(status(data))

# frecuencias de categorias
freq(data)

# descriptivas de la variables numericas
print(profiling_num(data))
plot_num(data)
describe(data)







