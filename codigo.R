


# Comentarios iniciales ####

'A continuacion se presenta un codigo REPLICABLE para cualquier problema
   de regresion sin importar la naturaleza del negocio, se puede ajustar cambiando
   los parametros necesarios y entendiendo la estructura, pero no deberia ser dificil para
   la persona interesada en ejecutar una solucion rapida para entrenar varios modelos en un mismo
   codigo. Los modelos se basan en la libreria caret, por tanto algunos procesos pueden ser demorados.'

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
  mice,
  skimr,
  corrplot
)

# datos, arreglos y demas ####
data = read_xlsx('data.xlsx') %>% as.data.table()
names(data) = tolower(names(data))
glimpse(data)

head(data)

# Asignar tipos de var
var_num =
  c(
    'ant_avaluo_trim',
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
    'area_privada_area'
  )

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
data[, c_calidcocina := NULL]

keep =
  c(
    #'ant_avaluo_trim',
    'tot_banos',
    'n_deposito',
    'n_habitaciones',
    'n_totalgarajes',
    'n_pisos',
    'n_sotanos',
    #'n_vetustez',
    'a_edipiso',
    'k_ascensor',
    'latitud_double',
    'longitud_double'
  )

temp = data[, mget(keep)]
mean(temp$a_edipiso, na.rm = T) %>% print()

# imputacion con mice
# mice solo funciona con variables numericas
mice(
  temp,
  m = 5,
  maxit = 50,
  meth = 'pmm',
  seed = 500,
  printFlag = F
) %>%
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

# forma de ver el summary 
skim(data)

# box plot sobre alguna variable
boxplot(data$area_privada_valfinal)
summary(data$area_privada_valfinal)
data[area_privada_valfinal > 707584036]

# drop de variables
drop = c(
  'n_vetustez',
  'latitud_double',
  'longitud_double',
  'zona_av',
  'barrio_map',
  'cluster'
)
data[, (drop) := NULL]




# MODELACION ####


# Correlaciones

var_num =
  c(
    'ant_avaluo_trim',
    'tot_banos',
    'n_deposito',
    'n_habitaciones',
    'n_totalgarajes',
    'n_pisos',
    'n_sotanos',
    'a_edipiso',
    'k_ascensor',
    "area_privada_valfinal",
    'area_privada_area'
  )

temp = data[, mget(var_num)]
corr = rcorr(as.matrix(temp))
corr

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(corr$r, corr$P)

# Correlaciones insignificantes en blanco
corrplot(corr$r, order="hclust", 
         p.mat = corr$P, sig.level = 0.01, insig = "blank")


# Modelos clasicos: primera aproximacion al problema
# formula
names(data)
mymodel <-
  as.formula(
    "area_privada_valfinal ~ area_privada_area + tot_banos + n_deposito + estrato + 
     + n_totalgarajes + c_conjagrupcerr + c_ubicacioninm + n_pisos + 
    n_sotanos + n_vetustez + a_edipiso + c_claseinmueble + idcategoria + k_ascensor +
    zona_av"
  )

data[, .N, f_fechaavaluo][order(f_fechaavaluo)]

#






# fracionamiento del dataset
# train dates 01/01/2017 - 31/12/2018
# dev dates 01/01/2019 - 30/09/2019
# test 01/10/2019 - 31/12/2019

# modificacion y asignacion
#data[, f_fechaavaluo := as.Date(f_fechaavaluo, '%Y/%m/%d')]
data$f_fechaavaluo = ymd(df$f_fechaavaluo)
df = copy(data)

data = df %>%
  filter(year(f_fechaavaluo) %in% c(2017:2019))

train = data %>%
  filter(year(f_fechaavaluo) %in% c(2017:2018))

test <-
  df[f_fechaavaluo >= "2019-01-01" & f_fechaavaluo <= "2019-09-30"]

hang_on <-
  df[f_fechaavaluo >= "2019-10-01" & f_fechaavaluo <= "2019-12-31"]













