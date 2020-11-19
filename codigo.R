

'http://web.ipac.caltech.edu/staff/fmasci/home/astro_refs/BuildingPredictiveModelsR_caret.pdf'

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
if (!require("pacman"))
  install.packages("pacman")
# Se utiliza pacman para agilizar la instalacion
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
  corrplot,
  dataPreparation,
  tictoc,
  beepr,
  rattle
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
set.seed(111)
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

# modelacion ####

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
    cor  = (cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(corr$r, corr$P)

# Correlaciones insignificantes en blanco
corrplot(
  corr$r,
  order = "hclust",
  p.mat = corr$P,
  sig.level = 0.01,
  insig = "blank"
)


# formula
names(data)
mymodel <-
  as.formula(
    "area_privada_valfinal ~ ant_avaluo_trim + tot_banos + n_deposito +
    n_habitaciones + estrato + n_totalgarajes + c_conjagrupcerr + c_ubicacioninm +
    n_sotanos + a_edipiso + c_claseinmueble + idcategoria + k_ascensor + area_privada_area"
  )

# Standarizacion de las variables numericas
numericVars <- which(sapply(data, is.numeric))
numericVarNames <- names(numericVars)
cat('Hay', length(numericVars), 'variables numericas')

scales =
  build_scales(data_set = data,
               cols = numericVarNames,
               verbose = TRUE)

print(scales)
data %>% head()

data = fast_scale(data_set = data,
                  scales = scales,
                  verbose = TRUE)

factor_vars = which(sapply(data, is.factor))
factorVarNames <- names(factor_vars)
cat('Hay', length(factor_vars), 'variables numericas')

# codificacion en variables categoricas
# encoding = build_encoding(data_set = data,
#                           cols = factorVarNames,
#                           verbose = TRUE)
#
# data = one_hot_encoder(
#   data_set = data,
#   encoding = encoding,
#   drop = TRUE,
#   verbose = TRUE
# )

# Se realiza un fraccionamiento por % de data set de la siguiente forma:
# 0.7 Train, 0.15 Val, 0.15 Test
# El ejercicio original se fraciona por fechas, pero por temas de replicacion, se
# opta por realizar los dos tipos de fraccionamiento. Por tanto se haran los dos ejercicios.
#

## Fraccionamiento por %
# train - test
set.seed(111)
dt = sort(sample(nrow(data), nrow(data) * .7))
train = data[dt,]
test = data[-dt,]

# pd:dev se desarrolla mas adelante si se requiere

# parametros y algunos ajustes para los modelos
cores = parallel::detectCores() - 1
cl = makePSOCKcluster(cores)
registerDoParallel(cl)

fitControl = trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10,
  allowParallel = T
)

## modelos

# 1 stepwise
tic()
step.model = train(
  mymodel,
  data = data,
  method = "lmStepAIC",
  trControl = fitControl,
  trace = F
)
toc()
step.model$results
beep(3)
# PD: Entre mas variables categoricas tenga el modelo, mayor es el trabajo computacional

# coeficientes
# step.model$finalModel
# summary
summary(step.model$finalModel)

# Todas las variables 'funcional', se decide seguir con la misma ecuacion.
rm(step.model) %>% gc()

modelLookup("rpart2")
# cart Model
cartModel <-
  train(
    mymodel,
    data = train,
    metric = "RMSE",
    method = "rpart2",
    trControl = fitControl
  )

cartModel$results
plot(cartModel)
ggplot(varImp(cartModel))

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(cartModel$finalModel)
text(cartModel$finalModel, digits = 3)

# Bayesian Generalized Linear Model
bayesModel =
  train(
    mymodel,
    data = train,
    metric = "RMSE",
    method = "bayesglm",
    trControl = fitControl
  )

bayesModel$results
ggplot(varImp(bayesModel))


# Boosted Generalized Linear Model
glmModel =
  train(
    mymodel,
    data = train,
    metric = "RMSE",
    method = 'glmboost',
    trControl = fitControl
  )

glmModel$results
ggplot(varImp(glmModel))
plot(glmModel)

# Boosted Trees
gbmModel =
  train(
    mymodel,
    data = train,
    metric = "RMSE",
    method = "gbm",
    trControl = fitControl,
    verbose = FALSE
  )

gbmModel$results
plot(gbmModel)

gbmGrid <-  expand.grid(
  interaction.depth = c(1, 5, 9),
  n.trees = (1:30) * 50,
  shrinkage = 0.1,
  n.minobsinnode = 20
)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(
  Class ~ .,
  data = training,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE,
  ## Now specify the exact models
  ## to evaluate:
  tuneGrid = gbmGrid
)
gbmFit2

# Random Forest
tic()
rfModel =
  train(
    mymodel,
    data = train,
    metric = "RMSE",
    method = 'rf',
    trControl = fitControl
  )
beep(3)
toc()

rfModel$results
ggplot(varImp(rfModel))
plot(rfModel)

# XGBoost
tune.gridxgb = expand.grid(
  eta = c(0.05, 0.3, 0.075),
  # 3
  nrounds = c(75, 100, 200),
  # 3
  max_depth = 2:7,
  # 4
  min_child_weight = c(2.0, 2.25),
  #2
  colsample_bytree = c(0.3, 0.4, 0.5),
  # 3
  gamma = 0,
  #1
  subsample = 1
)  # 1

# 3*3*4*2*3*1*1 = 216
dim(tune.gridxgb)

tic()
xgbModel =
  train(
    mymodel,
    data = train,
    metric = "RMSE",
    method = 'xgbTree',
    tuneGrid = tune.gridxgb,
    trControl = fitControl
  )
beep(3)
toc()

xgbModel$results
ggplot(varImp(xgbModel))
plot(xgbModel)


# Detener la paralelizacion 
stopCluster(cl)






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

# Informacion del ambiente
sessionInfo(package = NULL)

# Otros modelos que pueden ser utilizados con el paquete caret:
'https://rdrr.io/cran/caret/man/models.html'
'https://topepo.github.io/caret/train-models-by-tag.html'
