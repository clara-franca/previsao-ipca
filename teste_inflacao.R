############################################################
## file: teste_inflacao.R
## title: Previsão do IPCA
############################################################

rm(list = ls(all.names = TRUE)) # clear

############################################################
## Packages
############################################################

pacman::p_load(tidyverse,readxl,tsibble,forecast,keras,
               tensorflow,reticulate,nnfor,WaveletArima,
               imputeTS,caret,OOS,writexl,
               "neuralnet","NeuralNetTools",
               rsample)


############################################################
## Data
############################################################

load(file="covariates_Train_ts.RData")
load(file="covariates_Test_ts.RData")
load(file="dependent_variables_ts.RData")
load(file="responses_series_level.RData")

############################################################
## Functions - cross validation
############################################################

f_arima <- function(y, h, seed){
  fit = forecast::auto.arima(y)
  forecast::forecast(fit, h)
}

f_sarimax_lasso <- function(y, h, xreg, newxreg, seed){
  set.seed(seed)
  lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian", 
                                 intercept = FALSE,
                                 alpha =1) 
  lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
  namesCoef <- colnames(xreg)[lfc]
  print(namesCoef)
  
  if (sum(lfc) == 0) {
    namesCoef <- colnames(xreg)
  } else {
    namesCoef <- colnames(xreg)[lfc]
  }
  

  fc_object <- tryCatch({
    
    fit <- forecast::auto.arima(y, xreg = as.matrix(as.data.frame(xreg[,which(colnames(xreg) %in% c(namesCoef))])))
    forecast::forecast(fit, h=h, xreg= as.matrix(as.data.frame(newxreg[,which(colnames(newxreg) %in% c(namesCoef))])))
    
  }, error = function(e) {
    
    cat("ERROR caught in f_sarimax_lasso:", conditionMessage(e), "\n")
    cat("Falling back to a simpler model for this iteration.\n")
    
    fit_fallback <- forecast::auto.arima(y)
    return(forecast::forecast(fit_fallback, h=h))
    
  })
  
  return(fc_object)
}


f_nn <- function(y, h, xreg, newxreg, seed) {
  set.seed(seed) 
  lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                 alpha =1) 
  namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
  lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
  namesCoef <- colnames(xreg)[lfc]
  print(namesCoef)
  if (sum(lfc) == 0) {
    namesCoef <- colnames(xreg)
  } else {
    namesCoef <- colnames(xreg)[lfc]
  }
  
  xtrain <- as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])) 
  xtest <- as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))]))
  
  y_train <- as.vector(y)
  x_train <- as.matrix(xtrain)
  x_test  <- as.matrix(xtest[1:h, , drop=FALSE])

  scaler <- preProcess(rbind(x_train, x_test), method = c("center", "scale"))
  x_train <- predict(scaler, x_train)
  x_test  <- predict(scaler, x_test)
  y_mean <- mean(y_train)
  y_sd   <- sd(y_train)
  y_train <- (y_train - y_mean) / y_sd
  
  model <- keras_model_sequential() %>%
    layer_dense(units=128, activation="tanh", input_shape=ncol(x_train)) %>%
    layer_dense(units=64, activation="tanh") %>%
    layer_dense(units=1)
  model %>% compile(optimizer=optimizer_adamax(learning_rate = 0.01), loss="mse")
  
  model %>% fit(
    x=x_train, y=y_train,
    epochs=250, batch_size=32, verbose=1,
    callbacks=callback_early_stopping(
      monitor="loss", patience=25, restore_best_weights=TRUE
    ),
    validation_split=0.2
  )
  
  pred_s  <- as.numeric(model %>% predict(x_test))
  nnPred <- pred_s * y_sd + y_mean
  
  nn <- forecast::meanf(as.vector(nnPred),h=h)
  nn$mean <- as.vector(nnPred)
  nn$lower[,1] <- NA
  nn$lower[,2] <- NA
  nn$upper[,1] <- NA
  nn$upper[,2] <- NA
  nn
  
}

f_nnetar <- function(y, h, xreg, newxreg, seed){
  set.seed(seed)
  lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                 alpha =1) 
  
  namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
  lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
  namesCoef <- colnames(xreg)[lfc]
  print(namesCoef)
  if (sum(lfc) == 0) {
    namesCoef <- colnames(xreg) 
  } else {
    namesCoef <- colnames(xreg)[lfc]
  }
  
  forecast::nnetar(y, xreg = as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),MaxNWts = 5000 ) %>%
    forecast::forecast(h=h, xreg= as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))]))) 
  
}

############################################################
## Functions - forecasting
############################################################

f_print_arima <- function(pib_go_ts, 
                          Hor,
                          Start_Hor,
                          responses_series_level,
                          seed = seed){
  
  forecast_ahead <- forecast::auto.arima(pib_go_ts) %>% forecast::forecast(h=Hor) 
  ## forecast values
  series_values <- as.vector(responses_series_level[,paste(variaveis_forecasting)])
  forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]
  print(summary(forecast_ahead))
  return(forecast_ahead_indice)
}

f_print_sarimax_lasso <- function(pib_go_ts=pib_go_ts,
                                  Hor=Hor,
                                  Start_Hor=Start_Hor,
                                  responses_series_level=responses_series_level,
                                  covariates_Train_ts=covariates_Train_ts,
                                  covariates_Test_ts=covariates_Test_ts,
                                  seed = seed){
  
  set.seed(seed)
  lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                                 intercept = FALSE,
                                 alpha =1)
  namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
  lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
  namesCoef <- colnames(covariates_Train_ts)[lfc]
  print(namesCoef)
  if (sum(lfc) == 0) {
    namesCoef <- colnames(covariates_Train_ts)  # fallback: todas as covariáveis
  } else {
    namesCoef <- colnames(covariates_Train_ts)[lfc]
  }
  
  X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
  X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))
  
  
  forecast_ahead <- forecast::auto.arima(pib_go_ts, xreg = X_reg_train) %>%
    forecast::forecast(h=Hor, xreg= X_reg_test[1:Hor,])
  
  ## forecast values
  series_values <- as.vector(responses_series_level[,paste(variaveis_forecasting)])
  forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]
  
  print(summary(forecast_ahead))
  
  return(forecast_ahead_indice)
}



f_print_nn <- function(pib_go_ts=pib_go_ts, 
                       Hor=Hor,
                       Start_Hor=Start_Hor,
                       responses_series_level=responses_series_level,
                       covariates_Train_ts=covariates_Train_ts,
                       covariates_Test_ts=covariates_Test_ts,
                       seed = seed) {
  set.seed(seed) 
  lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian", 
                                 intercept = FALSE,
                                 alpha =1) 
  namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
  lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
  namesCoef <- colnames(covariates_Train_ts)[lfc]
  print(namesCoef)
  if (sum(lfc) == 0) {
    namesCoef <- colnames(covariates_Train_ts)  # fallback: todas as covariáveis
  } else {
    namesCoef <- colnames(covariates_Train_ts)[lfc]
  }
  
  
  xtrain <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
  xtest <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))
  
  
  y_train <- as.vector(pib_go_ts)
  x_train <- as.matrix(xtrain)
  x_test  <- as.matrix(xtest[1:Hor, , drop=FALSE])
  
  ## ajustando escala
  scaler <- preProcess(rbind(x_train, x_test), method = c("center", "scale"))
  x_train <- predict(scaler, x_train)
  x_test  <- predict(scaler, x_test)
  y_mean <- mean(y_train)
  y_sd   <- sd(y_train)
  y_train <- (y_train - y_mean) / y_sd
  
  model <- keras_model_sequential() %>%
    layer_dense(units=128, activation="tanh", input_shape=ncol(x_train)) %>%
    layer_dense(units=64, activation="tanh") %>%
    layer_dense(units=1)
  model %>% compile(optimizer_adamax(learning_rate = 0.01), loss="mse")
  
  model %>% fit(
    x=x_train, y=y_train,
    epochs=250, batch_size=32, verbose=0,
    callbacks=callback_early_stopping(
      monitor="loss", patience=25, restore_best_weights=TRUE
    ),
    validation_split=0.2
  )
  
  
  pred_s  <- model %>% predict(x_test)
  final_prediction <- pred_s * y_sd + y_mean
  
  
  ## forecast values
  series_values <- as.vector(responses_series_level[,paste(variaveis_forecasting)])
  forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(final_prediction))))[-1]
  
  return(forecast_ahead_indice)
}

f_print_nnetar <- function(pib_go_ts=pib_go_ts, 
                           Hor=Hor,
                           Start_Hor=Start_Hor,
                           responses_series_level=responses_series_level,
                           covariates_Train_ts=covariates_Train_ts,
                           covariates_Test_ts=covariates_Test_ts,
                           seed = seed){
  
  set.seed(seed) 
  lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian", 
                                 intercept = FALSE,
                                 alpha =1) 
  namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
  lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
  namesCoef <- colnames(covariates_Train_ts)[lfc]
  print(namesCoef)
  if (sum(lfc) == 0) {
    namesCoef <- colnames(covariates_Train_ts)  # fallback: todas as covariáveis
  } else {
    namesCoef <- colnames(covariates_Train_ts)[lfc]
  }
  
  X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
  X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))
  
  
  forecast_ahead <- forecast::nnetar(pib_go_ts, xreg = X_reg_train, MaxNWts = 5000) %>%
    forecast::forecast(h=Hor, xreg= X_reg_test[1:Hor,]) 
  
  ## forecast values
  series_values <- as.vector(responses_series_level[,paste(variaveis_forecasting)])
  forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]
  
  
  print(summary(forecast_ahead))
  return(forecast_ahead_indice)
}


############################################################
## Function - forecasting - type
############################################################

modelType_forecasts <- function(pib_go_ts=pib_go_ts, 
                                Hor=Hor,
                                Start_Hor=Start_Hor,
                                responses_series_level=responses_series_level,
                                covariates_Train_ts=covariates_Train_ts,
                                covariates_Test_ts=covariates_Test_ts,
                                modelType = "SARIMAX",
                                seed){
  
  
  
  CV_for <- switch(modelType,
                   SARIMA      = f_print_arima(pib_go_ts=pib_go_ts, 
                                               Hor=Hor,
                                               Start_Hor=Start_Hor,
                                               responses_series_level=responses_series_level,
                                               seed = seed),
                   
                   SARIMAX     =  f_print_sarimax_lasso(pib_go_ts=pib_go_ts, 
                                                        Hor=Hor,
                                                        Start_Hor=Start_Hor,
                                                        responses_series_level=responses_series_level,
                                                        covariates_Train_ts=covariates_Train_ts,
                                                        covariates_Test_ts=covariates_Test_ts,
                                                        seed = seed),
                   REDENEURAL     =  f_print_nn(pib_go_ts=pib_go_ts, 
                                                Hor=Hor,
                                                Start_Hor=Start_Hor,
                                                responses_series_level=responses_series_level,
                                                covariates_Train_ts=covariates_Train_ts,
                                                covariates_Test_ts=covariates_Test_ts,
                                                seed = seed),
                   REDENEURALETAR     =  f_print_nnetar(pib_go_ts=pib_go_ts, 
                                                        Hor=Hor,
                                                        Start_Hor=Start_Hor,
                                                        responses_series_level=responses_series_level,
                                                        covariates_Train_ts=covariates_Train_ts,
                                                        covariates_Test_ts=covariates_Test_ts,
                                                        seed = seed)
                   
  )
  
  return(CV_for)
}


############################################################
## Loop
############################################################

variaveis_forecasting <-
  c(
    "y_adjusted.ipca_br"  
  )

# matrix to store forecasts
## Define horizon/window
Start_Hor <- '2024-10-01'


## Data filtration
pib_go_ts <- ts(dependent_variables_ts[1:164,which(colnames(dependent_variables_ts)==
                                                     paste(variaveis_forecasting))], 
                start=c(2011,2), frequency = 12)
seed <- -1238477562

#############################################################################
### INÍCIO DA SEÇÃO DE VALIDAÇÃO CRUZADA COM LOOP FOR EXPLÍCITO            ###
#############################################################################

## Definições para a Validação Cruzada
Hor_CV <- 12      # Horizonte de previsão para a validação cruzada
InPer <- 120      # Tamanho da janela de treinamento inicial

# Número total de observações na série
n_total <- length(pib_go_ts)

# Pontos de tempo onde a previsão será originada
# Começa do final da janela inicial até o ponto onde ainda é possível prever 'Hor_CV' passos
forecast_origins <- InPer:(n_total - Hor_CV)

# Matrizes para armazenar os erros para cada horizonte (h=1, ..., 12)
errors_arima <- matrix(NA, nrow = length(forecast_origins), ncol = Hor_CV)
errors_sarimax_lasso <- matrix(NA, nrow = length(forecast_origins), ncol = Hor_CV)
errors_nn <- matrix(NA, nrow = length(forecast_origins), ncol = Hor_CV)
errors_nnetar <- matrix(NA, nrow = length(forecast_origins), ncol = Hor_CV)

# Loop principal de validação cruzada
for (k in 1:length(forecast_origins)) {
  
  # Ponto de origem da previsão atual (o último ponto no conjunto de treinamento)
  origin <- forecast_origins[k]
  
  cat("Executando validação cruzada na origem:", origin, "/", n_total - Hor_CV, "\n")
  
  # 1. Fatiar os dados em treino e teste para esta iteração
  y_train <- window(pib_go_ts, end = time(pib_go_ts)[origin])
  xreg_train <- as.data.frame(covariates_Train_ts[1:origin, , drop = FALSE])
  
  y_test_actual <- pib_go_ts[(origin + 1):(origin + Hor_CV)]
  newxreg_test <- as.data.frame(covariates_Train_ts[(origin + 1):(origin + Hor_CV), , drop = FALSE])
  
  # 2. Treinar os modelos, prever e calcular o erro
  
  # --- Modelo ARIMA ---
  fc_arima <- f_arima(y_train, h = Hor_CV, seed = seed)
  errors_arima[k, ] <- y_test_actual - fc_arima$mean
  
  # --- Modelo SARIMAX LASSO ---
  fc_sarimax <- f_sarimax_lasso(y_train, h = Hor_CV, xreg = xreg_train, newxreg = newxreg_test, seed = seed)
  errors_sarimax_lasso[k, ] <- y_test_actual - fc_sarimax$mean
  
  # --- Modelo Rede Neural ---
  fc_nn <- f_nn(y_train, h = Hor_CV, xreg = xreg_train, newxreg = newxreg_test, seed = seed)
  errors_nn[k, ] <- y_test_actual - fc_nn$mean
  
  # --- Modelo NNETAR ---
  fc_nnetar <- f_nnetar(y_train, h = Hor_CV, xreg = xreg_train, newxreg = newxreg_test, seed = seed)
  errors_nnetar[k, ] <- y_test_actual - fc_nnetar$mean
}

# 3. Calcular as métricas de erro (MAE) a partir das matrizes de erro
MAE_arima <- colMeans(abs(errors_arima), na.rm = TRUE)
RMSE_arima <- colMeans(errors_arima^2, na.rm = TRUE)^0.5
MAE_sarimax_lasso <- colMeans(abs(errors_sarimax_lasso), na.rm = TRUE)
RMSE_sarimax_lasso <- colMeans(errors_sarimax_lasso^2, na.rm = TRUE)^0.5
MAE_nn <- colMeans(abs(errors_nn), na.rm = TRUE)
RMSE_nn <- colMeans(errors_nn^2, na.rm = TRUE)^0.5
MAE_nnetar <- colMeans(abs(errors_nnetar), na.rm = TRUE)
RMSE_nnetar <- colMeans(errors_nnetar^2, na.rm = TRUE)^0.5

#############################################################################
### FIM DA SEÇÃO DE VALIDAÇÃO CRUZADA                                       ###
#############################################################################


## Tabela com os erros
tab = data.frame(MAE_arima,            
                 MAE_sarimax_lasso,
                 MAE_nn,
                 MAE_nnetar
)
print(tab)
tab2 = data.frame(RMSE_arima,            
                  RMSE_sarimax_lasso,
                  RMSE_nn,
                  RMSE_nnetar
)

names_models <- c("SARIMA",            ## 1
                  "SARIMAX",
                  "REDENEURAL",
                  "REDENEURALETAR")
write_xlsx(tab,paste0(paste0("hor_meantab_",variaveis_forecasting),".xlsx"))
write_xlsx(tab2,paste0(paste0("hor_meantab_RMSE",variaveis_forecasting),".xlsx"))
# Print best models
write.table(knitr::kable(as.data.frame(tab), "pipe"),
            file=paste0(paste0("hor_meantab_",variaveis_forecasting),".txt"), append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)

tab_mean = apply(tab,2,mean,na.rm=TRUE)
maes_models <- data.frame(names_models,tab_mean)
maes_models_sorted <- maes_models[order(maes_models$tab_mean),]
print(maes_models_sorted)

# Horizonte para a previsão final
Hor <- 39  

Best_1_forecast <- modelType_forecasts(pib_go_ts=pib_go_ts, 
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = maes_models_sorted$names_models[1],
                                       seed = seed)

Best_2_forecast <- modelType_forecasts(pib_go_ts=pib_go_ts, 
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = maes_models_sorted$names_models[2],
                                       seed = seed)

Best_3_forecast <- modelType_forecasts(pib_go_ts=pib_go_ts, 
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = maes_models_sorted$names_models[3],
                                       seed = seed)

Best_4_forecast <- modelType_forecasts(pib_go_ts=pib_go_ts, 
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = maes_models_sorted$names_models[4],
                                       seed = seed)



Bests_forecasts <- data.frame(
  Best_1_forecast,
  Best_2_forecast,
  Best_3_forecast,
  Best_4_forecast)


################################################
## Inflação acumulada
################################################

## compute percentage change
PIB_TS <- ts(responses_series_level[1:165,which(colnames(responses_series_level)==
                                                  paste(variaveis_forecasting))], 
             start=c(2011,1), frequency = 12)

r_o_c <- function(x, lag = 1){
  n <- length(x)
  val <-  ((x[(1+lag):n] - x[1:(n-lag)])/x[1:(n-lag)])*100
  return(val)
}

# A lógica aqui para combinar dados observados com previsões precisa ser cuidadosa.
# Vamos garantir que os dados observados tenham o comprimento correto.
# O número de meses observados é 22 (de 2022-12 a 2024-09).
observed_data_for_plot <- window(PIB_TS, start=c(2022,12), end=c(2024,9))

PC_VL_I <- as.data.frame(rbind(matrix(rep(observed_data_for_plot, 4), ncol=4),
                               as.matrix(Bests_forecasts)))
colnames(PC_VL_I) <- c(maes_models_sorted$names_models)

# O número total de linhas deve ser 22 (observado) + 39 (previsto) = 61
# A sequência de datas deve refletir isso
date_sequence <- seq.Date(from = as.Date("2022-12-01"), by = 'month', length.out = nrow(PC_VL_I))

PC_VL_I$ano <- format(date_sequence, format= "%Y")
PC_VL_I$mon <- format(date_sequence, format= "%m")

## indices
PC_VL_I_PRINT <- PC_VL_I 
PC_VL_I_PRINT$mon <- NULL
PC_VL_I_PRINT$ano <- NULL 
PC_VL_I_PRINT$mes_ano <- format(date_sequence, format= "%b %Y") 
write_xlsx(PC_VL_I_PRINT, paste0(paste0("inflacao_indices_",variaveis_forecasting),".xlsx"))

# Recalculando a inflação acumulada
PC_VL_INFLA_base <- PC_VL_I[, -which(colnames(PC_VL_I) %in% c("ano", "mon"))]
PC_VL_INFLA_roc <- as.data.frame(apply(PC_VL_INFLA_base, 2, r_o_c))
PC_VL_INFLA_roc <- 1 + PC_VL_INFLA_roc / 100
PC_VL_INFLA_roc$ano <- format(date_sequence[-1], "%Y") # As datas começam do segundo mês

PC_VL_INFLA <- PC_VL_INFLA_roc %>%
  group_by(ano) %>%
  summarise(across(everything(), cumprod))

PC_VL_INFLA <- as.data.frame(PC_VL_INFLA)
PC_VL_INFLA <- subset(PC_VL_INFLA, ano >= 2024)
anos <- PC_VL_INFLA$ano
PC_VL_INFLA$ano <- NULL
PC_VL_INFLA <- round((PC_VL_INFLA - 1) * 100, 2)
PC_VL_INFLA$Medias <- round(rowMeans(PC_VL_INFLA, na.rm=TRUE), 2)

# A sequência de datas para o resultado final
final_date_sequence <- seq.Date(from = as.Date("2024-01-01"), by = 'month', length.out = nrow(PC_VL_INFLA))
PC_VL_INFLA$mes_ano <- format(final_date_sequence, format= "%b %Y") 

write_xlsx(PC_VL_INFLA, paste0(paste0("inflacao_acumulada_",variaveis_forecasting),".xlsx"))

