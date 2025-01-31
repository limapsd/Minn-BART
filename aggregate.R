load("MacroUncQ.rda")

lags <- 13
M  <- length(var.sets$`VAR-22`)
K  <- M*lags

T0 <- 160
T1 <- nrow(yraw)
h  <- 4
estimation_window <-  T1-T0-h

x_names <- paste0(var.sets$`VAR-22`, ".t-", sort(rep(1:lags,M)))

model_names <- c("bart", "dart", "minn", "bartfsv", "dartfsv", "minnfsv")
num_models <- length(model_name)

point_forecasts_array <- array(data = NA, c(estimation_window, h, M, num_models))
dimnames(point_forecasts_array) <- list(paste0("w_",1:estimation_window), paste0("t+",1:h), var.sets$`VAR-22`, model_names)

var_count_array <- array(data = NA, c(estimation_window, K, M, num_models))
dimnames(var_count_array) <- list(paste0("w_",1:estimation_window), x_names, var.sets$`VAR-22`,model_names)

volatilty_forecasts_array <- array(data = NA, c(estimation_window, h, M, num_models))
dimnames(volatilty_forecasts_array) <- list(paste0("w_",1:estimation_window), paste0("t+",1:h), var.sets$`VAR-22`, model_names)

for(model in model_name){
  for(n_w in 1:estimation_window){
    
    files <- list.files("/Results")
    selected_file <- files[grep(paste0(model,"_window_", n_w, "\\.rds$"), files)]
    saved_model <- readRDS(paste0("/Results/",selected_file))
    
    predicitons <- saved_model$predictions
    volatility  <- saved_model$sigma_predictions
    var_count   <- saved_model$var_count_matrix
    
    point_forecasts_array[n_w,,,model]     <- apply(predicitons, 2:3, median)
    var_count_array[n_w,,,model]           <- apply(var_count>0, 2:3, mean)
    volatilty_forecasts_array[n_w,,,model] <- apply(volatility, 2:3, median)
  }
}

aggregation <- list("forecast" = point_forecasts_array,"vol_forecast" = volatilty_forecasts_array, "var_count" = var_count_array)
saveRDS(aggregation,file = paste0("Aggregation/aggr.rds"))

