setwd("/Users/pl23287/Documents/Phd/Research/Minessota BART - Project/Code/Paper_Scripts/Minn-BART")

load("MacroUncQ.rda")


lags <- 13
M  <- length(var.sets$`VAR-22`)
K  <- M*lags

T0 <- 160
T1 <- nrow(yraw)
h  <- 4
estimation_window <-  T1-T0-h # total to be saved

yraw_f <- yraw[,var.sets$`VAR-22`]
time_index <- time(yraw)
rownames(yraw_f) <-time_index
# Training Data from 1965Q1 - 2004Q4 1-160
# 
# T0 <- 160
# T1 <- nrow(yraw_f)
# h  <- 4
y_test_array <-array(NA, dim = c(estimation_window, h, M),
                          dimnames =list(paste0("w_",1:estimation_window), paste0("t+",1:h), var.sets$`VAR-22`))

for(i in 1:(T1-T0-h)){
  y_train <- yraw_f[1:(T0 + i -1),]
  y_test  <- yraw_f[(T0 + i):(T0 + i + h -1),]
  y_test_array[i,,] <- y_test
}

file_names <- list.files("Aggregation")
model_names <- sub("_.*", "", file_names)
window_number <- as.numeric(sub(".*_(\\d+)\\.rds", "\\1", file_names))

# Create a data frame
df <- data.frame(file_name = file_names, model_name = model_names, window_number = window_number)
index <- order(df$model_name,df$window_number)
df_sorted <- df[index,]
models <- unique(model_names)

##########################################################
######## Array of Save Objects that are BART #############
##########################################################

num_models_bart  <- length(models[-7])
models_bart <-models[-7]
x_n <- paste0(colnames(var.sets$`VAR-22`), ".t-", sort(rep(1:lags,M)))

point_forecast_array_bart <- array(NA , dim = c(estimation_window, h, M, num_models_bart),
                       dimnames = list(paste0("w_",1:estimation_window), paste0("t+",1:h),
                       var.sets$`VAR-22`, models_bart)) 

errors2_forecast_array_bart  <-array(NA, dim = c(estimation_window,h, M, num_models_bart), 
                               dimnames = list(paste0("w_",1:estimation_window),
                                               paste0("t+",1:h),
                                               var.sets$`VAR-22`,
                                               models_bart)) 

var_count_array <-array(NA, dim = c(estimation_window, K, M, num_models_bart),
                        dimnames = list(paste0("w_",1:estimation_window),
                                        x_n,
                                        var.sets$`VAR-22`,
                                        models_bart) )

LPL_array  <- array(NA, dim = c(estimation_window, h, length(models)),
                        dimnames = list(paste0("w_",1:estimation_window),
                                   paste0("t+",1:h),
                                   models))


##########################################################
######## Array of Save Objects that are BVAR #############
##########################################################
num_models_bvar  <- length(models[7])
models_bvar <-models[7]
variables_names <- c(var.sets$`VAR-8`[2:8],"INDPRO")
M_bvar <- length(variables_names)

point_forecast_array_bvar    <- array(NA , dim = c(estimation_window, h, M_bvar, num_models_bvar),
                                   dimnames = list(paste0("w_",1:estimation_window), paste0("t+",1:h),
                                                   variables_names, models_bvar)) 


errors2_forecast_array_bvar  <-array(NA, dim = c(estimation_window,h, M_bvar, num_models_bvar), 
                                     dimnames = list(paste0("w_",1:estimation_window),
                                                     paste0("t+",1:h),
                                                     variables_names,
                                                     models_bvar)) 


y_test_array_bvar <-array(NA, dim = c(estimation_window, h, M_bvar),
                     dimnames =list(paste0("w_",1:estimation_window), paste0("t+",1:h), variables_names))

for(i in 1:(T1-T0-h)){
  yraw_fbv <- yraw[,variables_names]
  y_train <- yraw_fbv[1:(T0 + i -1),]
  y_test  <- yraw_fbv[(T0 + i):(T0 + i + h -1),]
  y_test_array_bvar[i,,] <- y_test
}



##########################################################
##########################################################

for(m in models){
  agg_df <- df_sorted[df_sorted$model_name == m,]
  cmax <- max(agg_df$window_number)
  cmin <- min(agg_df$window_number)
  for(w in cmin:cmax){
    if(m == "smallbvar"){
      selected_files <- file_names[grep(paste0(m, "_window_", w, "\\.rds$"), file_names)]
      saved_model <- readRDS(paste0("Aggregation/", selected_files))
      predictions <- saved_model$predictions
      point_forecast_array_bvar[w,,,m] <- apply(predictions, 1:2, median)
      errors2_forecast_array_bvar[w,,,m] <- (y_test_array_bvar[i,,] - apply(predictions, 1:2, median))^2
      LPL_array[w,,m] <- saved_model$LPL
    }else{
      selected_files <- file_names[grep(paste0(m, "_window_", w, "\\.rds$"), file_names)]
      saved_model <- readRDS(paste0("Aggregation/", selected_files))
      predictions <- saved_model$predictions
      point_forecast_array_bart[w,,,m] <-apply(predictions,2:3, median)
      errors2_forecast_array_bart[w,,,m] <- (y_test_array[i,,] - apply(predictions,2:3, median))^2
      var_count_array[w,,,m] <- apply(saved_model$var_count>0,2:3,mean)
      
      numerical_normalizer <- apply(saved_model$LPL_draws, 2 , max) - 700
      LPL <- log(colMeans(exp( t(t(saved_model$LPL_draws) - numerical_normalizer)))) + numerical_normalizer
      LPL_array[w,,m] <- LPL
    }
  }
}

LPL_results <- apply(LPL_array,2:3, sum)


par(mfrow = c(2,2))
for(i in 1:h){
  plot(cumsum(LPL_array[,i,"bart"]), type = "l",
              ylab = "", xlab = "",
              main = bquote(LPDS[t + .(i)]) )
  lines(cumsum(LPL_array[,i,"bartfsv"]), lty = 2, col ="black")
  
  lines(cumsum(LPL_array[,i,"minn"]), col = "red")
  lines(cumsum(LPL_array[,i,"minnfsv"]), lty = 2, col ="red")
  
  lines(cumsum(LPL_array[,i,"dart"]), col = "blue")
  lines(cumsum(LPL_array[,i,"dartfsv"]), lty = 2, col ="blue")
  
  legend("topright", 
         legend = c("BART", "BART-FSV", "MINN", "MINN-FSV", "DART", "DART-FSV"), 
         col = c("black", "black", "red", "red", "blue", "blue"), 
         lty = c(1, 2, 1, 2, 1, 2), 
         lwd = 0.5, # Line width (optional, adjust if needed)
         bty = "n",
         cex = 0.45) # Remove box around legend (optional)
}

rmse_models <- sqrt(apply(errors2_forecast_array_bart, 2:4, mean))
rmse_bvar <-sqrt(apply(errors2_forecast_array_bvar, 2:4, mean))

relative_rmse <- function(rmse_array,rmse_bvar = NULL, model = "bartfsv", variable, round_lvl = 3){
  if(model %in% c("bart", "bartfsv", "dart", "dartfsv", "minn", "minnfsv") ){
    d_m <- rmse_array[,variable,model]
    n_m <- rmse_array[,variable,]
    f_m <- round(n_m/d_m, round_lvl)
  }else{
    d_m <- rmse_bvar[,variable,model]
    n_m <- rmse_array[,variable,]
    f_m <- round(n_m/d_m, round_lvl)
  }
  
  return(f_m)
}


plot_model_performance <- function(data,variable_name = NULL) {
  data <-as.data.frame(data)
  # Convert row names to the time variable
  time_labels <- rownames(data)
  time <- 1:nrow(data)  # Numeric sequence for plotting
  
  # Define colors for models
  base_models <- unique(sub("fsv$", "", colnames(data)))  # Extract unique base model names
  colors <- c("black", "red", "blue")[1:length(base_models)]
  names(colors) <- base_models
  
  # Set up the plot
  plot(time, data[[1]], type = "n", ylim = c(min(data), max(data)),
       xaxt = "n", xlab = "", ylab = "", main = paste0(variable_name))
  axis(1, at = time, labels = time_labels)  # Add time labels
  
  
  # Loop through base models and add lines for solid and dashed variants
  for (base_model in base_models) {
    solid_col <- colors[base_model]
    dashed_col <- solid_col
    if (base_model %in% colnames(data)) {
      lines(time, data[[base_model]], col = solid_col, lty = 1)  # Solid line
    }
    fsv_col <- paste0(base_model, "fsv")
    if (fsv_col %in% colnames(data)) {
      lines(time, data[[fsv_col]], col = dashed_col, lty = 2)  # Dashed line
    }
  }
  
  # Add a legend dynamically
  legend_labels <- c()
  legend_colors <- c()
  legend_lty <- c()
  for (base_model in base_models) {
    solid_col <- colors[base_model]
    if (base_model %in% colnames(data)) {
      legend_labels <- c(legend_labels, base_model)
      legend_colors <- c(legend_colors, solid_col)
      legend_lty <- c(legend_lty, 1)
    }
    fsv_col <- paste0(base_model, "fsv")
    if (fsv_col %in% colnames(data)) {
      legend_labels <- c(legend_labels, fsv_col)
      legend_colors <- c(legend_colors, solid_col)
      legend_lty <- c(legend_lty, 2)
    }
  }
  
  legend("topright", legend = legend_labels, col = legend_colors, lty = legend_lty, cex = 1, bty = "n")
}


par(mfrow = c(1,3))
rmse_cpi_bvar <- relative_rmse(rmse_models,rmse_bvar = rmse_bvar, model = "smallbvar",variable ="CPIAUCSL")
rmse_ff_bvar  <- relative_rmse(rmse_models,rmse_bvar = rmse_bvar, model = "smallbvar",variable ="FEDFUNDS")
rmse_gdp_bvar <- relative_rmse(rmse_models,rmse_bvar = rmse_bvar, model = "smallbvar",variable ="GDPC1")

rmse_ind <-relative_rmse(rmse_models,rmse_bvar = rmse_bvar, model = "smallbvar", variable ="INDPRO")
rmse_gdp <-relative_rmse(rmse_models, rmse_bvar = rmse_bvar, model = "smallbvar", variable ="CE16OV")

plot_model_performance(rmse_gdp_bvar, variable_name = "GDPC1")
plot_model_performance(rmse_ff_bvar, variable_name = "FEDFUNDS")
plot_model_performance(rmse_cpi_bvar, variable_name = "CPIAUCSL")











lag_index = matrix(0, M, lags)

for(i in 1:M){
  lag_index[i,] = seq(i, K, by = M)
}

rownames(lag_index) <- var.sets$`VAR-22`

plot(var_count_array[1,,"CPIAUCSL" ,"minnfsv"],
     pch = 20, xlab = "Predictors", ylab = "PIP",
     main = paste0("Minn-FSV"), ylim = c(0,1), xlim = c(1,K))
points(lag_index["CPIAUCSL",],var_count_array[1,,"CPIAUCSL" ,"minnfsv"][lag_index["CPIAUCSL",]], pch = 20, col = "red")
abline(v = c(lag_index[nrow(lag_index),]), col = "grey", lty = 2 )
for(i in 2:cmax){
  points(var_count_array[i,,"CPIAUCSL" ,"minnfsv"])
  points(lag_index["CPIAUCSL",],var_count_array[i,,"CPIAUCSL" ,"minnfsv"][lag_index["CPIAUCSL",]], pch = 20, col = "red")
  abline(v = c(lag_index[nrow(lag_index),]), col = "grey", lty = 2 )
}


plot(var_count_array[1,,"CPIAUCSL" ,"dartfsv"],
     pch = 20, xlab = "Predictors", ylab = "PIP",
     main = paste0("DART-FSV"), ylim = c(0,1), xlim = c(1,K))
points(lag_index["CPIAUCSL",],var_count_array[1,,"CPIAUCSL" ,"dartfsv"][lag_index["CPIAUCSL",]], pch = 20, col = "red")
abline(v = c(lag_index[nrow(lag_index),]), col = "grey", lty = 2 )
for(i in 2:cmax){
  points(var_count_array[i,,"CPIAUCSL" ,"dartfsv"])
  points(lag_index["CPIAUCSL",],var_count_array[i,,"CPIAUCSL" ,"dartfsv"][lag_index["CPIAUCSL",]], pch = 20, col = "red")
  abline(v = c(lag_index[nrow(lag_index),]), col = "grey", lty = 2 )
}

par(mfrow = c(2,2))
for(i in 1:h){
  plot(cumsum(LPL_array[,i,"smallbvar"]), type = "l",
       ylab = "", xlab = "",
       main = bquote(LPDS[t + .(i)]) )
}
