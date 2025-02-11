load("MacroUncQ.rda")
library(bayesianVARs)
lags <- 13L
M  <- length(var.sets$`VAR-8`)
K  <- M*lags

T0 <- 160
T1 <- nrow(yraw)
h  <- 4
estimation_window <-  T1-T0-h # total to be saved
variables_names <- c(var.sets$`VAR-8`[2:8],"INDPRO")
yraw_f <- yraw[,variables_names]
time_index <- time(yraw)
rownames(yraw_f) <-time_index

y_test_array <-array(NA, dim = c(estimation_window, h, ncol(yraw_f)),
                     dimnames =list(paste0("w_",1:estimation_window),
                                    paste0("t+",1:h), 
                                    variables_names))
for(i in 1:(T1-T0-h)){
  
  y_train <- yraw_f[1:(T0 + i -1),]
  y_test  <- yraw_f[(T0 + i):(T0 + i + h -1),]
  y_test_array[i,,] <- y_test
  
}

file_names <- list.files("Results")
model_names <- sub("_.*", "", file_names)
window_number <- as.numeric(sub(".*_(\\d+)\\.rds", "\\1", file_names))

# Create a data frame
df <- data.frame(file_name = file_names, model_name = model_names, window_number = window_number)
index <- order(df$model_name,df$window_number)
df_sorted <- df[index,]
models <- unique(model_names)

num_models <- length(models)

for(m in models){
  agg_df <- df_sorted[df_sorted$model_name == m,]
  cmax <- max(agg_df$window_number)
  cmin <- min(agg_df$window_number)
  for(w in cmin:cmax){
    if(m == "smallbvar"){
      
      y_test <- y_test_array[w,,]
      selected_files <- file_names[grep(paste0(m, "_window_", w, "\\.rds$"), file_names)]
      saved_model <- readRDS(paste0("Results/", selected_files))
      predictions <- predict(saved_model, ahead = 1:h, Y_obs = y_test, LPL = TRUE)
      
      saved_info <- predictions
      file_name_model <- paste0(m,"_window_",w,".rds")
      saveRDS(saved_info, file = paste0("Aggregation/",file_name_model))
      file.remove(paste0("Results/", file_name_model))
    }else{
    selected_files <- file_names[grep(paste0(m, "_window_", w, "\\.rds$"), file_names)]
    saved_model <- readRDS(paste0("Results/", selected_files))
  
    predictions <- saved_model$predictions
    volatility  <- saved_model$sigma_predictions
    var_count   <- saved_model$var_count_matrix
    LPL_draws   <- saved_model$LPL_draws
    
    saved_info<-list("predictions" = predictions, "volatility" = volatility,
                     "var_count" =var_count,"LPL_draws" = LPL_draws)
    
    file_name_model <- paste0(m,"_window_",w,".rds")
    saveRDS(saved_info, file = paste0("Aggregation/",file_name_model))
    file.remove(paste0("Results/", file_name_model))
    }
  }
}
