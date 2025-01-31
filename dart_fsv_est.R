load(paste0("/MacroUncQ.rda"))

library(stochtree)

yraw_f <- yraw[,var.sets$`VAR-22`]
time_index <- time(yraw)
rownames(yraw_f) <-time_index
# Training Data from 1965Q1 - 2004Q4 1-160

T0 <- 160
T1 <- nrow(yraw_f)
h  <- 4

num_burnin <-20000
num_mcmc   <-10000

for(i in 1:(T1-T0-h)){
  
  y_train <- yraw_f[1:(T0 + i -1),]
  y_test  <- yraw_f[(T0 + i):(T0 + i + h -1),]
  
  print(paste0("Expanding Window... ", i))
  
  model <- stochtree::fsv_mbart(data = y_train, Y_test = y_test, n_ahead = h, lags = 13, bart_prior = "dart", SV=TRUE, num_burnin = num_burnin, num_mcmc = num_mcmc)
  
  saveRDS(model, file = paste0("/Results/dartfsv_window_",i,".rds"))
  rm(model)
  gc()
}

