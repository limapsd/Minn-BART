load("MacroUncQ.rda")

library(bayesianVARs)
variables_names <- c(var.sets$`VAR-22`)
yraw_f <- yraw[,variables_names]
time_index <- time(yraw)
rownames(yraw_f) <-time_index
# Training Data from 1965Q1 - 2004Q4 1-160
T0 <- 160
T1 <- nrow(yraw_f)
h  <- 4
p  <- 13L

num_burnin <-20000
num_mcmc   <-10000

for(i in 1:(T1-T0-h)){
  
  y_train <- yraw_f[1:(T0 + i -1),]
  y_test  <- yraw_f[(T0 + i):(T0 + i + h -1),]
  
  print(paste0("Expanding Window... ", i))
  
  prior_phi <- specify_prior_phi(data = y_train,
                                 lags = p,
                                 prior = "HMP")
  
  prior_sigma <- specify_prior_sigma(data = y_train,
                                     type = "cholesky",
                                     cholesky_heteroscedastic = T,
                                     cholesky_U_prior = "HS")
  
  var_model <- bvar(y_train, lags = p, draws = num_mcmc, burnin = num_burnin,
                    prior_phi = prior_phi, prior_sigma = prior_sigma,
                    sv_keep = "all")  
  
  saveRDS(var_model, file = paste0("Results/bvar_window_",i,".rds"))
  rm(var_model)
  gc()
}

