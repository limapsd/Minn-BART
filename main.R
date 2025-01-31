
model_names <- c("bart", "dart", "minn", "bart_fsv", "dart_fsv", "minn_fsv")
n_models <-length(model_names)

if("Results" %in% list.files() == FALSE) dir.create("Results")
if("Agreggation" %in% list.files() == FALSE) dir.create("Agreggation")
if("Logs" %in% list.files() == FALSE) dir.create("Logs")

for(i in 1:n_models){
    scripts_name <- paste0(model_names[i],"_est.R")   
    batch.script <- paste0("nice Rscript --verbose ", scripts_name," 2>&1 Logs/", model_names[i],".txt &")

    system(batch.script)
    Sys.sleep(5)
}
