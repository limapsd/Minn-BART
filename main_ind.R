main <- function(){
    args <- commandArgs(TRUE)
    
    # Ensure directories exist
    if(!"Results" %in% list.files()) dir.create("Results")
    if(!"Aggregation" %in% list.files()) dir.create("Aggregation")
    if(!"Logs" %in% list.files()) dir.create("Logs")

    model_names <- c("bart", "dart", "minn", "bart_fsv", "dart_fsv", "minn_fsv")
    
    # Handle missing argument case
    if(length(args) < 1) {
        stop("Error: No model name provided. Please specify a valid model. Choose from:", paste(model_names, collapse=", ")
    }

    model <- as.character(args[1])

    if(model %in% model_names){
        scripts_name <- paste0(model, "_est.R")   
        batch.script <- paste0("nice Rscript --verbose ", scripts_name, " 2>&1 Logs/", model, ".txt &")
        system(batch.script)
        Sys.sleep(5)
    } else {
        stop(paste("Error: Invalid model name '", model, "'. Choose from:", paste(model_names, collapse=", ")))
    }
}

main()
