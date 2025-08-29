#' Metaboprep 1 pipeline
#' 
#' @description This function runs the original metaboprep1 pipeline using the old parameter file input format. 
#' The function requires access to the internet as the old package (default github commit `bbe1f85`)
#' will be dynamically downloaded and used to process the data. 
#' 
#' @param parameter_file character, full file path to the metaboprep 1 parameter file
#' @param gitcommit character, Github commit - default pinned to the last stable metaboprep 1 version `bbe1f85`
#' @param attempt_report logical, whether to attempt metaboprep1 report generation. Default=FALSE as this can lead to errors on some operating systems.
#'
#' @returns NULL
#' @export
#'
run_metaboprep1 <- function(parameter_file, gitcommit = "bbe1f85", attempt_report = FALSE) {
  
  # testing 
  if (FALSE) {
    parameter_file  <- "/Users/xx20081/Downloads/parameter_file.sh"
    gitcommit = "bbe1f85"
  }
  
  # get the previous version and attached ====
  tmp_lib <- tempfile("mainlib_") # "~/Desktop/mainlib" #  
  dir.create(tmp_lib)
  cwd <- getwd()
  
  
  # ensure cleanup always runs ====
  on.exit({
    setwd(cwd)
    unlink(tmp_lib, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  
  
  # clone the old repo ====
  system2("git", c("clone", "https://github.com/MRCIEU/metaboprep.git", tmp_lib))
  setwd(tmp_lib)
  system2("git", c("checkout", gitcommit))
  
  
  # adjust the pipeline script, removing the report generation as default ====
  pipeline_script <- "run_metaboprep_pipeline.R"
  script_lines <- readLines(pipeline_script)
  if (!attempt_report) {
    cut_index <- grep("\\(X\\) Make Report", script_lines, fixed = FALSE)
    script_lines <- script_lines[1:cut_index]
  }
  inject_lines <- c(
    "library(devtools)",
    sprintf('load_all("%s")', tmp_lib)
  )
  new_script <- c(inject_lines, "", script_lines)
  writeLines(new_script, pipeline_script)
  
  
  # run metaboprep1 pipeline ====
  system2(file.path(R.home("bin"), "Rscript"),
          c("run_metaboprep_pipeline.R", parameter_file))
  
  
  return(NULL)
}