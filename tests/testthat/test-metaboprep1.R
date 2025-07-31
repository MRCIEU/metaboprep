library(testthat)

test_that("metaboprep output same as metaboprep1", {
  # skip if offline 
  skip_if_offline()
  
  # example data
  fp  <- system.file("extdata", "metabolon_v1.1_example.xlsx", package = "metaboprep")

  # get the previous version and attached 
  tmp_lib <- tempfile("mainlib_") # "~/Desktop/mainlib" #  
  dir.create(tmp_lib)
  system2("git", c("clone", "https://github.com/MRCIEU/metaboprep.git", tmp_lib))
  cwd <- getwd()
  setwd(tmp_lib)
  system2("git", c("checkout", "bbe1f85"))
  
  # copy the data across
  new_fp <- file.path(tmp_lib, "inst", "extdata", "metabolon_v1.1_example.xlsx")
  file.copy(fp, new_fp, overwrite = TRUE)
  
  # extract the parameters
  param_file <- "parameter_file.sh"
  lines <- readLines(param_file)
  parse_params <- function(lines) {
    params <- list()
    for (line in lines) {
      line_trim <- trimws(line)
      if (nchar(line_trim) == 0 || startsWith(line_trim, "##") || startsWith(line_trim, "#")) {
        next
      }
      if (grepl("=", line_trim)) {
        parts <- strsplit(line_trim, "=", fixed = TRUE)[[1]]
        key <- trimws(parts[1])
        value <- trimws(parts[2])
        params[[key]] <- value
      }
    }
    params
  }
  
  params <- parse_params(lines)
  
  # populate with data 
  params[["projectname"]] <- "OLD_METABOPREP"
  params[["datadirectory"]] <- dirname(new_fp)
  params[["metabolite_data_file"]] <- basename(new_fp)
  params[["Nightingale_OR_Metabolon"]] <- "Metabolon"
  params[["feature_missingness"]] <- "0.05"
  params[["sample_missingness"]] <- "0.2"
  params[["total_peak_area_SD"]] <- "3"
  params[["outlier_udist"]] <- "3"
  params[["outlier_treatment"]] <- "leave_be"
  params[["tree_cut_height"]] <- "0.5"
  params[["PC_outlier_SD"]] <- "3"
  params[["feat_anno_run_mode_col"]] <- "platform"
  
  # write out fresh file 
  write_params <- function(lines, params, outfile) {
    out_lines <- lines
    for (i in seq_along(lines)) {
      line <- lines[i]
      line_trim <- trimws(line)
      if (nchar(line_trim) == 0 || startsWith(line_trim, "##") || startsWith(line_trim, "#")) {
        next
      }
      if (grepl("=", line_trim)) {
        parts <- strsplit(line_trim, "=", fixed = TRUE)[[1]]
        key <- trimws(parts[1])
        if (key %in% names(params)) {
          new_val <- params[[key]]
          out_lines[i] <- paste0(key, "=", new_val)
        }
      }
    }
    writeLines(out_lines, outfile)
  }
  
  write_params(lines, params, "parameter_file_updated.sh")
  
  
  # run metaboprep1 
  pipeline_script <- "run_metaboprep_pipeline.R"
  script_lines <- readLines(pipeline_script)
  cut_index <- grep("\\(X\\) Make Report", script_lines, fixed = FALSE)
  script_lines <- script_lines[1:cut_index]
  inject_lines <- c(
    "library(devtools)",
    sprintf('load_all("%s")', tmp_lib)
  )
  new_script <- c(inject_lines, "", script_lines)
  writeLines(new_script, pipeline_script)

  system2("Rscript", c("run_metaboprep_pipeline.R", file.path(getwd(), "parameter_file_updated.sh")))
  
  # get the metaboprep1 data
  output_dir    <- "/Users/xx20081/Desktop/mainlib/inst/extdata"
  release_dir   <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  release_dir   <- release_dir[grepl("metaboprep_release_\\d{4}_\\d{2}_\\d{2}$", release_dir)]
  raw_files     <- list.files(file.path(release_dir, "raw_data"), full.names = TRUE)
  raw_data      <- read.csv(grep("OrigScale", raw_files, value=TRUE), sep="\t") 
  raw_samp      <- read.csv(grep("sampledata", raw_files, value=TRUE), sep="\t") 
  raw_feat      <- read.csv(grep("featuredata", raw_files, value=TRUE), sep="\t") 
  qc_files      <- list.files(file.path(release_dir, "filtered_data"), full.names = TRUE)
  qc_data       <- read.csv(grep("metabolite_data", qc_files, value=TRUE), sep="\t") 
  qc_samp       <- read.csv(grep("sample_data", qc_files, value=TRUE), sep="\t") 
  qc_feat       <- read.csv(grep("feature_data", qc_files, value=TRUE), sep="\t") 
  raw_sum_files <- list.files(file.path(release_dir, "sumstats", "raw_dataset"), full.names = TRUE)
  raw_sum_samp  <- read.csv(grep("sample_anno", raw_sum_files, value=TRUE), sep="\t") 
  raw_sum_feat  <- read.csv(grep("feature_anno", raw_sum_files, value=TRUE), sep="\t") 
  qc_sum_files  <- list.files(file.path(release_dir, "sumstats", "filtered_dataset"), full.names = TRUE)
  qc_sum_samp   <- read.csv(grep("sample_anno", qc_sum_files, value=TRUE), sep="\t") 
  qc_sum_feat   <- read.csv(grep("feature_anno", qc_sum_files, value=TRUE), sep="\t") 
  
  # run new metaboprep
  dat <- read_metabolon(fp, sheet="OrigScale")
  m <- Metaboprep(data = dat$data, 
                  features = dat$features, 
                  samples = dat$samples)
  m <- batch_normalise(m,
                       run_mode_col = "platform", 
                       run_mode_colmap = c(pos="pos", neg="neg"),
                       source_layer = "input",
                       dest_layer = "batch_normalised")
  m <- quality_control(m, 
                       source_layer = "batch_normalised",
                       sample_missingness  = as.numeric(params$sample_missingness), 
                       feature_missingness = as.numeric(params$feature_missingness), 
                       total_peak_area_sd  = as.numeric(params$total_peak_area_SD), 
                       outlier_udist       = as.numeric(params$outlier_udist), 
                       outlier_treatment   = params$outlier_treatment, 
                       tree_cut_height     = as.numeric(params$tree_cut_height),
                       feature_selection   = "least_missingness",
                       pc_outlier_sd       = as.numeric(params$PC_outlier_SD), 
                       max_num_pcs = 6)
  
  # compare sample exclusions 
  orig_samp_excl <- setdiff(raw_samp$SAMPLE_NAME, qc_samp$SAMPLE_NAME)
  new_samp_excl  <- unname(unlist(m@exclusions$samples))
  expect_equal(sort(orig_samp_excl), sort(new_samp_excl))
  
  # compare feature exclusions 
  orig_feat_excl <- setdiff(raw_feat$COMP_ID, qc_feat$COMP_ID)
  new_feat_excl  <- unname(unlist(m@exclusions$features))
  expect_equal(sort(as.character(orig_feat_excl)), sort(as.character(new_feat_excl)))
  
  
})
