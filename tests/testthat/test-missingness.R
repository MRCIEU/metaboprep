library(testthat)

test_that("missingness assessment works", {

  # init ====
  set.seed(42)
  n_samples         <- 100
  n_metabolites     <- 90
  samp_miss_thresh  <- 0.2
  feat_miss_thresh  <- 0.3
  miss_sample_idxs  <- 2:6 
  miss_feature_idxs <- 2:6
  
  
  # simulate data ====
  data <- matrix(rnorm(n_samples * n_metabolites), nrow = n_samples)
  rownames(data) <- paste0("Sample_", 1:n_samples)
  colnames(data) <- paste0("Met_", 1:n_metabolites)
  
  
  # simulate missing data ====
  # complete missingness
  data[1, ] <- NA_real_
  data[, 1] <- NA_real_
  
  # a bit of missingness
  for (i in miss_sample_idxs) {
    missing_cols <- sample(1:n_metabolites, size = ceiling((samp_miss_thresh+0.05) * n_metabolites)) # thresh + 5% missingness
    data[i, missing_cols] <- NA
  }
  for (j in miss_feature_idxs) {
    missing_rows <- sample(1:n_samples, size = ceiling((feat_miss_thresh+0.05) * n_samples)) # thresh + 5% missingness
    data[missing_rows, j] <- NA
  }
    
  
  # create sample df ====
  samples <- data.frame(
    sample_id = rownames(data)
  )
  
  
  # create feature df ====
  features <- data.frame(
    feature_id = colnames(data)
  )
  
  
  # load metaboprep object ====
  m <- Metaboprep(data, samples, features)
  
  
  # run the QC ====
  m <- quality_control(m, sample_missingness = samp_miss_thresh, feature_missingness = feat_miss_thresh)
  
  
  # check m@exclusions ====
  expect_identical(m@exclusions$samples$extreme_sample_missingness, 
                   rownames(data)[1])
  expect_identical(m@exclusions$features$extreme_feature_missingness,
                   colnames(data)[1])
  expect_identical(m@exclusions$samples$user_defined_sample_missingness, 
                   rownames(data)[miss_sample_idxs])
  expect_identical(m@exclusions$features$user_defined_feature_missingness,
                   colnames(data)[miss_feature_idxs])
  
  
  # check m@feature_summary & m@sample_summary ====
  expect_identical(m@feature_summary["missingness",,"input"], 
                   apply(data, 2, function(x) sum(is.na(x)) / length(x)))
  expect_identical(m@sample_summary[, "missingness","input"], 
                   apply(data, 1, function(x) sum(is.na(x)) / length(x)))
  expect_identical(m@feature_summary["missingness",,"qc"], 
                   stats::setNames(c(rep(NA_real_, 1+length(miss_feature_idxs)), rep(0, ncol(data)-(1+length(miss_feature_idxs)))), colnames(data)))
  expect_identical(m@sample_summary[, "missingness","qc"], 
                   stats::setNames(c(rep(NA_real_, 1+length(miss_sample_idxs)), rep(0, nrow(data)-(1+length(miss_sample_idxs)))), rownames(data)))
  
  
  # visualise
  if (FALSE) {
    
    library(ggplot2)
    library(reshape2)
    
    data_df <- as.data.frame(data)
    data_df$Sample <- rownames(data_df)
    data_df <- melt(data_df, id.vars = "Sample", variable.name = "Metabolite", value.name = "Value")
    data_df$Missing <- is.na(data_df$Value)
    data_df$Sample <- factor(data_df$Sample, levels = rownames(data))
    
    
    # Plot
    ggplot(data_df, aes(x = Metabolite, y = Sample, fill = Missing)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"), name = "Missing") +
      labs(title = "Missingness Heatmap", x = "Metabolite", y = "Sample") +
      theme_minimal() +
      theme(
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  }
  
})
