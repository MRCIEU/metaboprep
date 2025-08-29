library(testthat)

test_that("sample pca outlier works", {
  
  # init ====
  set.seed(42)
  n_samples     <- 100
  n_outliers    <- 5
  n_metabolites <- 100
  mean_outlier  <- 4
  sd_outlier    <- 2
  n_pcs_check   <- 10
  threshold_sd  <- 3
  
  
  # simulate normal data ====
  normal_data <- matrix(rnorm(n_samples * n_metabolites), nrow = n_samples)
  rownames(normal_data) <- paste0("Sample_", 1:n_samples)
  colnames(normal_data) <- paste0("Met_", 1:n_metabolites)
  
  
  # simulate abnormal data ====
  inject_indices <- sample(1:n_samples, n_outliers)
  outliers <- matrix(rnorm(n_outliers * n_metabolites, mean = mean_outlier, sd = sd_outlier), nrow = n_outliers)
  data_with_outliers <- normal_data
  data_with_outliers[inject_indices, ] <- outliers
  
  
  # manually find outliers ====
  pca <- prcomp(data_with_outliers, center = TRUE, scale. = TRUE)
  ev  <- eigen(cor(data_with_outliers))
  ap  <- nFactors::parallel(subject=nrow(data_with_outliers), var=ncol(data_with_outliers), rep=100, cent=.05)
  ns  <- nFactors::nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  af  <- as.numeric( ns[[1]][["naf"]] )
  if(af < 2) { af = 2 }
  
  outlier_indices <- integer(0)
  for (i in 1:af) {
    pc_vec <- pca$x[, i]
    mu <- mean(pc_vec)
    sigma <- sd(pc_vec)
    idx <- which(abs(pc_vec - mu) > threshold_sd * sigma)
    outlier_indices <- union(outlier_indices, idx)
  }
  outlier_samples <- rownames(pca$x)[outlier_indices]
  
  
  # create sample df ====
  samples <- data.frame(
    sample_id = rownames(data_with_outliers),
    outlier_label = ifelse(seq_len(n_samples) %in% inject_indices, "Outlier", "Normal")
  )

  
  # create feature df ====
  features <- data.frame(
    "feature_id" = colnames(data_with_outliers)
  )
  
  
  # load metaboprep object ====
  m <- Metaboprep(data_with_outliers, samples, features)
  
  
  # run the QC ====
  m <- quality_control(m, pc_outlier_sd = threshold_sd, max_num_pcs = 2)
  
  
  # check ====
  expect_identical(m@exclusions$samples$user_defined_sample_pca_outlier, 
                   outlier_samples)
  
  
  # visualise
  if (FALSE) {
    pc_scores <- as.data.frame(pca$x[, 1:2])
    pc_scores$Sample <- rownames(data_with_outliers)
    pc_scores$Type <- ifelse(rownames(data_with_outliers) %in% rownames(data_with_outliers)[inject_indices], "Outlier", "Normal")
    
    library(ggplot2)
    
    ggplot(pc_scores, aes(x = PC1, y = PC2, color = Type, label = Sample)) +
      geom_point(size = 3) +
      geom_text(data = subset(pc_scores, Type == "Outlier"), vjust = -1, size = 3) +
      scale_color_manual(values = c("Normal" = "black", "Outlier" = "red")) +
      theme_minimal() +
      labs(title = "PCA of Simulated Data with Extreme Outliers",
           x = "PC1",
           y = "PC2",
           color = "Sample Type")
  }
  
})
