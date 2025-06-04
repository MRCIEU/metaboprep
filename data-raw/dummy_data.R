## code to prepare dummy datasets goes here
library(devtools)
load_all()

set.seed(123)
n_samples  <- 100
n_features <- 20
samples <- data.frame(
  sample_id = paste0("id_", 1:n_samples),
  age       = sample(18:70, size = n_samples, replace = TRUE),
  sex       = sample(c("male", "female"), size = n_samples, replace = TRUE), 
  pos       = sample(c("batch1", "batch2"), size = n_samples, replace = TRUE),
  neg       = sample(c("batch1", "batch2"), size = n_samples, replace = TRUE)
)
features <- data.frame(
  feature_id         = paste0("metab_id_", 1:n_features),
  platform           = sample(c("pos","neg"), size = n_features, replace = TRUE, prob = c(0.20,0.80)),
  pathway            = NA_character_,
  derived_feature    = sample(c(T,F), size = n_features, replace = TRUE, prob = c(0.05,0.95)),
  xenobiotic_feature = sample(c(T,F), size = n_features, replace = TRUE, prob = c(0.10,0.90))
)
data <- matrix(runif(n_samples * n_features), nrow = n_samples, ncol = n_features, dimnames = list(rows = rev(samples[["sample_id"]]), cols = features[["feature_id"]]))

write.csv(data, file.path(system.file("extdata", package = "metaboprep"), "dummy_data.csv"))
write.csv(samples, file.path(system.file("extdata", package = "metaboprep"), "dummy_samples.csv"))
write.csv(features, file.path(system.file("extdata", package = "metaboprep"), "dummy_features.csv"))