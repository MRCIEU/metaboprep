# the purpose of this script is to play and test with the new metaboprep 2 package
#
# Nick Sunderland nicholas.sunderland@bristol.ac.uk
# David Hughes David.Hughes@pbrc.edu
# Laura Corbin laura.corbin@bristol.ac.uk
# Alec McKinlay am17168@bristol.ac.uk

# load metaboprep (must be run from inside the local package)
# cd path/to/local/metaboprep
library(devtools)
load_all()


# the new package will take in 3 data items
# 1. a matrix of data (rownames = sample ids, colnames = feature ids)
# 2. a data.frame of sample information
# 3. a data.frame of feature information
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


# creating a metaboprep object
m <- Metaboprep(data = data, samples = samples, features = features)
m


# print some elements of the object 
print(m@data[1:5, 1:5, 1])
print(m@samples[1:5,])
print(m@features[1:5,])


# try running a processing block
m <- m |>
  batch_normalise()
m
m@data[, , "batch_normalised"]
