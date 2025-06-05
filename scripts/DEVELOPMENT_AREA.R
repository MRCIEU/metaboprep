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
# see the /data-raw/dummy_data.R script for creation of these datasets
data     <- read.csv(system.file("extdata", "dummy_data.csv", package = "metaboprep"), header=T, row.names = 1) |> as.matrix()
samples  <- read.csv(system.file("extdata", "dummy_samples.csv", package = "metaboprep"), header=T, row.names = 1)
features <- read.csv(system.file("extdata", "dummy_features.csv", package = "metaboprep"), header=T, row.names = 1)

set.seed(123)
set_na_randomly <- function(mat, prop = 0.1) {
  stopifnot(is.matrix(mat))
  
  n <- length(mat)
  idx <- sample(seq_len(n), size = floor(prop * n), replace = FALSE)
  mat[idx] <- NA
  mat
}
data <- set_na_randomly(data, prop = 0.3)
data[1,] <- NA

# creating a metaboprep object
m <- Metaboprep(data = data, samples = samples, features = features)
m


# run QC pipeline
mqc <- m |>
  quality_control(source_layer = "input", 
                  sample_missingness = 0.5, 
                  feature_missingness = 0.3, 
                  total_peak_area_sd = 5, 
                  outlier_udist=5, 
                  outlier_treatment="leave_be", 
                  winsorize_quantile = 1.0, 
                  tree_cut_height=0.5, 
                  pc_outlier_sd =5, 
                  derived_col="derived_feature", 
                  xenobiotic_col="xenobiotic_feature", 
                  sample_ids=NULL, 
                  feature_ids=NULL)
mqc

View(mqc@data[,,"qc"])
View(mqc@samples)
View(mqc@features)





### example metabolon data 
dat <- read_metabolon_v1(system.file("extdata", "metabolon_v1_example.xlsx", package = "metaboprep"))

# creating a metaboprep object
m <- Metaboprep(data = dat$data[,,1], samples = dat$samples, features = dat$features)

# run QC pipeline
mqc <- m |>
  quality_control(source_layer = "input", 
                  sample_missingness = 0.5, 
                  feature_missingness = 0.3, 
                  total_peak_area_sd = 5, 
                  outlier_udist=5, 
                  outlier_treatment="leave_be", 
                  winsorize_quantile = 1.0, 
                  tree_cut_height=0.5, 
                  pc_outlier_sd =5, 
                  derived_col=NULL, 
                  xenobiotic_col=NULL, 
                  sample_ids=NULL, 
                  feature_ids=NULL)
mqc
metaboprep = mqc

View(mqc@data[,,"qc"])
View(mqc@samples)
View(mqc@features)









# print some elements of the object 
print(m@data[1:5, 1:5, 1])
print(m@samples[1:5,])
print(m@features[1:5,])


# try running batch_normalise block
mb <- m |>
  batch_normalise(run_mode_col = "platform", run_mode_colmap = c(pos="pos", neg="neg"))
mb


# sample summary
ss <- m |> sample_summary(source_layer = "input", outlier_udist=1)
ss

# feature summary
fs <- m |> feature_summary(source_layer = "input", outlier_udist=1, tree_cut_height = 0.5) 
fs
attributes(fs)

# pcs outliers
pcs <- m |> pc_and_outliers(source_layer = "input", feature_ids = fs$feature_id[fs$independent_features & !is.na(fs$independent_features)])
pcs
attributes(pcs)
  

# summarise together
summ <- m |>
  summarise(source_layer = "input", outlier_udist=1, tree_cut_height = 0.5)
summ



