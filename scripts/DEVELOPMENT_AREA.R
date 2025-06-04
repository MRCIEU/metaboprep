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
data     <- read.csv(system.file("extdata", "dummy_data.csv", package = "metaboprep"), header=T, row.names = 1)
samples  <- read.csv(system.file("extdata", "dummy_samples.csv", package = "metaboprep"), header=T, row.names = 1)
features <- read.csv(system.file("extdata", "dummy_features.csv", package = "metaboprep"), header=T, row.names = 1)


# creating a metaboprep object
m <- Metaboprep(data = data, samples = samples, features = features)
m


# print some elements of the object 
print(m@data[1:5, 1:5, 1])
print(m@samples[1:5,])
print(m@features[1:5,])


# try running batch_normalise block
m <- m |>
  batch_normalise() |> 
  sample_summary(source_layer = "input", outlier_udist=1)
m
m@data[, , "batch_normalised"]
m@sample_summary[, , "input"]


