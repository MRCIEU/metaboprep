library(devtools)
load_all()

# metabolon BBS
raw_data_fp <- "/Volumes/MRC-IEU-research/data/bybandsleeve_wt/primary_dataset/metabolomic/mass_spec/metabolon/released/2025-06-04/data/woc_removed/bbs_only_metabolite_data.txt"
raw_samp_fp <- "/Volumes/MRC-IEU-research/data/bybandsleeve_wt/primary_dataset/metabolomic/mass_spec/metabolon/released/2025-06-04/data/woc_removed/bbs_only_sample_data.txt"
raw_feat_fp <- "/Volumes/MRC-IEU-research/data/bybandsleeve_wt/primary_dataset/metabolomic/mass_spec/metabolon/released/2025-06-04/data/woc_removed/bbs_plus_alspac_no_outliers_2025_04_04_Filtered_feature_data.txt"

# read 
bbs_data     <- read.csv(raw_data_fp, sep="\t") |> as.matrix()
bbs_samples  <- read.csv(raw_samp_fp, sep="\t")
bbs_features <- read.csv(raw_feat_fp, sep="\t")

# ensure correct sample and feature id cols 
names(bbs_samples) [which(names(bbs_samples)=="PARENT_SAMPLE_NAME")]  <- "sample_id"
names(bbs_features)[which(names(bbs_features)=="feature_names")]      <- "feature_id"

# ensure xenobiotic and derived columna
bbs_features$xenobiotic <- grepl("Xenobiotics", bbs_features$SUPER_PATHWAY)

# create metaboprep
m <- Metaboprep(data = bbs_data, samples = bbs_samples, features = bbs_features)

# set parameters 
metaboprep          <- m 
source_layer        <- "input" 
sample_missingness  <- 0.99 
feature_missingness <- 0.99 
total_peak_area_sd  <- NA 
outlier_udist       <- 50 
outlier_treatment   <- "leave_be" 
winsorize_quantile  <- 1.0 
tree_cut_height     <- 0.5 
pc_outlier_sd       <- NA
features_exclude_but_keep    <- "xenobiotic" 
sample_ids          <- NULL 
feature_ids         <- NULL


# run QC
m <- quality_control(metaboprep          = metaboprep, 
                     source_layer        = source_layer, 
                     sample_missingness  = sample_missingness, 
                     feature_missingness = feature_missingness, 
                     total_peak_area_sd  = total_peak_area_sd, 
                     outlier_udist       = outlier_udist, 
                     outlier_treatment   = outlier_treatment, 
                     #winsorize_quantile  = winsorize_quantile, 
                     tree_cut_height     = tree_cut_height, 
                     pc_outlier_sd       = pc_outlier_sd, 
                     features_exclude_but_keep    = features_exclude_but_keep, 
                     sample_ids          = sample_ids, 
                     feature_ids         = feature_ids)


