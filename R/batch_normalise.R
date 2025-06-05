#' @title Batch Normalisation
#' @description
#' Run batch normalisation based on the platform flag in the features data 
#' @param metaboprep an object of class Metaboprep
#' @param run_mode_col character, column name in features data containing the run mode
#' @param run_mode_colmap named character vector or list, c(mode = "mode col name in samples") 
#' @param source_layer character, which data layer to get the data from
#' @param dest_layer character, which data layer to put the the data in to
#' @include class_metaboprep.R
#' @importFrom stats median
#' @export
batch_normalise <- new_generic(name = "batch_normalise", dispatch_args = c("metaboprep"), function(metaboprep, run_mode_col, run_mode_colmap, source_layer = "input", dest_layer = "batch_normalised") { S7_dispatch() })
#' @name batch_normalise
method(batch_normalise, Metaboprep) <- function(metaboprep, run_mode_col, run_mode_colmap, source_layer = "input", dest_layer = "batch_normalised") {

  # checks
  run_mode_col <- match.arg(run_mode_col, choices = names(metaboprep@features))
  stopifnot("`run_mode_colmap` names must match unique `run_mode_col` entries" = length(setdiff(metaboprep@features[[run_mode_col]], names(run_mode_colmap)))==0)
  stopifnot("`run_mode_colmap` values must be columns in sample data" = all(unname(run_mode_colmap) %in% names(metaboprep@samples)))
  
  # add a copy of the raw data to the back of the matrix stack
  metaboprep@data <- array(
    c(metaboprep@data, metaboprep@data[, , "input"]),
    dim = c(dim(metaboprep@data)[1], dim(metaboprep@data)[2], dim(metaboprep@data)[3] + 1),
    dimnames = list(
      dimnames(metaboprep@data)[[1]],
      dimnames(metaboprep@data)[[2]],
      c(dimnames(metaboprep@data)[[3]], dest_layer)
    )
  )

  # get the unique flags by which to batch normalise
  for (i in seq_along(run_mode_colmap)) {
    
    mode     <- names(run_mode_colmap)[i]
    mode_col <- run_mode_colmap[[i]]

    f_idx <- which(metaboprep@features[[run_mode_col]] == mode)

    batch_ids <- unique(metaboprep@samples[[mode_col]])

    for (bid in batch_ids) {

      s_idx <- which(metaboprep@samples[[mode_col]] == bid)

      m <- apply(metaboprep@data[s_idx, f_idx, source_layer], 2, function(x) stats::median(x, na.rm = TRUE))

      for (j in 1:length(m)) {
        metaboprep@data[s_idx, f_idx[j], dest_layer] <- metaboprep@data[s_idx, f_idx[j], dest_layer] / m[j]
      }
    }
  }

  return(metaboprep)
}
