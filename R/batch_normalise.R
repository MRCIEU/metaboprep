#' @title Batch Normalisation
#' @description
#' Run batch normalisation based on the platform flag in the features data 
#' @param metaboprep an object of class Metaboprep
#' @param source_layer character, which data layer to get the data from
#' @param dest_layer character, which data layer to put the the data in to
#' @include class_metaboprep.R
#' @importFrom stats median
#' @export
batch_normalise <- new_generic(name = "batch_normalise", dispatch_args = c("metaboprep"), function(metaboprep, source_layer = "input", dest_layer = "batch_normalised") { S7_dispatch() })
#' @name batch_normalise
method(batch_normalise, Metaboprep) <- function(metaboprep, source_layer = "input", dest_layer = "batch_normalised") {

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
  platform_ids <- sort(unique(metaboprep@features[["platform"]]))
  
  # ensure platform_ids columns exist in the samples data
  if (!all(platform_ids %in% names(metaboprep@samples))) {
    msg <- paste0("Unable to batch normalise - platform columns not found in samples data: ", paste0(setdiff(platform_ids, names(metaboprep@samples)), collapse=", "))
    stop(msg)
  }

  for (plat in platform_ids) {

    f_idx <- which(metaboprep@features[["platform"]] == plat)

    batch_ids <- unique(metaboprep@samples[[plat]])

    for (bid in batch_ids) {

      s_idx <- which(metaboprep@samples[[plat]] == bid)

      m <- apply(metaboprep@data[s_idx, f_idx, source_layer], 2, function(x) stats::median(x, na.rm = TRUE))

      for (j in 1:length(m)) {
        metaboprep@data[s_idx, f_idx[j], dest_layer] <- metaboprep@data[s_idx, f_idx[j], dest_layer] / m[j]
      }
    }
  }

  return(metaboprep)
}
