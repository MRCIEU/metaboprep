#' A copy of the rntransform function from the GenAbel package, edited for the option of spliting tied values.
#'
#' This function allows you to rank normal transform a vector of data.
#' @param formula 
#' @param data your vector of numerical values to rank normal transform 
#' @param family Defaulted to gaussian. Do not suggest editing 
#' @param split_ties Defualted to FALSE. If TRUE is provided all tied values will be randomly ranked in the function rank() 
#' @keywords nigtingale
#' @export
#' @examples
#' rntransform()
rntransform = function (formula, data, family = gaussian, split_ties = FALSE) 
{
  if (is(try(formula, silent = TRUE), "try-error")) {
    if (is(data, "gwaa.data")) 
      data1 <- phdata(data)
    else if (is(data, "data.frame")) 
      data1 <- data
    else stop("'data' must have 'gwaa.data' or 'data.frame' class")
    formula <- data1[[as(match.call()[["formula"]], "character")]]
  }
  var <- ztransform(formula, data, family)
  if(split_ties == TRUE){
    out <- rank(var, ties.method = "random") - 0.5
  } else {
    out <- rank(var) - 0.5
  }
  out[is.na(var)] <- NA
  mP <- 0.5/max(out, na.rm = T)
  out <- out/(max(out, na.rm = T) + 0.5)
  out <- qnorm(out)
  out
}

