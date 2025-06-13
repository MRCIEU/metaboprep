.onLoad <- function(...) {
  S7::methods_register()
}

utils::globalVariables(c(
  "SampleID", "Sample_Type", "OlinkID", "NPX", "."
))