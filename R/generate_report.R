#' A function to generate a metaboprep summary html report
#'
#' This function generates the html report.
#' @param full_path_2_Rdatafile full path to the Rdatafile 
#' @param dir_4_report directory to place the report
#' @param path_2_Rmd_template full path to the html report template
#' @keywords knit html report
#' @return Writes a html report to file
#' @importFrom knitr knit2html
#' @export
#' @examples
#' generate_report()
generate_report = function(full_path_2_Rdatafile = "ReportData.Rdata", dir_4_report = "./", path_2_Rmd_template = file.path( system.file("rmarkdown", package="metaboprep"), "metaboprep_Report_v0.Rmd" ) ){
   
   ## load R data file to environment
   message("Loading the R data file")
   load(full_path_2_Rdatafile)

   ## knit report
   message("knit the report to html")
   knitr::knit2html( 
      input = path_2_Rmd_template,
      output = paste0(dir_4_report, "metaboprep_summary_report.html") 
      )
}


