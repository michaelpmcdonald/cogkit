#' Parse ePrime .txt logfile
#'
#' @param filename of ePrime logfile.txt
#'
#' @return a dataframe from the experiment, with subject number, session number, and date
#' @export
parseEdat <- function(filename){
  # uses edatparser to parse the eprime file, then recovers subject number,
  # session number, and date, and adds them to the returned dataframe
  file <- edatparser::edat(filename)
  subjectNumber <- edatparser::get_subject_number(file)
  sessionNumber <- edatparser::get_session(file)
  sessionDate <- edatparser::get_date(file)
  fixedData <- as.data.frame(file)
  fixedData$subject <- subjectNumber
  fixedData$session <- sessionNumber
  fixedData$date <- sessionDate
  return(fixedData)
}

#' Read and merge files of a particular type, useful when individual subject
#' files are saved separately by the experimental software
#'
#' @param pattern a regular expression pattern to describe files to import.  Should include
#' information about the filename as well as its extension
#' @param ... additional parameters such as column specifications for read_tsv
#'
#' @return a single merged files
#' @export
#'
#'
#' @examples pattern="ageiat_raw+.*iqdat"
read_merge <- function(pattern, ...){
  filenames <- list.files(pattern)
  files <- lapply(filenames, readr::read_tsv, ...)
  file_merged <- files %>% dplyr::bind_rows()
  return(file_merged)
}
