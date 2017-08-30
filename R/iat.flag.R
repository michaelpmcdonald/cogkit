#' Flag subjects for excessive speed
#'
#' @param df dataframe containing columns subject, pairing, and latency
#' @param threshold value (in seconds) to assess as a speed threshold (i.e. trials under 300ms)
#' @param criterion value (as proportion) above which to flag a subject for discard, i.e. .10 of trials under 300ms
#'
#' @return a vector of subjects, proportion under threshold, and flag (logical)
#' @export
#' @importFrom dplyr %>%
iat_flagSpeed <- function(df, threshold = .3, criterion = .1){
  df %>%
    dplyr::select(subject, pairing, latency) %>%
    dplyr::filter(pairing %in% c("3","4","6","7")) %>% # only examine in paired blocks
    dplyr::group_by(subject) %>%
    dplyr::summarize(meanUnderThreshold = mean(latency <= threshold), # returns proportion under 300ms
                     speed_flag = mean(latency <= threshold) > criterion)
}

#' Flag Error Rate (IAT)
#'
#' @param df dataframe containing subject number, pairing, and correct (1/0 or T/F)
#' @param criterion maximum acceptable error rate
#'
#' @return dataframe of subject numbers and flag values (true/false)
#' @export
#' @importFrom dplyr %>%
iat_flagError <- function(df, criterion){
  flag_data <- df %>%
    dplyr::select(subject, pairing, correct) %>%
    dplyr::group_by(subject, pairing) %>%
    dplyr::summarize(error_rate = mean(1 - correct)) %>%
    dplyr::group_by(subject) %>%
    dplyr::summarize(error_rate = mean(error_rate),
                     error_flag = max(error_rate) >= criterion)
  return(flag_data)
}

#' Generate flags for the IAT
#'
#' @param experiment experiment object
#'
#' @return experiment + exclusionCriteria
#' @export
flag.iat <- function(...){

}
