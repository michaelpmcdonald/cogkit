#' Score Experiment
#'
#' @param df Dataframe containing experimental data produced by an experiment presentation software
#' @param type Type of experiment, e.g. "simon" "IAT"
#' @param ...
#'
#' @return NA - referrer function
#' @export
scoreExpt <- function(df, type, excludedSubjects = NA, platform, ...){
  class(df) <- c(class(df), type)
  expt_score(df, excludedSubjects, platform, ...)
}

#' Generic scoring function helper
#'
#' @param df
#'
#' @return NA = referrer function
#' @export
expt_score <- function(df, excludedSubjects, platform, ...) {UseMethod("expt_score")}

expt_score.default <- function(df, ...){
  print("Can't score a generic.")
}

#' Setup the experiment object returned by scoring routines
#'
#' @param df raw experimental data frame
#' @param excludedSubjects vector of excluded subjects
#' @param platform experimental platform
#'
#' @return expt experimental object for further processing
expt_setup <- function(df, excludedSubjects, platform){
  expt <- list()
  expt$raw <- df
  expt$platform <- platform
  expt$excludedSubjects <- excludedSubjects
  return(expt)
}
