#' Score Experiment
#'
#' @param df Dataframe containing experimental data produced by an experiment presentation software
#' @param type Type of experiment, e.g. "simon" "IAT"
#' @param ...
#'
#' @return NA - referrer function
#' @export
scoreExpt <- function(df, type, ...){
  class(df) <- c(class(df), type)
  expt_score(df, ...)
}

#' Generic scoring function helper
#'
#' @param df
#'
#' @return NA = referrer function
#' @export
expt_score <- function(df, ...) {UseMethod("expt_score")}

expt_score.default <- function(df, ...){
  print("Can't score a generic.")
}
