#' Process Experiment
#'
#' @param df Dataframe containing experimental data produced by an experiment presentation software
#' @param task Experimental task type, e.g. "simon" "iat"
#' @param ...
#'
#' @return NA - referrer function
#' @export
process <- function(raw, task, platform = "default", excludeSubjects = NA, ...){
  experiment <- setup(raw, excludeSubjects, platform, task) %>%
    disaffect(.) %>%
    score(.) %>%
    flag(.)
  return(experiment)
}

#' Setup the experiment object returned by scoring routines
#'
#' @param df raw experimental data frame
#' @param excludedSubjects vector of excluded subjects
#' @param platform experimental platform
#'
#' @return expt experimental object for further processing
setup <- function(raw, excludeSubjects, platform, task){
  experiment <- list()
  experiment$raw <- raw
  experiment$platform <- platform
  experiment$excludeSubjects <- excludeSubjects
  experiment$task <- task
  class(experiment) <- c(task, "experiment")
  return(experiment)
}

#' Generic disaffect function helper
#'
#' @param experiment
#'
#' @return NA = referrer function
#' @export
disaffect <- function(experiment, ...) {UseMethod("disaffect")}

disaffect.default <- function(experiment, ...){
  print("Can't disaffect a generic.")
}

#' Generic score function helper
#'
#' @param experiment
#'
#' @return NA = referrer function
#' @export
score <- function(experiment, excludeSubjects, ...) {UseMethod("score")}

score.default <- function(experiment, ...){
  print("Can't score a generic.")
}


#' Generic flag function helper
#'
#' @param experiment
#'
#' @return NA = referrer function
#' @export
flag <- function(df, ...) {UseMethod("expt_flag")}

flag.default <- function(df, ...){
  print("Can't flag a generic.")
}
