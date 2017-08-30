#' Disaffect platform-specific output
#'
#' @param experiment
#'
#' @return experiment object + standardized data frame stzd
#' @export
disaffect.iat <- function(experiment){
  ## A standard IAT data structure contains the columns:
  ## date, time, group, subject, blockcode, blocknum, trialcode, trialnum,
  ## response, correct, latency, stimulusnumber1, stimulusitem1

  if (is.na(experiment$platform)){
    warning("Experiment cannot be disaffected without specifying a platform.")
  }

  if (experiment$platform == "inquisit"){

    experiment$stzd <- experiment$raw %>% select("date" = date,
                                                 "time" = time,
                                                 "group" = group,
                                                 "subject" = subject,
                                                 "trialcode" = trialcode,
                                                 "trialnum" = trialnum,
                                                 "response" = response,
                                                 "correct" = correct,
                                                 "latency" = latency,
                                                 "stimulusnumber" = stimulusnumber1,
                                                 "stimulusitem" = stimulusitem1)
  }

  return(experiment)
}
