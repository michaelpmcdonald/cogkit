#' Score a Probabalistic Stimulus Selection task
#'
#' @param df a dataframe
#' @param platform experimental platform
#'
#' @return results
#' @export
expt_score.pss <- function(df, platform = "eprime"){
  # Need to return:
  # 1. Choose A accuracy
  # 2. Avoid B accuracy
  # 3. Overall accuracy
  # 4. Number of training trials


  chooseAtrials <- c("AC", "CA", "AD", "DA", "AE", "EA", "AF", "FA")
  avoidBtrials <- c("BC", "CB", "BD", "DB", "BE", "EB", "BF", "FB")

  df <- df %>%
    filter(Procedure == "Test")

  df$chooseAvoid <- NA
  df$chooseAvoid <- ifelse(df$TrialType %in% chooseAtrials,
                           "chooseA", df$chooseAvoid)
  df$chooseAvoid <- ifelse(df$TrialType %in% avoidBtrials,
                           "avoidB", df$chooseAvoid)

  results <- df %>%
    group_by(subject, chooseAvoid) %>%
    summarise(accuracy = mean(as.numeric(StimulusPresentation2.ACC), na.rm=TRUE),
              n_trials = n()) %>%
    select(subject, chooseAvoid, accuracy) %>%
    filter(!is.na(chooseAvoid)) %>%
    spread(chooseAvoid, accuracy)
  return(results)
}
