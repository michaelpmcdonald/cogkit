#' Score Simon Task
#'
#' @param df raw dataframe from experimental platform
#' @param platform specify experimental platform (default = "inquisit")
#'
#' @return scored dataframe
#' @importFrom lubridate day month year
#' @export
expt_score.simon <- function(df, excludedSubjects, platform, ...){
  # Set up the expt object
  expt <- expt_setup(df, excludedSubjects, platform)
  class(expt) <- c("simon", "experiment")

  # Create a new vector that represents the type of the preceding trial
  previous_type <- df$values.congruence[-1]
  previous_type <- append(previous_type, NA)
  df$previous_type <- previous_type
  df <- filter(df, !(blockcode == "testblock" & trialnum == 29))

  # Create scored data frame
  scored <- df %>%
    filter(blockcode == "testblock") %>%
    select(subject, values.congruence, previous_type, latency, correct) %>%
    group_by(subject, values.congruence, previous_type) %>%
    summarize(mean_latency = mean(latency),
              accuracy = mean(correct),
              sd_lat = sd(latency),
              n = n()) %>%
    unite(temp, mean_latency, accuracy, sd_lat, n, sep = "W", remove = TRUE)  %>%
    spread(values.congruence, temp) %>%
    separate(congruent, c("cong_latency", "cong_accuracy", "cong_sd", "cong_n"), sep = "W", convert = TRUE) %>%
    separate(incongruent, c("inc_latency", "inc_accuracy", "inc_sd", "inc_n"), sep = "W", convert = TRUE) %>%
    mutate(t_diff = as.numeric(inc_latency) - as.numeric(cong_latency)) %>%
    mutate(d = t_diff / sqrt( (cong_sd^2*(cong_n - 1) + inc_sd^2*(inc_n - 1) / (cong_n + inc_n + 1))))

  # Add attributes
  class(scored) <-  c("simon", class(scored))
  start.date <- min(lubridate::mdy(df$date))
  end.date <- max(lubridate::mdy(df$date))
  attr(scored, "Start date") <- paste(day(start.date), month(start.date), year(start.date), sep = "-")
  attr(scored, "End date") <- paste(day(end.date), month(end.date), year(end.date), sep = "-")
  expt$scored <- scored
  return(expt)
}

#' Summary method for Simon task
#' @method summary simon
#' @param x experiment object to be summarized
#' @param method method to use for confidence intervals - "classical" or "bootstrap"
#' @export
#'
#' @return NA
summary.simon <- function(x, statmethod = "classical", ...){
  ans <- list()
  class(ans) <- "summary.simon"
  ans$raw <- x[['raw']]
  raw <- x[['raw']]
  scored <- x[['scored']]
  ans$collection <- data.frame(
    "subj.count" = length(scored$subject),
    "start.num" = min(scored$subject),
    "end.num" = max(scored$subject),
    "begin.date" = as.character(min(lubridate::mdy(raw$date))),
    "end.date" = as.character(max(lubridate::mdy(raw$date))),
    "coll.days" = length(unique(raw$date)),
    "subj.perday" = length(scored$subject) / length(unique(raw$date))
  )
  ans$statmethod <- statmethod
  ans$descriptives <- ungroup(scored) %>%
    select(inc_latency,
           cong_latency,
           inc_accuracy,
           cong_accuracy,
           t_diff,
           d) %>%
    describe(., method = statmethod) %>%
    base::round( . , digits = 2)

  return(ans)

}

#' Print method for Simon experiments
#'
#' @method print summary.simon
#' @param x input simon object
#' @param ... NA
#' @export
#'
#' @return NA
print.summary.simon <- function(x, ...){
  collection <- x$collection
  cat("Data collected between", as.character(collection$begin.date), "and", as.character(collection$end.date), "\n    across", collection$coll.days, "days of collection.\n", sep = " ")
  cat("Total included subjects: ", collection$subj.count, "\nSubjects per collection day: ", round(collection$subj.perday, 1), "\n\n", sep = "")
  print(attr(x[['descriptives']], 'statmethodstring'))
  print(x[['descriptives']])
}

