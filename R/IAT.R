#' Score IAT
#'
#' @description Processes IAT experimental data and returns D score and
#' appropriate statistics
#'
#' @param df Experimental dataframe.  Must contain columns subject,
#' @param platform experimental platform (default = "inquisit")
#'
#' @return a list of data.frames
#' @export
#' @import dplyr
#' @import tidyr
expt_score.iat <- function(df, excludedSubjects = NA, platform = "inquisit", ...){
  # Set up the expt object
  expt <- expt_setup(df, excludedSubjects, platform)
  class(expt) <- c("iat", "experiment")

  # Recode factors in a way that's easier to use
  df <- df %>%
    dplyr::mutate(pairing = forcats::fct_recode(blockcode,
                                                "1" = "attributepractice",
                                                '2' = "targetcompatiblepractice",
                                                '3' = "compatibletest1",
                                                '4' = "compatibletest2",
                                                '5' = "targetincompatiblepractice",
                                                '6' = "incompatibletest1",
                                                '7' = "incompatibletest2")
    )

  tbl_D_calc_statistics_long <- df %>%
    dplyr::select(subject, pairing, latency, correct) %>%
    dplyr::group_by(subject, pairing) %>%
    dplyr::summarize(mean_latency = mean(latency),
                     sd_latency = sd(latency),
                     acc = mean(correct, na.rm = TRUE),
                     n_trials = n()) %>%
    dplyr::filter(!is.na(pairing) & !is.na(mean_latency) & !is.na(sd_latency) & !is.na(n_trials))
  # Separately spread statistics into wide form and rename the variables appropriately
  # (there are better ways to do this, but it's not worth rewriting)

  temp1 <- tbl_D_calc_statistics_long %>% select(subject, pairing, mean_latency) %>% spread(pairing, mean_latency)
  temp2 <- tbl_D_calc_statistics_long %>% select(subject, pairing, sd_latency) %>% spread(pairing, sd_latency)
  temp3 <- tbl_D_calc_statistics_long %>% select(subject, pairing, n_trials) %>% spread(pairing, n_trials)
  temp4 <- tbl_D_calc_statistics_long %>% select(subject, pairing, acc) %>% spread(pairing, acc)
  names(temp1) <- c("subject", paste("lat", 1:7, sep=""))
  names(temp2) <- c("subject", paste("sd", 1:7, sep=""))
  names(temp3) <- c("subject", paste("n", 1:7, sep=""))
  names(temp4) <- c("subject", paste("acc", 1:7, sep=""))


  # Join temporary tables into one wide-form table
  tbl_D_calc_statistics <- temp1 %>% left_join(temp2) %>% left_join(temp3) %>% left_join(temp4)

  # # IAT calculations # #

  tbl_D_calc_statistics$dPractice <- with(tbl_D_calc_statistics, dScore(lat6, lat3, sd6, sd3, n6, n3))
  tbl_D_calc_statistics$dTest <- with(tbl_D_calc_statistics, dScore(lat7, lat4, sd7, sd4, n7, n4))
  tbl_D_calc_statistics$dAll <- (.5*tbl_D_calc_statistics$dPractice+.5*tbl_D_calc_statistics$dTest)


  # Create a variable that indicates whether or not a dataset is complete
  tbl_D_calc_statistics$complete <- !is.na(rowSums(tbl_D_calc_statistics))

  tbl_D_calc_statistics$meanLat <- with(tbl_D_calc_statistics, (lat3+lat4+lat6+lat7)/4)

  expt$scored <- data.frame(tbl_D_calc_statistics)

  # Determine exclusion flags
  speed_flag <- iat_flagSpeed(df) %>% mutate(speed = flag)
  error_flag <- iat_flagError(df, .4) %>% mutate(error = flag)
  flags <- speed_flag %>% left_join(., error_flag)
  expt$flags <- flags


  return(expt)
}

expt_flag.iat <- function(df, platform)

#' Calculate a D score based on vectors of means, SDs, and Ns
#'
#' @param m1 mean of block 1
#' @param m2 mean of block 2
#' @param sd1 standard deviation of block 1
#' @param sd2 standard deviation of block 2
#' @param n1 n (count) of block 1
#' @param n2 n (count) of block 2
#'
#' @return vector of D scores
#'
#' @importFrom dplyr '%>%'
#'
#' @export
dScore <- function(m1, m2, sd1, sd2, n1, n2){
  # with a set of block mean latencies, sds, and ns, returns a d score
  numerator <- m1-m2
  denominator <- sqrt( ( ( (n1-1)*sd1^2+(n2-1)*sd2^2) +
                           ( (n1+n2) * ((m1-m2)^2 ) / 4) ) / (n1+n2-1))
  d <- numerator/denominator
  return(d)
}


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
                     flag = mean(latency <= threshold) > criterion)
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
    dplyr::summarize(flag = max(error_rate) >= criterion)
  return(flag_data)
}

#' Find compatibility order of IAT
#'
#' @param df same as scoring frame
#'
#' @return data frame of subjects with order
#' @importFrom dplyr %>%
#' @export
iat_findOrder <- function(df){
  results <- df %>%
    dplyr::filter(blocknum == 3 & trialnum == 2) %>%
    dplyr::mutate(order = forcats::fct_recode(blockcode,
                                              "compatiblefirst" = "compatibletest1",
                                              'incompatiblefirst' = "incompatibletest1")) %>%
    dplyr::select(subject, order)
  return(results)
}

# find stimuli pair in a particular block for a particular subject
# df %>% filter(subject == XX & blockcode == "compatibletest1" & response == 23 & correct == 1) %>% select(trialcode) %>% unique()
