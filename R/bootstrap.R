#' Bootstrap correlation coefficients from two vectors of values.
#'
#' @param X vector of values
#' @param Y vector of values (same length as X)
#' @param size number of resampling iterations
#' @return dataframe of estimated correlations, estimated p.values, and the
#' observed correlation in the data.
#'
#' @importFrom dplyr '%>%'
#'
#' @export
bootCor <- function(X, Y, size = 10000){
  df <- data.frame(X, Y) %>% dplyr::filter(!is.na(X) & !is.na(Y))
  correlation <- broom::tidy(cor.test(df$X, df$Y, use = "complete"))$estimate
  results <- df %>% broom::bootstrap(size) %>% do(broom::tidy(cor.test(.$X, .$Y, use = "complete")))
  return(data.frame("estimate" = results$estimate, "p.value" = results$p.value, "observed" = correlation))
}


#' Bootstrap correlation coefficients from two vectors of values.
#'
#' @param X vector of values
#' @param Y vector of values (same length as X)
#' @param size number of resampling iterations
#' @return Observed correlation (from sample) and 5% and 95% quantiles
#'
#' @importFrom dplyr '%>%'
#'
#' @export
metaBootCor <- function(X, Y, size=10000){
  results <- bootCor(X, Y, size) %>%
    dplyr::summarize(observed_r = max(observed, na.rm=TRUE),
              lower025 = stats::quantile(estimate, .025, na.rm=TRUE),
              upper975 = stats::quantile(estimate, .975, na.rm=TRUE))
  return(results)
}

#' Bootstrap statistical summaries
#'
#' @param x vector of observations
#' @param size Number of bootstrapping iterations
#'
#' @return Dataframe with summary statistics for bootstrapped median and .05/.95 quantiles
#' of the input observations
#' @export
bootSummary <- function(x, size=10000){
  # Bootstraps mean summary statistics and 95% CI from a sample, @size iterations,
  # returning a data.frame with
  n <- length(x)
  observedMean <- mean(x, na.rm = TRUE)
  bootSample <- replicate(size, mean(sample(x, n, replace = TRUE)))
  bootSDSample <- replicate(size, sd(sample(x, n, replace = TRUE)))
  results <- data.frame("observedMean" = observedMean,
                        "n" = n,
                        "median" = stats::median(bootSample),
                        "lower.025" = stats::quantile(bootSample, .025),
                        "upper.975" = stats::quantile(bootSample, .975),
                        "sample.SD.median" = stats::median(bootSDSample),
                        "sample.SD.lower025" = stats::quantile(bootSDSample, .025),
                        "sample.SD.upper975" = stats::quantile(bootSDSample, .975),
                        row.names = "")
  return(results)
}



