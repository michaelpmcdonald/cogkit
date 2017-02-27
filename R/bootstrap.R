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
  # Bootstraps a correlation coefficient from a sample, repeated @size iterations,
  # returning a vector of estimates & p values
  require(broom)
  require(dplyr)
  df <- data.frame(X, Y) %>% filter(!is.na(X) & !is.na(Y))
  correlation <- tidy(cor.test(df$X, df$Y, use = "complete"))$estimate
  results <- df %>% broom::bootstrap(size) %>% do(tidy(cor.test(.$X, .$Y, use = "complete")))
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
    summarize(observed_r = max(observed, na.rm=TRUE),
              lower05 = quantile(estimate, .05, na.rm=TRUE),
              upper95 = quantile(estimate, .95, na.rm=TRUE))
  return(results)
}

bootSummary <- function(x, size=10000){
  # Bootstraps mean summary statistics and 95% CI from a sample, @size iterations,
  # returning a data.frame with
  n <- length(x)
  observedMean <- mean(x, na.rm = TRUE)
  bootSample <- replicate(size, mean(sample(x, n, replace = TRUE)))
  results <- data.frame("observed" = observedMean,
                        "n" = n,
                        "median" = median(bootSample),
                        "lower05" = quantile(bootSample, .05),
                        "upper95" = quantile(bootSample, .95))
  return(results)
}


