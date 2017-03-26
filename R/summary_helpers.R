#' Simple descriptor function
#'
#' @param df data frame object
#' @param method "classical" or "bootstrap"
#' @param ... ...
#'
#' @return ans a named vector of statistical values
#' @export
#'
describe <- function(df, method = "classical", ...){
  if (method == "classical") {
    ans <- broom::tidy(data.frame(df)) # use data.frame(df) to force tidy to use .df method
    if(nrow(ans) > 1) { # variable name needs to be fixed when there's only one variable
      row.names(ans) <- ans$column
      ans <- ans %>% dplyr::select(-column)
    }
    attr(ans, "statmethodstring") <- "Classical statistics: "
  } else if (method == "bootstrap") {
    x <- lapply(df, bootSummary)
    ans <- data.frame(matrix(unlist(x), nrow = ncol(df), byrow = T)) # Turn the output list into a data.frame
    row.names(ans) <- names(x) # give the data.frame informative row names
    names(ans) <- c("Observed mean", "n", "median", "95% CI lower", "95% CI upper")
    attr(ans, "statmethodstring") <- "Bootstrapped statistics & conf. intervals, k = 10,000: "
  }
  return(ans)
}


# I could use a generic print method for summaries, which would print out the experiment name, collection dates, subject and other meta-information, descriptives, and then any "special" items along with their "special descriptions".  This would make a standard form, and could just loop through any specials.
#
# An experiment summary should contain:
# Experiment name (and citation?)
# Collection dates
# Subject counts
# Exclusion information (counts, reasons)
# Basic descriptives
# Any special (but basic) contrasts
