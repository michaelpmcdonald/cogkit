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
    ans <- broom::tidy(data.frame(df)) # use data.frame(df) to force tidy to use the .df method
    if(nrow(ans) > 1) {
      row.names(ans) <- ans$column
      ans <- ans %>% dplyr::select(-column)
    }
    attr(ans, "statmethodstring") <- "Classical statistics: "
  } else if (method == "bootstrap") {
    x <- lapply(df, bootSummary)
    ans <- data.frame(matrix(unlist(x), nrow = ncol(df), byrow = T))
    row.names(ans) <- names(x)
    names(ans) <- c("Observed mean", "n", "median", "95% CI lower", "95% CI upper")
    attr(ans, "statmethodstring") <- "Bootstrapped statistics & conf. intervals, k = 10,000: "
  }
  return(ans)
}
