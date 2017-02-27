ll_diffusion <- function(pars, rt, response)
{
  densities <- tryCatch(
    ddiffusion(rt, response=response,
               a=pars[1], v=pars[2], t0=pars[3],z=pars[4],
               sz=pars[5],
               st0=pars[6], sv=pars[7]),
    error = function(e) 0)
  if (any(densities == 0)) return(1e6)
  return(-sum(log(densities)))
}

diffusionEstimate <- function(df, fixed = FALSE){
  class(df) <- 'data.frame'
  df <- df %>% select(rt = q, response = resp)
  if(fixed){
    upperBound <- c(Inf, Inf, Inf, Inf, 0, 0, 0)
    startParams <- c(1.2, 1, .1, .5, 0, 0, 0)
  } else {
    upperBound <- c(Inf, Inf, Inf, Inf, Inf, Inf, Inf)
    startParams <- c(1.2, 1, .1, .5, .25, .25, .25)
  }
  results <- nlminb(startParams, ll_diffusion, lower = 0, upper = upperBound,
                    rt=df$rt, response=df$response)
  print(results$par)
  results <- as.character(paste(round(results$par[1:7],digits=3), letters[1:7], sep="_"))
  return(data.frame(results))
}
