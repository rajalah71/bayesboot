#' Bayesian bootstrap
#'
#' Calculates the value of a function on the bayesian bootstrap data. Uses the dirichlet distribution to generate positive real valued weights for the data as opposed to using integer weights (normal bootstrapo).
#'
#' @param data Data
#' @param f A function to estimate with bootstrap
#' @param b Bootstrap sample size
#' @param plot Should the results be plotted (default = TRUE)
#' @param quantilepoints Quantile points to be plotted (default = c(0.025, 0.975))
#'
#' @return Values of the the function 'f' calculated on the bayesian bootstrap data
#'
#' @importFrom gtools rdirichlet
#' @importFrom graphics hist legend lines segments
#' @importFrom stats quantile
#'
#' @examples
#' # Calculate bootstrap means
#' data = c(1, 1, 3, 4, 7, 12)
#' bayesboot(data)
#'
#' @export
bayesboot = function(data, f = mean, b = 10000, plot = TRUE, quantilepoints = c(0.025, 0.975)){
  boot_results = rep(NA, b)
  len = length(data)

  for(i in (1:b)){
    # Weights from dirichlet distribution
    weights = rdirichlet(1, rep(1, len))*len
    # Calculate the value of the function on the data with the weights
    boot_results[i] = f(data*weights)
  }

  # Plot if plot == TRUE
  if(plot == TRUE){
    hist(boot_results, main = "Bayesian bootstrap", xlab = "Values", freq = FALSE)
    # add quantiles 0.025 and 0.975
    quantiles = quantile(boot_results, quantilepoints)
    # add horizontal lines to indicate the quantiles
    lines(c(quantiles[1], quantiles[2]), c(0, 0), col = "red", lwd = 3)
    # add small vertical lines at the ends of the horizontal lines
    segments(x0 = quantiles[1], y0 = -0.02, x1 = quantiles[1], y1 = 0.02, col = "red", lwd = 2.5)
    segments(x0 = quantiles[2], y0 = -0.02, x1 = quantiles[2], y1 = 0.02, col = "red", lwd = 2.5)
    # Add a legend with the quantiles
    legend("topright", legend = c("Quantiles", paste0(quantilepoints[1], ": ", round(quantiles[1], 3)), paste0(quantilepoints[2], ": ", round(quantiles[2], 3))), col = "red", cex = 0.8)
  }

  return(boot_results)
}
