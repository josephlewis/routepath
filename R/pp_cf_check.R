#' prior and predictive check
#'
#' This function calculates the route path from the start and end locations of lines using a user-supplied cost surface
#'
#' @param priors a matrix or data frame of priors.
#'
#' @param distribution cost factor distribution. Currently only implemented the 'laplace' (default)
#'
#' @param no_samples number of samples drawn from distribution
#'
#' @return xx
#'
#' @author Joseph Lewis
#'
#' @import graphics
#'
#' @export
#'
#'
pp_cf_check <- function(priors, distribution = "laplace", no_samples = 100, alpha = 0.1) {

  if(!inherits(priors, c("data.frame"))) {
    stop("priors argument is invalid. Expecting a data.frame object")
  }

  priors <- priors[sample(nrow(priors), no_samples, replace = TRUE),, drop=FALSE]

  if (distribution == "laplace")  {

    laplace_cf <- function(x, a, b, c, d, e, f) {
      cf <- (a * (exp(-(abs((x * e) + c) / (1/abs(b))))) * d)
      cf <- ifelse(cf > f, f, cf)
    }

    max_ylim <- max(priors[,1]) * max(priors[,4])

    for(i in 1:nrow(priors)) {
      graphics::curve(laplace_cf(a = priors[i,1],
                                 b = priors[i,2],
                                 c = priors[i,3],
                                 d = priors[i,4],
                                 e = priors[i,5],
                                 f = priors[i,6],
                                 x),
                      from=-0.9, to= 0.9, add=i!=1, col= alpha("#99CBFF", alpha), ylim=c(0,max_ylim), lwd = 2, xlab = "Slope (rise over run)", ylab = "Relative Speed", bty="n")
    }

    graphics::curve(laplace_cf(a = mean(priors[,1]),
                               b = mean(priors[,2]),
                               c = mean(priors[,3]),
                               d = mean(priors[,4]),
                               e = mean(priors[,5]),
                               f = mean(priors[i,6]),
                               x),
                    from=-0.9, to= 0.9, add= T, col= "#004A99", ylim=c(0,max_ylim), lwd = 3)

    graphics::axis(1, at = seq(-0.9, 0.9, by = 0.1))
    graphics::abline(h = 0, v = 0, col = "gray60", lwd = 1, lty = 2)
    graphics::legend("topright" , legend=c("Cost Function", "Mean Cost Function", "Zero-line"),
                     col=c("#99CBFF", "#004A99", "gray60"), lty=c(1,1,2), cex=1,
                     box.lty=0, lwd=3)
  }
}

