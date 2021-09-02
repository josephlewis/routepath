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
pp_cf_check <- function(priors, distribution = "laplace", no_samples = 100, alpha = 1, tobler = TRUE) {

  priors <- priors[sample(nrow(priors), no_samples, replace = TRUE),, drop=FALSE]

  if (distribution == "laplace")  {

    laplace_cf <- function(x, a, b, c, d, e, f) {
      cf <- (a * (exp(-(abs((x * e) + c) / (1/abs(b))))) * d)
      cf <- ifelse(cf > f, f, cf)
    }

    max_ylim <- max(priors[,"a"]) * max(priors[,"d"] * max(priors[,"e"]))

    for(i in 1:nrow(priors)) {
      graphics::curve(laplace_cf(a = priors[i,"a"],
                                 b = priors[i,"b"],
                                 c = priors[i,"c"],
                                 d = priors[i,"d"],
                                 e = priors[i,"e"],
                                 f = priors[i,"f"],
                                 x),
                      from=-0.9, to= 0.9, add=i!=1, col= rgb(red = 153, green = 205, blue = 255, alpha = alpha*255, maxColorValue = 255), ylim=c(0,max_ylim), lwd = 2, xlab = "Slope (rise over run)", ylab = "Relative Speed", bty="n")
    }

    graphics::curve(laplace_cf(a = mean(priors[,"a"]),
                               b = mean(priors[,"b"]),
                               c = mean(priors[,"c"]),
                               d = mean(priors[,"d"]),
                               e = mean(priors[,"e"]),
                               f = mean(priors[,"d"]),
                               x),
                    from=-0.9, to= 0.9, add= T, col= "#004A99", ylim=c(0,max_ylim), lwd = 3)

    if (tobler) {

      graphics::curve(laplace_cf(a = max_ylim,
                                 b = 3.5,
                                 c = 0.05,
                                 d = 1,
                                 e = 1,
                                 f = 1,
                                 x),
                      from=-0.9, to= 0.9, add= T, col= "red", ylim=c(0,max_ylim), lwd = 3)

      graphics::legend("topright" , legend=c("Simulated Cost Function", "Mean Simulated\nCost Function", "Tobler's Hiking Function (scaled)", "Zero-line"),
                       col=c("#99CBFF", "#004A99", "red", "gray60"), lty=c(1,1,1,2), cex=1,
                       box.lty=0, lwd=2)


    } else {

      graphics::legend("topright" , legend=c("Simulated Cost Function", "Mean Simulated\nCost Function", "Zero-line"),
                       col=c("#99CBFF", "#004A99","gray60"), lty=c(1,1,2), cex=1,
                       box.lty=0, lwd=2)
      }

    graphics::axis(1, at = seq(-0.9, 0.9, by = 0.1))
    graphics::abline(h = 0, v = 0, col = "gray60", lwd = 1, lty = 2)

  }
}



