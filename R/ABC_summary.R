ABC_summary <- function(routepath, cred_mass = c(0.5, 0.95)) {

  cred_mass <- check_cred_mass(cred_mass)

  routepath1 <- routepath

  p_col_index <- grep("^p\\.", colnames(routepath1))
  by_index <- which(colnames(routepath1) == "line_id")

  summaries <- list()

  for (i in p_col_index) {

    mymins <- stats::aggregate(routepath1[,i], by = list(routepath1[,by_index]), FUN = min)
    mymeans <- stats::aggregate(routepath1[,i], by = list(routepath1[,by_index]), FUN = mean)
    mymaxs <- stats::aggregate(routepath1[,i], by = list(routepath1[,by_index]), FUN = max)

    myhdis <- list()

    for (j in 1:length(cred_mass)) {

      myhdis[[j]] <- stats::aggregate(routepath1[,i], by = list(routepath1[,by_index]), FUN = function(x) { HDInterval::hdi(x, cred_mass[j])})$x

    }

    myhdis <- do.call(cbind, myhdis)

      sums <- cbind(mymins[1],
                    colnames(routepath1)[i],
                    mymins[2],
                    mymeans[2],
                    mymaxs[2],
                    myhdis)

      colnames(sums) <- c(by, "parameter", "min", "mean", "max", paste0("hdi_", colnames(myhdis), "_", rep(cred_mass, each = 2)))

      summaries[[i]] <- sums


  }

  summaries <- do.call(rbind, summaries)

  return(summaries)

}
