#' Calculates route path from start and end locations of lines using user-supplied cost surface
#'
#' This function calculates the route path from the start and end locations of lines using a user-supplied cost surface
#'
#' @param matrix matrix of outputs from ABC rejection
#'
#' @param thresholdss numeric values of distances between known route and simulated routes
#'
#' @return plot
#'
#' @author Joseph Lewis
#'
#' @import foreach
#' @import dplyr
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @import ggdendro
#' @import sf
#'
#' @export

plot_similarity <- function(matrix, thresholds, cluster = TRUE, ...) {

    if (!inherits(matrix, c("sf", "data.frame"))) {
        stop("matrix argument is invalid. Expecting a sf or data.frame object")
    }

    if (!inherits(thresholds, c("numeric"))) {
        stop("matrix argument is invalid. Expecting a numeric vector object")
    }

    if (inherits(matrix, "sf")) {
        matrix <- sf::st_drop_geometry(matrix)
    }

    pair_matrix <- function(posterior, thresholds) {

        posterior$id <- rep(1:(nrow(posterior)/length(unique(posterior$line_id))), times = length(unique(posterior$line_id)))

        xx <- posterior %>%
            dplyr::filter(stats <= thresholds)

        p <- expand.grid(1:length(unique(posterior$line_id)), 1:length(unique(posterior$line_id)))

        test <- list()

        for (i in 1:nrow(p)) {

            pp <- p[i, ]

            ppp <- xx$id[xx$line_id == as.numeric(pp[1])]
            pppp <- xx$id[xx$line_id == as.numeric(pp[2])]

            pppp <- (length(intersect(ppp, pppp))/length(c(ppp, pppp))) * 2

            if (is.na(pppp)) {

                pppp <- 0

            }

            test[[i]] <- pppp

        }

        test <- unlist(test)

        p <- cbind(p, Similarity = test)

        p$Var1 <- as.factor(p$Var1)
        p$Var2 <- as.factor(p$Var2)

        return(p)

    }

    dendo_cluster <- function(mat) {

        qq <- reshape(mat, idvar = "Var1", timevar = "Var2", direction = "wide")

        qqq <- qq[2:ncol(qq)]

        qa <- as.dendrogram(hclust(d = dist(x = qqq)))

        dendro.plot <- ggdendrogram(data = qa, rotate = TRUE)

        otter.order <- order.dendrogram(qa)

        mat$Var1 <- factor(x = mat$Var1, levels = mat$Var1[otter.order], ordered = TRUE)

        return(mat)

    }

    routepath_similarity <- foreach::foreach(row_no = thresholds, .packages = "dplyr") %do% {

    mat <- pair_matrix(posterior = matrix, thresholds = row_no)

    if (cluster) {
        mat <- mat %>%
                dendo_cluster(.)

        }

    plot <- ggplot(data = mat, aes(x = Var1, y = Var2, fill = Similarity)) +
        geom_tile() +
        labs(x = "Road Number", y = "Road Number", title = row_no) +
        scale_fill_gradientn(limits = c(0, 1), colours = c("darkorange1","darkmagenta", "navyblue")) +
        theme_classic()

    }

    plot(gridExtra::arrangeGrob(grobs = routepath_similarity))

}





