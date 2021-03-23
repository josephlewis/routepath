# abc_reject <- function(parameters, lines) {
#
#   parameters_reject <- lapply(X = parameters, FUN = function(x) { abc::abc(target = summary_stat_target, param = x[,2:ncol(x)], sumstat = x[,"stats"], tol = tol, method = "rejection")})
#
#   parameters_abc <- cbind(line_id = rep(1:length(lines), each = sum(parameters_reject[[1]]$region)),
#                         do.call(rbind, lapply(X = parameters_reject, FUN = function(x) { x$unadj.values})))
#
# }
#
#
