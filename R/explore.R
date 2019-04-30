#' Explore packages and functions interactively
#'
#' Given package and function names interactively explore them in a graph.
#'
#' @param names Character names of the packages and functions to explore.
#'   Functions must be loaded in the namespace (with `library(FUN_PKG)`), or be
#'   written as `FUN_PKG::FUN_NAME`.
#'   It is recommended to write package names as `"package:PKG_NAME"`.
#' @param include_deps Logical if dependencies from other packages should be
#' included. Dependencies stand for "Depends" and "Imports".
#'
#' @importFrom magrittr %>%
#' @importFrom visNetwork visNetwork visEdges visGroups visLegend visOptions
#'
#' @examples
#' \dontrun{
#'   explore(c("package:packageexplorer", "base::library"),
#'           include_deps = TRUE)
#' }
#'
#' @export
explore <- function(names, include_deps = FALSE) {
  sep <- get_pkgs_and_funs(names)
  if (length(sep$pkgs) > 0) {
    cat("Going to explore packages:\n")
    cat("\t", sep$pkgs, "\n")
  }
  if (length(sep$funs) > 0) {
    cat("Going to explore functions:\n")
    cat("\t", paste0(names(sep$funs), "::", sep$funs), "\n")
  }

  all_pkgs <- c(sep$pkgs, names(sep$funs))
  if (include_deps) {
    deps <- get_dependencies(c(sep$pkgs, names(sep$funs)))
    all_pkgs <- c(deps, all_pkgs)
  }

  invisible(lapply(all_pkgs, library, character.only = TRUE))
  all_pkgs <- unique(paste0("package:", all_pkgs))

  graph <- make_call_graph(all_pkgs)
  graph <- filter_graph(graph, sep)
  visNetwork(graph$nodes, graph$edges) %>%
    visEdges(arrows = "to") %>%
    visGroups() %>%
    visOptions(selectedBy = "group", highlightNearest = TRUE)
}
