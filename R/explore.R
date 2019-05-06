#' Explore packages and functions interactively
#'
#' Given package and function names interactively explore them in a graph.
#'
#' @param names Character names of the packages and functions to explore.
#'   Functions must be loaded in the namespace (with `library(FUN_PKG)`), or be
#'   written as `FUN_PKG::FUN_NAME`.
#'   It is recommended to write package names as `"package:PKG_NAME"`.
#' @param include_deps Logical if dependencies from other packages should be
#'   included. Dependencies stand for "Depends" and "Imports".
#' @param plot logical indicating if graph should be plotted. It can take long
#'   time to render if it is too big.
#'
#' @return the graph as list, with nodes and edges that can be directly used by
#'   visNetwork library.
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
explore <- function(names, include_deps = FALSE, plot = TRUE) {
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
  envs <- all_pkgs[grepl("^\\.", all_pkgs)]
  all_pkgs <- all_pkgs[!grepl("^\\.", all_pkgs)]
  if (include_deps) {
    deps <- get_dependencies(all_pkgs)
    all_pkgs <- c(deps, all_pkgs)
  }

  invisible(lapply(all_pkgs, library, character.only = TRUE))
  if (length(all_pkgs) > 0)
    all_pkgs <- unique(paste0("package:", all_pkgs))

  graph <- make_call_graph(union(all_pkgs, envs))
  graph <- filter_graph(graph, sep)
  if (plot)
    visNetwork(graph$nodes, graph$edges) %>%
      visEdges(arrows = "to") %>%
      visGroups() %>%
      visOptions(selectedBy = "group", highlightNearest = TRUE) %>%
      print()

  return(invisible(graph))
}
