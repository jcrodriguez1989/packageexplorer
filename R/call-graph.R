# Auxiliar methods here
# Not any exported method
# Code from CodeDepends library, with some modifications

#' @importFrom utils find
get_pkgs_and_funs <- function(names) {
  # obtain functions that contain namespace "::"
  w_nmspace <- grepl("::", names)
  funs_w_nmspace <- names[w_nmspace]
  names(funs_w_nmspace) <- gsub("::.*", "", funs_w_nmspace)
  funs_w_nmspace <- gsub(".*::", "", funs_w_nmspace)
  names <- names[!w_nmspace]

  # obtain functions with no namespace
  no_nmspace <- unlist(sapply(names, function(x) length(find(x)) == 1))
  funs_no_nmspace <- names[no_nmspace]
  names(funs_no_nmspace) <- gsub(".*:", "", sapply(funs_no_nmspace, find))
  if (length(no_nmspace) > 0)
    names <- names[!no_nmspace]

  # rest are packages
  pkgs <- gsub(".*:", "", names)

  return(list(pkgs = pkgs, funs = c(funs_w_nmspace, funs_no_nmspace)))
}

make_call_graph <- function(object, all, ...) {
  UseMethod("make_call_graph")
}

make_call_graph.character <- function(obj, all = FALSE) {
  path <- search()
  funs <- lapply(obj, get_functions)
  make_call_graph(unlist(funs, recursive = FALSE),
    all = all,
    fun_names = unlist(lapply(funs, names)),
    packages = rep(gsub("^package:", "", obj), sapply(funs, length))
  )
}

#' @importFrom codetools findGlobals
make_call_graph.list <- function(obj, all = FALSE, fun_names = names(obj),
                                 packages = attr(obj, "packages")) {
  stopifnot(all(fun_names == names(obj)))

  ids <- fun_names
  edges <- lapply(obj, function(f) {
    calls <- character(0)
    if (typeof(f) == "closure") {
      calls <- findGlobals(f, merge = FALSE)$functions
    }
    if (all) {
      return(calls)
    }
    calls[calls %in% ids]
  })

  pkgs <- unique(packages)
  pkgs <- pkgs[!grepl("^\\.", pkgs)]
  pkg_deps <- sapply(pkgs, function(act_pkg) {
    intersect(get_dependencies(act_pkg), pkgs)
  })

  # if we have duplicated nodes, then check to which namespace it should point
  dupl <- ids[duplicated(ids)]
  fun_w_pkg <- paste(packages, ids, sep = ":")
  edges <- lapply(seq_along(edges), function(i) {
    act_edge <- intersect(edges[[i]], ids)
    if (!any(act_edge %in% dupl)) {
      return(match(act_edge, ids))
    }
    res <- match(act_edge[!act_edge %in% dupl], ids)
    act_dupl <- act_edge[act_edge %in% dupl]
    act_deps <- c(packages[[i]], pkg_deps[[ packages[[i]] ]])

    dupl_res <- match(unlist(lapply(act_dupl, function(x) {
      x_w_pkg <- paste(act_deps, x, sep = ":")
      c(x_w_pkg[x_w_pkg %in% fun_w_pkg], NA)[[1]]
    })), fun_w_pkg)
    c(res, dupl_res[!is.na(dupl_res)])
  })

  if (all) {
    z <- lapply(edges, function(x) x[ !(x %in% c("|", "||")) ])
    ids <- unique(c(unlist(z), ids))
    edges <- replicate(length(ids), character(), FALSE)
    names(edges) <- ids
    edges[ names(z) ] <- z
  }

  create_graph(edges, fun_names, packages)
}

get_functions <- function(pos, syms = objects(pos)) {
  objs <- lapply(syms, function(x) {
    f <- get(x, pos)
    if (is.function(f)) f else NULL
  })
  names(objs) <- syms
  objs[!sapply(objs, is.null)]
}

# for pkg_names, it return its Depends and Imports
#' @importFrom magrittr %>%
#' @importFrom utils installed.packages
get_dependencies <- function(pkg_names, only_installed = TRUE) {
  inst_pkgs <- installed.packages()[, c("Depends", "Imports")]
  deps <- inst_pkgs[pkg_names, ] %>%
    strsplit(",") %>%
    unlist() %>%
    trimws() %>%
    gsub(pattern = "\n.*", replacement = "") %>%
    gsub(pattern = " .*", replacement = "") %>%
    unique()
  deps <- deps[!is.na(deps)]

  if (only_installed) {
    deps <- deps[deps %in% rownames(inst_pkgs)]
  }
  return(unname(deps))
}

create_graph <- function(edges, fun_names, packages) {
  nodes <- data.frame(
    id = seq_along(fun_names),
    label = fun_names,
    group = packages,
    title = unlist(lapply(fun_names, fun_to_args)),
    shape = "box"
  )

  edges <- data.frame(do.call(rbind, lapply(seq_along(edges), function(i) {
    node <- edges[[i]]
    suppressWarnings(cbind(i, cbind(node)))
  })))
  colnames(edges) <- c("from", "to")
  # edges$label <- "uses"

  return(list(nodes = nodes, edges = edges))
}

fun_to_args <- function(fun_name) {
  fun_formals <- formals(fun_name)
  if (length(fun_formals) == 0) {
    return("")
  }
  is_char <- unlist(lapply(fun_formals, is.character))
  has_def <- !unlist(lapply(fun_formals, is.name))
  fun_formals[is_char] <- paste0('"', fun_formals[is_char], '"')
  fun_arg_vals <- paste0(" = ", as.character(fun_formals))
  fun_arg_vals[!has_def] <- ""
  paste0(
    "<code>",
    fun_name, "<br>",
    "---------", "<br>",
    paste(paste0(names(fun_formals), fun_arg_vals), collapse = ",<br>"),
    "</code>"
  )
}

filter_graph <- function(graph, pkgs_funs) {
  edges <- graph$edges
  nodes <- graph$nodes

  new_nodes <- nodes[
    nodes$group %in% pkgs_funs$pkgs |
    (nodes$group %in% names(pkgs_funs$funs) & nodes$label %in% pkgs_funs$funs),]
  new_edges <- edges[edges$from %in% new_nodes$id, ]
  new_nodes <- rbind(
    new_nodes,
    nodes[nodes$id %in% new_edges$to, ]
  )
  new_nodes <- new_nodes[!duplicated(new_nodes), ]
  return(list(nodes = new_nodes, edges = new_edges))
}
