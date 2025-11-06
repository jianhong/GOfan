#' Simplify DAG by keeping strongest parent in sub-graphs
#' @description
#' simplify graph by keeping only strongest parent, to make the DAG to a tree like
#' graph
#' @param g A igraph object
#' @param org A Go3AnnDbBimap object
#' @return Simplified graph
#' @importFrom igraph V make_empty_graph vertices edges neighbors simplify
#' @export
#' @examples
#' # example code
#'
simplifyDAG <- function(g, org) {
    stopifnot(is(g, 'igraph'))
    stopifnot(is(org, 'OrgDb') || is(org, 'Go3AnnDbBimap'))
    edges_to_keep <- c()
    allAlias <- getGOalias(names(V(g)), org=org)
    for (v in names(V(g))) {
        parents <- neighbors(g, v, mode = "in")
        if (length(parents) <= 1) next

        # compute Jaccard with each parent
        jaccs <- vapply(names(parents), function(p)
            jaccard(allAlias[[v]], allAlias[[p]]),
            FUN.VALUE = numeric(1L))
        best_parent <- parents[which.max(jaccs)]
        edges_to_keep <- rbind(edges_to_keep, c(best_parent$name, v))
    }

    # create new simplified graph with only kept edges
    all_nodes <- V(g)$name
    g_simple <- make_empty_graph(directed = TRUE) +
        vertices(all_nodes) +
        edges(t(edges_to_keep))
    g_simple <- simplify(g_simple, remove.multiple = TRUE)
    return(g_simple)
}
