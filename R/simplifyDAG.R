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
#' library(igraph)
#' library(org.Dr.eg.db)
#' edges <- data.frame(
#'   ancestor=c("GO:0007154", "GO:0007267", "GO:0099536", "GO:0099537"),
#'   offsprings=c("GO:0099536", "GO:0099536", "GO:0099537", "GO:0098916"))
#' g <- graph_from_data_frame(edges)
#' g1 <- simplifyDAG(g, org.Dr.eg.db)
simplifyDAG <- function(g, org) {
    stopifnot(is(g, 'igraph'))
    stopifnot(is(org, 'OrgDb') || is(org, 'Go3AnnDbBimap'))
    stopifnot('No GO terms available in the graph'=length(names(V(g)))>0)
    edges_to_keep <- c()
    allAlias <- getGOalias(names(V(g)), org=org)
    for (v in names(V(g))) {
        parents <- neighbors(g, v, mode = "in")
        if (length(parents) < 1) next

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
    g_simple <- removeIsolatedVertices(g_simple)
    return(g_simple)
}

#' @importFrom igraph degree delete_vertices
removeIsolatedVertices <- function(g){
    vertex_degrees <- degree(g, mode='all')
    isolated_vertices <- which(vertex_degrees == 0)
    if(length(isolated_vertices)){
        delete_vertices(g, isolated_vertices)
    }else{
        g
    }
}
