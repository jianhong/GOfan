#' Cut the enriched GO iterms by the distances from the root after simplify
#' @description
#' Cut the input igraph object by the distances from the root.
#' @param g A igraph object
#' @param leaveTerms Leaves must contained GO terms.
#' @param cutoff The cutoff distance from the root
#' @param filterNodesByEdgeNumber Filter the ub graphs by the edge number.
#' @param mustkeep The GO iterms must be kept.
#' @param onlyKeep Only keep branches with give GO iterms.
#' @return A igraph object after filtering
#' @importFrom igraph V bfs induced_subgraph components as_edgelist delete_vertices vcount
#' @export
#' @examples
#' # example code
#'
filterGraph <- function(g, leaveTerms,
                        cutoff=4, filterNodesByEdgeNumber=0, mustkeep=c(),
                        onlyKeep=c()){
    stopifnot(is(g, 'igraph'))
    stopifnot(is.numeric(filterNodesByEdgeNumber))
    ## split it into small pieces
    comps <- components(g, mode = "weak")

    subgraphs <- lapply(unique(comps$membership), function(i)
        induced_subgraph(g, which(comps$membership == i))
    )
    ## filter
    subgraphs <- subgraphs[vapply(subgraphs,
                          FUN = function(x)
                              length(V(x))>filterNodesByEdgeNumber ||
                              any(mustkeep %in% names(V(x))),
                          FUN.VALUE = logical(1L))]
    ## simplify the graphs
    simplified_subgraphs <- lapply(subgraphs, function(sg) {
        root <- get_root(sg)
        ssg <- simplified_subgraphs(sg, names(root), cutoff=cutoff)
        ## filter by the leaves
        ssg <- filter_leaves_iteratively(ssg, leaveTerms)
        if(vcount(ssg)==0){
            return(NULL)
        }
        if(length(onlyKeep)){
            if(!any(onlyKeep %in% names(V(ssg)))){
                return(NULL)
            }
        }
        list(root = root$name, graph = ssg)
    })

    ## change it to 2 columns data
    vs_df <- lapply(simplified_subgraphs, function(sgl){
        if(length(sgl)==0){
            return(NULL)
        }
        sg <- sgl$graph
        e <- as_edgelist(sg)
        e <- rbind(e, c('', sgl$root))
        colnames(e) <- c('ancestor', 'offspring')
        e <- as.data.frame(e)
    })
    vs_df <- do.call(rbind, vs_df)
    return(vs_df)
}

simplified_subgraphs <- function(g, root_node, cutoff=4){
    # Run a BFS and get the distances (layers) from the root
    # 'distances' is an attribute of the returned object from the subcomponent() function.
    bfs_result <- bfs(g, root=root_node, mode="all", dist=TRUE)
    desired_nodes <- names(bfs_result$dist[bfs_result$dist<=cutoff])
    selected_vertices <- V(g)[names(V(g)) %in% desired_nodes]
    sg <- induced_subgraph(g, selected_vertices)
}

filter_leaves_iteratively <- function(g, keep_nodes) {
    repeat {
        # find leaves (outdegree = 0)
        leaves <- V(g)[degree(g, mode = "out") == 0]
        # find leaves not in keep_nodes
        remove_leaves <- leaves[!(names(leaves) %in% keep_nodes)]
        # stop if no leaves to remove
        if (length(remove_leaves) == 0) break
        # remove them
        g <- delete_vertices(g, remove_leaves)
    }
    g
}
