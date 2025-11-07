#' Creating igraph graphs from ancestor_offspring data frame
#' @description
#' This function creates an igraph graph from one data frames containing
#' the ancestor and offspring information.
#' @param df A data frame, output of \link{getAncestors_df}
#' @return A igraph graph.
#' @importFrom igraph graph_from_data_frame
#' @export
#' @examples
#' goids <- c("GO:0099536", "GO:0099537", "GO:0007268", "GO:0098916","GO:0050804")
#' anc <- getAncestors_df(goids, onto='BP')
#' goGraph(anc)
#'
goGraph <- function(df){
    stopifnot(is.data.frame(df))
    stopifnot(all(c('ancestor', 'offspring') %in% colnames(df)))

    ## remove the root, which will be used for split the componds
    df <- df[df$ancestor!='all' & df$offspring!='all', ]
    ## create a graph
    graph_from_data_frame(df[, c('ancestor', 'offspring')],
                          directed = TRUE)
}
