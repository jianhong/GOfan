#' Prepare the graph for Sunburst plot
#' @param df A data frame with enriched GO iterms
#' @param org An Go3AnnDbBimap object eg org.Dr.egGO2ALLEGS
#' @param termID Column name in df which store the GO IDs
#' @param onto The ontology category of the GO IDs
#' @return A igraph graph.
#' @importFrom AnnotationDbi Term
#' @export
getGraph <- function(df,
                     org,
                     termID='ID',
                     onto = c('BP', 'CC', 'MF')){
    stopifnot(is.data.frame(df))
    stopifnot(termID %in% colnames(df))
    onto <- match.arg(onto)
    stopifnot(is(org, 'OrgDb') || is(org, 'Go3AnnDbBimap'))
    ## get all ancesters
    go_terms <- df[, termID]
    anc <- getAncestors_df(go_terms, onto = onto)
    g <- goGraph(anc)
    ## simplify graph by keeping only strongest parent, to make the DAG to a tree like
    g <- simplifyDAG(g, org=org) ## time consuming
    return(g)
}
