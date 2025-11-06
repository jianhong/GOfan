#' Recursive function to extract (ancestor, offspring) pairs
#' @param GO_IDs The Gene ontology term ids
#' @param onto The category of the GO ids. It should be one of "BP", "CC", or
#' "MF".
#' @return A data frame with columns ancestor and offspring
#' @importFrom GO.db GOMFANCESTOR
#' @importFrom GO.db GOBPANCESTOR
#' @importFrom GO.db GOCCANCESTOR
#' @export
#' @examples
#' ids <- c("GO:0099536", "GO:0099537", "GO:0007268",
#'          "GO:0098916", "GO:0050804", "GO:0099177")
#' df <- getAncestors_df(ids, onto="BP")
#' head(df)
getAncestors_df <- function(GO_IDs, onto = c("BP", "CC", "MF")) {
    onto <- match.arg(onto)
    is_GO_IDs(GO_IDs)

    # Choose ontology environment only once
    go_env <- switch(onto,
                     MF = GOMFANCESTOR,
                     BP = GOBPANCESTOR,
                     CC = GOCCANCESTOR)

    # Cache to store previously computed ancestors
    cache <- new.env(parent = emptyenv())

    # Recursive helper (memoized)
    get_anc <- function(id) {
        if (exists(id, envir = cache, inherits = FALSE)) {
            return(get(id, envir = cache))
        }
        if (is.na(id) || id == "all") {
            return(character(0))
        }
        res <- AnnotationDbi::mget(id, go_env, ifnotfound = list(NA))[[1]]
        if (is.null(res) || all(is.na(res))) {
            res <- character(0)
        } else {
            res <- unique(c(res, unlist(lapply(res, get_anc),
                                        use.names = FALSE)))
        }
        assign(id, res, envir = cache)
        return(res)
    }

    # Collect all offspring–ancestor pairs
    df_list <- lapply(GO_IDs, function(id) {
        ancestors <- get_anc(id)
        if (length(ancestors) > 0) {
            data.frame(ancestor = ancestors,
                       offspring = id,
                       stringsAsFactors = FALSE)
        } else {
            NULL
        }
    })

    # Combine and remove duplicates
    df <- do.call(rbind, df_list)
    df <- unique(df)

    return(df)
}
