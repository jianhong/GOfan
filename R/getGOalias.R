#' Extract all the entrez IDs from a given GO IDs
#' @param GO_IDs The Gene ontology term id
#' @param org The OrgDb
#' @return A list of entrez IDs for the given GO IDs
#' @importFrom AnnotationDbi select
#' @export
#' @examples
#' library(org.Dr.eg.db)
#' ids <- c(
#'     "GO:0099536", "GO:0099537", "GO:0007268",
#'     "GO:0098916", "GO:0050804", "GO:0099177"
#' )
#' eids <- getGOalias(ids, org.Dr.eg.db)
getGOalias <- function(GO_IDs, org) {
    is_GO_IDs(GO_IDs)
    if (is(org, "Go3AnnDbBimap")) {
        out <- mget(GO_IDs, org, ifnotfound = NA)
    } else {
        out <- tryCatch(
            {
                suppressMessages( ## used to suppress the select 1:1 message
                    res <- AnnotationDbi::select(org,
                        keys = GO_IDs,
                        columns = c("ENTREZID"),
                        keytype = "GOALL"
                    )
                )
                res <- split(res, res$GOALL)
                res <- lapply(res, function(ids) {
                    .ele <- as.character(ids$ENTREZID)
                    names(.ele) <- as.character(ids$EVIDENCEALL)
                    .ele
                })
                res[GO_IDs]
            },
            error = function(e) {
                NA
            }
        )
    }
    names(out) <- GO_IDs

    return(out)
}
