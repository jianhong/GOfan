#' Sunburst plot for enriched GO term
#' @param df A data frame with enriched GO terms
#' @param org An OrgDb object
#' @param g An igraph graph. Output of \link{getGraph}.
#' @param termID Column name in df which store the GO IDs
#' @param fill Column name in df used to set the fill colors
#' @param sub_rect Column name in df used to set the area of a proportional
#' sub-rectangle, which represent a share of the whole. The values should be
#' a number in the range from 0 to 1. If it is a count number, it will be convert
#' to a proportion by divided the total number of features in the term.
#' Otherwise, will simply re-scale to .
#' @param GO_annotation_level_cutoff The cutoff of the GO annotation levels
#' @param filterNodesByEdgeNumber Filter the sub graphs by the edge numbers.
#' @param mustkeep The GO terms must be kept.
#' @param onlyKeep Only keep branches with give GO terms.
#' @param fillNAby0 Fill the NA values by 0 or not for the color column.
#' @param onto The ontology category of the GO IDs
#' @param plotBy plot tools, plotly or ggplot2.
#' @param ... parameter passed to \link{ggSunburst}.
#' @return A plot handle
#' @importFrom AnnotationDbi Term
#' @importFrom methods is
#' @importFrom stats as.formula
#' @importFrom plotly plot_ly
#' @export
#' @examples
#' library(org.Dr.eg.db)
#' df <- data.frame(
#'     ID = c("GO:0007267", "GO:0099536", "GO:0099537", "GO:0098916"),
#'     qvalue = -10 * log10(runif(4, max = 0.05))
#' )
#' sunburstGO(df, org.Dr.eg.db)
sunburstGO <- function(df,
                       org,
                       g,
                       termID = "ID", fill = "qvalue",
                       sub_rect = NULL,
                       GO_annotation_level_cutoff = 4,
                       filterNodesByEdgeNumber = 2,
                       mustkeep = c(),
                       onlyKeep = c(),
                       fillNAby0 = TRUE,
                       onto = c("BP", "CC", "MF"),
                       plotBy = c("plotly", "ggplot2"),
                       ...) {
    stopifnot(is.data.frame(df))
    stopifnot(termID %in% colnames(df))
    stopifnot(fill %in% colnames(df))
    stopifnot(is(org, "OrgDb"))
    stopifnot(is.numeric(GO_annotation_level_cutoff))
    stopifnot(is.logical(fillNAby0))
    plotBy <- match.arg(plotBy)
    onto <- match.arg(onto)
    fillNAby0 <- fillNAby0[1]
    if (missing(g)) {
        g <- getGraph(df, org = org, termID = termID, onto = "BP")
    } else {
        stopifnot(is(g, "igraph"))
    }

    ## filter graph by GO annotation levels
    vs_df <- filterGraph(g,
        cutoff = GO_annotation_level_cutoff,
        filterNodesByEdgeNumber = filterNodesByEdgeNumber,
        mustkeep = mustkeep, onlyKeep = onlyKeep,
        leaveTerms = df[, termID]
    )
    if (plotBy == "plotly") {
        ## add q-value as color
        vs_df[, fill] <- df[match(vs_df$offspring, df[, termID]), fill]
        if (fillNAby0) vs_df[is.na(vs_df[, fill]), fill] <- 0
        ## add lables
        vs_df$label <- Term(vs_df$offspring)

        pl <- plot_ly(vs_df,
            ids = as.formula("~offspring"),
            labels = as.formula("~label"),
            parents = as.formula("~ancestor"),
            marker = list(colors = as.formula(paste0("~", fill))),
            type = "sunburst"
        )
        return(pl)
    } else {
        combined_g <- graph_from_data_frame(vs_df[, c("ancestor", "offspring")],
            directed = TRUE
        )
        tree_df <- graph2tree(combined_g)
        plotdata <- tree2df(tree_df)
        ## add q-value as color
        plotdata$fill <- df[match(plotdata$id, df[, termID]), fill]
        if (fillNAby0) plotdata[is.na(plotdata[, "fill"]), "fill"] <- 0
        ## add lables
        plotdata$label <- Term(plotdata$id)
        args <- list(...)
        if (!"legendTitle" %in% names(args)) {
            args$legendTitle <- fill
        }
        if ("fontsize" %in% names(args)) {
            if (is.character(args$fontsize)) {
                if (args$fontsize %in% colnames(df)) {
                    if (args$fontsize %in% c(
                        colnames(plotdata),
                        "polar_angle", "rotate90"
                    )) {
                        stop(
                            args$fontsize, " is a column of plotdata.",
                            "please try to rename the column in input."
                        )
                    } else {
                        plotdata[, args$fontsize] <-
                            df[match(plotdata$id, df[, termID]), args$fontsize]
                    }
                } else {
                    stop("'fontsize' is not a column in df")
                }
            }
        }
        if (!is.null(sub_rect) && sub_rect %in% colnames(df)) {
            plotdata$sub_rect <- df[
                match(plotdata$id, df[, termID]),
                sub_rect
            ]
            args$legendTitle <- c(args$legendTitle, sub_rect)
            if (is.character(plotdata$sub_rect)) {
                plotdata$sub_rect <- parse_ratio(plotdata$sub_rect)
            } else {
                if (is.numeric(df[, sub_rect])) {
                    if (all(df[, sub_rect] == round(df[, sub_rect])) &&
                        any(df[, sub_rect] > 1)) {
                        ## it is a count number
                        allAlias <- getGOalias(plotdata$id, org = org)
                        plotdata$sub_rect <- plotdata$sub_rect /
                            lengths(lapply(allAlias, unique))
                        plotdata$sub_rect[is.na(plotdata$sub_rect)] <- 0
                        if (any(plotdata$sub_rect > 1)) {
                            stop(
                                "Can not get the proper sub_rect proportion.",
                                "Please provide numbers within [0, 1]."
                            )
                        }
                    }
                } else {
                    stop(
                        "Can not get the proper sub_rect proportion.",
                        "Please provide numbers within [0, 1]."
                    )
                }
            }
        }
        args$plotdata <- plotdata
        return(do.call(ggSunburst, args))
    }
}
