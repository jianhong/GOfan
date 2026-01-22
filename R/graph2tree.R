#' @importFrom igraph V degree all_simple_paths subcomponent
#' @importFrom stats na.omit
graph2tree <- function(g) {
    # Find the root (no incoming edges)
    roots <- V(g)[degree(g, mode = "in") == 0]

    # Find all leaves (no outgoing edges)
    leaves <- V(g)[degree(g, mode = "out") == 0]

    # Helper to get all simple paths from root to each leaf
    all_paths <- list()
    for (r in roots) {
        for (l in leaves) {
            paths <- all_simple_paths(g, from = r, to = l, mode = "out")
            all_paths <- c(all_paths, paths)
        }
    }

    # Convert each path to a character vector
    path_list <- lapply(all_paths, function(p) names(V(g)[p]))

    # Pad to equal length (for data.frame)
    maxlen <- max(lengths(path_list))
    path_mat <- t(vapply(path_list, function(p) {
        c(p, rep(NA, maxlen - length(p)))
    },
    FUN.VALUE = character(maxlen)
    ))

    # Make it a data.frame with column names
    colnames(path_mat) <- paste0("level", seq_len(maxlen))
    df_tree <- as.data.frame(path_mat, stringsAsFactors = FALSE)

    # Compute offspring (descendant) counts for every node
    offspring_counts <- vapply(V(g)$name, function(v) {
        length(subcomponent(g, v, mode = "out")) - 1 # descendants minus itself
    }, FUN.VALUE = numeric(1L))
    names(offspring_counts) <- V(g)$name

    # Add offspring counts for each column
    for (i in seq_len(maxlen)) {
        colname <- paste0("level", i)
        count_col <- paste0(colname, "_offspring_count")
        df_tree[[count_col]] <- offspring_counts[df_tree[[colname]]]
    }

    # Sort each column by offspring count
    for (col in paste0("level", seq_len(maxlen))) {
        vals <- unique(na.omit(df_tree[[col]]))
        vals_sorted <- vals[order(-offspring_counts[vals])]
        df_tree[[col]] <- factor(df_tree[[col]], levels = vals_sorted)
    }

    # Sort rows hierarchically
    df_tree <- df_tree[
        do.call(order, df_tree[paste0("level", seq_len(maxlen))]),
        !grepl("_offspring_count", colnames(df_tree))
    ]

    df_tree
}

#' @importFrom utils head
tree2df <- function(df_tree) {
    df_tree <- df_tree[, -1, drop = FALSE]
    ## y is the level number
    y <- rep(seq.int(ncol(df_tree)), each = nrow(df_tree))
    ## y is the stack number
    r <- lapply(df_tree, function(.ele) rle(as.character(.ele))$lengths)
    x0 <- lapply(r, function(.r) cumsum(c(0, head(.r, -1))))
    x1 <- lapply(r, function(.r) cumsum(.r))
    x0 <- unlist(mapply(rep, x0, r, SIMPLIFY = FALSE))
    x1 <- unlist(mapply(rep, x1, r, SIMPLIFY = FALSE))
    go <- as.character(as.matrix(df_tree))
    x <- (x0 + x1) / 2
    out_df <- data.frame(
        id = go,
        x = x,
        y = y,
        xmin = x0,
        xmax = x1,
        ymin = y - 0.5,
        ymax = y + 0.5
    )
    out_df <- unique(out_df)
    out_df <- out_df[!is.na(out_df$id), ]
    rownames(out_df) <- NULL
    out_df
}
