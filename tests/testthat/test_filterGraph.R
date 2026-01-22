test_that("GO DAG pruning and simplification", {
    edges <- data.frame(
        ancestor = c("GO:0007267", "GO:0099536", "GO:0099537", "GO:0098916"),
        offspring = c("GO:0099536", "GO:0099537", "GO:0098916", "GO:0098919")
    )
    g <- graph_from_data_frame(edges)
    ## cutoff
    g1 <- lapply(seq(2, 4), filterGraph, g = g, leaveTerms = names(V(g)))
    expect_equal(vapply(g1, nrow, numeric(1L)), c(3, 4, 5))

    ## leaveTerms
    g1 <- filterGraph(g, leaveTerms = edges$ancestor, cutoff = 5)
    expect_true(all(edges$ancestor %in% as.character(unlist(g1))))
    expect_false("GO:0098919" %in% as.character(unlist(g1)))
})
