test_that("goGraph works not correct", {
    edges <- data.frame(
        ancestor=c("GO:0007154", "GO:0007267", "GO:0099536", "GO:0099537"),
        offspring=c("GO:0099536", "GO:0099536", "GO:0099537", "GO:0098916"))
    g <- goGraph(edges)
    expect_s3_class(g, 'igraph')
    expect_false('all' %in% names(V(g)))
})
