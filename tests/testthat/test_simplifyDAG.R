test_that("simplifyDAG works not correct", {
    edges <- data.frame(
        ancestor=c("GO:0007154", "GO:0007267", "GO:0099536", "GO:0099537"),
        offspring=c("GO:0099536", "GO:0099536", "GO:0099537", "GO:0098916"))
    g <- graph_from_data_frame(edges)
    g1 <- simplifyDAG(g, org.Dr.eg.db)
    expect_true(length(V(g1))==4)
})
