test_that("getGraph works not correct", {
    goids <- c(
        "GO:0099536", "GO:0099537", "GO:0007268",
        "GO:0098916", "GO:0050804"
    )
    g <- getGraph(data.frame(ID = goids), org = org.Dr.eg.db, onto = "BP")
    edges <- as_edgelist(g)
    edges <- paste(edges[, 1], edges[, 2], sep = "->")
    exp <- c(
        "GO:0007267->GO:0099536", "GO:0099536->GO:0099537",
        "GO:0098916->GO:0007268", "GO:0099537->GO:0098916",
        "GO:0099177->GO:0050804"
    )
    expect_true(all(exp %in% edges))
    expect_true(all(edges %in% exp))
})
