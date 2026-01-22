test_that("getGOalias", {
    a <- AnnotationDbi::mget("GO:0099536", org.Dr.egGO2ALLEGS)
    b <- getGOalias("GO:0099536", org.Dr.eg.db)
    expect_identical(a, b)
})
