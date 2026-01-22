test_that("getAncestors works not correct", {
    ids <- c("GO:0008152")
    df <- getAncestors(ids, onto = "BP")
    expect_true("all" %in% df$ancestor)
    expect_true("GO:0008150" %in% df$ancestor)
})
