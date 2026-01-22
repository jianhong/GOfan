test_that("GeneRatio parsing works not correctly", {
    test_ratios <- c("5/100", "10/200", "15/150")
    parsed <- GOfan:::parse_ratio(test_ratios)

    expect_equal(length(parsed), 3)
    expect_true(parsed[1] == 0.05)
    expect_true(parsed[2] == 0.05)
    expect_true(parsed[3] == 0.1)

    # Test invalid ratios
    expect_error(GOfan:::parse_ratio("invalid"))
    expect_error(GOfan:::parse_ratio("10"))
})

test_that("color2gray works not correctly", {
    a <- GOfan:::color2gray(1:7, 0)
    expect_true(a[1] == "#000000")
    expect_true(a[7] == "#FFFFFF")
    a <- do.call(rbind, strsplit(sub("#", "", a), ""))
    expect_equal(a[, 1], a[, 3])
    expect_equal(a[, 1], a[, 5])
    expect_equal(a[, 2], a[, 4])
    expect_equal(a[, 2], a[, 6])
    b <- GOfan:::color2gray(1:7, 0.5)
    expect_true(b[1] == "#808080")
})

test_that("rescale works not correctly", {
    # Map 5 from [0, 10] to [0, 100]
    expect_equal(GOfan:::rescale(5, from = c(0, 10), to = c(0, 100)), 50)

    # Map 0 from [0, 10] to [0, 100]
    expect_equal(GOfan:::rescale(0, from = c(0, 10), to = c(0, 100)), 0)

    # Map 10 from [0, 10] to [0, 100]
    expect_equal(GOfan:::rescale(10, from = c(0, 10), to = c(0, 100)), 100)

    # Map midpoint
    expect_equal(GOfan:::rescale(0.5, from = c(0, 1), to = c(0, 10)), 5)

    # Linear map handles vector inputs
    x <- c(0, 2.5, 5, 7.5, 10)
    result <- GOfan:::rescale(x, from = c(0, 10), to = c(0, 100))
    expected <- c(0, 25, 50, 75, 100)

    expect_equal(result, expected)
    expect_equal(length(result), length(x))

    # Linear map preserves proportions
    x <- c(0, 0.25, 0.5, 0.75, 1)
    result <- GOfan:::rescale(x, from = c(0, 1), to = c(0, 100))

    # Check that spacing is preserved
    diffs <- diff(result)
    expect_equal(diffs, rep(25, 4))

    # Linear map is identity when ranges are equal
    x <- c(1, 2, 3, 4, 5)
    result <- GOfan:::rescale(x, from = c(1, 5), to = c(1, 5))

    expect_equal(result, x)

    # Map from [-10, 10] to [0, 100]
    expect_equal(GOfan:::rescale(-10, from = c(-10, 10), to = c(0, 100)), 0)
    expect_equal(GOfan:::rescale(0, from = c(-10, 10), to = c(0, 100)), 50)
    expect_equal(GOfan:::rescale(10, from = c(-10, 10), to = c(0, 100)), 100)

    # Map from [0, 10] to [-100, 100]
    expect_equal(GOfan:::rescale(0, from = c(0, 10), to = c(-100, 100)), -100)
    expect_equal(GOfan:::rescale(5, from = c(0, 10), to = c(-100, 100)), 0)
    expect_equal(GOfan:::rescale(10, from = c(0, 10), to = c(-100, 100)), 100)

    # Reversed output range (inverts values)
    expect_equal(GOfan:::rescale(0, from = c(0, 10), to = c(100, 0)), 100)
    expect_equal(GOfan:::rescale(5, from = c(0, 10), to = c(100, 0)), 50)
    expect_equal(GOfan:::rescale(10, from = c(0, 10), to = c(100, 0)), 0)

    # Reversed input range
    expect_equal(GOfan:::rescale(0, from = c(10, 0), to = c(0, 100)), 100)
    expect_equal(GOfan:::rescale(5, from = c(10, 0), to = c(0, 100)), 50)
    expect_equal(GOfan:::rescale(10, from = c(10, 0), to = c(0, 100)), 0)

    # Map from [0, 0.5] to [0, 100]
    expect_equal(GOfan:::rescale(0.25, from = c(0, 0.5), to = c(0, 100)), 50)

    # Map from [0, 100] to [0, 0.5]
    expect_equal(GOfan:::rescale(50, from = c(0, 100), to = c(0, 0.5)), 0.25)

    # Map from [5, 15] to [100, 200]
    expect_equal(GOfan:::rescale(5, from = c(5, 15), to = c(100, 200)), 100)
    expect_equal(GOfan:::rescale(10, from = c(5, 15), to = c(100, 200)), 150)
    expect_equal(GOfan:::rescale(15, from = c(5, 15), to = c(100, 200)), 200)

    # Linear map handles very small ranges
    expect_equal(GOfan:::rescale(0.00005, from = c(0, 0.0001), to = c(0, 1)),
        0.5,
        tolerance = 1e-10
    )

    # Linear map handles very large ranges, Map from [0, 1e10] to [0, 1]
    expect_equal(GOfan:::rescale(5e9, from = c(0, 1e10), to = c(0, 1)), 0.5)

    # Linear map handles single value
    result <- GOfan:::rescale(5, from = c(0, 10), to = c(0, 100))
    expect_equal(result, 50)
    expect_length(result, 1)

    # Linear map handles empty vector
    result <- GOfan:::rescale(numeric(0), from = c(0, 10), to = c(0, 100))
    expect_equal(result, numeric(0))
    expect_length(result, 0)

    # When from[1] == from[2], should return midpoint of output range
    expect_warning(
        result <- GOfan:::rescale(c(5, 5, 5), from = c(5, 5), to = c(0, 100)),
        "zero width"
    )
    expect_equal(result, c(50, 50, 50))

    # Linear map handles values outside input range
    # Values outside the input range should still map linearly
    expect_equal(GOfan:::rescale(-5, from = c(0, 10), to = c(0, 100)), -50)
    expect_equal(GOfan:::rescale(15, from = c(0, 10), to = c(0, 100)), 150)

    # Exact boundary values
    expect_equal(GOfan:::rescale(0, from = c(0, 10), to = c(0, 100)), 0)
    expect_equal(GOfan:::rescale(10, from = c(0, 10), to = c(0, 100)), 100)

    # Values very close to boundaries
    expect_equal(GOfan:::rescale(1e-10, from = c(0, 10), to = c(0, 100)),
        1e-9,
        tolerance = 1e-15
    )
    expect_equal(GOfan:::rescale(10 - 1e-10, from = c(0, 10), to = c(0, 100)),
        100 - 1e-9,
        tolerance = 1e-15
    )

    # Missing arguments
    expect_error(GOfan:::rescale(), "required")
    expect_warning(GOfan:::rescale(5), "range has zero width")

    # Non-numeric inputs
    expect_error(
        GOfan:::rescale("5", from = c(0, 10), to = c(0, 100)),
        "numeric"
    )
    expect_error(
        GOfan:::rescale(5, from = c("0", "10"), to = c(0, 100)),
        "numeric"
    )
    expect_error(
        GOfan:::rescale(5, from = c(0, 10), to = c("0", "100")),
        "numeric"
    )

    # Wrong length ranges
    expect_error(
        GOfan:::rescale(5, from = c(0), to = c(0, 100)),
        "length 2"
    )
    expect_error(
        GOfan:::rescale(5, from = c(0, 5, 10), to = c(0, 100)),
        "length 2"
    )
    expect_error(GOfan:::rescale(5, from = c(0, 10), to = c(0)), "length 2")
    expect_error(
        GOfan:::rescale(5, from = c(0, 10), to = c(0, 50, 100)),
        "length 2"
    )

    # NA in x should produce NA in output
    result <- GOfan:::rescale(c(0, NA, 10), from = c(0, 10), to = c(0, 100))
    expect_equal(result[1], 0)
    expect_true(is.na(result[2]))
    expect_equal(result[3], 100)

    # NA in ranges should error
    expect_error(GOfan:::rescale(5, from = c(NA, 10), to = c(0, 100)), "NA")
    expect_error(GOfan:::rescale(5, from = c(0, 10), to = c(NA, 100)), "NA")

    # Inf in x should map to Inf
    result <- GOfan:::rescale(c(0, Inf, -Inf), from = c(0, 10), to = c(0, 100))
    expect_equal(result[1], 0)
    expect_equal(result[2], Inf)
    expect_equal(result[3], -Inf)

    # Inf in ranges should error
    expect_error(GOfan:::rescale(5, from = c(0, Inf), to = c(0, 100)), "Inf")
    expect_error(GOfan:::rescale(5, from = c(0, 10), to = c(0, Inf)), "Inf")
})

test_that("jaccard works not correctly", {
    # Identical sets
    expect_equal(GOfan:::jaccard(c(1, 2, 3), c(1, 2, 3)), 1)

    # Completely different sets
    expect_equal(GOfan:::jaccard(c(1, 2, 3), c(4, 5, 6)), 0)

    # Partial overlap
    expect_equal(GOfan:::jaccard(c(1, 2, 3), c(2, 3, 4)), 0.5)

    # One element in common
    expect_equal(GOfan:::jaccard(c(1, 2), c(2, 3, 4)), 0.25)

    set1 <- c(1, 2, 3, 4)
    set2 <- c(3, 4, 5, 6)

    # Jaccard index is symmetric
    expect_equal(GOfan:::jaccard(set1, set2), GOfan:::jaccard(set2, set1))

    # Duplicates should be treated as single elements
    set1 <- c(1, 1, 2, 2, 3, 3)
    set2 <- c(2, 3, 3, 4, 4, 4)

    # Should be same as c(1,2,3) vs c(2,3,4)
    expect_equal(GOfan:::jaccard(set1, set2), 0.5)

    # Both empty
    expect_equal(GOfan:::jaccard(c(), c()), 0)

    # One empty
    expect_equal(GOfan:::jaccard(c(), c(1, 2, 3)), 0)
    expect_equal(GOfan:::jaccard(c(1, 2, 3), c()), 0)

    # Same single element
    expect_equal(GOfan:::jaccard(c(1), c(1)), 1)

    # Different single elements
    expect_equal(GOfan:::jaccard(c(1), c(2)), 0)

    # One set has one element, other has multiple
    expect_equal(GOfan:::jaccard(c(1), c(1, 2, 3)), 1 / 3)

    # NULL
    expect_equal(GOfan:::jaccard(NULL, c(1, 2, 3)), 0)
    expect_equal(GOfan:::jaccard(c(1, 2, 3), NULL), 0)
    expect_equal(GOfan:::jaccard(NULL, NULL), 0)

    # characters
    set1 <- c("apple", "banana", "cherry")
    set2 <- c("banana", "cherry", "date")

    expect_equal(GOfan:::jaccard(set1, set2), 0.5)

    # factor
    set1 <- factor(c("A", "B", "C"))
    set2 <- factor(c("B", "C", "D"))

    expect_equal(GOfan:::jaccard(set1, set2), 0.5)
})
