lumpData <- function(counts) factor(rep(names(counts), counts), levels = names(counts))

test_that("vijLumpLevels: categories below the proportion are lumped", {
    f <- lumpData(c(a = 50, b = 30, c = 6, d = 5, e = 4, f = 5))
    g <- vijLumpLevels(f, rep(1, length(f)), method = "prop", prop = 0.1, otherLevel = "Other")
    expect_equal(levels(g), c("a", "b", "Other"))
    expect_equal(as.vector(table(g)), c(50, 30, 20))
})

test_that("vijLumpLevels: a single small category is not lumped", {
    f <- lumpData(c(a = 60, b = 35, c = 5))
    expect_null(vijLumpLevels(f, rep(1, length(f)), method = "prop", prop = 0.1, otherLevel = "Other"))
})

test_that("vijLumpLevels: none and auto methods", {
    f <- lumpData(c(a = 50, b = 30, c = 6, d = 5, e = 4))
    w <- rep(1, length(f))
    expect_null(vijLumpLevels(f, w, method = "none", prop = 0.1, otherLevel = "Other"))
    # "auto": the least frequent categories, while "Other" stays the smallest one
    expect_equal(levels(vijLumpLevels(f, w, method = "auto", prop = 0.1, otherLevel = "Other")), c("a", "Other"))
})

test_that("vijLumpLevels: weights, decimal weights and label", {
    f <- factor(c("a", "b", "c", "d"))
    g <- vijLumpLevels(f, c(60.5, 30, 4.75, 4.75), method = "prop", prop = 0.1, otherLevel = "Autres")
    expect_equal(levels(g), c("a", "b", "Autres"))
    expect_equal(as.character(g), c("a", "b", "Autres", "Autres"))
})

test_that("vijLumpLevels: NA is never lumped and counts in the denominator", {
    # c = 5 is 2.5% of 200 (NA counted), 5% of 100 (NA not counted)
    f <- factor(c("a", "b", "c", "d", NA))
    g <- vijLumpLevels(f, c(50, 40, 5, 5, 100), method = "prop", prop = 0.04, otherLevel = "Other")
    expect_equal(levels(g), c("a", "b", "Other"))
    expect_true(is.na(g[5]))
})

test_that("vijLumpLevels: empty levels are lumped but not counted", {
    # one small category and an empty level: not lumped
    f <- factor(c(rep("a", 60), rep("b", 35), rep("c", 5)), levels = c("a", "b", "c", "e"))
    expect_null(vijLumpLevels(f, rep(1, length(f)), method = "prop", prop = 0.1, otherLevel = "Other"))
    # empty levels only: not lumped (no empty "Other")
    f <- factor(c(rep("a", 60), rep("b", 40)), levels = c("a", "b", "e1", "e2"))
    expect_null(vijLumpLevels(f, rep(1, length(f)), method = "prop", prop = 0.1, otherLevel = "Other"))
    # two small categories: the empty level goes into "Other" too
    f <- factor(c(rep("a", 60), rep("b", 30), rep("c", 5), rep("d", 5)), levels = c("a", "b", "c", "d", "e"))
    g <- vijLumpLevels(f, rep(1, length(f)), method = "prop", prop = 0.1, otherLevel = "Other")
    expect_equal(levels(g), c("a", "b", "Other"))
})

test_that("vijLumpLevels: an existing category with the same label receives the lumped ones", {
    f <- lumpData(c(a = 50, b = 25, Other = 20, c = 5))
    g <- vijLumpLevels(f, rep(1, length(f)), method = "prop", prop = 0.1, otherLevel = "Other")
    expect_equal(levels(g), c("a", "b", "Other"))
    expect_equal(as.vector(table(g)), c(50, 25, 25))
})

test_that("vijToNumeric: infinite values become missing, attributes kept", {
    x <- c(1.5, -Inf, NaN, NA, Inf, 2)
    attr(x, "measureType") <- "continuous"
    out <- vijToNumeric(x)
    expect_equal(as.vector(out), c(1.5, NA, NaN, NA, NA, 2))
    expect_equal(attr(out, "measureType"), "continuous")
    expect_identical(vijToNumeric(c(1L, NA, 3L)), c(1L, NA, 3L))
})

test_that("vijToNumeric: factors as jmvcore::toNumeric()", {
    f <- factor(c("x", "y", NA))
    attr(f, "values") <- c(10L, 20L)
    expect_equal(vijToNumeric(f), c(10L, 20L, NA))
    g <- factor(c("x", "y"))
    expect_identical(vijToNumeric(g), g)
})

dropData <- function() {
    df <- data.frame(g = c("a", NA, "b", "c", NA), h = c(1, 2, NA, 4, 5))
    df$f <- factor(c("x", "y", NA, "x", "y"))
    attr(df$f, "values") <- c(10L, 20L)
    row.names(df) <- c(2L, 5L, 7L, 9L, 12L)
    df
}

test_that("vijDropMissing: only the named columns are filtered", {
    df <- dropData()
    expect_identical(vijDropMissing(df, NULL), df)
    expect_equal(vijDropMissing(df, "g")$g, c("a", "b", "c"))
    expect_equal(vijDropMissing(df, "g")$h, c(1, NA, 4))
    expect_equal(vijDropMissing(df, c("g", "f"))$g, c("a", "c"))
    expect_s3_class(vijDropMissing(df, "g"), "data.frame", exact = TRUE)
})

test_that("vijDropMissing: row names and custom column attributes are kept", {
    df <- dropData()
    out <- vijDropMissing(df, "g")
    expect_identical(attr(out, "row.names"), c(2L, 7L, 9L))
    expect_equal(attr(out$f, "values"), c(10L, 20L))
    expect_equal(jmvcore::toNumeric(out$f), c(10L, NA, 10L))
})

test_that("vijDropMissing: same as jmvcore::naOmit() on all columns", {
    df <- dropData()
    expected <- jmvcore::naOmit(df)
    attr(expected, "na.action") <- NULL
    attr(expected, "nRowsOmitted") <- NULL
    expect_identical(vijDropMissing(df, names(df)), expected)
})
