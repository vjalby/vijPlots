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
