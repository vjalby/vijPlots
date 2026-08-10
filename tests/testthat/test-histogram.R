# Tests for the histogram module (R/histogram.b.R, R/histogram.h.R)
#
# Two levels are exercised:
#  - the exported `histogram()` function, called the way an end user (or
#    jamovi) would call it, checking the data that ends up in
#    `results$plot$state`;
#  - the same `results$plot` object, printed through the real jmvcore
#    render pipeline (`Image$print()` -> `.render()`, resolving the actual
#    global theme/palette) and snapshot-tested with vdiffr, same method as
#    test-barchart.R.

# ---- Test data --------------------------------------------------------------

testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

# ---- histogram(): basic runs -------------------------------------------------
#
# NOTE: the generated wrapper's `for (v in group) ...` / `for (v in facet)
# ...` lines are not guarded by `missing()`, so `group`/`facet` must always
# be passed explicitly (even as NULL) when calling the wrapper with `data=`
# supplied directly, otherwise it errors with "argument is missing, with
# no default".

test_that("histogram() runs with a single continuous variable", {
    results <- vijPlots::histogram(data = testData, aVar = "Petal.Width", group = NULL, facet = NULL)
    expect_true(is.data.frame(results$plot$state))
    expect_true("Petal.Width" %in% names(results$plot$state))
})

test_that("histogram() runs with a grouping variable", {
    results <- vijPlots::histogram(data = testData, aVar = "Petal.Width", group = "Species", facet = NULL)
    expect_true("Species" %in% names(results$plot$state))
    expect_true(is.factor(results$plot$state$Species))
})

test_that("histogram() runs with a facet variable", {
    results <- vijPlots::histogram(data = testData, aVar = "Petal.Width", group = NULL, facet = "group2")
    expect_true("group2" %in% names(results$plot$state))
})

# NOTE: no "no dependent variable" test here. jmvcore::select(df, character(0))
# itself errors on the installed jmvcore (2.7.7) - reproducible with a plain
# `jmvcore::select(iris, character(0))` outside of vijPlots entirely - so
# Analysis$init() crashes before histogramClass's own `.run()` guard
# (`if (is.null(self$options$aVar) ...) return(FALSE)`) is ever reached. This
# is a jmvcore-level issue, not specific to histogram; see conversation notes.

test_that("histogram() produces no state when the data set is empty", {
    expect_no_error(
        results <- vijPlots::histogram(data = testData[0, ], aVar = "Petal.Width", group = NULL, facet = NULL))
    expect_null(results$plot$state)
})

# ---- visual snapshots ---------------------------------------------------------

test_that("histogram: single variable, no grouping", {
    testPlot <- vijPlots::histogram(data = testData, aVar = "Petal.Width", group = NULL, facet = NULL)$plot
    expect_plot_snapshot("histogram-single-var", testPlot)
})

test_that("histogram: grouped", {
    testPlot <- vijPlots::histogram(data = testData, aVar = "Petal.Width", group = "Species", facet = NULL)$plot
    expect_plot_snapshot("histogram-grouped", testPlot)
})

test_that("histogram: faceted", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = "Species", facet = "group2")$plot
    expect_plot_snapshot("histogram-faceted", testPlot)
})

test_that("histogram: normal curve overlay", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = NULL, facet = NULL, normalCurve = TRUE)$plot
    expect_plot_snapshot("histogram-normal-curve", testPlot)
})

test_that("histogram: density overlay", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = NULL, facet = NULL, density = TRUE)$plot
    expect_plot_snapshot("histogram-density", testPlot)
})

test_that("histogram: density-type histogram", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = NULL, facet = NULL, histtype = "density")$plot
    expect_plot_snapshot("histogram-density-type", testPlot)
})

test_that("histogram: lines only (bins hidden)", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = NULL, facet = NULL,
        showBins = FALSE, showLines = TRUE)$plot
    expect_plot_snapshot("histogram-lines-only", testPlot)
})

test_that("histogram: bins and lines together", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = NULL, facet = NULL,
        showBins = TRUE, showLines = TRUE)$plot
    expect_plot_snapshot("histogram-bins-and-lines", testPlot)
})

test_that("histogram: grouped lines (stacked)", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = "Species", facet = NULL,
        showBins = FALSE, showLines = TRUE, groupingN = "stack")$plot
    expect_plot_snapshot("histogram-grouped-lines-stacked", testPlot)
})

test_that("histogram: grouped, normal curve only (no bins) still shows a legend", {
    testPlot <- vijPlots::histogram(
        data = testData, aVar = "Petal.Width", group = "Species", facet = NULL,
        showBins = FALSE, normalCurve = TRUE)$plot
    expect_plot_snapshot("histogram-grouped-normal-curve-only", testPlot)
})
