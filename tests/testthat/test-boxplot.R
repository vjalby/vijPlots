testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

test_that("boxplot: one variable", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = NULL,
        label = NULL,
        facet = NULL
        )$plot
    expect_plot_snapshot("boxplot-oneVar", testPlot)
})

test_that("boxplot: one variable with groups", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = "group2",
        facet = NULL
    )$plot
    expect_plot_snapshot("boxplot-oneVar-Groups", testPlot)
})

test_that("boxplot: several variables, no group", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width", "Petal.Length"),
        group = NULL,
        label = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("boxplot-severalVars", testPlot)
})

test_that("boxplot: several variables with a group (dodged, multi-group legend)", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width", "Petal.Length"),
        group = "Species",
        label = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("boxplot-severalVars-Groups", testPlot)
})

test_that("boxplot: faceted", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = NULL,
        facet = "group2"
    )$plot
    expect_plot_snapshot("boxplot-faceted", testPlot)
})

test_that("boxplot: horizontal", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = NULL,
        facet = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("boxplot-horizontal", testPlot)
})

test_that("boxplot: staples and notches", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = NULL,
        facet = NULL,
        staple = TRUE,
        notches = TRUE
    )$plot
    expect_plot_snapshot("boxplot-staples-notches", testPlot)
})

test_that("boxplot: mean marker", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = NULL,
        facet = NULL,
        showMean = TRUE
    )$plot
    expect_plot_snapshot("boxplot-mean", testPlot)
})

test_that("boxplot: single color", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = NULL,
        label = NULL,
        facet = NULL,
        singleColor = TRUE,
        colorNo = 2
    )$plot
    expect_plot_snapshot("boxplot-singleColor", testPlot)
})

test_that("boxplot: sorted (decreasing median) with several variables", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width", "Petal.Length"),
        group = NULL,
        label = NULL,
        facet = NULL,
        order = "decreasing"
    )$plot
    expect_plot_snapshot("boxplot-sorted-decreasing", testPlot)
})

test_that("boxplot: outliers hidden despite a label variable", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = "group2",
        facet = NULL,
        showOutliers = FALSE
    )$plot
    expect_plot_snapshot("boxplot-outliers-hidden", testPlot)
})

test_that("boxplot: missing group level kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "group2",
        label = NULL,
        facet = NULL,
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("boxplot-ignoreNA-false", testPlot)
})

test_that("boxplot: titles, axis and legend text options", {
    testPlot <- vijPlots::boxplot(
        data = testData,
        vars = c("Petal.Width"),
        group = "Species",
        label = NULL,
        facet = NULL,
        titleText = "Petal Width by Species",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "iris dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom",
        xAxisText = "Species group",
        yAxisText = "Petal width (cm)",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("boxplot-titles-axis-legend", testPlot)
})
