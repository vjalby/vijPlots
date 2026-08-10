testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

test_that("qqplot: basic normal Q-Q", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL
    )$plot
    expect_plot_snapshot("qqplot-basic", testPlot)
})

test_that("qqplot: grouped", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = "Species"
    )$plot
    expect_plot_snapshot("qqplot-grouped", testPlot)
})

test_that("qqplot: with confidence band", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        band = TRUE
    )$plot
    expect_plot_snapshot("qqplot-band", testPlot)
})

test_that("qqplot: detrended", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        detrend = TRUE
    )$plot
    expect_plot_snapshot("qqplot-detrended", testPlot)
})

test_that("qqplot: P-P plot", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        type = "PP"
    )$plot
    expect_plot_snapshot("qqplot-pp", testPlot)
})

test_that("qqplot: standardized values", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        standardize = TRUE
    )$plot
    expect_plot_snapshot("qqplot-standardized", testPlot)
})

test_that("qqplot: natural log transform", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        transLog = TRUE
    )$plot
    expect_plot_snapshot("qqplot-logTransform", testPlot)
})

test_that("qqplot: exponential distribution", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        distrib = "exp"
    )$plot
    expect_plot_snapshot("qqplot-exponential", testPlot)
})

test_that("qqplot: Q-Q line reference (instead of identity)", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        refType = "qqline"
    )$plot
    expect_plot_snapshot("qqplot-qqline", testPlot)
})

test_that("qqplot: user-specified distribution parameters", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        paramMethod = "paraValue",
        param1 = 3.75,
        param2 = 1.75
    )$plot
    expect_plot_snapshot("qqplot-userParams", testPlot)
})

test_that("qqplot: titles, axis and legend text options", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = "Species",
        titleText = "Q-Q Plot of Petal Length",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "iris dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom",
        xAxisText = "Theoretical quantiles",
        yAxisText = "Sample quantiles",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("qqplot-titles-axis-legend", testPlot)
})
