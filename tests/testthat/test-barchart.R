testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))


testthat::test_that("barchart: no group", {
    # generate plot
    testPlot <- vijPlots::barchart(data = testData, xVar="Species" , yVar="Petal.Width", group = NULL, facet = NULL)$plot
    # check that plot matches
    expect_plot_snapshot("barchart-nogroup", testPlot)
})

testthat::test_that("barchart: with errorbar", {
    testPlot <- vijPlots::barchart(
        data = testData,
        xVar="Species" ,
        yVar="Petal.Width",
        group = NULL,
        facet = NULL,
        errorBars = "ci",
        order = "decreasing",
        showLabels = FALSE
        )$plot
    expect_plot_snapshot("barchart-errorbar", testPlot)
})

testthat::test_that("barchart: with grouping variable", {
    testPlot <- vijPlots::barchart(
        data = testData,
        xVar="Species" ,
        yVar="Petal.Width",
        group = group2,
        facet = NULL,
        )$plot
    expect_plot_snapshot("barchart-group", testPlot)
})

testthat::test_that("barchart: horizontal with grouping variable", {
    testPlot <- vijPlots::barchart(
        data = testData,
        xVar="Species" ,
        yVar="Petal.Width",
        group = group2,
        facet = NULL,
        barType = "stack",
        horizontal = TRUE,
        ignoreNA = FALSE,
        colorPalette = "Set3",
        yaxis = "sum"
    )$plot
    expect_plot_snapshot("barchart-group-horizontal", testPlot)
})

testthat::test_that("barchart: with axis options", {
    testPlot <- vijPlots::barchart(
        data = testData,
        xVar="Species" ,
        yVar="Petal.Width",
        group = NULL,
        facet = NULL,
        barType = "stack", # ignored
        colorPalette = "tidy::friendly",
        singleColor = FALSE,
        yaxis = "median",
        borderColor = "black",
        textColor = "red",
        titleText = "A nice plot",
        titleFontSize = "20",
        subtitleText = "made with vijPlots",
        subtitleFontFace = "italic",
        xAxisText = "Iris species",
        xAxisFontSize = 12,
        xAxisPosition = "1",
        yAxisLabelRotation = 30,
        yAxisRangeType = "manual",
        yAxisRangeMin = -2,
        yAxisRangeMax = 5,
        yTicks = 30
    )$plot
    expect_plot_snapshot("barchart-with-options", testPlot)
})
