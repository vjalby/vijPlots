testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

test_that("lollipop: mean (default)", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL
    )$plot
    expect_plot_snapshot("lollipop-mean", testPlot)
})

test_that("lollipop: median", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        yaxis = "median"
    )$plot
    expect_plot_snapshot("lollipop-median", testPlot)
})

test_that("lollipop: min / max range", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        yaxis = "minmax"
    )$plot
    expect_plot_snapshot("lollipop-minmax", testPlot)
})

test_that("lollipop: sorted decreasing", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        order = "decreasing"
    )$plot
    expect_plot_snapshot("lollipop-sorted-decreasing", testPlot)
})

test_that("lollipop: sorted increasing", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        order = "increasing"
    )$plot
    expect_plot_snapshot("lollipop-sorted-increasing", testPlot)
})

test_that("lollipop: horizontal", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("lollipop-horizontal", testPlot)
})

test_that("lollipop: faceted", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = "group2"
    )$plot
    expect_plot_snapshot("lollipop-faceted", testPlot)
})

test_that("lollipop: dot and line customization", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        dotColor = "#377EB8",
        lineColor = "#984EA3",
        dotSize = 8,
        lineWidth = 2
    )$plot
    expect_plot_snapshot("lollipop-dotLineCustomization", testPlot)
})

test_that("lollipop: titles, axis and legend text options", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        titleText = "Mean Petal Width by Species",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "iris dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::iris",
        captionAlign = "1",
        xAxisText = "Species group",
        yAxisText = "Mean petal width (cm)",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("lollipop-titles-axis-legend", testPlot)
})
