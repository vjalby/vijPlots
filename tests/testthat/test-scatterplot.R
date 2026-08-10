testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

test_that("scatterplot: basic", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("scatterplot-basic", testPlot)
})

test_that("scatterplot: grouped by color", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("scatterplot-grouped-color", testPlot)
})

test_that("scatterplot: grouped with shapes", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        groupShapes = TRUE
    )$plot
    expect_plot_snapshot("scatterplot-grouped-shapes", testPlot)
})

test_that("scatterplot: point size mapped to a continuous variable", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = "Sepal.Width",
        facet = NULL
    )$plot
    expect_plot_snapshot("scatterplot-pointSize", testPlot)
})

test_that("scatterplot: with point labels", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = "group2",
        ptSize = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("scatterplot-labels", testPlot)
})

test_that("scatterplot: faceted", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = "group2"
    )$plot
    expect_plot_snapshot("scatterplot-faceted", testPlot)
})

test_that("scatterplot: linear regression line with confidence interval", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        regLine = TRUE,
        lineMethod = "lm",
        lineSE = TRUE
    )$plot
    expect_plot_snapshot("scatterplot-regline-lm", testPlot)
})

test_that("scatterplot: loess regression line without confidence interval", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        regLine = TRUE,
        lineMethod = "loess",
        lineSE = FALSE
    )$plot
    expect_plot_snapshot("scatterplot-regline-loess", testPlot)
})

test_that("scatterplot: horizontal and vertical reference lines", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        hline = TRUE,
        yinter = 1.5,
        vline = TRUE,
        xinter = 4
    )$plot
    expect_plot_snapshot("scatterplot-refLines", testPlot)
})

test_that("scatterplot: single color", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        singleColor = "black"
    )$plot
    expect_plot_snapshot("scatterplot-singleColor", testPlot)
})

test_that("scatterplot: plot border", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        plotBorder = TRUE
    )$plot
    expect_plot_snapshot("scatterplot-border", testPlot)
})

test_that("scatterplot: missing group level kept (keepNA = TRUE)", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        keepNA = TRUE
    )$plot
    expect_plot_snapshot("scatterplot-keepNA", testPlot)
})

test_that("scatterplot: titles, axis and legend text options", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        titleText = "Petal Width vs Petal Length",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "iris dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom",
        xAxisText = "Petal length (cm)",
        yAxisText = "Petal width (cm)",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("scatterplot-titles-axis-legend", testPlot)
})
