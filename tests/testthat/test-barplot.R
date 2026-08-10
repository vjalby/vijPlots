testData <- datasets::penguins

test_that("barplot: single variable", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("barplot-singleVar", testPlot)
})

test_that("barplot: grouped (dodge2)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL
    )$plot
    expect_plot_snapshot("barplot-grouped-dodge2", testPlot)
})

test_that("barplot: grouped (tight dodge)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        barType = "dodge"
    )$plot
    expect_plot_snapshot("barplot-grouped-dodge", testPlot)
})

test_that("barplot: stacked", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        barType = "stack"
    )$plot
    expect_plot_snapshot("barplot-stacked", testPlot)
})

test_that("barplot: stacked, reversed order", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        barType = "stack",
        reverseStack = TRUE
    )$plot
    expect_plot_snapshot("barplot-stacked-reversed", testPlot)
})

test_that("barplot: horizontal", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("barplot-horizontal", testPlot)
})

test_that("barplot: percent within group", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        yaxis = "percent",
        percentWithin = "group"
    )$plot
    expect_plot_snapshot("barplot-percent-withinGroup", testPlot)
})

test_that("barplot: percent within category", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        yaxis = "percent",
        percentWithin = "category"
    )$plot
    expect_plot_snapshot("barplot-percent-withinCategory", testPlot)
})

test_that("barplot: sorted decreasing", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "island",
        columns = NULL,
        facet = NULL,
        order = "decreasing"
    )$plot
    expect_plot_snapshot("barplot-sorted-decreasing", testPlot)
})

test_that("barplot: faceted", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = "island"
    )$plot
    expect_plot_snapshot("barplot-faceted", testPlot)
})

test_that("barplot: single color", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        singleColor = TRUE,
        colorNo = 3
    )$plot
    expect_plot_snapshot("barplot-singleColor", testPlot)
})

test_that("barplot: missing values kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("barplot-ignoreNA-false", testPlot)
})

test_that("barplot: titles, axis and legend text options", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        titleText = "Penguins by Species and Sex",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "palmerpenguins dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::penguins",
        captionAlign = "1",
        legendText = "Sex",
        legendPosition = "bottom",
        xAxisText = "Species",
        yAxisText = "Number of penguins",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("barplot-titles-axis-legend", testPlot)
})
