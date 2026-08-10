testData <- datasets::penguins

test_that("piechart: basic pie chart", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text"
    )$plot
    expect_plot_snapshot("piechart-basic", testPlot)
})

test_that("piechart: donut chart", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        donut = TRUE
    )$plot
    expect_plot_snapshot("piechart-donut", testPlot)
})

test_that("piechart: count labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        labels = "count"
    )$plot
    expect_plot_snapshot("piechart-labels-count", testPlot)
})

test_that("piechart: percent labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        labels = "percent"
    )$plot
    expect_plot_snapshot("piechart-labels-percent", testPlot)
})

test_that("piechart: group labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        labels = "group"
    )$plot
    expect_plot_snapshot("piechart-labels-group", testPlot)
})

test_that("piechart: group + count labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        labels = "group+count"
    )$plot
    expect_plot_snapshot("piechart-labels-groupCount", testPlot)
})

test_that("piechart: group + percent labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        labels = "group+percent"
    )$plot
    expect_plot_snapshot("piechart-labels-groupPercent", testPlot)
})

test_that("piechart: label boxes with overlap prevention", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "island",
        facet = NULL,
        labType = "label",
        labels = "group+percent",
        overlap = TRUE
    )$plot
    expect_plot_snapshot("piechart-labelBoxes-overlap", testPlot)
})

test_that("piechart: faceted (with missing category dropped)", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "sex",
        facet = "species",
        labType = "text",
        labels = "percent"
    )$plot
    expect_plot_snapshot("piechart-faceted", testPlot)
})

test_that("piechart: no border", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        borderColor = "none"
    )$plot
    expect_plot_snapshot("piechart-noBorder", testPlot)
})

test_that("piechart: custom label offset and size", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        labels = "group",
        labOffset = 5,
        labSize = 18
    )$plot
    expect_plot_snapshot("piechart-labelOffsetSize", testPlot)
})

test_that("piechart: titles, axis and legend text options", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        labType = "text",
        titleText = "Penguins by Species",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "palmerpenguins dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::penguins",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom"
    )$plot
    expect_plot_snapshot("piechart-titles-axis-legend", testPlot)
})
