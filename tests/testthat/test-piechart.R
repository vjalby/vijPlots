testData <- datasets::penguins

test_that("piechart: basic pie chart", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text"
    )$plot
    expect_plot_snapshot("piechart-basic", testPlot)
})

test_that("piechart: donut chart", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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

weightedData <- data.frame(
    cat = factor(c("A", "A", "B", "B", "B", "C"))
)
weightedData$w <- c(10, 5, 20, 3, 2, 7)
# expected weighted totals per cat: A=15, B=25, C=7

test_that("piechart: weighted by counts variable", {
    r <- vijPlots::piechart(
        data = weightedData,
        aVar = "cat",
        facet = NULL,
        counts = "w",
        labType = "text"
    )
    expect_equal(r$plot$state$.COUNTS, weightedData$w)

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 25, 7)))

    expect_plot_snapshot("piechart-weighted-counts", r$plot)
})

test_that("piechart: weighted via jamovi's built-in weights (jmv-weights attribute)", {
    # regression test: without weightsSupport: 'full' in piechart.a.yaml, jmvcore's
    # Analysis$run() auto-expands rows per jmv-weights *before* .run() sees self$data,
    # leaving a stale jmv-weights attribute that crashes .COUNTS construction
    dataWithAttr <- weightedData["cat"]
    attr(dataWithAttr, "jmv-weights") <- weightedData$w
    attr(dataWithAttr, "jmv-weights-name") <- "w"

    r <- vijPlots::piechart(
        data = dataWithAttr,
        aVar = "cat",
        facet = NULL,
        counts = NULL,
        labType = "text"
    )
    expect_equal(r$plot$state$.COUNTS, weightedData$w)

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 25, 7)))
})

test_that("piechart: negative counts are rejected", {
    negData <- weightedData
    negData$w[1] <- -3
    r <- vijPlots::piechart(
        data = negData,
        aVar = "cat",
        facet = NULL,
        counts = "w",
        labType = "text"
    )
    expect_true(".warning" %in% names(r))
    expect_equal(r[[".warning"]]$content, "Counts may not be negative.")
    expect_null(r$plot$state)
})
