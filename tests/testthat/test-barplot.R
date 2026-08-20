testData <- datasets::penguins

test_that("barplot: single variable", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL
    )$plot
    expect_plot_snapshot("barplot-singleVar", testPlot)
})

test_that("barplot: grouped (dodge2)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL
    )$plot
    expect_plot_snapshot("barplot-grouped-dodge2", testPlot)
})

test_that("barplot: grouped (tight dodge)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
        order = "decreasing"
    )$plot
    expect_plot_snapshot("barplot-sorted-decreasing", testPlot)
})

test_that("barplot: faceted", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = "island",
        counts = NULL
    )$plot
    expect_plot_snapshot("barplot-faceted", testPlot)
})

test_that("barplot: single color", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
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
        counts = NULL,
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
        counts = NULL,
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

weightedData <- data.frame(
    cat = factor(c("A", "A", "A", "B", "B", "C")),
    grp = factor(c("X", "Y", "X", "X", "Y", "X"))
)
weightedData$w <- c(10, 20, 5, 30, 15, 3)
# expected weighted totals per cat+grp: A/X=15, A/Y=20, B/X=30, B/Y=15, C/X=3

test_that("barplot: weighted by counts variable", {
    r <- vijPlots::barplot(
        data = weightedData,
        rows = "cat",
        columns = "grp",
        facet = NULL,
        counts = "w"
    )
    expect_equal(r$plot$state$.COUNTS, weightedData$w)

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 20, 30, 15, 3)))

    expect_plot_snapshot("barplot-weighted-counts", r$plot)
})

test_that("barplot: weighted via jamovi's built-in weights (jmv-weights attribute)", {
    # regression test: without weightsSupport: 'full' in barplot.a.yaml, jmvcore's
    # Analysis$run() auto-expands rows per jmv-weights *before* .run() sees self$data,
    # leaving a stale jmv-weights attribute that crashes .COUNTS construction
    dataWithAttr <- weightedData[c("cat", "grp")]
    attr(dataWithAttr, "jmv-weights") <- weightedData$w
    attr(dataWithAttr, "jmv-weights-name") <- "w"

    r <- vijPlots::barplot(
        data = dataWithAttr,
        rows = "cat",
        columns = "grp",
        facet = NULL,
        counts = NULL
    )
    expect_equal(r$plot$state$.COUNTS, weightedData$w)

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 20, 30, 15, 3)))
})

test_that("barplot: negative counts are rejected", {
    negData <- weightedData
    negData$w[1] <- -3
    expect_error(
        vijPlots::barplot(
            data = negData,
            rows = "cat",
            columns = "grp",
            facet = NULL,
            counts = "w"
        ),
        "Counts may not be negative."
    )
})
