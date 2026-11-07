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

test_that("lollipop: facet label customization", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = "group2",
        facetFontFace = "bold",
        facetFontSize = "16",
        facetAlign = "0",
        facetStyle = "border"
    )$plot
    expect_plot_snapshot("lollipop-facet-labels", testPlot)
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

test_that("lollipop: grid lines (vertical)", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        yGridLines = TRUE
    )$plot
    expect_plot_snapshot("lollipop-gridlines-vertical", testPlot)
})

test_that("lollipop: grid lines (horizontal)", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        horizontal = TRUE,
        xGridLines = TRUE
    )$plot
    expect_plot_snapshot("lollipop-gridlines-horizontal", testPlot)
})

test_that("lollipop: hidden category axis (vertical), segments start at the baseline", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        xHideAxis = TRUE
    )$plot
    expect_plot_snapshot("lollipop-hideaxis-vertical", testPlot)
})

test_that("lollipop: hidden value axis (horizontal)", {
    testPlot <- vijPlots::lollipop(
        data = testData,
        aVar = "Petal.Width",
        group = "Species",
        facet = NULL,
        horizontal = TRUE,
        xHideAxis = TRUE
    )$plot
    expect_plot_snapshot("lollipop-hideaxis-horizontal", testPlot)
})

test_that("lollipop: hidden category axis, no lower margin only when the segments start at 0", {
    # Value axis range (data units) and clipping of the built plot
    built <- function(y, horizontal = FALSE, ...) {
        r <- vijPlots::lollipop(data = data.frame(g = factor(c("A", "B", "C")), y = y), aVar = "y", group = "g",
                                facet = NULL, horizontal = horizontal, ...)
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        p <- ggplot2::last_plot()
        list(range = ggplot2::ggplot_build(p)$layout$panel_params[[1]][[if (horizontal) "x" else "y"]]$continuous_range,
             clip = p$coordinates$clip)
    }
    positive <- c(1, 2, 3)
    # category axis hidden (x when vertical, y when horizontal): from 0, the dots on 0 not clipped
    v <- built(positive, xHideAxis = TRUE)
    expect_equal(v$range[1], 0)
    expect_equal(v$clip, "off")
    expect_equal(built(positive, horizontal = TRUE, yHideAxis = TRUE)$range[1], 0)
    expect_equal(built(c(0, 2, 3), xHideAxis = TRUE, yaxis = "identity")$range[1], 0)
    # value axis hidden, nothing hidden, min-max ranges, a negative value: the usual 5% margin
    expect_lt(built(positive, yHideAxis = TRUE)$range[1], 0)
    expect_equal(built(positive)$clip, "on")
    expect_lt(built(positive, xHideAxis = TRUE, yaxis = "minmax")$range[1], 1)
    expect_lt(built(c(1, -2, 3), xHideAxis = TRUE)$range[1], -2)
    # a manual range is left as is
    manual <- list(yAxisRangeType = "manual", yAxisRangeMin = 0, yAxisRangeMax = 5)
    expect_equal(do.call(built, c(list(positive, xHideAxis = TRUE), manual))$range,
                 do.call(built, c(list(positive), manual))$range)
})
