testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

test_that("raincloud: no group", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = NULL,
        groupTwo = NULL
    )$plot
    expect_plot_snapshot("raincloud-noGroup", testPlot)
})

test_that("raincloud: overlapped group only", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = NULL,
        groupTwo = "group2"
    )$plot
    expect_plot_snapshot("raincloud-overlappedOnly", testPlot)
})

test_that("raincloud: side-by-side group only", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = NULL
    )$plot
    expect_plot_snapshot("raincloud-sideBySideOnly", testPlot)
})

test_that("raincloud: side-by-side and overlapped groups", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = "group2"
    )$plot
    expect_plot_snapshot("raincloud-bothGroups", testPlot)
})

test_that("raincloud: nudged boxplots with both groups", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = "group2",
        nudgeBoxplot = TRUE
    )$plot
    expect_plot_snapshot("raincloud-nudgedBoxplot", testPlot)
})

test_that("raincloud: horizontal", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("raincloud-horizontal", testPlot)
})

test_that("raincloud: reverse disposition", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = NULL,
        reverse = TRUE
    )$plot
    expect_plot_snapshot("raincloud-reverse", testPlot)
})

test_that("raincloud: single color", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = NULL,
        singleColor = TRUE,
        colorNo = 2
    )$plot
    expect_plot_snapshot("raincloud-singleColor", testPlot)
})

test_that("raincloud: missing group level kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "group2",
        groupTwo = NULL,
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("raincloud-ignoreNA-false", testPlot)
})

test_that("raincloud: titles, axis and legend text options", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = "group2",
        titleText = "Petal Width by Species",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "iris dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Group",
        legendPosition = "bottom",
        xAxisText = "Species group",
        yAxisText = "Petal width (cm)",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("raincloud-titles-axis-legend", testPlot)
})

test_that("raincloud: grid lines (vertical)", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = NULL,
        yGridLines = TRUE
    )$plot
    expect_plot_snapshot("raincloud-gridlines-vertical", testPlot)
})

test_that("raincloud: grid lines (horizontal)", {
    testPlot <- vijPlots::raincloud(
        data = testData,
        aVar = "Petal.Width",
        groupOne = "Species",
        groupTwo = NULL,
        horizontal = TRUE,
        xGridLines = TRUE
    )$plot
    expect_plot_snapshot("raincloud-gridlines-horizontal", testPlot)
})

test_that("raincloud: missing values are labelled (Missing) on the axis and in the legend", {
    r <- vijPlots::raincloud(data = testData, aVar = "Petal.Width", groupOne = "Species",
                             groupTwo = "group2", ignoreNA = FALSE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    # ggdist warns about the tiny NA x group2 subgroups (bandwidth fallback): unrelated to labels
    built <- suppressWarnings(ggplot2::ggplot_build(ggplot2::last_plot()))
    # NA group 1 on the x-axis
    xLabels <- built$layout$panel_params[[1]]$x$get_labels()
    expect_true("(Missing)" %in% xLabels)
    expect_false("NA" %in% xLabels)
    # NA group 2 in the legend, the same labels on the merged fill and colour scales
    for (aes in c("fill", "colour"))
        expect_equal(built$plot$scales$get_scales(aes)$get_labels(), c("A", "B", "C", "(Missing)"))
    # a single legend (fill, colour and slab_color still merged, size and slab_color hidden)
    gt <- ggplot2::ggplot_gtable(built)
    guideBox <- gt$grobs[[which(gt$layout$name == "guide-box-right")]]
    expect_equal(sum(grepl("^guides", guideBox$layout$name)), 1)
})
