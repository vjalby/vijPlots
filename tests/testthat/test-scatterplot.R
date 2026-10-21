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

test_that("scatterplot: missing group level kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("scatterplot-ignoreNA-false", testPlot)
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

test_that("scatterplot: grid lines", {
    testPlot <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = NULL,
        labelVar = NULL,
        ptSize = NULL,
        facet = NULL,
        xGridLines = TRUE,
        yGridLines = TRUE
    )$plot
    expect_plot_snapshot("scatterplot-gridlines", testPlot)
})

test_that("scatterplot: missing group and facet labelled (Missing) (ignoreNA = FALSE)", {
    r <- vijPlots::scatterplot(
        data = testData,
        xaxis = "Petal.Length",
        yaxis = "Petal.Width",
        group = "Species",
        labelVar = NULL,
        ptSize = NULL,
        facet = "group2",
        ignoreNA = FALSE,
        groupShapes = TRUE
    )
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())
    # the same labels on the merged colour/fill/shape legend, NA points in gray
    for (aes in c("colour", "fill", "shape"))
        expect_equal(built$plot$scales$get_scales(aes)$get_labels(), c("setosa", "versicolor", "virginica", "(Missing)"))
    expect_true("gray" %in% built$data[[1]]$colour)
    gt <- ggplot2::ggplot_gtable(built)
    guideBox <- gt$grobs[[which(gt$layout$name == "guide-box-right")]]
    expect_equal(sum(grepl("^guides", guideBox$layout$name)), 1)
    # facet strips
    strips <- unlist(lapply(gt$grobs[grepl("^strip", gt$layout$name)], function(g) {
        unlist(lapply(g$grobs, function(s) tryCatch(s$children[[2]]$children[[1]]$label, error = function(e) NULL)))
    }))
    expect_true("(Missing)" %in% strips)
    expect_false("NA" %in% strips)
})

test_that("scatterplot: a missing label keeps the point, a missing group removes it", {
    naData <- data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, NA),
                         g = factor(c("a", "b", NA, "a")), lab = c("p1", NA, "p3", "p4"))
    r <- vijPlots::scatterplot(data = naData, xaxis = "x", yaxis = "y", group = "g",
                               labelVar = "lab", ptSize = NULL, facet = NULL)
    # default (ignoreNA): point 2 kept without label, point 3 (no group) and 4 (no y) left out
    expect_equal(r$plot$state$x, c(1, 2))
    r <- vijPlots::scatterplot(data = naData, xaxis = "x", yaxis = "y", group = "g",
                               labelVar = "lab", ptSize = NULL, facet = NULL, ignoreNA = FALSE)
    expect_equal(r$plot$state$x, c(1, 2, 3))
})

test_that("scatterplot: the former keepNA option has no effect", {
    args <- list(data = testData, xaxis = "Petal.Length", yaxis = "Petal.Width", group = "Species",
                 labelVar = NULL, ptSize = NULL, facet = NULL)
    expect_equal(do.call(vijPlots::scatterplot, c(args, keepNA = TRUE))$plot$state,
                 do.call(vijPlots::scatterplot, c(args, keepNA = FALSE))$plot$state)
})
