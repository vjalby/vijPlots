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

testthat::test_that("barchart: bootstrap CI is the same at every render", {
    testPlot <- vijPlots::barchart(
        data = testData,
        xVar="Species" ,
        yVar="Petal.Width",
        group = NULL,
        facet = NULL,
        errorBars = "ci",
        bootstrap = TRUE
        )$plot
    errorBarLimits <- function() {
        plot <- suppressMessages(testPlot$plot$fun())
        built <- ggplot2::ggplot_build(plot)
        errorBarLayer <- which(vapply(plot$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))
        built$data[[errorBarLayer]][, c("ymin", "ymax")]
    }
    testthat::expect_equal(errorBarLimits(), errorBarLimits())
    expect_plot_snapshot("barchart-errorbar-bootstrap", testPlot)
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

testthat::test_that("barchart: state holds one summarised row per bar", {
    r <- vijPlots::barchart(
        data = testData,
        xVar = "Species",
        yVar = "Petal.Width",
        group = NULL,
        facet = NULL,
        errorBars = "se"
    )
    state <- r$plot$state
    cleanData <- testData[!is.na(testData$Species) & !is.na(testData$Petal.Width), ]
    expectedMean <- tapply(cleanData$Petal.Width, cleanData$Species, mean)
    expectedSe <- tapply(cleanData$Petal.Width, cleanData$Species, function(x) stats::sd(x) / sqrt(length(x)))

    testthat::expect_equal(nrow(state), nlevels(testData$Species))
    testthat::expect_equal(state$.Y, as.numeric(expectedMean))
    testthat::expect_equal(state$.YMIN, as.numeric(expectedMean - expectedSe))
    testthat::expect_equal(state$.YMAX, as.numeric(expectedMean + expectedSe))
})

testthat::test_that("barchart: variables named like the summary columns (y, ymin, ymax)", {
    # regression test: the summary computed in .run() must not clash with user variables
    namedData <- data.frame(ymax = testData$Petal.Width, ymin = testData$Species, y = testData$group2)
    r <- vijPlots::barchart(
        data = namedData,
        xVar = "ymin",
        yVar = "ymax",
        group = "y",
        facet = NULL,
        errorBars = "se"
    )
    reference <- vijPlots::barchart(
        data = testData,
        xVar = "Species",
        yVar = "Petal.Width",
        group = "group2",
        facet = NULL,
        errorBars = "se"
    )
    testthat::expect_equal(r$plot$state$.Y, reference$plot$state$.Y)
    testthat::expect_equal(r$plot$state$.YMIN, reference$plot$state$.YMIN)
    testthat::expect_equal(levels(r$plot$state$y), levels(testData$group2))
    testthat::expect_no_error(ggplot2::ggplot_build(suppressMessages(r$plot$plot$fun())))
})

testthat::test_that("barchart: a variable named like an internal column is rejected", {
    reservedData <- data.frame(.YMIN = testData$Species, value = testData$Petal.Width)
    testthat::expect_error(
        vijPlots::barchart(data = reservedData, xVar = ".YMIN", yVar = "value", group = NULL, facet = NULL),
        "reserved name"
    )
})

testthat::test_that("barchart: grid lines (vertical)", {
    testPlot <- vijPlots::barchart(data = testData, xVar = "Species", yVar = "Petal.Width", group = NULL, facet = NULL,
                                   yGridLines = TRUE)$plot
    expect_plot_snapshot("barchart-gridlines-vertical", testPlot)
})

testthat::test_that("barchart: grid lines (horizontal)", {
    testPlot <- vijPlots::barchart(data = testData, xVar = "Species", yVar = "Petal.Width", group = NULL, facet = NULL,
                                   horizontal = TRUE, xGridLines = TRUE)$plot
    expect_plot_snapshot("barchart-gridlines-horizontal", testPlot)
})

testthat::test_that("barchart: missing values are labelled (Missing) on the axis, legend and facets", {
    renderBuilt <- function(...) {
        r <- vijPlots::barchart(data = testData, yVar = "Petal.Width", ignoreNA = FALSE, ...)
        print(r$plot)
        ggplot2::ggplot_build(ggplot2::last_plot())
    }
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    # NA category on the x-axis, NA group in the legend (gray bars)
    built <- renderBuilt(xVar = "Species", group = "group2", facet = NULL)
    xLabels <- built$layout$panel_params[[1]]$x$get_labels()
    expect_true("(Missing)" %in% xLabels)
    expect_false("NA" %in% xLabels)
    expect_equal(built$plot$scales$get_scales("fill")$get_labels(), c("A", "B", "C", "(Missing)"))
    expect_true("gray" %in% built$data[[1]]$fill)
    # NA facet
    built <- renderBuilt(xVar = "Species", group = NULL, facet = "group2")
    gt <- ggplot2::ggplot_gtable(built)
    strips <- unlist(lapply(gt$grobs[grepl("^strip", gt$layout$name)], function(g) {
        unlist(lapply(g$grobs, function(s) tryCatch(s$children[[2]]$children[[1]]$label, error = function(e) NULL)))
    }))
    expect_true("(Missing)" %in% strips)
    expect_false("NA" %in% strips)
})

testthat::test_that("barchart: horizontal, labels at the bars' end (fixed gap)", {
    r <- vijPlots::barchart(data = testData, xVar = "Species", yVar = "Petal.Width", group = NULL, facet = NULL,
                            horizontal = TRUE, showLabels = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    # text after two spaces, with hjust = 0: the same gap whatever the label length
    built <- ggplot2::layer_data(ggplot2::last_plot(), 2)
    expect_true(all(startsWith(built$label, "  ")))
    expect_true(all(built$hjust == 0))
})
