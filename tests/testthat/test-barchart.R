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
    state <- r$plot$state$data
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
    testthat::expect_equal(r$plot$state$data$.Y, reference$plot$state$data$.Y)
    testthat::expect_equal(r$plot$state$data$.YMIN, reference$plot$state$data$.YMIN)
    testthat::expect_equal(levels(r$plot$state$data$y), levels(testData$group2))
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

testthat::test_that("barchart: error bars centred on their bars with Grouped (dodge2), even in an incomplete category", {
    # Chinstrap has no penguin with a missing sex: 2 bars, against 3 for Adelie and Gentoo
    r <- vijPlots::barchart(data = datasets::penguins, xVar = "species", yVar = "body_mass", group = "sex",
                            facet = NULL, errorBars = "se", ignoreNA = FALSE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    p <- ggplot2::last_plot()
    bars <- ggplot2::layer_data(p, 1)
    errors <- ggplot2::layer_data(p, length(p$layers))
    centre <- function(d) data.frame(x = round(d$x), group = d$group, centre = round((d$xmin + d$xmax) / 2, 6))
    both <- merge(centre(bars), centre(errors), by = c("x", "group"))
    expect_equal(nrow(both), 8)
    expect_equal(both$centre.x, both$centre.y)
    # the same cap width everywhere
    expect_equal(length(unique(round(errors$xmax - errors$xmin, 6))), 1)
})

testthat::test_that("barchart: error bar Width is a fraction of the bar's width (1 = as wide as the bar)", {
    capRatio <- function(...) {
        r <- vijPlots::barchart(data = datasets::penguins, xVar = "species", yVar = "body_mass",
                                facet = NULL, errorBars = "se", ...)
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        p <- ggplot2::last_plot()
        bars <- ggplot2::layer_data(p, 1)
        errors <- ggplot2::layer_data(p, length(p$layers))
        unique(round((errors$xmax - errors$xmin) / (bars$xmax - bars$xmin), 6))
    }
    expect_equal(capRatio(group = "sex", barType = "dodge2", errorBarWidth = 1), 1)
    expect_equal(capRatio(group = "sex", barType = "dodge", errorBarWidth = 1), 1)
    expect_equal(capRatio(group = NULL, errorBarWidth = 1), 1)
    expect_equal(capRatio(group = "sex", barType = "dodge2", errorBarWidth = 0.2), 0.2)
})

testthat::test_that("barchart: labels outside the bars on the side of the value's sign", {
    signData <- data.frame(x = factor(rep(c("A", "B"), each = 2)), y = c(3, 5, -2, -4))
    labelsOf <- function(horizontal) {
        r <- vijPlots::barchart(data = signData, xVar = "x", yVar = "y", group = NULL, facet = NULL,
                                showLabels = TRUE, horizontal = horizontal)
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        built <- ggplot2::ggplot_build(ggplot2::last_plot())
        # room on both sides for the labels: the axis goes as far below the negative bar as above the positive one
        range <- built$layout$panel_params[[1]][[if (horizontal) "x" else "y"]]$continuous_range
        expect_equal(-3 - range[1], range[2] - 4, tolerance = 1e-6)   # bars: mean 4 (A), mean -3 (B)
        ggplot2::layer_data(ggplot2::last_plot(), 2)[, c("label", "vjust", "hjust")]
    }
    # vertical: above the positive bar, below the negative one
    v <- labelsOf(FALSE)
    expect_equal(v$label, c("4", "-3"))
    expect_equal(v$vjust, c(-0.5, 1.5))
    # horizontal: right of the positive bar (spaces before), left of the negative one (spaces after)
    h <- labelsOf(TRUE)
    expect_equal(h$label, c("  4", "-3  "))
    expect_equal(h$hjust, c(0, 1))
})

testthat::test_that("barchart: no label on a bar or segment whose value rounds to 0", {
    zeroData <- data.frame(x = factor(rep(c("A", "B"), each = 2)), g = factor(rep(c("g1", "g2"), 2)),
                           y = c(3, 0.004, 0, 2))
    labelsOf <- function(...) {
        r <- vijPlots::barchart(data = zeroData, xVar = "x", yVar = "y", facet = NULL, showLabels = TRUE, ...)
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        ggplot2::layer_data(ggplot2::last_plot(), 2)$label
    }
    expect_equal(labelsOf(group = NULL), c("1.5", "1"))                     # means 1.502, 1 (not hidden)
    expect_setequal(labelsOf(group = "g", barType = "dodge"), c("3", "", "", "2"))
    expect_setequal(labelsOf(group = "g", barType = "stack"), c("3", "", "", "2"))
})

testthat::test_that("barchart: stacked sum label at the end of the stack, on the side of the total's sign", {
    stackData <- data.frame(x = factor(rep(c("A", "B", "C"), each = 6)),
                            g = factor(rep(c("g1", "g2", "g3"), times = 6)),
                            y = c(rep(c(5, -3, 2), 2), rep(c(-4, 6, -1), 2), rep(c(-2, -3, -1), 2)))
    r <- vijPlots::barchart(data = stackData, xVar = "x", yVar = "y", group = "g", facet = NULL,
                            yaxis = "sum", barType = "stack", showLabels = TRUE)
    # sums per segment: A 10, -6, 4 (total 8); B -8, 12, -2 (total 2); C -4, -6, -2 (total -12)
    totals <- r$plot$state$totals
    expect_equal(totals$.TOTAL, c(8, 2, -12))
    expect_equal(totals$.END, c(14, 12, -12))   # top of the positive part, or bottom of the negative part
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    labels <- ggplot2::layer_data(ggplot2::last_plot(), 3)
    expect_equal(labels$label, c("8", "2", "-12"))
    expect_equal(labels$y, c(14, 12, -12))
    expect_equal(labels$vjust, c(-0.5, -0.5, 1.5))
})

# ---- Observations (showPoints) ----

# Layers of the rendered plot, by geom class
barchartLayers <- function(r) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    p <- ggplot2::last_plot()
    index <- function(geom) which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
    list(layer = function(geom) ggplot2::layer_data(p, index(geom)),
         position = function(geom) p$layers[[index(geom)]]$position)
}

testthat::test_that("barchart: observations are kept in the state only when drawn", {
    points <- function(...) vijPlots::barchart(data = testData, xVar = "Species", yVar = "Petal.Width",
                                               facet = NULL, ...)$plot$state$points
    expect_null(points(group = NULL))                                           # off by default
    expect_null(points(group = "group2", showPoints = TRUE, barType = "stack")) # raw values don't belong on a stack
    for (yaxis in c("sum", "max", "min"))
        expect_null(points(group = NULL, showPoints = TRUE, yaxis = yaxis))
    expect_false(is.null(points(group = NULL, showPoints = TRUE, barType = "stack")))   # no group: not stacked
    expect_false(is.null(points(group = NULL, showPoints = TRUE, yaxis = "median")))

    # one row per observation: x position, group and value
    r <- vijPlots::barchart(data = testData, xVar = "Species", yVar = "Petal.Width", group = "group2", facet = NULL,
                            showPoints = TRUE)
    kept <- !is.na(testData$Species) & !is.na(testData$Petal.Width) & !is.na(testData$group2)
    expect_equal(names(r$plot$state$points), c(".X", "group2", ".PY"))
    expect_equal(r$plot$state$points$.PY, testData$Petal.Width[kept])
})

testthat::test_that("barchart: each observation is inside its own bar, with Grouped (dodge2) and Grouped (tight)", {
    # Chinstrap has no missing sex: 2 bars, against 3 for Adelie and Gentoo; and very different counts per bar
    # (regression test: ggplot2's own PositionDodge2 spreads the points over the whole category)
    for (barType in c("dodge2", "dodge")) {
        layers <- barchartLayers(vijPlots::barchart(data = datasets::penguins, xVar = "species", yVar = "body_mass",
                                                    group = "sex", facet = NULL, ignoreNA = FALSE, showPoints = TRUE,
                                                    barType = barType))
        # our own position, not ggplot2's position_jitterdodge(): before ggplot2 4.0 (jamovi 2.7) it fails without a
        # fill/colour aesthetic to dodge by
        expect_s3_class(layers$position("GeomPoint"), "PositionJitterDodge")
        bars <- layers$layer("GeomCol")
        points <- layers$layer("GeomPoint")
        expect_equal(nrow(bars), 8)
        expect_equal(nrow(points), sum(!is.na(datasets::penguins$body_mass)))
        bars$cell <- paste(round(bars$x), bars$group)
        points$cell <- paste(round(points$x), points$group)
        both <- merge(points, bars[c("cell", "xmin", "xmax")], by = "cell")
        expect_equal(nrow(both), nrow(points))
        expect_true(all(both$x >= both$xmin - 1e-9 & both$x <= both$xmax + 1e-9))
    }
})

testthat::test_that("barchart: observations' spread is seeded per bar and leaves the random numbers alone", {
    penguins <- datasets::penguins
    used <- !is.na(penguins$body_mass)
    pointsX <- function(data, barType) {
        r <- vijPlots::barchart(data = data, xVar = "species", yVar = "body_mass", group = "sex", facet = NULL,
                                ignoreNA = FALSE, showPoints = TRUE, barType = barType)
        barchartLayers(r)$layer("GeomPoint")$x
    }
    inBar <- penguins$species %in% "Adelie" & penguins$sex %in% "female" & used
    edited <- penguins
    edited$body_mass[inBar] <- edited$body_mass[inBar] + 100
    other <- !inBar[used]
    for (barType in c("dodge2", "dodge")) {
        x <- pointsX(penguins, barType)
        expect_equal(pointsX(penguins, barType), x)                  # same data, same points
        # changing one bar's values moves its points only
        xEdited <- pointsX(edited, barType)
        expect_equal(xEdited[other], x[other])
        expect_false(isTRUE(all.equal(xEdited[!other], x[!other])))
    }

    # neither the bootstrap nor the spread change the global random number generator
    set.seed(123)
    seedBefore <- get(".Random.seed", globalenv())
    barchartLayers(vijPlots::barchart(data = penguins, xVar = "species", yVar = "body_mass", group = "sex", facet = NULL,
                                      errorBars = "ci", bootstrap = TRUE, showPoints = TRUE))
    expect_identical(get(".Random.seed", globalenv()), seedBefore)
})

testthat::test_that("barchart: observation and error bar colors, shape and size", {
    layersOf <- function(...) barchartLayers(vijPlots::barchart(data = datasets::penguins, xVar = "species",
                                                                yVar = "body_mass", group = NULL, facet = NULL,
                                                                errorBars = "se", showPoints = TRUE, ...))
    defaults <- layersOf()
    expect_equal(unique(defaults$layer("GeomPoint")$colour), "black")
    expect_equal(unique(defaults$layer("GeomPoint")$shape), 16)
    expect_equal(unique(defaults$layer("GeomPoint")$size), 2)
    expect_equal(unique(defaults$layer("GeomErrorbar")$colour), "black")
    custom <- layersOf(pointColor = "red", pointShape = "square", pointSize = 3, errorBarColor = "gray")
    expect_equal(unique(custom$layer("GeomPoint")$colour), "red")
    expect_equal(unique(custom$layer("GeomPoint")$shape), 15)
    expect_equal(unique(custom$layer("GeomPoint")$size), 3)
    expect_equal(unique(custom$layer("GeomErrorbar")$colour), "gray")
    expect_equal(unique(layersOf(pointShape = "opencircle")$layer("GeomPoint")$shape), 1)
})

testthat::test_that("barchart: observations over Grouped bars (incomplete category)", {
    testPlot <- vijPlots::barchart(data = datasets::penguins, xVar = "species", yVar = "body_mass", group = "sex",
                                   facet = NULL, ignoreNA = FALSE, errorBars = "se", showLabels = FALSE,
                                   showPoints = TRUE)$plot
    expect_plot_snapshot("barchart-points-dodge2", testPlot)
})

testthat::test_that("barchart: observations over Grouped (tight) bars, horizontal", {
    testPlot <- vijPlots::barchart(data = datasets::penguins, xVar = "species", yVar = "body_mass", group = "sex",
                                   facet = NULL, ignoreNA = FALSE, barType = "dodge", horizontal = TRUE,
                                   showLabels = FALSE, showPoints = TRUE)$plot
    expect_plot_snapshot("barchart-points-dodge-horizontal", testPlot)
})

testthat::test_that("barchart: observations with options, no group", {
    testPlot <- vijPlots::barchart(data = datasets::penguins, xVar = "species", yVar = "body_mass", group = NULL,
                                   facet = NULL, errorBars = "se", showPoints = TRUE, pointColor = "red",
                                   pointShape = "opencircle", pointSize = 3, errorBarColor = "gray")$plot
    expect_plot_snapshot("barchart-points-options", testPlot)
})
