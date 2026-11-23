testData <- datasets::iris
testData[c(2,4,6,8),1] <- NA
testData[c(3,4,5,8),2] <- NA
testData[c(2,8),5] <- NA
testData[80:84,5] <- NA
testData[130:140,5] <- NA
testData$group2 <- factor(rep(c("A", "B", NA, "B", "A", "C", "B"), length.out = nrow(testData)))

test_that("qqplot: basic normal Q-Q", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL
    )$plot
    expect_plot_snapshot("qqplot-basic", testPlot)
})

test_that("qqplot: grouped", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = "Species"
    )$plot
    expect_plot_snapshot("qqplot-grouped", testPlot)
})

test_that("qqplot: with confidence band", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        band = TRUE
    )$plot
    expect_plot_snapshot("qqplot-band", testPlot)
})

test_that("qqplot: detrended", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        detrend = TRUE
    )$plot
    expect_plot_snapshot("qqplot-detrended", testPlot)
})

test_that("qqplot: P-P plot", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        type = "PP"
    )$plot
    expect_plot_snapshot("qqplot-pp", testPlot)
})

test_that("qqplot: standardized values", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        standardize = TRUE
    )$plot
    expect_plot_snapshot("qqplot-standardized", testPlot)
})

test_that("qqplot: natural log transform", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        transLog = TRUE
    )$plot
    expect_plot_snapshot("qqplot-logTransform", testPlot)
})

test_that("qqplot: exponential distribution", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        distrib = "exp"
    )$plot
    expect_plot_snapshot("qqplot-exponential", testPlot)
})

test_that("qqplot: Q-Q line reference (instead of identity)", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        refType = "qqline"
    )$plot
    expect_plot_snapshot("qqplot-qqline", testPlot)
})

test_that("qqplot: ELL and Kolmogorov-Smirnov bands", {
    for (method in c("ell", "ks")) {
        testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = NULL,
                                     band = TRUE, methodQQ = method)$plot
        expect_plot_snapshot(paste0("qqplot-band-", method), testPlot)
    }
})

test_that("qqplot: band with groups, the Q-Q line and detrended", {
    testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species",
                                 band = TRUE, methodQQ = "ell", refType = "qqline")$plot
    expect_plot_snapshot("qqplot-band-grouped-qqline", testPlot)
    testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = NULL,
                                 band = TRUE, refType = "qqline", detrend = TRUE)$plot
    expect_plot_snapshot("qqplot-band-qqline-detrended", testPlot)
})

test_that("qqplot: P-P plot with ELL band, grouped and detrended", {
    testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = NULL,
                                 type = "PP", band = TRUE, methodPP = "ell")$plot
    expect_plot_snapshot("qqplot-pp-band", testPlot)
    testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species",
                                 type = "PP", band = TRUE, methodPP = "ell", detrend = TRUE)$plot
    expect_plot_snapshot("qqplot-pp-band-grouped-detrended", testPlot)
})

test_that("qqplot: bootstrap and tail-sensitive bands", {
    for (method in c("boot", "ts")) {
        testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = NULL,
                                     band = TRUE, methodQQ = method)$plot
        expect_plot_snapshot(paste0("qqplot-band-", method), testPlot)
    }
    testPlot <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = NULL,
                                 type = "PP", band = TRUE, methodPP = "boot")$plot
    expect_plot_snapshot("qqplot-pp-band-boot", testPlot)
})

test_that("qqplot: random bands are reproducible and leave the global RNG alone", {
    set.seed(1)
    seed <- get(".Random.seed", envir = globalenv())
    run <- function() vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species",
                                       band = TRUE, methodQQ = "boot")$plot$state$qq
    first <- run()
    expect_identical(get(".Random.seed", envir = globalenv()), seed)
    expect_identical(run(), first)
})

test_that("qqplot: a state saved by vijPlots <= 1.4.1 (data and parameters) is still drawn", {
    results <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species", band = TRUE,
                                methodQQ = "ell", paramMethod = "paraValue", param1 = 3.75, param2 = 1.75)
    expected <- renderedPlot(results)
    results$plot$setState(list(data = jmvcore::naOmit(testData[c("Petal.Length", "Species")]),
                               params = list(mean = 3.75, sd = 1.75)))
    p <- renderedPlot(results)
    expect_length(p$layers, 3)
    for (i in seq_along(p$layers))
        expect_identical(ggplot2::layer_data(p, i), ggplot2::layer_data(expected, i))
})

test_that("qqplot: each group gives that group's own plot", {
    data <- jmvcore::naOmit(testData[c("Petal.Length", "Species")])
    cases <- list(
        list(),
        list(standardize = TRUE),
        list(standardize = TRUE, paramMethod = "paraValue", param1 = 0, param2 = 1, band = TRUE, methodQQ = "ell"),
        list(band = TRUE, methodQQ = "boot"),
        list(band = TRUE, methodQQ = "ts"),
        list(band = TRUE, methodQQ = "ell", refType = "qqline", detrend = TRUE),
        list(distrib = "gamma", paramEstMethod = "mme", band = TRUE, methodQQ = "ks"),
        list(distrib = "lnorm", type = "PP", band = TRUE, methodPP = "boot"),
        list(distrib = "exp", type = "PP", band = TRUE, methodPP = "ell"))
    for (case in cases) {
        qq <- rlang::inject(vijPlots::qqplot(data = data, dep = "Petal.Length", group = "Species",
                                             !!!case))$plot$state$qq
        for (level in levels(data$Species)) {
            single <- rlang::inject(vijPlots::qqplot(data = data[data$Species == level, ], dep = "Petal.Length",
                                                     group = NULL, !!!case))$plot$state$qq
            for (k in c("points", "line", "band")) {
                if (is.null(single[[k]])) {
                    expect_null(qq[[k]])
                    next
                }
                part <- qq[[k]][qq[[k]]$Species == level, names(single[[k]])]
                rownames(part) <- NULL
                expect_equal(part, single[[k]], label = paste(level, k, deparse(case)))
            }
        }
    }
})

test_that("qqplot: parameters table with one row per group", {
    table <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species")$paramTable$asDF
    expect_equal(nrow(table), 3)
    expect_equal(table$group, levels(testData$Species))
    expect_equal(table$mean, as.vector(tapply(testData$Petal.Length, testData$Species, mean, na.rm = TRUE)))
})

test_that("qqplot: one plot per group", {
    run <- function(perGroup, ...) vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species",
                                                    band = TRUE, plotPerGroup = perGroup, ...)
    expect_identical(run(TRUE, methodQQ = "boot", standardize = TRUE)$plot$state,
                     run(FALSE, methodQQ = "boot", standardize = TRUE)$plot$state)
    p <- renderedPlot(run(TRUE))
    expect_s3_class(p$facet, "FacetWrap")
    expect_false(any(c("colour", "fill") %in% c(names(p$mapping), unlist(lapply(p$layers, function(l) names(l$mapping))))))
    expect_equal(renderedPlot(run(TRUE, facetBy = "column", facetNumber = 2))$facet$params$ncol, 2)
    expect_equal(renderedPlot(run(TRUE, facetBy = "row", facetNumber = 2))$facet$params$nrow, 2)
    expect_plot_snapshot("qqplot-one-plot-per-group", run(TRUE, methodQQ = "ell", facetBy = "row", facetNumber = 1,
                                                          facetStyle = "background", facetFontFace = "bold")$plot)
    expect_false(any(unlist(renderedPlot(run(TRUE))$facet$params$free)))
    expect_true(all(unlist(renderedPlot(run(TRUE, facetFreeScales = TRUE))$facet$params$free)))
    built <- ggplot2::ggplot_build(renderedPlot(run(TRUE, facetFreeScales = TRUE, xAxisRangeType = "manual",
                                                     xAxisRangeMin = 0, xAxisRangeMax = 8)))
    expect_equal(unique(lapply(built$layout$panel_params, `[[`, "x.range")), list(c(0, 8) + c(-0.4, 0.4)))
    expect_length(unique(lapply(built$layout$panel_params, `[[`, "y.range")), 3)
    expect_plot_snapshot("qqplot-one-plot-per-group-free", run(TRUE, methodQQ = "ell", facetBy = "row",
                                                               facetNumber = 1, facetFreeScales = TRUE)$plot)
})

test_that("qqplot: given parameters are common to all groups", {
    results <- vijPlots::qqplot(data = testData, dep = "Petal.Length", group = "Species", band = TRUE,
                                paramMethod = "paraValue", param1 = 3.75, param2 = 1.75)
    table <- results$paramTable$asDF
    expect_equal(nrow(table), 1)
    expect_null(table$group)
    expect_equal(c(table$mean, table$sd), c(3.75, 1.75))
})

test_that("qqplot: names the group that cannot be estimated or standardized", {
    data <- data.frame(x = c(1, 2, 3, 4, 5, 6, 6), g = factor(c("a", "a", "a", "b", "c", "c", "c")))
    expect_error(vijPlots::qqplot(data = data, dep = "x", group = "g"),
                 "At least two observations are required in group b")
    data$g <- factor(c("a", "a", "a", "b", "b", "c", "c"))
    expect_error(vijPlots::qqplot(data = data, dep = "x", group = "g"),
                 "equal to zero in group c")
    expect_error(vijPlots::qqplot(data = data, dep = "x", group = "g", standardize = TRUE,
                                  paramMethod = "paraValue", param1 = 0, param2 = 1),
                 "equal to zero in group c")
    expect_no_error(vijPlots::qqplot(data = data, dep = "x", group = "g",
                                     paramMethod = "paraValue", param1 = 0, param2 = 1))
    data <- data.frame(x = c(0.4, 0.5, 0.6, 0.01, 0.99, 0.02, 0.98), g = factor(c("a", "a", "a", "b", "b", "b", "b")))
    expect_error(vijPlots::qqplot(data = data, dep = "x", group = "g", distrib = "beta", paramEstMethod = "mme"),
                 "Unable to estimate the distribution parameters of group b using MME method")
})

test_that("qqplot: user-specified distribution parameters", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        paramMethod = "paraValue",
        param1 = 3.75,
        param2 = 1.75
    )$plot
    expect_plot_snapshot("qqplot-userParams", testPlot)
})

test_that("qqplot: titles, axis and legend text options", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = "Species",
        titleText = "Q-Q Plot of Petal Length",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "iris dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom",
        xAxisText = "Theoretical quantiles",
        yAxisText = "Sample quantiles",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("qqplot-titles-axis-legend", testPlot)
})

test_that("qqplot: grid lines", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        xGridLines = TRUE,
        yGridLines = TRUE
    )$plot
    expect_plot_snapshot("qqplot-gridlines", testPlot)
})

test_that("qqplot: hidden axes", {
    testPlot <- vijPlots::qqplot(
        data = testData,
        dep = "Petal.Length",
        group = NULL,
        xHideAxis = TRUE,
        yHideAxis = TRUE
    )$plot
    expect_plot_snapshot("qqplot-hideaxis", testPlot)
})

test_that("qqplot: a single observation is rejected with a clear message", {
    expect_error(vijPlots::qqplot(data = data.frame(x = 3), dep = "x", group = NULL),
                 "At least two observations")
    # also when only one value is left after removing missing values
    expect_error(vijPlots::qqplot(data = data.frame(x = c(3, NA, NA)), dep = "x", group = NULL),
                 "At least two observations")
})

test_that("qqplot: Beta MME rejects data the moments cannot fit", {
    # s² >= m(1-m): the moment estimates of both shapes would be negative
    expect_error(vijPlots::qqplot(data = data.frame(x = c(0.01, 0.99)), dep = "x", group = NULL,
                                  distrib = "beta", paramEstMethod = "mme"),
                 "Unable to estimate the distribution parameters using MME")
})

test_that("qqplot: theme and font family", {
    p <- renderedPlot(vijPlots::qqplot(data = testData, dep = "Petal.Length", group = NULL,
                                       plotTheme = "ggBw", fontFamily = "serif"))
    expectBwSerif(p)
})
