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
