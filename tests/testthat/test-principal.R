testData <- datasets::iris
names(testData) <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width", "Species")
testData$Num <- 1:150

test_that("principal: summary table (eigenvalues, variance explained)", {
    r <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species"
    )
    summary <- r$summaryTable$asDF
    expect_equal(unname(summary$eigenvalue), c(2.918497817, 0.9140304715, 0.1467568756, 0.02071483643), tolerance = 1e-6)
    expect_equal(unname(summary$initVarProp), c(0.7296244541, 0.2285076179, 0.03668921889, 0.005178709107), tolerance = 1e-6)
    expect_equal(unname(summary$initVarCum), c(0.7296244541, 0.958132072, 0.9948212909, 1), tolerance = 1e-6)
})

test_that("principal: loading table", {
    r <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showLoadings = TRUE
    )
    loadings <- r$loadingTable$asDF
    expect_equal(unname(loadings$var), c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"))
    expect_equal(unname(loadings$`loading:1`), c(0.8901687649, -0.4601427064, 0.9915551834, 0.9649789607), tolerance = 1e-6)
    expect_equal(unname(loadings$`loading:2`), c(0.3608298881, 0.8827162692, 0.02341518838, 0.06399984704), tolerance = 1e-6)
    expect_equal(unname(loadings$QLT), c(0.9225986381, 0.9909193221, 0.9837299528, 0.935280375), tolerance = 1e-6)
})

test_that("principal: KMO and Bartlett's test", {
    r <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showKMO = TRUE
    )
    kmo <- r$kmoTable$asDF
    expect_equal(unname(kmo$test), c("Bartlett's Test of Sphericity", "Kaiser-Meyer-Olkin Measure of Sampling Adequacy (MSA)"))
    expect_equal(unname(kmo$statistic), c(706.959243, 0.540076675), tolerance = 1e-6)
    expect_equal(unname(kmo$df), c(6, NA))
})

test_that("principal: observation table (first 5 observations)", {
    r <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showObservations = TRUE
    )
    obs <- head(r$obsTable$asDF, 5)
    expect_equal(unname(obs$obs), c("1", "2", "3", "4", "5"))
    expect_equal(unname(as.character(obs$group)), c("setosa", "setosa", "setosa", "setosa", "setosa"))
    expect_equal(unname(obs$`1`), c(-2.257141176, -2.074013015, -2.356335112, -2.291706786, -2.381862704), tolerance = 1e-6)
    expect_equal(unname(obs$`2`), c(0.4784238321, -0.671882687, -0.3407664246, -0.5953998627, 0.6446756594), tolerance = 1e-6)
    expect_equal(unname(obs$qlt), c(0.9968578292, 0.9864649801, 0.9995167432, 0.9977577125, 0.9997491324), tolerance = 1e-6)
})

test_that("principal: observation plot, grouped by Species", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species"
    )$obsPlot
    expect_plot_snapshot("principal-obsPlot", testPlot)
})

test_that("principal: observation plot, no group", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = NULL
    )$obsPlot
    expect_plot_snapshot("principal-obsPlot-noGroup", testPlot)
})

test_that("principal: variable (loading) plot", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species"
    )$varPlot
    expect_plot_snapshot("principal-varPlot", testPlot)
})

test_that("principal: scree plot", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = NULL
    )$screePlot
    expect_plot_snapshot("principal-screePlot", testPlot)
})

test_that("principal: biplot (form)", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showBiplot = TRUE,
        biplotType = "formPlot"
    )$biPlot
    expect_plot_snapshot("principal-biplot-form", testPlot)
})

test_that("principal: biplot (covariance)", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        showBiplot = TRUE,
        biplotType = "covPlot"
    )$biPlot
    expect_plot_snapshot("principal-biplot-cov", testPlot)
})

test_that("principal: Varimax rotation", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        rotation = "Varimax"
    )$varPlot
    expect_plot_snapshot("principal-varimax", testPlot)
})

test_that("principal: unstandardized variables", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        stdVariables = FALSE
    )$varPlot
    expect_plot_snapshot("principal-unstandardized", testPlot)
})

test_that("principal: custom dimensions and axes", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        dimNum = 3,
        xaxis = 1,
        yaxis = 3
    )$obsPlot
    expect_plot_snapshot("principal-customDims", testPlot)
})

test_that("principal: point labels (labelVar)", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = "Species",
        groupVar = NULL
    )$obsPlot
    expect_plot_snapshot("principal-labelVar", testPlot)
})

test_that("principal: titles, axis and legend text options", {
    testPlot <- vijPlots::principal(
        data = testData,
        vars = c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width"),
        labelVar = NULL,
        groupVar = "Species",
        obsTitleText = "Iris observations",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        obsSubtitleText = "PCA on 4 flower measurements",
        subtitleFontFace = "italic",
        obsCaptionText = "Source: datasets::iris",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom"
    )$obsPlot
    expect_plot_snapshot("principal-titles-axis-legend", testPlot)
})
