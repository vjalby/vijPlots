testData <- structure(list(`Credit Cards` = structure(c(1L, 5L, NA, 5L, 3L,
2L, 1L, 3L, 6L, 7L, 1L, 6L, 3L, 1L, 2L, 5L, 1L, 4L), levels = c("VISA",
"Mastercard;Amex", "Mastercard;VISA", "Amex;VISA", "Mastercard;VISA;Diners Club",
"Mastercard;Amex;Diners Club", "VISA;Diners Club"), class = "factor"),
    Mastercard = structure(c(1L, 2L, NA, 2L, 2L, 2L, 1L, 2L,
    2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L), levels = c("0",
    "1"), class = "factor", values = 0:1), Amex = structure(c(1L,
    1L, NA, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L,
    1L, 2L), levels = c("0", "1"), class = "factor", values = 0:1),
    VISA = structure(c(2L, 2L, NA, 2L, 2L, 1L, 2L, 2L, 1L, 2L,
    2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L), levels = c("0", "1"), class = "factor", values = 0:1),
    `Diners Club` = structure(c(1L, 2L, NA, 2L, 1L, 1L, 1L, 1L,
    2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L), levels = c("0",
    "1"), class = "factor", values = 0:1), Age = structure(c(1L,
    2L, 2L, 1L, 3L, 1L, 2L, 3L, 2L, 2L, 3L, 2L, 2L, 1L, 3L, 3L,
    2L, 3L), levels = c("Young Adult", "Adult", "Senior"), class = c("ordered",
    "factor"), values = 1:3)), row.names = c("1", "2", "3", "4",
"5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
"16", "17", "18"), class = "data.frame")

test_that("mrcrosstabs: dummy variables mode (morevar), counts, group = Age", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL
    )
    ct <- r$crosstab$asDF
    expect_equal(unname(ct$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total", "Number of cases"))
    expect_equal(unname(ct$`Young Adult`), c(3, 2, 1, 1, 7, 4))
    expect_equal(unname(ct$Adult), c(5, 4, 4, 2, 15, 7))
    expect_equal(unname(ct$Senior), c(5, 4, 1, 2, 12, 6))
    expect_equal(unname(ct$Total), c(13, 10, 6, 5, 34, 17))
})

test_that("mrcrosstabs: multi-valued variable mode (onevar) matches dummy variables mode", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "onevar",
        repVar = "Credit Cards",
        resps = NULL,
        separator = ";",
        group = NULL,
        group2 = "Age"
    )
    ct <- r$crosstab$asDF
    expect_equal(unname(ct$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total", "Number of cases"))
    expect_equal(unname(ct$`Young Adult`), c(3, 2, 1, 1, 7, 4))
    expect_equal(unname(ct$Adult), c(5, 4, 4, 2, 15, 7))
    expect_equal(unname(ct$Senior), c(5, 4, 1, 2, 12, 6))
    expect_equal(unname(ct$Total), c(13, 10, 6, 5, 34, 17))
})

test_that("mrcrosstabs: computedValues = % of cases", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        computedValues = "cases"
    )
    ct <- r$crosstab$asDF
    expect_equal(unname(ct$`Young Adult`), c(0.75, 0.5, 0.25, 0.25, 1.75), tolerance = 1e-9)
    expect_equal(unname(ct$Adult), c(0.7142857142857143, 0.5714285714285714, 0.5714285714285714, 0.2857142857142857, 2.142857142857143), tolerance = 1e-9)
    expect_equal(unname(ct$Senior), c(0.8333333333333334, 0.6666666666666666, 0.1666666666666667, 0.3333333333333333, 2), tolerance = 1e-9)
})

test_that("mrcrosstabs: computedValues = % of responses", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        computedValues = "responses"
    )
    ct <- r$crosstab$asDF
    expect_equal(unname(ct$`Young Adult`), c(0.4285714285714286, 0.2857142857142857, 0.1428571428571428, 0.1428571428571428, 1), tolerance = 1e-9)
    expect_equal(unname(ct$Total), c(0.3823529411764706, 0.2941176470588235, 0.1764705882352941, 0.1470588235294118, 1), tolerance = 1e-9)
})

test_that("mrcrosstabs: computedValues = % by row (options)", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        computedValues = "options"
    )
    ct <- r$crosstab$asDF
    expect_equal(unname(ct$`Young Adult`), c(0.2307692307692308, 0.2, 0.1666666666666667, 0.2, 0.2058823529411765, 0.2352941176470588), tolerance = 1e-9)
    expect_equal(unname(ct$Adult), c(0.3846153846153846, 0.4, 0.6666666666666666, 0.4, 0.4411764705882353, 0.4117647058823529), tolerance = 1e-9)
    expect_equal(unname(ct$Total), c(1, 1, 1, 1, 1, 1), tolerance = 1e-9)
})

test_that("mrcrosstabs: plot, dummy variables mode (default)", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL
    )$plot
    expect_plot_snapshot("mrcrosstabs-morevar", testPlot)
})

test_that("mrcrosstabs: plot, multi-valued variable mode", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "onevar",
        repVar = "Credit Cards",
        resps = NULL,
        separator = ";",
        group = NULL,
        group2 = "Age"
    )$plot
    expect_plot_snapshot("mrcrosstabs-onevar", testPlot)
})

test_that("mrcrosstabs: plot, options on the x-axis (xaxis = xrows)", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        xaxis = "xrows"
    )$plot
    expect_plot_snapshot("mrcrosstabs-xaxis-xrows", testPlot)
})

test_that("mrcrosstabs: plot, stacked bars", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        bartype = "stack"
    )$plot
    expect_plot_snapshot("mrcrosstabs-stacked", testPlot)
})

test_that("mrcrosstabs: plot, stacked bars reversed", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        bartype = "stack",
        reverseStack = TRUE
    )$plot
    expect_plot_snapshot("mrcrosstabs-stacked-reversed", testPlot)
})

test_that("mrcrosstabs: plot, horizontal", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("mrcrosstabs-horizontal", testPlot)
})

test_that("mrcrosstabs: plot, count labels shown", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        showLabels = TRUE
    )$plot
    expect_plot_snapshot("mrcrosstabs-showLabels", testPlot)
})

test_that("mrcrosstabs: plot, count labels above the bar (labelPosition = top)", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        showLabels = TRUE,
        labelPosition = "top"
    )$plot
    expect_plot_snapshot("mrcrosstabs-labelPosition-top", testPlot)
})

test_that("mrcrosstabs: plot, titles, axis and legend text options", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        titleText = "Credit cards by age group",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "Check-all-that-apply survey",
        subtitleFontFace = "italic",
        captionText = "Source: fictitious survey data",
        captionAlign = "1",
        legendText = "Card",
        legendPosition = "bottom",
        xAxisText = "Age group",
        yAxisText = "Count",
        xAxisLabelRotation = 0,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("mrcrosstabs-titles-axis-legend", testPlot)
})
