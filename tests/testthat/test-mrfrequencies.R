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

test_that("mrfrequencies: dummy variables mode (morevar), default order", {
    r <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club")
    )
    freq <- r$responses$asDF
    expect_equal(unname(freq$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total"))
    expect_equal(unname(freq$freq), c(13, 10, 6, 5, 34))
    expect_equal(unname(freq$responsepercent),
                 c(0.382352941176471, 0.294117647058824, 0.176470588235294, 0.147058823529412, 1),
                 tolerance = 1e-9)
    expect_equal(unname(freq$casepercent),
                 c(0.764705882352941, 0.588235294117647, 0.352941176470588, 0.294117647058824, 2),
                 tolerance = 1e-9)
    expect_equal(r$responses$notes$noc$note, "Number of cases: 17")
})

test_that("mrfrequencies: multi-valued variable mode (onevar) matches dummy variables mode", {
    r <- vijPlots::mrfrequencies(
        data = testData,
        mode = "onevar",
        repVar = "Credit Cards",
        resps = NULL,
        separator = ";"
    )
    freq <- r$responses$asDF
    expect_equal(unname(freq$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total"))
    expect_equal(unname(freq$freq), c(13, 10, 6, 5, 34))
    expect_equal(r$responses$notes$noc$note, "Number of cases: 17")
})

test_that("mrfrequencies: order = none keeps input order", {
    r <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        order = "none"
    )
    freq <- r$responses$asDF
    expect_equal(unname(freq$var), c("Mastercard", "Amex", "VISA", "Diners Club", "Total"))
    expect_equal(unname(freq$freq), c(10, 5, 13, 6, 34))
})

test_that("mrfrequencies: order = increasing", {
    r <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        order = "increasing"
    )
    freq <- r$responses$asDF
    expect_equal(unname(freq$var), c("Amex", "Diners Club", "Mastercard", "VISA", "Total"))
    expect_equal(unname(freq$freq), c(5, 6, 10, 13, 34))
})

test_that("mrfrequencies: emptyAsNA = FALSE counts the all-missing row as a case", {
    r <- vijPlots::mrfrequencies(
        data = testData,
        mode = "onevar",
        repVar = "Credit Cards",
        resps = NULL,
        separator = ";",
        emptyAsNA = FALSE
    )
    freq <- r$responses$asDF
    expect_equal(unname(freq$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total"))
    expect_equal(unname(freq$freq), c(13, 10, 6, 5, 34))
    expect_equal(unname(freq$casepercent),
                 c(0.722222222222222, 0.555555555555556, 0.333333333333333, 0.277777777777778, 1.88888888888889),
                 tolerance = 1e-9)
    expect_equal(r$responses$notes$noc$note, "Number of cases: 18")
})

test_that("mrfrequencies: plot, dummy variables mode (default)", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club")
    )$plot
    expect_plot_snapshot("mrfrequencies-morevar", testPlot)
})

test_that("mrfrequencies: plot, multi-valued variable mode", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "onevar",
        repVar = "Credit Cards",
        resps = NULL,
        separator = ";"
    )$plot
    expect_plot_snapshot("mrfrequencies-onevar", testPlot)
})

test_that("mrfrequencies: plot, y-axis = counts", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        yaxis = "counts"
    )$plot
    expect_plot_snapshot("mrfrequencies-yaxis-counts", testPlot)
})

test_that("mrfrequencies: plot, y-axis = % of responses", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        yaxis = "responses"
    )$plot
    expect_plot_snapshot("mrfrequencies-yaxis-responses", testPlot)
})

test_that("mrfrequencies: plot, horizontal", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("mrfrequencies-horizontal", testPlot)
})

test_that("mrfrequencies: plot, count labels shown", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        showLabels = TRUE
    )$plot
    expect_plot_snapshot("mrfrequencies-showLabels", testPlot)
})

test_that("mrfrequencies: plot, count labels above the bar (labelPosition = top)", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        showLabels = TRUE,
        labelPosition = "top"
    )$plot
    expect_plot_snapshot("mrfrequencies-labelPosition-top", testPlot)
})

test_that("mrfrequencies: plot, multi-color (singleColor = FALSE)", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        singleColor = FALSE
    )$plot
    expect_plot_snapshot("mrfrequencies-multiColor", testPlot)
})

test_that("mrfrequencies: plot, titles, axis and legend text options", {
    testPlot <- vijPlots::mrfrequencies(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        titleText = "Credit cards owned",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "Check-all-that-apply survey",
        subtitleFontFace = "italic",
        captionText = "Source: fictitious survey data",
        captionAlign = "1",
        xAxisText = "Card",
        yAxisText = "Percent of cases",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("mrfrequencies-titles-axis-legend", testPlot)
})
