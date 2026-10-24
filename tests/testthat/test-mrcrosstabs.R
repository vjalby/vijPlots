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

# The crosstab as a data.frame, the group columns named by their level (without the ".G." prefix)
crosstabDF <- function(r) {
    df <- r$crosstab$asDF
    names(df) <- sub("^\\.G\\.", "", names(df))
    df
}

test_that("mrcrosstabs: dummy variables mode (morevar), counts, group = Age", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL
    )
    ct <- crosstabDF(r)
    expect_equal(unname(ct$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total", "Number of cases"))
    expect_equal(unname(ct$`Young Adult`), c(3, 2, 1, 1, 7, 4))
    expect_equal(unname(ct$Adult), c(5, 4, 4, 2, 15, 7))
    expect_equal(unname(ct$Senior), c(5, 4, 1, 2, 12, 6))
    expect_equal(unname(ct$.TOTAL), c(13, 10, 6, 5, 34, 17))
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
    ct <- crosstabDF(r)
    expect_equal(unname(ct$var), c("VISA", "Mastercard", "Diners Club", "Amex", "Total", "Number of cases"))
    expect_equal(unname(ct$`Young Adult`), c(3, 2, 1, 1, 7, 4))
    expect_equal(unname(ct$Adult), c(5, 4, 4, 2, 15, 7))
    expect_equal(unname(ct$Senior), c(5, 4, 1, 2, 12, 6))
    expect_equal(unname(ct$.TOTAL), c(13, 10, 6, 5, 34, 17))
})

test_that("mrcrosstabs: dummy variables mode supports Y/N coding (not just 0/1)", {
    ynData <- data.frame(
        Q1 = factor(c("Y","N","Y","Y","N","Y")),
        Q2 = factor(c("N","Y","Y","N","Y","N")),
        Grp = factor(c("A","A","B","B","A","B"))
    )
    r <- vijPlots::mrcrosstabs(
        data = ynData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Q1", "Q2"),
        group = "Grp",
        group2 = NULL,
        endorsed = "Y",
        order = "none"
    )
    ct <- crosstabDF(r)
    expect_equal(unname(ct$var), c("Q1", "Q2", "Total", "Number of cases"))
    expect_equal(unname(ct$A), c(1, 2, 3, 3))
    expect_equal(unname(ct$B), c(3, 1, 4, 3))
    expect_equal(unname(ct$.TOTAL), c(4, 3, 7, 6))
})

test_that("mrcrosstabs: dummy variables mode supports 1/2 coding", {
    numData <- data.frame(
        Q1 = factor(c(1, 2, 1, 1)),
        Q2 = factor(c(2, 1, 1, 2)),
        Grp = factor(c("A", "A", "B", "B"))
    )
    r <- vijPlots::mrcrosstabs(
        data = numData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Q1", "Q2"),
        group = "Grp",
        group2 = NULL,
        endorsed = "1",
        order = "none"
    )
    ct <- crosstabDF(r)
    expect_equal(unname(ct$var), c("Q1", "Q2", "Total", "Number of cases"))
    expect_equal(unname(ct$.TOTAL), c(3, 2, 5, 4))
})

test_that("mrcrosstabs: empty endorsed value is rejected", {
    ynData <- data.frame(Q1 = factor(c("Y","N")), Q2 = factor(c("N","Y")), Grp = factor(c("A","B")))
    expect_error(
        vijPlots::mrcrosstabs(
            data = ynData,
            mode = "morevar",
            repVar = NULL,
            resps = c("Q1", "Q2"),
            group = "Grp",
            group2 = NULL,
            endorsed = ""
        ),
        "Counted value string may not be empty."
    )
})

test_that("mrcrosstabs: empty separator is rejected (mode = onevar)", {
    expect_error(
        vijPlots::mrcrosstabs(
            data = testData,
            mode = "onevar",
            repVar = "Credit Cards",
            resps = NULL,
            separator = "",
            group = NULL,
            group2 = "Age"
        ),
        "Separator string may not be empty."
    )
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
    ct <- crosstabDF(r)
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
    ct <- crosstabDF(r)
    expect_equal(unname(ct$`Young Adult`), c(0.4285714285714286, 0.2857142857142857, 0.1428571428571428, 0.1428571428571428, 1), tolerance = 1e-9)
    expect_equal(unname(ct$.TOTAL), c(0.3823529411764706, 0.2941176470588235, 0.1764705882352941, 0.1470588235294118, 1), tolerance = 1e-9)
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
    ct <- crosstabDF(r)
    expect_equal(unname(ct$`Young Adult`), c(0.2307692307692308, 0.2, 0.1666666666666667, 0.2, 0.2058823529411765, 0.2352941176470588), tolerance = 1e-9)
    expect_equal(unname(ct$Adult), c(0.3846153846153846, 0.4, 0.6666666666666666, 0.4, 0.4411764705882353, 0.4117647058823529), tolerance = 1e-9)
    expect_equal(unname(ct$.TOTAL), c(1, 1, 1, 1, 1, 1), tolerance = 1e-9)
    # the last row is the distribution of the cases among the groups, labelled as such
    expect_equal(unname(ct$var)[6], "% of cases")
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

test_that("mrcrosstabs: grid lines (vertical)", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        yGridLines = TRUE
    )$plot
    expect_plot_snapshot("mrcrosstabs-gridlines-vertical", testPlot)
})

test_that("mrcrosstabs: grid lines (horizontal)", {
    testPlot <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        horizontal = TRUE,
        xGridLines = TRUE
    )$plot
    expect_plot_snapshot("mrcrosstabs-gridlines-horizontal", testPlot)
})

test_that("mrcrosstabs: table blocks marked as in jmv::conttables", {
    r <- vijPlots::mrcrosstabs(data = testData, mode = "morevar", repVar = NULL,
                               resps = c("Mastercard", "Amex", "VISA", "Diners Club"), group = "Age", group2 = NULL)
    table <- r$crosstab
    hasFormat <- function(format) vapply(seq_len(table$rowCount), function(row)
        bitwAnd(table$getCell(rowNo = row, col = 1)$format, format) == format, logical(1))
    # 4 options, then Total and Number of cases: the options one block (begin on the first, end on the last),
    # each summary row a block of its own (begin and end)
    expect_equal(hasFormat(jmvcore::Cell.BEGIN_GROUP), c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
    expect_equal(hasFormat(jmvcore::Cell.END_GROUP), c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
    expect_equal(hasFormat(jmvcore::Cell.BEGIN_END_GROUP), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("mrcrosstabs: plot, horizontal, labels at the bars' end (fixed gap)", {
    r <- vijPlots::mrcrosstabs(
        data = testData,
        mode = "morevar",
        repVar = NULL,
        resps = c("Mastercard", "Amex", "VISA", "Diners Club"),
        group = "Age",
        group2 = NULL,
        showLabels = TRUE,
        labelPosition = "top",
        horizontal = TRUE
    )
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    # text after two spaces, with hjust = 0: the same gap whatever the label length
    built <- ggplot2::layer_data(ggplot2::last_plot(), 2)
    expect_true(all(startsWith(built$label, "  ")))
    expect_true(all(built$hjust == 0))
    expect_plot_snapshot("mrcrosstabs-horizontal-labels-top", r$plot)
})

test_that("mrcrosstabs: no label for an empty bar (an option nobody chose in a group)", {
    emptyData <- testData
    emptyData$Amex[emptyData$Age == "Young Adult"] <- 0
    r <- vijPlots::mrcrosstabs(data = emptyData, mode = "morevar", repVar = NULL,
                               resps = c("Mastercard", "Amex", "VISA", "Diners Club"), group = "Age", group2 = NULL,
                               showLabels = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    p <- ggplot2::last_plot()
    bars <- ggplot2::layer_data(p, 1)
    labels <- ggplot2::layer_data(p, 2)
    expect_true(any(bars$y == 0))              # the empty bar is still there (keeps its place)
    expect_false("0" %in% labels$label)        # but has no label
})

test_that("mrcrosstabs: percentages missing (not NaN) when their denominator is 0", {
    # group b: no response selected; option q2: chosen by nobody
    d <- data.frame(q1 = factor(c("1", "1", "0", "0")), q2 = factor(c("0", "0", "0", "0")), g = factor(c("a", "a", "b", "b")))
    pct <- function(values) crosstabDF(vijPlots::mrcrosstabs(data = d, resps = c("q1", "q2"), group = "g", group2 = NULL,
                                                             repVar = NULL, computedValues = values))
    responses <- pct("responses")
    expect_true(all(is.na(responses$b)))
    expect_false(any(is.nan(responses$b)))
    options <- pct("options")
    expect_true(all(is.na(unlist(options[options$var == "q2", c("a", "b", ".TOTAL")]))))
})

test_that("mrcrosstabs: multi-valued variable mode with a single row", {
    r <- vijPlots::mrcrosstabs(data = data.frame(v = factor("a;b;c"), g = factor("x")), mode = "onevar", repVar = "v",
                               group2 = "g", group = NULL, resps = NULL)
    df <- crosstabDF(r)
    options <- df[!(df$var %in% c("Total", "Number of cases")), ]
    expect_setequal(options$var, c("a", "b", "c"))
    expect_equal(options$x, c(1, 1, 1))
})

test_that("mrcrosstabs: a single option keeps its name", {
    d <- data.frame(q1 = factor(c("1", "0", "1", NA)), g = factor(c("x", "x", "y", "y")))
    r <- vijPlots::mrcrosstabs(data = d, resps = "q1", group = "g", group2 = NULL, repVar = NULL)
    expect_equal(crosstabDF(r)$var[1], "q1")
    expect_equal(rownames(r$plot$state), "q1")
    d <- data.frame(v = factor(c("a", "a", NA)), g = factor(c("x", "y", "x")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL)
    expect_equal(crosstabDF(r)$var[1], "a")
})

test_that("mrcrosstabs: plot with a single group", {
    d <- data.frame(q1 = factor(c("1", "0", "1")), q2 = factor(c("1", "1", "0")), g = factor(c("x", "x", "x")))
    r <- vijPlots::mrcrosstabs(data = d, resps = c("q1", "q2"), group = "g", group2 = NULL, repVar = NULL)
    expect_true(is.data.frame(r$plot$state))
    expect_equal(names(r$plot$state), "x")
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_no_error(print(r$plot))
})

test_that("mrcrosstabs: a group level with no case counted 0, plotted without the Total column", {
    d <- data.frame(v = factor(c("a;b", "b", "a")), g = factor(c("x", "x", "y"), levels = c("x", "y", "z")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL)
    expect_equal(names(r$plot$state), c("x", "y", "z"))
    expect_equal(rownames(r$plot$state), c("a", "b"))
    df <- crosstabDF(r)
    expect_equal(df$z, c(0, 0, 0, 0))   # a, b, Total, Number of cases
    expect_equal(df$x, c(1, 2, 3, 2))
})

test_that("mrcrosstabs: a response named like the grouping variable is counted", {
    d <- data.frame(v = factor(c("g;b", "b", "g")), g = factor(c("x", "y", "x")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL)
    df <- crosstabDF(r)
    expect_equal(df$x[df$var == "g"], 2)
    expect_equal(df$y[df$var == "g"], 0)
})

test_that("mrcrosstabs: a group level named Total does not replace the Overall column", {
    d <- data.frame(v = factor(c("a;b", "b", "a")), g = factor(c("Total", "x", "Total")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL)
    df <- crosstabDF(r)
    expect_equal(setdiff(names(df), "var"), c("Total", "x", ".TOTAL"))
    expect_equal(df$.TOTAL, df$Total + df$x)
    expect_equal(names(r$plot$state), c("Total", "x"))
})

test_that("mrcrosstabs: plot with a group variable named Count and a level named Options", {
    d <- data.frame(v = factor(c("a;b", "b", "a")), Count = factor(c("Options", "x", "Options")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "Count", group = NULL, resps = NULL)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_no_error(print(r$plot))
    p <- ggplot2::last_plot()
    expect_equal(p$labels$x, "Count")      # xaxis = "xcols": the groups on the x-axis
    expect_equal(p$labels$fill, "v")
    expect_equal(length(unique(ggplot2::layer_data(p, 1)$fill)), 2)   # options a and b
})

test_that("mrcrosstabs: a group level named var does not replace the options column", {
    d <- data.frame(v = factor(c("a;b", "b", "a")), g = factor(c("var", "x", "var")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL)
    df <- r$crosstab$asDF
    expect_equal(names(df), c("var", ".G.var", ".G.x", ".TOTAL"))
    expect_equal(df$var, c("a", "b", "Total", "Number of cases"))
    expect_equal(df$.G.var, c(2, 1, 3, 2))
    expect_equal(r$crosstab$columns[[2]]$title, "var")
})

test_that("mrcrosstabs: a grouping variable with only missing values gives no result, no error", {
    d <- data.frame(v = factor(c("a;b", "b")), g = factor(c(NA, NA), levels = "x"))
    expect_no_error(r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL))
    expect_null(r$plot$state)
    d <- data.frame(q1 = factor(c("1", "0")), g = factor(c(NA, NA), levels = "x"))
    expect_no_error(r <- vijPlots::mrcrosstabs(data = d, resps = "q1", group = "g", group2 = NULL, repVar = NULL))
    expect_null(r$plot$state)
})

test_that("mrcrosstabs: vertical, labels at the top and manual y range: no extra room beyond the range", {
    d <- data.frame(v = factor(c("a;b", "b", "a")), g = factor(c("x", "x", "y")))
    r <- vijPlots::mrcrosstabs(data = d, mode = "onevar", repVar = "v", group2 = "g", group = NULL, resps = NULL,
                               showLabels = TRUE, labelPosition = "top", yAxisRangeType = "manual", yAxisRangeMin = 0, yAxisRangeMax = 10)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    range <- ggplot2::ggplot_build(ggplot2::last_plot())$layout$panel_params[[1]]$y$continuous_range
    expect_equal(range, c(-0.5, 10.5))   # the default expansion (5 %) only
})
