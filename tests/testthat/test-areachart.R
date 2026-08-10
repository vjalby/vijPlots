testData <- data.frame(
    Month = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE),
    Var1 = c(15L, 12L, 16L, 15L, 14L, 13L, 12L, 13L, 14L, 15L, 16L, 18L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    Var2 = c(10L, 11L, 12L, 15L, 14L, 15L, 16L, 18L, 19L, 15L, 12L, 13L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    dateStd = factor(c("2023-01-01", "2023-01-02", "2023-01-15", "2023-01-30", "2023-02-01", "2023-02-15", "2023-03-01", "2023-04-01", "2023-05-01", "2023-06-01", "2023-07-01", "2023-09-01", "2023-10-01", "2023-11-02", "2023-12-01", "2023-12-31", NA, NA, NA, NA, NA, NA, NA, NA), levels = c("2023-01-01", "2023-01-02", "2023-01-15", "2023-01-30", "2023-02-01", "2023-02-15", "2023-03-01", "2023-04-01", "2023-05-01", "2023-06-01", "2023-07-01", "2023-09-01", "2023-10-01", "2023-11-02", "2023-12-01", "2023-12-31"), ordered = TRUE),
    dateFR1 = factor(c("01/01/2023", "02/01/2023", "15/01/2023", "30/01/2023", "01/02/2023", "15/02/2023", "01/03/2023", "01/04/2023", "01/05/2023", "01/06/2023", "01/07/2023", "01/09/2023", "01/10/2023", "02/11/2023", "01/12/2023", "31/12/2023", NA, NA, NA, NA, NA, NA, NA, NA), levels = c("01/01/2023", "02/01/2023", "15/01/2023", "30/01/2023", "01/02/2023", "15/02/2023", "01/03/2023", "01/04/2023", "01/05/2023", "01/06/2023", "01/07/2023", "01/09/2023", "01/10/2023", "02/11/2023", "01/12/2023", "31/12/2023"), ordered = TRUE),
    dateUS1 = factor(c("01/01/2023", "01/02/2023", "01/15/2023", "01/30/2023", "02/01/2023", "02/15/2023", "03/01/2023", "04/01/2023", "05/01/2023", "06/01/2023", "07/01/2023", "09/01/2023", "10/01/2023", "11/02/2023", "12/01/2023", "12/31/2023", NA, NA, NA, NA, NA, NA, NA, NA), levels = c("01/01/2023", "01/02/2023", "01/15/2023", "01/30/2023", "02/01/2023", "02/15/2023", "03/01/2023", "04/01/2023", "05/01/2023", "06/01/2023", "07/01/2023", "11/02/2023", "12/01/2023", "12/31/2023", "09/01/2023", "10/01/2023"), ordered = TRUE),
    Var3 = c(10L, 11L, 13L, 13L, 15L, 16L, 19L, 20L, 19L, 20L, 17L, 16L, 16L, 15L, 12L, 11L, NA, NA, NA, NA, NA, NA, NA, NA),
    Var4 = c(6L, 6L, 8L, 10L, 9L, 11L, 13L, 15L, 16L, 14L, 14L, 12L, 12L, 9L, 7L, 6L, NA, NA, NA, NA, NA, NA, NA, NA),
    Month2 = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    Group = factor(c("One", "One", "One", "One", "One", "One", "One", "One", "One", "One", "One", "One", "Two", "Two", "Two", "Two", "Two", "Two", "Two", "Two", "Two", "Two", "Two", "Two"), levels = c("One", "Two")),
    VarA = c(10L, 11L, 13L, 13L, 16L, 17L, 18L, 15L, 13L, 10L, 10L, 8L, 4L, 4L, 5L, 7L, 8L, 10L, 11L, 10L, 9L, 7L, 5L, 3L),
    VarB = c(15L, 15L, 18L, 17L, 21L, 23L, 23L, 20L, 18L, 14L, 14L, 13L, 8L, 9L, 9L, 11L, 13L, 14L, 15L, 15L, 13L, 11L, 10L, 7L)
)

test_that("areachart: one variable, 2 groups, stacked", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month2",
        timeVar1 = NULL,
        vars = NULL,
        var = "VarA",
        group = "Group",
        position = "stack"
    )$plot
    expect_plot_snapshot("areachart-oneVar-twoGroups-stack", testPlot)
})

test_that("areachart: several variables stacked, with date", {
    testPlot <- vijPlots::areachart(
        mode = "severalVariables",
        data = testData,
        timeVar = NULL,
        timeVar1 = "dateUS1",
        vars = c("Var3", "Var4"),
        var = NULL,
        group = NULL,
        isDate = TRUE,
        position = "stack"
    )$plot
    expect_plot_snapshot("areachart-severalVars-date-stack", testPlot)
})

test_that("areachart: one variable, 2 groups, 100% stacked (fill)", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month2",
        timeVar1 = NULL,
        vars = NULL,
        var = "VarA",
        group = "Group",
        position = "fill"
    )$plot
    expect_plot_snapshot("areachart-oneVar-twoGroups-fill", testPlot)
})

test_that("areachart: one variable, 2 groups, overlapping (identity)", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month2",
        timeVar1 = NULL,
        vars = NULL,
        var = "VarA",
        group = "Group",
        position = "identity"
    )$plot
    expect_plot_snapshot("areachart-oneVar-twoGroups-identity", testPlot)
})

test_that("areachart: one variable, no group", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month",
        timeVar1 = NULL,
        vars = NULL,
        var = "Var1",
        group = NULL
    )$plot
    expect_plot_snapshot("areachart-oneVar-noGroup", testPlot)
})

test_that("areachart: several variables mode with a single variable", {
    testPlot <- vijPlots::areachart(
        mode = "severalVariables",
        data = testData,
        timeVar = NULL,
        timeVar1 = "Month",
        vars = "Var1",
        var = NULL,
        group = NULL
    )$plot
    expect_plot_snapshot("areachart-severalVars-singleVar", testPlot)
})

test_that("areachart: no line border", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month2",
        timeVar1 = NULL,
        vars = NULL,
        var = "VarA",
        group = "Group",
        showLine = FALSE
    )$plot
    expect_plot_snapshot("areachart-noLine", testPlot)
})

test_that("areachart: custom line width", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month2",
        timeVar1 = NULL,
        vars = NULL,
        var = "VarA",
        group = "Group",
        lineWidth = 3
    )$plot
    expect_plot_snapshot("areachart-lineWidth", testPlot)
})

test_that("areachart: manual y-axis range and ticks", {
    testPlot <- vijPlots::areachart(
        mode = "severalVariables",
        data = testData,
        timeVar = NULL,
        timeVar1 = "dateUS1",
        vars = c("Var3", "Var4"),
        var = NULL,
        group = NULL,
        isDate = TRUE,
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 40,
        yTicks = 4
    )$plot
    expect_plot_snapshot("areachart-manualYRange", testPlot)
})

test_that("areachart: titles, axis and legend text options", {
    testPlot <- vijPlots::areachart(
        mode = "oneVariable",
        data = testData,
        timeVar = "Month2",
        timeVar1 = NULL,
        vars = NULL,
        var = "VarA",
        group = "Group",
        titleText = "Monthly value by group",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "fictitious time series",
        subtitleFontFace = "italic",
        captionText = "Source: fictitious data",
        captionAlign = "1",
        legendText = "Group",
        legendPosition = "bottom",
        xAxisText = "Month",
        yAxisText = "Value",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14
    )$plot
    expect_plot_snapshot("areachart-titles-axis-legend", testPlot)
})
