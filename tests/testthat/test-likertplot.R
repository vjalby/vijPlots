testData <- data.frame(
    `Question 1` = factor(c("Strongly disagree", "Strongly agree", "Neither agree nor disagree", "Disagree", "Neither agree nor disagree", "Strongly disagree", "Agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Strongly disagree", "Strongly agree", "Strongly disagree", "Neither agree nor disagree", "Strongly agree", "Strongly disagree", "Strongly disagree", "Neither agree nor disagree", NA, NA), levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"), ordered = TRUE),
    `Question 2` = factor(c("Strongly agree", "Agree", "Agree", "Strongly agree", "Strongly disagree", "Neither agree nor disagree", "Disagree", "Strongly agree", "Strongly agree", "Disagree", "Neither agree nor disagree", "Strongly disagree", "Agree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Strongly disagree", "Disagree", NA, NA, NA, "Disagree"), levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"), ordered = TRUE),
    `Question 3` = factor(c("Strongly disagree", "Strongly agree", "Agree", "Neither agree nor disagree", "Strongly disagree", "Neither agree nor disagree", "Disagree", "Strongly agree", "Strongly disagree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Disagree", "Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree", "Strongly agree", "Agree", "Disagree", NA, NA, "Agree"), levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"), ordered = TRUE),
    Sex = factor(c("Male", "Male", "Male", "Female", "Male", "Female", "Female", "Male", "Female", "Female", "Male", "Male", "Female", "Female", "Female", "Male", "Female", "Male", "Female", "Female", "Female", "Male", "Female", NA), levels = c("Male", "Female")),
    Group = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "C", NA, "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C"), levels = c("A", "B", "C")),
    Q1 = c(1L, 5L, 3L, 2L, 3L, 1L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 1L, 5L, 1L, 3L, 5L, 1L, NA, 3L, NA, 1L),
    Q2 = factor(c("Strongly agree", "Agree", "Agree", "Strongly agree", "Strongly disagree", "Neither agree nor disagree", "Disagree", "Strongly agree", "Strongly agree", "Disagree", "Neither agree nor disagree", "Strongly disagree", "Agree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Strongly disagree", "Disagree", "Disagree", NA, NA, "Disagree"), levels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"), ordered = TRUE),
    Q3 = c(1L, 5L, 4L, 3L, 1L, 3L, 2L, 5L, 1L, 1L, 2L, 3L, 2L, 5L, 4L, 3L, 2L, 1L, 5L, 4L, 2L, 1L, NA, 4L),
    check.names = FALSE
)

# jamovi tags ordinal factors with a "values" attribute (the level's underlying numeric code),
# used by likertplot's toInteger/tidyUp logic
for (col in c("Question 1", "Question 2", "Question 3", "Q2"))
    attr(testData[[col]], "values") <- 1:5

test_that("likertplot: frequency table (counts), group = Sex", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        frequencyTable = TRUE,
        frequencies = "counts"
    )
    freq <- r$frequencies$asDF
    expect_equal(unname(freq$.question), c("Question 1", " ", "Question 2", " ", "Question 3", " "))
    expect_equal(unname(as.character(freq$Sex)), c("Male", "Female", "Male", "Female", "Male", "Female"))
    expect_equal(unname(freq$Sum), c(10L, 12L, 9L, 11L, 9L, 12L))
    expect_equal(unname(freq$`Strongly disagree`), c(3L, 5L, 3L, 1L, 3L, 2L))
    expect_equal(unname(freq$`Strongly agree`), c(2L, 1L, 2L, 3L, 2L, 2L))
})

test_that("likertplot: frequency table (percentages, median, mean), group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        frequencyTable = TRUE,
        frequencies = "percentages",
        showMedian = TRUE,
        showMean = TRUE
    )
    freq <- r$frequencies$asDF
    expect_equal(unname(freq$.question), c("Question 1", " ", " ", "Question 2", " ", " ", "Question 3", " ", " "))
    expect_equal(unname(as.character(freq$Group)), c("A", "B", "C", "A", "B", "C", "A", "B", "C"))
    expect_equal(unname(freq$Sum), c(7L, 7L, 7L, 6L, 7L, 7L, 6L, 7L, 8L))
    expect_equal(unname(freq$`Strongly disagree`), c(0.1428571429, 0.4285714286, 0.5714285714, 0.3333333333, 0.1428571429, 0.1428571429, 0.1666666667, 0.1428571429, 0.25), tolerance = 1e-6)
    expect_equal(unname(freq$`Strongly agree`), c(0.2857142857, 0.1428571429, 0, 0.3333333333, 0.1428571429, 0.2857142857, 0.1666666667, 0.4285714286, 0), tolerance = 1e-6)
    expect_equal(unname(freq$Median), c(3, 3, 1, 3, 3, 3, 2.5, 4, 3))
    expect_equal(unname(freq$Mean), c(3.142857143, 2.571428571, 1.714285714, 3, 3, 3.285714286, 2.666666667, 3.428571429, 2.75), tolerance = 1e-6)
    expect_equal(unname(freq$SD), c(1.573591585, 1.618347187, 0.9511897312, 1.897366596, 1.414213562, 1.496026483, 1.366260102, 1.718249386, 1.281739889), tolerance = 1e-6)
})

test_that("likertplot: Mann-Whitney U test, group = Sex", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        showMannU = TRUE
    )
    uTest <- r$comp$uTestTable$asDF
    expect_equal(unname(uTest$ques), c("Question 1", "Question 2", "Question 3"))
    expect_equal(unname(uTest$statistic), c(43, 45.5, 52), tolerance = 1e-6)
    expect_equal(unname(uTest$p.value), c(0.2578876232, 0.7400690641, 0.8907358895), tolerance = 1e-6)
})

test_that("likertplot: Mann-Whitney U test with overall p-value adjustment, group = Sex", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        showMannU = TRUE,
        pValue = "overall"
    )
    uTest <- r$comp$uTestTable$asDF
    expect_equal(unname(uTest$statistic), c(43, 45.5, 52), tolerance = 1e-6)
    expect_equal(unname(uTest$p.value), c(0.2578876232, 0.7400690641, 0.8907358895), tolerance = 1e-6)
    expect_equal(unname(uTest$adjusted.p), c(0.7736628696, 1, 1), tolerance = 1e-6)
})

test_that("likertplot: Mann-Whitney U test refuses more than two groups", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showMannU = TRUE
    )
    uTest <- r$comp$uTestTable$asDF
    expect_equal(unname(uTest$statistic), c(NA, NA, NA))
    expect_equal(unname(uTest$p.value), c(NA, NA, NA))
    expect_equal(r$comp$uTestTable$notes$p$note, "Mann-Whitney tests require two groups")
})

test_that("likertplot: Kruskal-Wallis test, group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showKW = TRUE
    )
    kw <- r$comp$kwTable$asDF
    expect_equal(unname(kw$ques), c("Question 1", "Question 2", "Question 3"))
    expect_equal(unname(kw$statistic), c(3.227550305, 0.1645021645, 1.067125161), tolerance = 1e-6)
    expect_equal(unname(kw$parameter), c(2L, 2L, 2L))
    expect_equal(unname(kw$p.value), c(0.1991344304, 0.9210406727, 0.586511748), tolerance = 1e-6)
})

test_that("likertplot: Kruskal-Wallis test with overall p-value adjustment, group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showKW = TRUE,
        pValue = "overall"
    )
    kw <- r$comp$kwTable$asDF
    expect_equal(unname(kw$p.value), c(0.1991344304, 0.9210406727, 0.586511748), tolerance = 1e-6)
    expect_equal(unname(kw$adjusted.p), c(0.5974032912, 1, 1), tolerance = 1e-6)
})

test_that("likertplot: Post-Hoc Dunn test, group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showPostHoc = TRUE,
        postHoc = "dunn"
    )
    pw <- r$comp$pwTable$asDF
    expect_equal(unname(pw$group1), c("A", "A", "B"))
    expect_equal(unname(pw$group2), c("B", "C", "C"))
    expect_equal(unname(pw$`Question 1 stat`), c(0.7606816023, 1.789839064, 1.029157462), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 1 p`), c(0.4468472661, 0.07347978717, 0.3034056877), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 stat`), c(0.02954133592, -0.3249546951, -0.3689711674), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 p`), c(0.976432852, 0.7452153605, 0.7121492159), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 stat`), c(-0.9146315305, -0.1078110865, 0.8706986573), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 p`), c(0.3603851002, 0.9141455484, 0.3839187118), tolerance = 1e-6)
})

test_that("likertplot: Post-Hoc Conover test, group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showPostHoc = TRUE,
        postHoc = "conover"
    )
    pw <- r$comp$pwTable$asDF
    expect_equal(unname(pw$`Question 1 stat`), c(0.7880266704, 1.854180401, 1.06615373), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 1 p`), c(0.4409359756, 0.08017322103, 0.3004465138), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 stat`), c(0.0280650642, -0.3087157062, -0.3505325396), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 p`), c(0.9779370935, 0.7612889374, 0.7302449663), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 stat`), c(-0.8918136997, -0.1051214623, 0.8489768448), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 p`), c(0.3842597521, 0.917441995, 0.4070438133), tolerance = 1e-6)
})

test_that("likertplot: Post-Hoc DSCF test, group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showPostHoc = TRUE,
        postHoc = "dscf"
    )
    pw <- r$comp$pwTable$asDF
    expect_equal(unname(pw$`Question 1 stat`), c(1.016449276, 2.521138001, 1.465730457), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 1 p`), c(0.7523910599, 0.1753683077, 0.5538276888), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 stat`), c(-0, -0.4138404217, -0.552524893), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 p`), c(1, 0.9538938976, 0.9193128571), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 stat`), c(-1.042136486, -0.3743097317, 1.420618325), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 p`), c(0.7415270083, 0.9621191115, 0.5739789704), tolerance = 1e-6)
})

test_that("likertplot: Post-Hoc Dunn test with groupwise p-value adjustment, group = Group", {
    r <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        showPostHoc = TRUE,
        postHoc = "dunn",
        pValue = "group"
    )
    pw <- r$comp$pwTable$asDF
    expect_equal(unname(pw$`Question 1 p.adj`), c(0.6068113754, 0.2204393615, 0.6068113754), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 2 p.adj`), c(1, 1, 1), tolerance = 1e-6)
    expect_equal(unname(pw$`Question 3 p.adj`), c(1, 1, 1), tolerance = 1e-6)
})

test_that("likertplot: centered plot, no group", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        type = "centered"
    )$plot
    expect_plot_snapshot("likertplot-centered-noGroup", testPlot)
})

test_that("likertplot: centered plot, group = Sex", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        type = "centered"
    )$plot
    expect_plot_snapshot("likertplot-centered-sex", testPlot)
})

test_that("likertplot: centered plot, group = Group (3 levels)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Group",
        type = "centered"
    )$plot
    expect_plot_snapshot("likertplot-centered-group", testPlot)
})

test_that("likertplot: stacked plot, no group", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        type = "stacked"
    )$plot
    expect_plot_snapshot("likertplot-stacked-noGroup", testPlot)
})

test_that("likertplot: stacked plot, group = Sex", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        type = "stacked"
    )$plot
    expect_plot_snapshot("likertplot-stacked-sex", testPlot)
})

test_that("likertplot: grouped by group instead of variable (groupBy = group)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        type = "centered",
        groupBy = "group"
    )$plot
    expect_plot_snapshot("likertplot-groupBy-group", testPlot)
})

test_that("likertplot: sorted by median (descending)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        sorting = "descending"
    )$plot
    expect_plot_snapshot("likertplot-sorted-descending", testPlot)
})

test_that("likertplot: reverse stacking order", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        type = "stacked",
        reverseLikert = TRUE
    )$plot
    expect_plot_snapshot("likertplot-reverse", testPlot)
})

test_that("likertplot: hide % labels below 5%", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        hideLabelsBelow = TRUE
    )$plot
    expect_plot_snapshot("likertplot-hideLabelsBelow", testPlot)
})

test_that("likertplot: centered plot without totals", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        type = "centered",
        addTotals = FALSE
    )$plot
    expect_plot_snapshot("likertplot-centered-noTotals", testPlot)
})

test_that("likertplot: stacked plot without median line", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = NULL,
        type = "stacked",
        addMedianLine = FALSE
    )$plot
    expect_plot_snapshot("likertplot-stacked-noMedianLine", testPlot)
})

test_that("likertplot: missing group level kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("likertplot-ignoreNA-false", testPlot)
})

test_that("likertplot: titles, axis and legend text options", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Question 2", "Question 3"),
        group = "Sex",
        titleText = "Survey results by Sex",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "3 Likert questions",
        subtitleFontFace = "italic",
        captionText = "Source: fictitious survey data",
        captionAlign = "1",
        legendText = "Response",
        legendPosition = "bottom",
        yAxisLabelFontSize = 14,
        xAxisLabelRotation = 0
    )$plot
    expect_plot_snapshot("likertplot-titles-axis-legend", testPlot)
})

test_that("likertplot: mixing types without toInteger is rejected", {
    expect_error(
        vijPlots::likertplot(
            data = testData,
            liks = c("Question 1", "Q1"),
            group = NULL,
            toInteger = FALSE
        ),
        "Select the \"Convert variables to integers\" option when the variables are not of the same type."
    )
})

test_that("likertplot: tidyUp alone does not bypass the same-type check", {
    expect_error(
        vijPlots::likertplot(
            data = testData,
            liks = c("Question 1", "Q1"),
            group = NULL,
            toInteger = FALSE,
            tidyUp = TRUE
        ),
        "Select the \"Convert variables to integers\" option when the variables are not of the same type."
    )
})

test_that("likertplot: toInteger reconciles an ordered factor with a plain integer (Question 1 + Q1)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Q1"),
        group = NULL,
        toInteger = TRUE
    )$plot
    expect_plot_snapshot("likertplot-toInteger-mixed", testPlot)
})

test_that("likertplot: toInteger + tidyUp with three mixed-type variables (Question 1, Q2, Q3)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Q2", "Q3"),
        group = NULL,
        toInteger = TRUE,
        tidyUp = TRUE
    )$plot
    expect_plot_snapshot("likertplot-toInteger-tidyUp-mixed", testPlot)
})

test_that("likertplot: two plain integer variables work without conversion (Q1 + Q3)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Q1", "Q3"),
        group = NULL
    )$plot
    expect_plot_snapshot("likertplot-plainIntegers", testPlot)
})

test_that("likertplot: tidyUp alone reconciles two already-labeled variables (Question 1 + Q2)", {
    testPlot <- vijPlots::likertplot(
        data = testData,
        liks = c("Question 1", "Q2"),
        group = NULL,
        tidyUp = TRUE
    )$plot
    expect_plot_snapshot("likertplot-tidyUp-labeled", testPlot)
})

