testData <- data.frame(
    STAFF = factor(c("Senior Managers", "Senior Managers", "Senior Managers", "Senior Managers", "Junior Managers", "Junior Managers", "Junior Managers", "Junior Managers", "Senior Employees", "Senior Employees", "Senior Employees", "Senior Employees", "Junior Employees", "Junior Employees", "Junior Employees", "Junior Employees", "Secretaries", "Secretaries", "Secretaries", "Secretaries", "Senior Managers", "Junior Managers", "Junior Managers", "Senior Employees", "Senior Employees", "Junior Employees", "Junior Employees", "Secretaries", "Secretaries"), levels = c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries")),
    SMOKE = factor(c("None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "None", "Light", "Medium", "Heavy", "Alcohol", "No Alcohol", "Alcohol", "No Alcohol", "Alcohol", "No Alcohol", "Alcohol", "No Alcohol", "Alcohol"), levels = c("None", "Light", "Medium", "Heavy", "No Alcohol", "Alcohol"), ordered = TRUE),
    COUNT = c(4L, 2L, 3L, 2L, 4L, 3L, 7L, 4L, 25L, 10L, 12L, 4L, 18L, 24L, 33L, 13L, 10L, 6L, 7L, 2L, 11L, 1L, 17L, 5L, 46L, 10L, 78L, 7L, 18L)
)

test_that("corresp: inertia (eigenvalues) table", {
    r <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT"
    )
    eig <- r$eigenvalues$asDF
    expect_equal(unname(eig$dim), c("1", "2", "Total"))
    expect_equal(unname(eig$inertia), c(0.03947232601, 0.02254491281, 0.06201723883), tolerance = 1e-6)
    expect_equal(unname(eig$proportion), c(0.6087989096, 0.3477200288, 1), tolerance = 1e-6)
    expect_equal(r$eigenvalues$notes$chisq$note, "X-squared = 25.03, df = 20,\n                               p-value = 0.20041")
})

test_that("corresp: contingency table", {
    r <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showContingency = TRUE
    )
    ct <- r$contingency$asDF
    expect_equal(unname(ct$STAFF), c("Senior Managers", "Junior Managers", "Senior Employees", "Junior Employees", "Secretaries", "Active Margin"))
    expect_equal(unname(ct$None), c(4, 4, 25, 18, 10, 61))
    expect_equal(unname(ct$`Active Margin`), c(22, 36, 102, 176, 50, 386))
})

test_that("corresp: row and column summary tables (first row)", {
    r <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        showSummaries = TRUE
    )
    rowSum <- r$rowSummary$asDF[1,]
    expect_equal(unname(rowSum$row), "Senior Managers")
    expect_equal(unname(rowSum$margin), 0.05699481865, tolerance = 1e-6)
    expect_equal(unname(rowSum$score1), -0.05409754112, tolerance = 1e-6)
    expect_equal(unname(rowSum$score2), -0.2913131521, tolerance = 1e-6)
    expect_equal(unname(rowSum$qlt), 0.9637078448, tolerance = 1e-6)

    colSum <- r$colSummary$asDF[1,]
    expect_equal(unname(colSum$col), "None")
    expect_equal(unname(colSum$margin), 0.1580310881, tolerance = 1e-6)
    expect_equal(unname(colSum$score1), 0.3735976041, tolerance = 1e-6)
    expect_equal(unname(colSum$score2), -0.1257482436, tolerance = 1e-6)
    expect_equal(unname(colSum$qlt), 0.9984944007, tolerance = 1e-6)
})

test_that("corresp: row plot", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT"
    )$rowplot
    expect_plot_snapshot("corresp-rowplot", testPlot)
})

test_that("corresp: column plot", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT"
    )$colplot
    expect_plot_snapshot("corresp-colplot", testPlot)
})

test_that("corresp: biplot", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT"
    )$biplot
    expect_plot_snapshot("corresp-biplot", testPlot)
})

test_that("corresp: alcohol columns as supplementary categories", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        supplementaryCols = "5,6"
    )$biplot
    expect_plot_snapshot("corresp-supplementaryCols", testPlot)
})

test_that("corresp: symmetric normalization", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        normalization = "symmetric"
    )$biplot
    expect_plot_snapshot("corresp-symmetric", testPlot)
})

test_that("corresp: sorted rows as supplementary (row-principal normalization)", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        normalization = "rowprincipal"
    )$biplot
    expect_plot_snapshot("corresp-rowprincipal", testPlot)
})

test_that("corresp: titles, axis and legend text options", {
    testPlot <- vijPlots::corresp(
        data = testData,
        mode = "obsTable",
        rows = "STAFF",
        cols = "SMOKE",
        columns = NULL,
        rowLabels = NULL,
        counts = "COUNT",
        biplotTitleText = "Staff smoking habits",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        biplotSubtitleText = "Correspondence analysis",
        subtitleFontFace = "italic",
        biplotCaptionText = "Source: fictitious survey data",
        captionAlign = "1"
    )$biplot
    expect_plot_snapshot("corresp-titles-axis-legend", testPlot)
})
