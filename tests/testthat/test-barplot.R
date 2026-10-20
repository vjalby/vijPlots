testData <- datasets::penguins

test_that("barplot: single variable", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL
    )$plot
    expect_plot_snapshot("barplot-singleVar", testPlot)
})

test_that("barplot: grouped (dodge2)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL
    )$plot
    expect_plot_snapshot("barplot-grouped-dodge2", testPlot)
})

test_that("barplot: grouped (tight dodge)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        barType = "dodge"
    )$plot
    expect_plot_snapshot("barplot-grouped-dodge", testPlot)
})

test_that("barplot: stacked", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        barType = "stack"
    )$plot
    expect_plot_snapshot("barplot-stacked", testPlot)
})

test_that("barplot: stacked, reversed order", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        barType = "stack",
        reverseStack = TRUE
    )$plot
    expect_plot_snapshot("barplot-stacked-reversed", testPlot)
})

test_that("barplot: horizontal", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("barplot-horizontal", testPlot)
})

test_that("barplot: percent within group", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        yaxis = "percent",
        percentWithin = "group"
    )$plot
    expect_plot_snapshot("barplot-percent-withinGroup", testPlot)
})

test_that("barplot: percent within category", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        yaxis = "percent",
        percentWithin = "category"
    )$plot
    expect_plot_snapshot("barplot-percent-withinCategory", testPlot)
})

test_that("barplot: sorted decreasing", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "island",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        order = "decreasing"
    )$plot
    expect_plot_snapshot("barplot-sorted-decreasing", testPlot)
})

test_that("barplot: faceted", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = "island",
        counts = NULL
    )$plot
    expect_plot_snapshot("barplot-faceted", testPlot)
})

test_that("barplot: single color", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        singleColor = TRUE,
        colorNo = 3
    )$plot
    expect_plot_snapshot("barplot-singleColor", testPlot)
})

test_that("barplot: missing values kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("barplot-ignoreNA-false", testPlot)
})

test_that("barplot: missing values kept in percent mode (ignoreNA = FALSE)", {
    # regression test: ggstats::StatProp drops rows whose fill/by is NA, so the NA bars
    # disappeared in percent mode (but not in count mode) and were left out of the denominators
    r <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        ignoreNA = FALSE,
        yaxis = "percent",
        percentWithin = "group"
    )
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    naBars <- built[is.na(as.character(built$by)), ] # NA value or explicit NA level
    expect_equal(nrow(built), 8)
    expect_equal(naBars$count, c(6, 5)) # Adelie, Gentoo
    expect_equal(naBars$prop, c(6, 5) / 11)
    expect_true(all(naBars$fill == "gray"))

    # no group, NA category: kept in the denominator
    r <- vijPlots::barplot(
        data = testData,
        rows = "sex",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        ignoreNA = FALSE,
        yaxis = "percent",
        singleColor = FALSE
    )
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(built$count, c(165, 168, 11))
    expect_equal(sum(built$prop), 1)

    expect_plot_snapshot("barplot-ignoreNA-false-percent", r$plot)
})

test_that("barplot: titles, axis and legend text options", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        titleText = "Penguins by Species and Sex",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "palmerpenguins dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::penguins",
        captionAlign = "1",
        legendText = "Sex",
        legendPosition = "bottom",
        xAxisText = "Species",
        yAxisText = "Number of penguins",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        xTicks = 3,
        yTicks = 5
    )$plot
    expect_plot_snapshot("barplot-titles-axis-legend", testPlot)
})

weightedData <- data.frame(
    cat = factor(c("A", "A", "A", "B", "B", "C")),
    grp = factor(c("X", "Y", "X", "X", "Y", "X"))
)
weightedData$w <- c(10, 20, 5, 30, 15, 3)
# expected weighted totals per cat+grp: A/X=15, A/Y=20, B/X=30, B/Y=15, C/X=3

test_that("barplot: weighted by counts variable", {
    r <- vijPlots::barplot(
        data = weightedData,
        rows = "cat",
        columns = "grp",
        facet = NULL,
        counts = "w"
    )
    # state is aggregated: one row per cat+grp, not one per case
    expect_equal(nrow(r$plot$state), 5)
    expect_equal(r$plot$state$.COUNTS, c(15, 20, 30, 15, 3))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 20, 30, 15, 3)))

    expect_plot_snapshot("barplot-weighted-counts", r$plot)
})

test_that("barplot: weighted via jamovi's built-in weights (jmv-weights attribute)", {
    # regression test: without weightsSupport: 'full' in barplot.a.yaml, jmvcore's
    # Analysis$run() auto-expands rows per jmv-weights *before* .run() sees self$data,
    # leaving a stale jmv-weights attribute that crashes .COUNTS construction
    dataWithAttr <- weightedData[c("cat", "grp")]
    attr(dataWithAttr, "jmv-weights") <- weightedData$w
    attr(dataWithAttr, "jmv-weights-name") <- "w"

    r <- vijPlots::barplot(
        data = dataWithAttr,
        rows = "cat",
        columns = "grp",
        facet = NULL,
        counts = NULL
    )
    # state is aggregated: one row per cat+grp, not one per case
    expect_equal(nrow(r$plot$state), 5)
    expect_equal(r$plot$state$.COUNTS, c(15, 20, 30, 15, 3))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 20, 30, 15, 3)))
})

test_that("barplot: negative counts are rejected", {
    negData <- weightedData
    negData$w[1] <- -3
    expect_error(
        vijPlots::barplot(
            data = negData,
            rows = "cat",
            columns = "grp",
            facet = NULL,
            counts = "w"
        ),
        "Counts may not be negative."
    )
})

test_that("barplot: a variable named like the internal counts column is rejected", {
    reservedData <- data.frame(.COUNTS = weightedData$cat, grp = weightedData$grp)
    expect_error(
        vijPlots::barplot(data = reservedData, rows = ".COUNTS", columns = "grp", facet = NULL, counts = NULL),
        "reserved name"
    )
})

test_that("barplot: frequency table is hidden by default", {
    r <- vijPlots::barplot(data = testData, rows = "species", columns = NULL, facet = NULL, counts = NULL)
    expect_false(r$frequencies$visible)
})

test_that("barplot: frequency table, single variable", {
    r <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        order = "decreasing",
        frequencyTable = TRUE
    )
    expect_true(r$frequencies$visible)
    expect_true(r$frequencies$getColumn("cumPercent")$visible)
    df <- r$frequencies$asDF
    # same sorting as the plot
    expect_equal(df$inner, c("Adelie", "Gentoo", "Chinstrap", "Total"))
    expect_equal(df$counts, c(152, 124, 68, 344))
    expect_equal(df$percent, c(152, 124, 68, 344) / 344)
    expect_equal(df$cumPercent, c(c(152, 276, 344) / 344, NA))
})

test_that("barplot: frequency table, percentages within groups", {
    r <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        frequencyTable = TRUE
    )
    expect_true(r$frequencies$getColumn("cumPercent")$visible)
    df <- r$frequencies$asDF
    expect_equal(df$outer, rep(c("female", "male"), each = 4))
    expect_equal(df$inner, rep(c("Adelie", "Chinstrap", "Gentoo", "Total"), 2))
    expect_equal(df$counts, c(73, 34, 58, 165, 73, 34, 61, 168))
    expect_equal(df$percent, c(c(73, 34, 58, 165) / 165, c(73, 34, 61, 168) / 168))
    # cumulative % restarts in each group, blank on the Total rows
    expect_equal(df$cumPercent, c(c(73, 107, 165) / 165, NA, c(73, 107, 168) / 168, NA))
})

test_that("barplot: frequency table matches the plot", {
    # within categories, with facets and weights
    tableData <- testData
    tableData$w <- seq_len(nrow(tableData)) %% 3 + 1
    renderBoth <- function(yaxis, ignoreNA) {
        r <- vijPlots::barplot(
            data = tableData,
            rows = "species",
            columns = "sex",
            facet = "island",
            counts = "w",
            ignoreNA = ignoreNA,
            yaxis = yaxis,
            percentWithin = "category",
            frequencyTable = TRUE
        )
        df <- r$frequencies$asDF
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        list(table = df[df$inner != "Total", ], bars = ggplot2::layer_data(ggplot2::last_plot(), 1))
    }
    res <- renderBoth("percent", TRUE)
    df <- res$table
    # every block (facet x category) sums to 100 %, an empty category in a facet is skipped
    expect_false(any(df$facet == "Biscoe" & df$outer == "Chinstrap"))
    expect_true(all(abs(tapply(df$percent, paste(df$facet, df$outer), sum) - 1) < 1e-9))
    # cumulative % reaches 100 % at the end of every block
    expect_true(all(abs(tapply(df$cumPercent, paste(df$facet, df$outer), max) - 1) < 1e-9))
    # the plot's bars, compared with the table's non-zero rows
    expect_equal(sort(res$bars$prop), sort(df$percent[df$counts > 0]))
    expect_equal(sort(res$bars$count), sort(df$counts[df$counts > 0]))

    # missing values kept: the NA group is a block of the table, as it is a bar of the count plot
    res <- renderBoth("count", FALSE)
    df <- res$table
    expect_true("NA" %in% df$outer)
    expect_equal(sort(res$bars$count), sort(df$counts[df$counts > 0]))
})

test_that("barplot: frequency table ignores percentWithin in count mode", {
    # percentWithin is disabled in the UI when yaxis is count: always within groups
    r <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        yaxis = "count",
        percentWithin = "category",
        frequencyTable = TRUE
    )
    expect_equal(r$frequencies$getColumn("outer")$title, "sex")
    expect_equal(r$frequencies$getColumn("inner")$title, "species")
    df <- r$frequencies$asDF
    expect_equal(df$outer, rep(c("female", "male"), each = 4))
    expect_equal(df$percent, c(c(73, 34, 58, 165) / 165, c(73, 34, 61, 168) / 168))
})

test_that("barplot: frequency table note on excluded missing values", {
    r <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        frequencyTable = TRUE
    )
    # 11 penguins without sex, left out overall (not per block)
    expect_equal(r$frequencies$notes$missing$note, "Missing values: 11")
    # missing values kept: they are in the table, no note
    r <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        ignoreNA = FALSE,
        frequencyTable = TRUE
    )
    expect_null(r$frequencies$notes$missing)
})

test_that("barplot: invalid weights are rejected even on a row with a missing value", {
    weightData <- data.frame(cat = factor(c("a", "b", NA)), w = c(2, 3, NA))
    # a missing weight is invalid data, even with ignoreNA checked
    expect_error(
        vijPlots::barplot(data = weightData, rows = "cat", columns = NULL, facet = NULL, counts = "w", ignoreNA = TRUE),
        "Counts may not contain missing values."
    )
    # a negative weight on a row left out for its missing category
    weightData$w[3] <- -1
    expect_error(
        vijPlots::barplot(data = weightData, rows = "cat", columns = NULL, facet = NULL, counts = "w", ignoreNA = TRUE),
        "Counts may not be negative."
    )
})

test_that("barplot: zero weights are treated as no data", {
    zeroData <- data.frame(
        cat = factor(c("a", "b", "a", "a", "b")),
        grp = factor(c("g1", "g1", "g2", "g1", "g2")),
        fac = factor(c("x", "x", "x", "y", "y")),
        w = c(0, 0, 5, 10, 0)
    )
    # a facet whose weights are all zero: no panel, no table block
    r <- vijPlots::barplot(data = zeroData, rows = "cat", columns = NULL, facet = "fac", counts = "w",
                           frequencyTable = TRUE)
    expect_false(any(r$plot$state$.COUNTS == 0))
    df <- r$frequencies$asDF
    expect_equal(unique(df$facet), c("x", "y"))
    expect_equal(df$counts, c(5, 0, 5, 10, 0, 10))
    expect_false(any(is.nan(df$percent)))
    # a group whose weights are all zero in a facet: skipped like a group without data
    r <- vijPlots::barplot(data = zeroData, rows = "cat", columns = "grp", facet = "fac", counts = "w",
                           frequencyTable = TRUE)
    df <- r$frequencies$asDF
    # facet x: only g2 (g1 has zero weights); facet y: only g1
    expect_equal(unique(paste(df$facet, df$outer)), c("x g2", "y g1"))
    expect_false(any(is.nan(df$percent)))
})

test_that("barplot: grid lines (vertical)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        yGridLines = TRUE
    )$plot
    expect_plot_snapshot("barplot-gridlines-vertical", testPlot)
})

test_that("barplot: grid lines (horizontal)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        horizontal = TRUE,
        xGridLines = TRUE
    )$plot
    expect_plot_snapshot("barplot-gridlines-horizontal", testPlot)
})
