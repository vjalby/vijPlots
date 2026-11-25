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

test_that("barplot: hidden y axis", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        yHideAxis = TRUE
    )$plot
    expect_plot_snapshot("barplot-hideAxis-y", testPlot)
})

test_that("barplot: hidden category axis, bars start at the baseline", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        xHideAxis = TRUE
    )$plot
    expect_plot_snapshot("barplot-hideAxis-x", testPlot)
})

test_that("barplot: hidden category axis keeps a manual value range", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        xHideAxis = TRUE,
        yAxisRangeType = "manual",
        yAxisRangeMin = 0,
        yAxisRangeMax = 200
    )$plot
    expect_plot_snapshot("barplot-hideAxis-x-manual", testPlot)
})

test_that("barplot: hidden x axis follows the displayed axis when horizontal", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = NULL,
        facet = NULL,
        counts = NULL,
        horizontal = TRUE,
        xHideAxis = TRUE
    )$plot
    expect_plot_snapshot("barplot-hideAxis-x-horizontal", testPlot)
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
    expect_true("(Missing)" %in% df$outer)
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

test_that("barplot: missing values are labelled (Missing) on the axis, legend and facets", {
    naData <- testData
    naData$isl <- naData$island
    naData$isl[1:4] <- NA
    for (yaxis in c("count", "percent")) {
        # NA group (sex) in the legend, NA facet
        r <- vijPlots::barplot(data = naData, rows = "species", columns = "sex", facet = "isl",
                               counts = NULL, ignoreNA = FALSE, yaxis = yaxis)
        grDevices::pdf(NULL)
        print(r$plot)
        p <- ggplot2::last_plot()
        built <- ggplot2::ggplot_build(p)
        expect_equal(built$plot$scales$get_scales("fill")$get_labels(), c("female", "male", "(Missing)"))
        expect_true("gray" %in% ggplot2::layer_data(p, 1)$fill)
        gt <- ggplot2::ggplot_gtable(built)
        strips <- unlist(lapply(gt$grobs[grepl("^strip", gt$layout$name)], function(g) {
            unlist(lapply(g$grobs, function(s) tryCatch(s$children[[2]]$children[[1]]$label, error = function(e) NULL)))
        }))
        expect_true("(Missing)" %in% strips)
        expect_false("NA" %in% strips)
        # NA category (sex) on the axis
        r <- vijPlots::barplot(data = naData, rows = "sex", columns = NULL, facet = NULL,
                               counts = NULL, ignoreNA = FALSE, yaxis = yaxis)
        print(r$plot)
        built <- ggplot2::ggplot_build(ggplot2::last_plot())
        expect_equal(built$layout$panel_params[[1]]$x$get_labels(), c("female", "male", "(Missing)"))
        grDevices::dev.off()
    }
})

test_that("barplot: count labels keep the decimals of weighted counts", {
    decData <- data.frame(cat = factor(c("a", "b", "a")), w = c(1.5, 2, 0.25))
    labelsOf <- function(...) {
        r <- vijPlots::barplot(data = decData, rows = "cat", columns = NULL, facet = NULL, counts = "w", ...)
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        ggplot2::layer_data(ggplot2::last_plot(), 2)$label
    }
    expect_equal(labelsOf(), c("1.75", "2"))
    # horizontal, labels at the bar's end: after two spaces (fixed gap), no longer truncated to integers
    expect_equal(labelsOf(horizontal = TRUE, labelPosition = "top"), c("  1.75", "  2"))
})

test_that("barplot: horizontal, labels at the bars' end (fixed gap)", {
    testPlot <- vijPlots::barplot(
        data = testData,
        rows = "species",
        columns = "sex",
        facet = NULL,
        counts = NULL,
        horizontal = TRUE,
        labelPosition = "top"
    )$plot
    expect_plot_snapshot("barplot-horizontal-labels-top", testPlot)
})

test_that("barplot: stacked, label position Top left selected: no extra room (labels in the middle)", {
    axisRange <- function(labelPosition) {
        r <- vijPlots::barplot(data = testData, rows = "species", columns = "sex", facet = NULL, counts = NULL,
                               barType = "stack", showLabels = TRUE, labelPosition = labelPosition)
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        ggplot2::ggplot_build(ggplot2::last_plot())$layout$panel_params[[1]]$y$continuous_range
    }
    expect_equal(axisRange("top"), axisRange("middle"))
})

# 3 categories x 5 groups, with small groups (g4, g5): shares of 4.1% (A-g5), 4.0% (B-g4), 1.6% (B-g5), 4.7% (C-g4)
# and 3.9% (C-g5) within the categories, none under 5% within the groups
hideCounts <- data.frame(
    category = rep(c("A", "B", "C"), each = 5),
    group = rep(paste0("g", 1:5), 3),
    n = c(71, 41, 17, 12, 6, 67, 35, 16, 5, 2, 64, 40, 13, 6, 5))
hideData <- hideCounts[rep(seq_len(nrow(hideCounts)), hideCounts$n), c("category", "group")]

# Labels of the text layer (NA: hidden), in the order of the layer's data
hideLabels <- function(...) {
    r <- vijPlots::barplot(data = hideData, rows = "category", columns = "group", facet = NULL, counts = NULL,
                           showLabels = TRUE, ...)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    p <- ggplot2::last_plot()
    layer <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
    ggplot2::layer_data(p, layer)$label
}

test_that("barplot: small percentages' labels hidden (stacked, within categories)", {
    shown <- hideLabels(barType = "stack", yaxis = "percent", percentWithin = "category", hideLabelsBelow = FALSE)
    expect_equal(sum(is.na(shown)), 0)
    hidden <- hideLabels(barType = "stack", yaxis = "percent", percentWithin = "category", hideLabelsBelow = TRUE)
    expect_equal(sum(is.na(hidden)), 5)
    expect_equal(hidden[!is.na(hidden)], shown[!is.na(hidden)])    # the other labels are untouched
})

test_that("barplot: small percentages' labels hidden, whatever the bar type", {
    for (barType in c("dodge2", "dodge")) {
        hidden <- hideLabels(barType = barType, yaxis = "percent", percentWithin = "category", hideLabelsBelow = TRUE)
        expect_equal(sum(is.na(hidden)), 5)
    }
})

test_that("barplot: hiding small percentages uses the displayed percentages", {
    # within groups, no share is under 5%
    hidden <- hideLabels(barType = "stack", yaxis = "percent", percentWithin = "group", hideLabelsBelow = TRUE)
    expect_equal(sum(is.na(hidden)), 0)
})

test_that("barplot: no label hidden over counts, nor above the bars", {
    expect_equal(sum(is.na(hideLabels(barType = "stack", yaxis = "count", hideLabelsBelow = TRUE))), 0)
    expect_equal(sum(is.na(hideLabels(barType = "dodge", yaxis = "percent", percentWithin = "category",
                                      labelPosition = "top", hideLabelsBelow = TRUE))), 0)
})

test_that("barplot: stacked percentages, small labels hidden", {
    testPlot <- vijPlots::barplot(
        data = hideData,
        rows = "category",
        columns = "group",
        facet = NULL,
        counts = NULL,
        barType = "stack",
        yaxis = "percent",
        percentWithin = "category",
        hideLabelsBelow = TRUE
    )$plot
    expect_plot_snapshot("barplot-hideLabels-percent", testPlot)
})

# Small categories: c, d, e below 15% on the category axis; G3, G4 (10% each) in the groups
lumpData <- data.frame(
    x = factor(c(rep("a", 50), rep("b", 30), rep("c", 6), rep("d", 5), rep("e", 4), rep(NA, 5))),
    g = factor(rep(c(rep("G1", 5), rep("G2", 3), "G3", "G4"), 10))
)

lumpBarplot <- function(..., singleColor = FALSE) vijPlots::barplot(data = lumpData, rows = "x", facet = NULL, counts = NULL,
                                               ignoreNA = FALSE, singleColor = singleColor, lumpProp = 15, ...)

lumpBarFills <- function(r) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    unique(ggplot2::layer_data(ggplot2::last_plot(), 1)$fill)
}

test_that("barplot: the checked variables are lumped", {
    r <- lumpBarplot(columns = NULL)
    expect_equal(levels(r$plot$state$x), c("a", "b", "c", "d", "e"))
    expect_length(attr(r$plot$state, "lumped"), 0)

    r <- lumpBarplot(columns = NULL, lumpRows = TRUE, order = "decreasing")
    expect_equal(levels(r$plot$state$x), c("a", "b", "Other"))
    expect_equal(attr(r$plot$state, "lumped"), "x")

    # "Group" checked without a group variable: no effect
    r <- lumpBarplot(columns = NULL, lumpColumns = TRUE)
    expect_length(attr(r$plot$state, "lumped"), 0)

    r <- lumpBarplot(columns = "g", lumpColumns = TRUE)
    expect_equal(levels(r$plot$state$x), c("a", "b", "c", "d", "e"))
    expect_equal(levels(r$plot$state$g), c("G1", "G2", "Other"))
    expect_equal(attr(r$plot$state, "lumped"), "g")

    r <- lumpBarplot(columns = "g", lumpRows = TRUE, lumpColumns = TRUE, lump = "auto")
    expect_equal(attr(r$plot$state, "lumped"), c("x", "g"))
    expect_equal(tail(levels(r$plot$state$x), 1), "Other")
    expect_equal(tail(levels(r$plot$state$g), 1), "Other")
})

test_that("barplot: frequency table with lumped categories", {
    r <- lumpBarplot(columns = NULL, lumpRows = TRUE, frequencyTable = TRUE)
    df <- r$frequencies$asDF
    expect_equal(df$inner, c("a", "b", "Other", "(Missing)", "Total"))
    expect_equal(df$counts, c(50, 30, 15, 5, 100))
})

test_that("barplot: Other gets its color only when it is the fill variable", {
    # no group: the fill is the category
    expect_true("#D9D9D9" %in% lumpBarFills(lumpBarplot(columns = NULL, lumpRows = TRUE)))
    expect_true("#666666" %in% lumpBarFills(lumpBarplot(columns = NULL, lumpRows = TRUE, lumpColor = "darkgray")))
    expect_true("#D9D9D9" %in% lumpBarFills(lumpBarplot(columns = NULL, lumpRows = TRUE, barType = "stack")))
    expect_false("#D9D9D9" %in% lumpBarFills(lumpBarplot(columns = NULL, lumpRows = TRUE, lumpColor = "palette")))
    # single color: Other too
    expect_length(lumpBarFills(lumpBarplot(columns = NULL, lumpRows = TRUE, singleColor = TRUE)), 1)
    # with a group, the fill is the group: lumped category axis only, no gray
    expect_false("#D9D9D9" %in% lumpBarFills(lumpBarplot(columns = "g", lumpRows = TRUE)))
    expect_true("#D9D9D9" %in% lumpBarFills(lumpBarplot(columns = "g", lumpColumns = TRUE)))
})

test_that("barplot: lumped categories and groups plot", {
    testPlot <- lumpBarplot(columns = "g", lumpRows = TRUE, lumpColumns = TRUE)$plot
    expect_plot_snapshot("barplot-lump", testPlot)
})

test_that("barplot: Pareto curve, cumulative proportions in bar order", {
    r <- lumpBarplot(columns = NULL, order = "decreasing", pareto = TRUE)
    expect_equal(as.character(r$plot$state$x), c("a", "b", "c", "d", "e", NA))
    expect_equal(r$plot$state$.CUM, c(50, 80, 86, 91, 95, 100) / 100)
    # "Other", then the missing values, last
    r <- lumpBarplot(columns = NULL, order = "decreasing", pareto = TRUE, lumpRows = TRUE)
    expect_equal(as.character(r$plot$state$x), c("a", "b", "Other", NA))
    expect_equal(r$plot$state$.CUM, c(50, 80, 95, 100) / 100)
})

test_that("barplot: Pareto curve stays flat on an empty category, with no bar or label on it", {
    data <- data.frame(x = factor(c(rep("a", 5), rep("b", 3), NA, NA), levels = c("a", "b", "z")))
    for (yaxis in c("count", "percent")) {
        r <- vijPlots::barplot(data = data, rows = "x", columns = NULL, facet = NULL, counts = NULL, ignoreNA = FALSE,
                               order = "decreasing", pareto = TRUE, yaxis = yaxis, showLabels = TRUE)
        expect_equal(r$plot$state$.CUM, c(0.5, 0.8, 0.8, 1))
        plot <- renderedPlot(r)
        curve <- ggplot2::layer_data(plot, 2)
        expect_equal(curve$x, 1:4)
        expect_equal(curve$y / max(curve$y), c(0.5, 0.8, 0.8, 1))
        expect_equal(as.numeric(ggplot2::layer_data(plot, 1)$x), c(1, 2, 4))  # bars: no z
        expect_equal(as.numeric(ggplot2::layer_data(plot, 4)$x), c(1, 2, 4))  # labels: no z
    }
})

test_that("barplot: no Pareto curve outside its conditions", {
    noCurve <- function(..., pareto = TRUE) expect_false(".CUM" %in% names(lumpBarplot(pareto = pareto, ...)$plot$state))
    noCurve(columns = NULL, order = "none")
    noCurve(columns = "g", order = "decreasing")
    noCurve(columns = NULL, order = "decreasing", barType = "stack")
    noCurve(columns = NULL, order = "decreasing", horizontal = TRUE)
    noCurve(columns = NULL, order = "decreasing", pareto = FALSE)
    r <- vijPlots::barplot(data = testData, rows = "species", columns = NULL, facet = "island", counts = NULL,
                           order = "decreasing", pareto = TRUE, yaxis = "percent")
    expect_false(".CUM" %in% names(r$plot$state))
})

test_that("barplot: Pareto curve on a count axis, with a secondary % axis", {
    r <- lumpBarplot(columns = NULL, order = "decreasing", pareto = TRUE)
    plot <- renderedPlot(r)
    curve <- ggplot2::layer_data(plot, 2)
    expect_equal(curve$y, c(50, 80, 86, 91, 95, 100))
    expect_false(inherits(plot$scales$get_scales("y")$secondary.axis, "waiver"))
    expect_plot_snapshot("barplot-pareto-count", r$plot)
})

test_that("barplot: a variable named .CUM is rejected only with the Pareto curve", {
    data <- lumpData
    names(data)[names(data) == "x"] <- ".CUM"
    r <- vijPlots::barplot(data = data, rows = ".CUM", columns = NULL, facet = NULL, counts = NULL, order = "decreasing")
    expect_false(is.null(r$plot$state))
    expect_error(
        vijPlots::barplot(data = data, rows = ".CUM", columns = NULL, facet = NULL, counts = NULL, order = "decreasing",
                          pareto = TRUE),
        "reserved name"
    )
})

test_that("barplot: frequency table with variables named facetLevel and outerLevel", {
    run <- function(data, columns, facet) do.call(vijPlots::barplot, list(data = data, rows = "species", columns = columns,
                                                                          facet = facet, counts = NULL, frequencyTable = TRUE))
    expected <- run(testData, "sex", "island")$frequencies$asDF
    data <- testData
    names(data)[names(data) == "sex"] <- "outerLevel"
    names(data)[names(data) == "island"] <- "facetLevel"
    expect_identical(unname(as.list(run(data, "outerLevel", "facetLevel")$frequencies$asDF)), unname(as.list(expected)))
})
