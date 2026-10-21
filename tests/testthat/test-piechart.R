testData <- datasets::penguins

test_that("piechart: basic pie chart", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text"
    )$plot
    expect_plot_snapshot("piechart-basic", testPlot)
})

test_that("piechart: donut chart", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        donut = TRUE
    )$plot
    expect_plot_snapshot("piechart-donut", testPlot)
})

test_that("piechart: clockwise", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "group",
        clockwise = TRUE
    )$plot

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(testPlot)
    expect_equal(ggplot2::last_plot()$coordinates$direction, -1)

    expect_plot_snapshot("piechart-clockwise", testPlot)
})

test_that("piechart: count labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "count"
    )$plot
    expect_plot_snapshot("piechart-labels-count", testPlot)
})

test_that("piechart: percent labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "percent"
    )$plot
    expect_plot_snapshot("piechart-labels-percent", testPlot)
})

test_that("piechart: group labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "group"
    )$plot
    expect_plot_snapshot("piechart-labels-group", testPlot)
})

test_that("piechart: group + count labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "group+count"
    )$plot
    expect_plot_snapshot("piechart-labels-groupCount", testPlot)
})

test_that("piechart: group + percent labels", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "group+percent"
    )$plot
    expect_plot_snapshot("piechart-labels-groupPercent", testPlot)
})

test_that("piechart: label boxes with overlap prevention", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "island",
        facet = NULL,
        counts = NULL,
        labType = "label",
        labels = "group+percent",
        overlap = TRUE
    )$plot
    expect_plot_snapshot("piechart-labelBoxes-overlap", testPlot)
})

test_that("piechart: faceted (with missing category dropped)", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "sex",
        facet = "species",
        counts = NULL,
        labType = "text",
        labels = "percent"
    )$plot
    expect_plot_snapshot("piechart-faceted", testPlot)
})

test_that("piechart: no border", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        borderColor = "none"
    )$plot
    expect_plot_snapshot("piechart-noBorder", testPlot)
})

test_that("piechart: custom label offset and size", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "group",
        labOffset = 5,
        labSize = 18
    )$plot
    expect_plot_snapshot("piechart-labelOffsetSize", testPlot)
})

test_that("piechart: titles, axis and legend text options", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        titleText = "Penguins by Species",
        titleFontFace = "bold.italic",
        titleAlign = "0",
        subtitleText = "palmerpenguins dataset",
        subtitleFontFace = "italic",
        captionText = "Source: datasets::penguins",
        captionAlign = "1",
        legendText = "Species",
        legendPosition = "bottom"
    )$plot
    expect_plot_snapshot("piechart-titles-axis-legend", testPlot)
})

weightedData <- data.frame(
    cat = factor(c("A", "A", "B", "B", "B", "C"))
)
weightedData$w <- c(10, 5, 20, 3, 2, 7)
# expected weighted totals per cat: A=15, B=25, C=7

test_that("piechart: weighted by counts variable", {
    r <- vijPlots::piechart(
        data = weightedData,
        aVar = "cat",
        facet = NULL,
        counts = "w",
        labType = "text"
    )
    # state is aggregated: one row per cat, not one per case
    expect_equal(nrow(r$plot$state), 3)
    expect_equal(r$plot$state$.COUNTS, c(15, 25, 7))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 25, 7)))

    expect_plot_snapshot("piechart-weighted-counts", r$plot)
})

test_that("piechart: weighted via jamovi's built-in weights (jmv-weights attribute)", {
    # regression test: without weightsSupport: 'full' in piechart.a.yaml, jmvcore's
    # Analysis$run() auto-expands rows per jmv-weights *before* .run() sees self$data,
    # leaving a stale jmv-weights attribute that crashes .COUNTS construction
    dataWithAttr <- weightedData["cat"]
    attr(dataWithAttr, "jmv-weights") <- weightedData$w
    attr(dataWithAttr, "jmv-weights-name") <- "w"

    r <- vijPlots::piechart(
        data = dataWithAttr,
        aVar = "cat",
        facet = NULL,
        counts = NULL,
        labType = "text"
    )
    # state is aggregated: one row per cat, not one per case
    expect_equal(nrow(r$plot$state), 3)
    expect_equal(r$plot$state$.COUNTS, c(15, 25, 7))

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    built <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
    expect_equal(sort(built$count), sort(c(15, 25, 7)))
})

test_that("piechart: negative counts are rejected", {
    negData <- weightedData
    negData$w[1] <- -3
    expect_error(
        vijPlots::piechart(
            data = negData,
            aVar = "cat",
            facet = NULL,
            counts = "w",
            labType = "text"
        ),
        "Counts may not be negative."
    )
})

test_that("piechart: a variable named like the internal counts column is rejected", {
    reservedData <- data.frame(.COUNTS = weightedData$cat)
    expect_error(
        vijPlots::piechart(data = reservedData, aVar = ".COUNTS", facet = NULL, counts = NULL),
        "reserved name"
    )
})

test_that("piechart: frequency table", {
    r <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        frequencyTable = TRUE
    )
    expect_true(r$frequencies$visible)
    df <- r$frequencies$asDF
    # no missing value: no note
    expect_null(r$frequencies$notes$missing)
    expect_equal(df$inner, c("Adelie", "Chinstrap", "Gentoo", "Total"))
    expect_equal(df$counts, c(152, 68, 124, 344))
    expect_equal(df$percent, c(152, 68, 124, 344) / 344)
    expect_equal(df$cumPercent, c(c(152, 220, 344) / 344, NA))
})

test_that("piechart: frequency table is hidden by default", {
    r <- vijPlots::piechart(data = testData, aVar = "species", facet = NULL, counts = NULL)
    expect_false(r$frequencies$visible)
})

test_that("piechart: frequency table with facet and counts", {
    facetData <- data.frame(
        cat = factor(c("a", "b", "a", "c", "b", NA, NA, NA, NA)),
        fac = factor(c("x", "x", "y", "y", "y", "y", "y", NA, "x"), levels = c("x", "y", "z")),
        w = c(2, 3, 1, 4, 5, 6, 7, 8, 9)
    )
    r <- vijPlots::piechart(
        data = facetData,
        aVar = "cat",
        facet = "fac",
        counts = "w",
        frequencyTable = TRUE
    )
    df <- r$frequencies$asDF
    # all categories in each facet, empty facet "z" skipped, percentages within facet
    expect_equal(df$facet, rep(c("x", "y"), each = 4))
    expect_equal(df$inner, rep(c("a", "b", "c", "Total"), 2))
    expect_equal(df$counts, c(2, 3, 0, 5, 1, 5, 4, 10))
    expect_equal(df$percent, c(c(2, 3, 0, 5) / 5, c(1, 5, 4, 10) / 10))
    # cumulative % restarts in each facet
    expect_equal(df$cumPercent, c(c(2, 5, 5) / 5, NA, c(1, 6, 10) / 10, NA))
    # excluded observations, weighted and overall (6 + 7 + 8 + 9)
    expect_equal(r$frequencies$notes$missing$note, "Missing values: 30")
})

test_that("piechart: percent labels below 5% are hidden", {
    smallData <- data.frame(cat = factor(c("a", "b", "c")), w = c(60, 37, 3))
    renderLabels <- function(labels, hide, labType = "text") {
        r <- vijPlots::piechart(
            data = smallData,
            aVar = "cat",
            facet = NULL,
            counts = "w",
            labType = labType,
            labels = labels,
            hideLabelsBelow = hide
        )
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off())
        print(r$plot)
        # labels use a thin space (U+2009) before "%"
        gsub("\u2009", " ", ggplot2::layer_data(ggplot2::last_plot(), 2)$label)
    }
    expect_equal(sort(renderLabels("percent", FALSE)), c("3.0 %", "37.0 %", "60.0 %"))
    expect_equal(sort(renderLabels("percent", TRUE)), c("37.0 %", "60.0 %"))
    expect_equal(sort(renderLabels("percent", TRUE, "label")), c("37.0 %", "60.0 %"))
    # group+percent keeps the group name of small slices
    expect_equal(sort(renderLabels("group+percent", TRUE)), c("a\n60.0 %", "b\n37.0 %", "c"))
})

test_that("piechart: hidden percent labels", {
    smallData <- data.frame(cat = factor(c("a", "b", "c")), w = c(60, 37, 3))
    testPlot <- vijPlots::piechart(
        data = smallData,
        aVar = "cat",
        facet = NULL,
        counts = "w",
        labType = "text",
        labels = "percent",
        hideLabelsBelow = TRUE
    )$plot
    expect_plot_snapshot("piechart-hide-labels-below", testPlot)
})

test_that("piechart: categories sorted by count", {
    sortData <- data.frame(cat = factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d")), w = c(10, 40, 0, 25))
    sortedLevels <- function(order) {
        r <- vijPlots::piechart(
            data = sortData,
            aVar = "cat",
            facet = NULL,
            counts = "w",
            order = order,
            frequencyTable = TRUE
        )
        list(state = levels(r$plot$state$cat), table = head(r$frequencies$asDF$inner, -1))
    }
    expect_equal(sortedLevels("none")$state, c("a", "b", "c", "d"))
    # sorted levels drive both the plot and the frequency table
    expect_equal(sortedLevels("decreasing"), list(state = c("b", "d", "a", "c"), table = c("b", "d", "a", "c")))
    expect_equal(sortedLevels("increasing"), list(state = c("c", "a", "d", "b"), table = c("c", "a", "d", "b")))
})

test_that("piechart: decreasing order", {
    testPlot <- vijPlots::piechart(
        data = testData,
        aVar = "species",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "group+percent",
        order = "decreasing"
    )$plot
    expect_plot_snapshot("piechart-order-decreasing", testPlot)
})

test_that("piechart: missing values kept (ignoreNA = FALSE)", {
    naData <- testData
    naData$isl <- naData$island
    naData$isl[1:4] <- NA
    r <- vijPlots::piechart(
        data = naData,
        aVar = "sex",
        facet = "isl",
        counts = NULL,
        ignoreNA = FALSE,
        frequencyTable = TRUE
    )
    df <- r$frequencies$asDF
    # NA is a category and a facet of its own, last, and counts in the percentages
    expect_equal(unique(df$facet), c("Biscoe", "Dream", "Torgersen", "(Missing)"))
    expect_equal(df$inner[df$facet == "(Missing)"], c("female", "male", "(Missing)", "Total"))
    expect_equal(df$counts[df$facet == "(Missing)"], c(2, 1, 1, 4))
    expect_equal(df$percent[df$facet == "Torgersen"], c(22, 22, 4, 48) / 48)

    # default: missing values are left out
    r <- vijPlots::piechart(data = naData, aVar = "sex", facet = "isl", counts = NULL, frequencyTable = TRUE)
    df <- r$frequencies$asDF
    expect_false("(Missing)" %in% c(df$facet, df$inner))
})

test_that("piechart: percent labels keep the NA slice (ignoreNA = FALSE)", {
    r <- vijPlots::piechart(
        data = testData,
        aVar = "sex",
        facet = NULL,
        counts = NULL,
        labType = "text",
        labels = "percent",
        ignoreNA = FALSE
    )
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    # same percentages as the frequency table: 165, 168 and 11 NA out of 344
    labels <- gsub(" ", " ", ggplot2::layer_data(ggplot2::last_plot(), 2)$label)
    expect_equal(labels, c("48.0 %", "48.8 %", "3.2 %"))
    expect_plot_snapshot("piechart-ignorena-false-percent", r$plot)
})

test_that("piechart: invalid weights are rejected even on a row with a missing value", {
    weightData <- data.frame(cat = factor(c("a", "b", NA)), w = c(2, 3, NA))
    # a missing weight is invalid data, even with ignoreNA checked
    expect_error(
        vijPlots::piechart(data = weightData, aVar = "cat", facet = NULL, counts = "w", ignoreNA = TRUE),
        "Counts may not contain missing values."
    )
    # a negative weight on a row left out for its missing category
    weightData$w[3] <- -1
    expect_error(
        vijPlots::piechart(data = weightData, aVar = "cat", facet = NULL, counts = "w", ignoreNA = TRUE),
        "Counts may not be negative."
    )
})

test_that("piechart: a facet whose weights are all zero is treated as a facet without data", {
    zeroData <- data.frame(cat = factor(c("a", "b", "a")), fac = factor(c("x", "x", "y")), w = c(0, 0, 10))
    r <- vijPlots::piechart(data = zeroData, aVar = "cat", facet = "fac", counts = "w", frequencyTable = TRUE)
    # no zero-weight row in the state: facet "x" has no slice
    expect_false(any(r$plot$state$.COUNTS == 0))
    df <- r$frequencies$asDF
    expect_equal(df$facet, rep("y", 3))
    expect_equal(df$counts, c(10, 0, 10))
    expect_equal(df$percent, c(1, 0, 1))
    # and no panel for facet "x"
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    expect_equal(nlevels(ggplot2::layer_data(ggplot2::last_plot(), 1)$PANEL), 1)
})

test_that("piechart: missing values are labelled (Missing) in the legend, facets, labels and table", {
    naData <- testData
    naData$isl <- naData$island
    naData$isl[1:4] <- NA
    r <- vijPlots::piechart(
        data = naData,
        aVar = "sex",
        facet = "isl",
        counts = NULL,
        labType = "text",
        labels = "group+percent",
        ignoreNA = FALSE,
        frequencyTable = TRUE
    )
    expect_true("(Missing)" %in% r$frequencies$asDF$inner)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(r$plot)
    p <- ggplot2::last_plot()
    built <- ggplot2::ggplot_build(p)
    # legend
    expect_equal(built$plot$scales$get_scales("fill")$get_labels(), c("female", "male", "(Missing)"))
    # slice labels (group + percent) and gray NA slice
    labels <- ggplot2::layer_data(p, 2)$label
    expect_true(any(startsWith(labels, "(Missing)\n")))
    expect_true("gray" %in% ggplot2::layer_data(p, 1)$fill)
    # facet strips
    gt <- ggplot2::ggplot_gtable(built)
    strips <- unlist(lapply(gt$grobs[grepl("^strip", gt$layout$name)], function(g) {
        unlist(lapply(g$grobs, function(s) tryCatch(s$children[[2]]$children[[1]]$label, error = function(e) NULL)))
    }))
    expect_true("(Missing)" %in% strips)
    expect_false("NA" %in% strips)
})
