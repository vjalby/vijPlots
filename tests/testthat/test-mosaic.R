testData <- datasets::penguins

test_that("mosaic: basic (species x sex)", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL
    )$plot
    expect_plot_snapshot("mosaic-basic", testPlot)
})

test_that("mosaic: horizontal", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        horizontal = TRUE
    )$plot
    expect_plot_snapshot("mosaic-horizontal", testPlot)
})

test_that("mosaic: sorted decreasing", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        order = "decreasing"
    )$plot
    expect_plot_snapshot("mosaic-sorted-decreasing", testPlot)
})

test_that("mosaic: sorted increasing", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        order = "increasing"
    )$plot
    expect_plot_snapshot("mosaic-sorted-increasing", testPlot)
})

test_that("mosaic: reversed stacking order", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        reverseStack = TRUE
    )$plot
    expect_plot_snapshot("mosaic-stacked-reversed", testPlot)
})

test_that("mosaic: y-axis as group labels", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        yAxis = "label"
    )$plot
    expect_plot_snapshot("mosaic-yaxis-label", testPlot)
})

test_that("mosaic: y-axis as group labels, legend forced on", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        yAxis = "label",
        alwaysShowGroupLegend = TRUE
    )$plot
    expect_plot_snapshot("mosaic-yaxis-label-legend-forced", testPlot)
})

test_that("mosaic: y-axis hidden", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        yAxis = "none"
    )$plot
    expect_plot_snapshot("mosaic-yaxis-none", testPlot)
})

test_that("mosaic: axes hidden", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        hideAxes = TRUE
    )$plot
    expect_plot_snapshot("mosaic-hideAxes", testPlot)
})

test_that("mosaic: category % shown", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        showCategoryPercent = TRUE
    )$plot
    expect_plot_snapshot("mosaic-category-percent-shown", testPlot)
})

test_that("mosaic: faceted", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = "island"
    )$plot
    expect_plot_snapshot("mosaic-faceted", testPlot)
})

test_that("mosaic: faceted with y-axis group labels", {
    # per-facet free x AND y scales (see .plot()'s freeScale handling)
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = "island",
        yAxis = "label"
    )$plot
    expect_plot_snapshot("mosaic-faceted-yaxis-label", testPlot)
})

test_that("mosaic: count labels", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        labelType = "count"
    )$plot
    expect_plot_snapshot("mosaic-label-count", testPlot)
})

test_that("mosaic: percent labels", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        labelType = "percent"
    )$plot
    expect_plot_snapshot("mosaic-label-percent", testPlot)
})

test_that("mosaic: residuals labels", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        labelType = "residuals"
    )$plot
    expect_plot_snapshot("mosaic-label-residuals", testPlot)
})

test_that("mosaic: missing values kept (ignoreNA = FALSE)", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("mosaic-ignoreNA-false", testPlot)
})

test_that("mosaic: faceted by a variable with missing values kept (ignoreNA = FALSE)", {
    # sex has NAs: facet_wrap() adds an NA panel, which needs its own x/y scales, and
    # the other panels must keep their group labels
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "island",
        counts = NULL,
        facet = "sex",
        yAxis = "label",
        ignoreNA = FALSE
    )$plot
    expect_plot_snapshot("mosaic-faceted-na-yaxis-label", testPlot)
})

test_that("mosaic: titles, axis and legend text options", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
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
        yAxisText = "Proportion",
        xAxisLabelRotation = 45,
        yAxisLabelFontSize = 14,
        yTicks = 5
    )$plot
    expect_plot_snapshot("mosaic-titles-axis-legend", testPlot)
})

test_that("mosaic: opacity mapped to residuals", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        residualsAsOpacity = TRUE
    )$plot
    expect_plot_snapshot("mosaic-opacity-residuals", testPlot)
})

test_that("mosaic: opacity legend shown alongside fill legend", {
    # exercises .init()'s doubled legend-height branch: both the fill legend
    # (group) and the alpha legend (residuals) are visible at once, with
    # legendPosition stacking them in the same top/bottom band
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        residualsAsOpacity = TRUE,
        showOpacityLegend = TRUE,
        legendPosition = "bottom"
    )$plot
    expect_plot_snapshot("mosaic-opacity-legend-bottom", testPlot)
})

test_that("mosaic: stale showOpacityLegend without residualsAsOpacity does not error", {
    # regression test: showOpacityLegend stays enabled behind a greyed-out
    # checkbox when residualsAsOpacity is unchecked (jamovi doesn't reset a
    # disabled control's stored value) - .init()/.plot() must not reserve
    # space for, or render, a legend that was never actually added to the plot
    r <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        residualsAsOpacity = FALSE,
        showOpacityLegend = TRUE
    )
    expect_false(".error" %in% names(r))
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_no_error(print(r$plot))
})

test_that("mosaic: thick tile border", {
    testPlot <- vijPlots::mosaic(
        data = testData,
        category = "species",
        group = "sex",
        counts = NULL,
        facet = NULL,
        borderWidth = 5
    )$plot
    expect_plot_snapshot("mosaic-thick-border", testPlot)
})

weightedData <- data.frame(
    cat = factor(c("A", "A", "B", "B")),
    grp = factor(c("X", "Y", "X", "Y"))
)
weightedData$w <- c(30, 10, 20, 40)
# expected totals: A = 40 (X=30, Y=10), B = 60 (X=20, Y=40)

test_that("mosaic: proportions and residuals are computed correctly", {
    r <- vijPlots::mosaic(
        data = weightedData,
        category = "cat",
        group = "grp",
        counts = "w",
        facet = NULL
    )
    state <- as.data.frame(r$plot$state)
    state <- state[order(state$cat, state$grp), ]

    expect_equal(state$.FREQ, c(30, 10, 20, 40))
    expect_equal(state$.PERCENTAGE, c(0.75, 0.25, 1/3, 2/3), tolerance = 1e-6)

    # cross-check against a direct chi-squared test on the same contingency table
    tab <- stats::xtabs(w ~ cat + grp, data = weightedData)
    expectedResiduals <- as.data.frame(suppressWarnings(stats::chisq.test(tab))$residuals)
    names(expectedResiduals) <- c("cat", "grp", "residuals")
    expectedResiduals <- expectedResiduals[order(expectedResiduals$cat, expectedResiduals$grp), ]
    expect_equal(state$.RESIDUALS, expectedResiduals$residuals, tolerance = 1e-6)
})

test_that("mosaic: weighted by counts variable", {
    r <- vijPlots::mosaic(
        data = weightedData,
        category = "cat",
        group = "grp",
        counts = "w",
        facet = NULL
    )
    expect_plot_snapshot("mosaic-weighted-counts", r$plot)
})

test_that("mosaic: weighted via jamovi's built-in weights (jmv-weights attribute)", {
    # regression test: without weightsSupport: 'full' in mosaic.a.yaml, jmvcore's
    # Analysis$run() auto-expands rows per jmv-weights *before* .run() sees self$data,
    # leaving a stale jmv-weights attribute that crashes .COUNTS construction
    dataWithAttr <- weightedData[c("cat", "grp")]
    attr(dataWithAttr, "jmv-weights") <- weightedData$w
    attr(dataWithAttr, "jmv-weights-name") <- "w"

    r <- vijPlots::mosaic(
        data = dataWithAttr,
        category = "cat",
        group = "grp",
        counts = NULL,
        facet = NULL
    )
    state <- as.data.frame(r$plot$state)
    state <- state[order(state$cat, state$grp), ]
    expect_equal(state$.FREQ, c(30, 10, 20, 40))
})

test_that("mosaic: ordered factor as category does not break the residuals join", {
    # regression test: as.data.frame() on chisq.test()$residuals turns its dimnames
    # into plain (unordered) factors; joining that back against mosaic_data used to
    # fail with a dplyr type-mismatch error whenever category/group was an ordinal
    # variable (still an ordered factor at that point). .run() now deliberately drops
    # ordered-ness up front (level order is preserved, and nothing downstream needs
    # the ordered flag itself), so the join never sees a type mismatch.
    orderedData <- data.frame(
        Age = factor(c("Young", "Young", "Old", "Old"), levels = c("Young", "Old"), ordered = TRUE),
        Sex = factor(c("M", "F", "M", "F"))
    )
    r <- vijPlots::mosaic(
        data = orderedData,
        category = "Age",
        group = "Sex",
        counts = NULL,
        facet = NULL
    )
    expect_false(".error" %in% names(r))
    state <- as.data.frame(r$plot$state)
    expect_false(is.ordered(state$Age))
    expect_equal(levels(state$Age), c("Young", "Old"))
})

test_that("mosaic: negative counts are rejected", {
    negData <- weightedData
    negData$w[1] <- -3
    expect_error(
        vijPlots::mosaic(
            data = negData,
            category = "cat",
            group = "grp",
            counts = "w",
            facet = NULL
        ),
        "Counts may not be negative."
    )
})

test_that("mosaic: a variable named like the internal counts column is rejected", {
    reservedData <- data.frame(.COUNTS = factor(c("a", "a", "b", "b")), grp = factor(c("u", "v", "u", "v")))
    expect_error(
        vijPlots::mosaic(data = reservedData, category = ".COUNTS", group = "grp", facet = NULL, counts = NULL),
        "reserved name"
    )
})

test_that("mosaic: invalid weights are rejected even on a row with a missing value", {
    weightData <- data.frame(cat = factor(c("a", "b", NA)), grp = factor(c("g1", "g2", "g1")),
                             w = c(2, 3, NA))
    expect_error(
        vijPlots::mosaic(data = weightData, category = "cat", group = "grp", facet = NULL,
                         counts = "w", ignoreNA = TRUE),
        "Counts may not contain missing values."
    )
    weightData$w[3] <- -1
    expect_error(
        vijPlots::mosaic(data = weightData, category = "cat", group = "grp", facet = NULL,
                         counts = "w", ignoreNA = TRUE),
        "Counts may not be negative."
    )
})

test_that("mosaic: zero weights are treated as no data", {
    zeroData <- data.frame(cat = factor(c("a", "a", "b", "b", "a")), grp = factor(c("g1", "g2", "g1", "g2", "g1")),
                           fac = factor(c("x", "x", "x", "x", "y")), w = c(3, 4, 0, 0, 0))
    r <- vijPlots::mosaic(data = zeroData, category = "cat", group = "grp", facet = "fac", counts = "w")
    state <- r$plot$state
    # no NaN width/height: category "b" and facet "y" (all zero weights) are left out, as without data
    expect_false(anyNA(state$.XWIDTH) || anyNA(state$.PERCENTAGE))
    expect_equal(unique(as.character(state$fac)), "x")
    expect_equal(unique(as.character(state$cat)), "a")
})

test_that("mosaic: missing values are labelled (Missing) on the axes, legend and facets", {
    naData <- datasets::penguins
    naData$isl <- naData$island
    naData$isl[1:4] <- NA
    renderBuilt <- function(...) {
        r <- vijPlots::mosaic(data = naData, counts = NULL, ignoreNA = FALSE, yAxis = "label", ...)
        print(r$plot)
        ggplot2::ggplot_build(ggplot2::last_plot())
    }
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    # NA category (sex): x-axis
    built <- renderBuilt(category = "sex", group = "species", facet = NULL)
    expect_equal(built$layout$panel_params[[1]]$x$get_labels(), c("female", "male", "(Missing)"))
    # NA group (sex): legend and y-axis
    built <- renderBuilt(category = "species", group = "sex", facet = NULL)
    expect_equal(built$plot$scales$get_scales("fill")$get_labels(), c("female", "male", "(Missing)"))
    expect_true("(Missing)" %in% built$layout$panel_params[[1]]$y$get_labels())
    # NA facet: strips
    built <- renderBuilt(category = "species", group = "sex", facet = "isl")
    gt <- ggplot2::ggplot_gtable(built)
    strips <- unlist(lapply(gt$grobs[grepl("^strip", gt$layout$name)], function(g) {
        unlist(lapply(g$grobs, function(s) tryCatch(s$children[[2]]$children[[1]]$label, error = function(e) NULL)))
    }))
    expect_true("(Missing)" %in% strips)
    expect_false("NA" %in% strips)
})
