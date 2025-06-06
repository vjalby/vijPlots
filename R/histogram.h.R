
# This file is automatically generated, you probably don't want to edit this

histogramOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "histogramOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            aVar = NULL,
            group = NULL,
            facet = NULL,
            histtype = "count",
            normalCurve = FALSE,
            binWidth = 0,
            binBoundary = 0,
            fillColor = "#A6C4F1",
            borderColor = "black",
            grouping = "none",
            colorPalette = NULL,
            usePalette = "forFilling",
            plotWidth = 0,
            plotHeight = 0,
            facetBy = "column",
            facetNumber = 1, ...) {

            super$initialize(
                package="vijPlots",
                name="histogram",
                requiresData=TRUE,
                ...)

            private$..aVar <- jmvcore::OptionVariable$new(
                "aVar",
                aVar,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"))
            private$..group <- jmvcore::OptionVariable$new(
                "group",
                group,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..facet <- jmvcore::OptionVariable$new(
                "facet",
                facet,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..histtype <- jmvcore::OptionList$new(
                "histtype",
                histtype,
                options=list(
                    "count",
                    "density"),
                default="count")
            private$..normalCurve <- jmvcore::OptionBool$new(
                "normalCurve",
                normalCurve,
                default=FALSE)
            private$..binWidth <- jmvcore::OptionNumber$new(
                "binWidth",
                binWidth,
                min=0,
                default=0)
            private$..binBoundary <- jmvcore::OptionNumber$new(
                "binBoundary",
                binBoundary,
                min=0,
                default=0)
            private$..fillColor <- jmvcore::OptionList$new(
                "fillColor",
                fillColor,
                options=list(
                    "none",
                    "white",
                    "black",
                    "#A6C4F1",
                    "#C5C5C5",
                    "#F0CD8C",
                    "#88C38B",
                    "#E18A8A",
                    "#E41A1C",
                    "#377EB8",
                    "#4DAF4A",
                    "#984EA3",
                    "#FF7F00",
                    "#FFFF33",
                    "#A65628",
                    "#F781BF",
                    "#999999",
                    "#FBB4AE",
                    "#B3CDE3",
                    "#CCEBC5",
                    "#DECBE4",
                    "#FED9A6",
                    "#FFFFCC",
                    "#E5D8BD",
                    "#FDDAEC",
                    "#F2F2F2"),
                default="#A6C4F1")
            private$..borderColor <- jmvcore::OptionList$new(
                "borderColor",
                borderColor,
                options=list(
                    "none",
                    "black",
                    "white",
                    "gray"),
                default="black")
            private$..grouping <- jmvcore::OptionList$new(
                "grouping",
                grouping,
                options=list(
                    "none",
                    "identity",
                    "stack",
                    "dodge"),
                default="none")
            private$..colorPalette <- jmvcore::OptionList$new(
                "colorPalette",
                colorPalette,
                options=list(
                    "jmv",
                    "Set1",
                    "Set2",
                    "Set3",
                    "Pastel1",
                    "Pastel2",
                    "Accent",
                    "Paired",
                    "Dark2",
                    "Spectral",
                    "RdYlGn",
                    "RdYlBu",
                    "RdGy",
                    "RdBu",
                    "PuOr",
                    "PRGn",
                    "PiYG",
                    "BrBG",
                    "Blues",
                    "Greens",
                    "Greys",
                    "Oranges",
                    "Purples",
                    "Reds",
                    "BuGn",
                    "BuPu",
                    "GnBu",
                    "OrRd",
                    "PuBu",
                    "PuBuGn",
                    "PuRd",
                    "RdPu",
                    "YlGn",
                    "YlGnBu",
                    "YlOrBr",
                    "YlOrRd"))
            private$..usePalette <- jmvcore::OptionList$new(
                "usePalette",
                usePalette,
                options=list(
                    "forFilling",
                    "forBorder"),
                default="forFilling")
            private$..plotWidth <- jmvcore::OptionNumber$new(
                "plotWidth",
                plotWidth,
                min=0,
                max=1000,
                default=0)
            private$..plotHeight <- jmvcore::OptionNumber$new(
                "plotHeight",
                plotHeight,
                min=0,
                max=1600,
                default=0)
            private$..facetBy <- jmvcore::OptionList$new(
                "facetBy",
                facetBy,
                options=list(
                    "row",
                    "column"),
                default="column")
            private$..facetNumber <- jmvcore::OptionNumber$new(
                "facetNumber",
                facetNumber,
                min=1,
                max=10,
                default=1)

            self$.addOption(private$..aVar)
            self$.addOption(private$..group)
            self$.addOption(private$..facet)
            self$.addOption(private$..histtype)
            self$.addOption(private$..normalCurve)
            self$.addOption(private$..binWidth)
            self$.addOption(private$..binBoundary)
            self$.addOption(private$..fillColor)
            self$.addOption(private$..borderColor)
            self$.addOption(private$..grouping)
            self$.addOption(private$..colorPalette)
            self$.addOption(private$..usePalette)
            self$.addOption(private$..plotWidth)
            self$.addOption(private$..plotHeight)
            self$.addOption(private$..facetBy)
            self$.addOption(private$..facetNumber)
        }),
    active = list(
        aVar = function() private$..aVar$value,
        group = function() private$..group$value,
        facet = function() private$..facet$value,
        histtype = function() private$..histtype$value,
        normalCurve = function() private$..normalCurve$value,
        binWidth = function() private$..binWidth$value,
        binBoundary = function() private$..binBoundary$value,
        fillColor = function() private$..fillColor$value,
        borderColor = function() private$..borderColor$value,
        grouping = function() private$..grouping$value,
        colorPalette = function() private$..colorPalette$value,
        usePalette = function() private$..usePalette$value,
        plotWidth = function() private$..plotWidth$value,
        plotHeight = function() private$..plotHeight$value,
        facetBy = function() private$..facetBy$value,
        facetNumber = function() private$..facetNumber$value),
    private = list(
        ..aVar = NA,
        ..group = NA,
        ..facet = NA,
        ..histtype = NA,
        ..normalCurve = NA,
        ..binWidth = NA,
        ..binBoundary = NA,
        ..fillColor = NA,
        ..borderColor = NA,
        ..grouping = NA,
        ..colorPalette = NA,
        ..usePalette = NA,
        ..plotWidth = NA,
        ..plotHeight = NA,
        ..facetBy = NA,
        ..facetNumber = NA)
)

histogramResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "histogramResults",
    inherit = jmvcore::Group,
    active = list(
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Histogram")
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="",
                width=600,
                height=400,
                renderFun=".plot",
                clearWith=list(
                    "aVar",
                    "group",
                    "facet",
                    "histtype",
                    "binWidth",
                    "binBoundary",
                    "normalCurve",
                    "borderColor",
                    "fillColor",
                    "grouping",
                    "colorPalette",
                    "usePalette",
                    "plotWidth",
                    "plotHeight",
                    "facetBy",
                    "facetNumber")))}))

histogramBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "histogramBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "vijPlots",
                name = "histogram",
                version = c(1,0,0),
                options = options,
                results = histogramResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Histogram
#'
#' 
#' @param data .
#' @param aVar .
#' @param group .
#' @param facet .
#' @param histtype .
#' @param normalCurve .
#' @param binWidth .
#' @param binBoundary .
#' @param fillColor .
#' @param borderColor .
#' @param grouping .
#' @param colorPalette .
#' @param usePalette .
#' @param plotWidth .
#' @param plotHeight .
#' @param facetBy .
#' @param facetNumber .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' @export
histogram <- function(
    data,
    aVar,
    group,
    facet,
    histtype = "count",
    normalCurve = FALSE,
    binWidth = 0,
    binBoundary = 0,
    fillColor = "#A6C4F1",
    borderColor = "black",
    grouping = "none",
    colorPalette,
    usePalette = "forFilling",
    plotWidth = 0,
    plotHeight = 0,
    facetBy = "column",
    facetNumber = 1) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("histogram requires jmvcore to be installed (restart may be required)")

    if ( ! missing(aVar)) aVar <- jmvcore::resolveQuo(jmvcore::enquo(aVar))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if ( ! missing(facet)) facet <- jmvcore::resolveQuo(jmvcore::enquo(facet))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(aVar), aVar, NULL),
            `if`( ! missing(group), group, NULL),
            `if`( ! missing(facet), facet, NULL))

    for (v in group) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in facet) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- histogramOptions$new(
        aVar = aVar,
        group = group,
        facet = facet,
        histtype = histtype,
        normalCurve = normalCurve,
        binWidth = binWidth,
        binBoundary = binBoundary,
        fillColor = fillColor,
        borderColor = borderColor,
        grouping = grouping,
        colorPalette = colorPalette,
        usePalette = usePalette,
        plotWidth = plotWidth,
        plotHeight = plotHeight,
        facetBy = facetBy,
        facetNumber = facetNumber)

    analysis <- histogramClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

