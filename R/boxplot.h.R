
# This file is automatically generated, you probably don't want to edit this

boxplotOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "boxplotOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            group = NULL,
            showOutliers = TRUE,
            showMean = FALSE,
            colorPalette = "jmv",
            singleColor = FALSE,
            order = "none", ...) {

            super$initialize(
                package="vijPlots",
                name="boxplot",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
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
            private$..showOutliers <- jmvcore::OptionBool$new(
                "showOutliers",
                showOutliers,
                default=TRUE)
            private$..showMean <- jmvcore::OptionBool$new(
                "showMean",
                showMean,
                default=FALSE)
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
                    "YlOrRd"),
                default="jmv")
            private$..singleColor <- jmvcore::OptionBool$new(
                "singleColor",
                singleColor,
                default=FALSE)
            private$..order <- jmvcore::OptionList$new(
                "order",
                order,
                options=list(
                    "decreasing",
                    "increasing",
                    "none"),
                default="none")

            self$.addOption(private$..vars)
            self$.addOption(private$..group)
            self$.addOption(private$..showOutliers)
            self$.addOption(private$..showMean)
            self$.addOption(private$..colorPalette)
            self$.addOption(private$..singleColor)
            self$.addOption(private$..order)
        }),
    active = list(
        vars = function() private$..vars$value,
        group = function() private$..group$value,
        showOutliers = function() private$..showOutliers$value,
        showMean = function() private$..showMean$value,
        colorPalette = function() private$..colorPalette$value,
        singleColor = function() private$..singleColor$value,
        order = function() private$..order$value),
    private = list(
        ..vars = NA,
        ..group = NA,
        ..showOutliers = NA,
        ..showMean = NA,
        ..colorPalette = NA,
        ..singleColor = NA,
        ..order = NA)
)

boxplotResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "boxplotResults",
    inherit = jmvcore::Group,
    active = list(
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Box Plot")
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="",
                width=600,
                height=400,
                renderFun=".plot",
                clearWith=list(
                    "vars",
                    "group",
                    "showOutliers",
                    "showMean",
                    "colorPalette",
                    "singleColor",
                    "order")))}))

boxplotBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "boxplotBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "vijPlots",
                name = "boxplot",
                version = c(1,0,0),
                options = options,
                results = boxplotResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Box Plot
#'
#' 
#' @param data .
#' @param vars .
#' @param group .
#' @param showOutliers .
#' @param showMean .
#' @param colorPalette .
#' @param singleColor .
#' @param order .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' @export
boxplot <- function(
    data,
    vars,
    group,
    showOutliers = TRUE,
    showMean = FALSE,
    colorPalette = "jmv",
    singleColor = FALSE,
    order = "none") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("boxplot requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if ( ! missing(group)) group <- jmvcore::resolveQuo(jmvcore::enquo(group))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL),
            `if`( ! missing(group), group, NULL))

    for (v in group) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- boxplotOptions$new(
        vars = vars,
        group = group,
        showOutliers = showOutliers,
        showMean = showMean,
        colorPalette = colorPalette,
        singleColor = singleColor,
        order = order)

    analysis <- boxplotClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

