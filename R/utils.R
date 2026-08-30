vijColorScale = function(pal, type = "fill", theme, drop = TRUE) {
    palette <- vijColorPalette(pal, type, theme)
    # drop = FALSE to include unused levels in color scales
    return(ggplot2::discrete_scale(aesthetics = type, palette = palette, na.value="gray", drop = drop))
}

vijColorPalette = function(pal, type = "fill", theme) {
    palType <- strsplit(pal, "::")[[1]][1]
    palName <- strsplit(pal, "::")[[1]][2]
    if (is.na(palName)) {
        palName <- palType
        palType <- "brewer"
    }
    if (palName == "jmv") {
        jmvPalette <- function(n) jmvcore::colorPalette(n, pal = theme$palette, type = type)
        attr(jmvPalette,"nlevels") <- 5
        return(jmvPalette)
    } else if (palType == "brewer") {
        return(scales::pal_brewer(palette = palName))
    } else if (palType == "viridis") {
        return(scales::pal_viridis(option = palName))
    } else if (palType == "dichromat") {
        return(scales::pal_dichromat(palName))
    } else if (palType == "tidy") {
        tidyColors <- switch(palName,
            friendly = c("#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00"),
            seaside  = c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500"),
            apple    = c("#ff3b30", "#ff9500", "#ffcc00", "#4cd964", "#5ac8fa", "#007aff", "#5856d6"),
            ibm      = c("#5B8DFE", "#725DEE", "#DD227D", "#FE5F00", "#FFB109"),
            candy    = c("#9b5de5", "#f15bb5", "#fee440", "#00bbf9", "#00f5d4"),
            alger    = c("#000000", "#1A5B5B", "#ACC8BE", "#F4AB5C", "#D1422F"),
            rainbow  = c("#FF7777", "#FFAB74", "#FFE577", "#DBF47B", "#91E480", "#7CC9E5", "#7DA8E6", "#887DE6", "#BC7BE4"),
            metro    = c("#4DACD6","#4FAE62","#F6C54D","#E37D46","#C02D45"),
            NULL
        )
        if (is.null(tidyColors))
            return(NULL)
        tidyPalette <- grDevices::colorRampPalette(tidyColors)
        attr(tidyPalette,"nlevels") <- length(tidyColors)
        return(tidyPalette)
    } else if (palType == "custom") {
        customColors <- switch(palName,
            lemovice  = c("#16144e", "#00dc8c", "#5fcdcd", "#007387", "#efbe7c", "#8c87a4", "#ff6e5a", "#bc6479", "#8faadc", "#006d4d"),
            carbon_dark = c("#6929c4", "#1192e8", "#005d5d", "#9f1853", "#fa4d56", "#570408", "#198038", "#002d9c", "#ee538b", "#b28600", "#009d9a", "#012749", "#8a3800", "#a56eff"),
            carbon_light = c("#8a3ffc", "#33b1ff", "#007d79", "#ff7eb6", "#fa4d56", "#fff1f1", "#6fdc8c", "#4589ff", "#d12771", "#d2a106", "#08bdba", "#bae6ff", "#ba4e00", "#d4bbff"),
            NULL
        )
        if (is.null(customColors))
            return(NULL)
        return(scales::pal_manual(values = customColors, type = "colour"))
    } else {
        return(NULL)
    }
}

vijColorPaletteNlevels = function(pal) {
    # scales >= 1.4.0 palette_nlevels() returns the number of colors (using nlevels attribute)
    # this function uses both palette_nlevels() and attr(pal, "nlevels") for security
    nl <- tryCatch(scales::palette_nlevels(pal), error = function(e) NA)
    if (is.na(nl))
        nl <- attr(pal, "nlevels")
    if (is.null(nl))
        nl <- 6L
    return(nl)
}

vijOneColorOfPalette = function(pal, type = "fill", theme, colorNo) {
    selectedColorPalette <- vijColorPalette(pal, type, theme)
    nbColors <- vijColorPaletteNlevels(selectedColorPalette)
    oneColorOfPalette <- selectedColorPalette(nbColors)[min(colorNo,nbColors)]
    return(oneColorOfPalette)
}

vijVar = function(name) if (!is.null(name)) rlang::sym(name) else NULL

vijTitlesAndLabels = function(options, defaults = list(), plotType = '', plot = NULL) {
    horizontal <- options[["horizontal"]]  %||% FALSE
    # Title & Subtitle
    if (plotType == '') {
        title <- options$titleText %||% ''
        subtitle <- options$subtitleText %||% ''
        caption <- options$captionText %||% ''
    } else {
        title <- options[[paste0(plotType,"TitleText")]] %||% ''
        subtitle <- options[[paste0(plotType,"SubtitleText")]] %||% ''
        caption <- options[[paste0(plotType,"CaptionText")]] %||% ''
    }
    #default <- eval.parent(quote(.("default")))
    default <- c("default", "par défaut", "por defecto", "per defecte",
                 "standard", "Standard", "standaard", "oletus", "zadano",
                 "výchozí", "domyślny", "padrão", "implicit", "privzeto",
                 "alapértelmezett", "predefinito",
                 "προεπιλογή","varsayılan", "по умолчанию", "за замовчуванням",
                 "இயல்பு", "ഡിഫോൾട്ട്",
                 "기본값", "初期値", "默认", "預設")
    # Title
    if (title == "")
        title <- NULL
    else if (title %in% default)
        title <- defaults$title
    # Subtitle
    if (subtitle == "")
        subtitle <- NULL
    else if (subtitle %in% default)
        subtitle <- defaults$subtitle
    # Caption
    if (caption == "")
        caption <- NULL
    else if (caption %in% default)
        caption <- defaults$caption
    # Legend
    legend <- options[["legendText"]] %||% ''
    if (legend == "")
        legend <- defaults$legend
    # Size Legend
    sizeLegend <- defaults$sizeLegend
    # xAxis
    if (horizontal)
        x <- options[["yAxisText"]] %||% ''
    else
        x <- options[["xAxisText"]] %||% ''
    if (x == "")
        x <- defaults$x
    # yAxis
    if (horizontal)
        y <- options[["xAxisText"]] %||% ''
    else
        y <- options[["yAxisText"]] %||% ''
    if (y == "")
        y <- defaults$y

    labsArgs <- list(title = title, subtitle = subtitle, caption = caption, size = sizeLegend, x = x, y = y)

    # Legend should be set only when used in aes to prevent ggplot message (hidden from user, though)
    # So we read the mappings to find the the name actually used. (using get_labs would call ggplot_build)
    if (is.null(plot)) {
        presentAes <- c("fill","colour")
    } else {
        presentAes <- character(0)
        for (l in plot$layers) {
            layerAes <- character(0)
            if (isTRUE(l$inherit.aes))
                layerAes <- c(layerAes, names(plot$mapping))
            layerAes <- c(layerAes, names(l$mapping))
            # A layer's own fixed parameter (e.g. geom_histogram(fill = fillColor))
            # shadows an inherited aes mapping for that layer, same as ggplot2's
            # own setup_plot_labels() exclusion of layer$aes_params.
            layerAes <- setdiff(layerAes, names(l$aes_params))
            presentAes <- c(presentAes, layerAes)
        }
        presentAes <- unique(presentAes)
    }
    if ("fill" %in% presentAes)
        labsArgs[["fill"]] <- legend
    if ("colour" %in% presentAes)
        labsArgs[["colour"]] <- legend
    if ("shape" %in% presentAes)
        labsArgs[["shape"]] <- legend

    return(ggplot2::labs(!!!labsArgs))
}

vijTitleAndLabelFormat = function(options, showLegend = TRUE) {
    horizontal <- options[["horizontal"]]  %||% FALSE
    if (showLegend) {
        legendPosition  <- options$legendPosition
        legendFontSize <- as.numeric(options$legendFontSize)
    } else {
        legendPosition <- "none"
        legendFontSize <- 14
    }
    # Font sizes and alignments come from List options, i.e. as strings; since
    # ggplot2 4.0.0 the theme elements validate their types and reject those.
    titleFontSize <- as.numeric(options$titleFontSize)
    subtitleFontSize <- as.numeric(options$subtitleFontSize)
    captionFontSize <- as.numeric(options$captionFontSize)
    xAxisFontSize <- as.numeric(options[["xAxisFontSize"]] %||% 14)
    yAxisFontSize <- as.numeric(options[["yAxisFontSize"]] %||% 14)
    xAxisLabelFontSize <- as.numeric(options[["xAxisLabelFontSize"]] %||% 12)
    xAxisLabelRotation <- as.numeric(options[["xAxisLabelRotation"]] %||% 0)
    yAxisLabelFontSize <- as.numeric(options[["yAxisLabelFontSize"]] %||% 12)
    yAxisLabelRotation <- as.numeric(options[["yAxisLabelRotation"]] %||% 0)
    # Facet style
    facetStyle <- options[["facetStyle"]] %||% "default"
    facetAlign <- as.numeric(options[["facetAlign"]] %||% 0.5)
    facetFontSize <- as.numeric(options[["facetFontSize"]] %||% 12)
    facetFontFace <- options[["facetFontFace"]] %||% "plain"
    return(ggplot2::theme(
        # Title, subtitle and caption
        plot.title = ggplot2::element_text(
            size = titleFontSize,
            face = options$titleFontFace,
            hjust = as.numeric(options$titleAlign)),
        plot.subtitle = ggplot2::element_text(
            size = subtitleFontSize,
            face = options$subtitleFontFace,
            hjust = as.numeric(options$subtitleAlign),
            margin = ggplot2::margin(-5, 0, 15, 0)),
        plot.caption = ggplot2::element_text(
            size = captionFontSize,
            face = options$captionFontFace,
            hjust = as.numeric(options$captionAlign)),
        # Legend
        legend.title=ggplot2::element_text(
            size = (legendFontSize + 1)),
        legend.text=ggplot2::element_text(
            size = legendFontSize),
        legend.position = legendPosition,
        legend.box = "vertical", # for legend at bottom
        legend.margin = ggplot2::margin(b=0), # for multiple legends
        # Facet Label
        strip.text = ggplot2::element_text(size = facetFontSize, face = facetFontFace, hjust = facetAlign),
        strip.background = switch(facetStyle, "background" = ggplot2::element_rect(fill="lightgray"), "border" = ggplot2::element_rect(color = "black", fill = "white"), ggplot2::element_rect()),
        # Axis Titles
        axis.title.x = ggplot2::element_text(
            size = xAxisFontSize,
            hjust = as.numeric(options[["xAxisPosition"]] %||% 0)
        ),
        axis.title.y = ggplot2::element_text(
            size = yAxisFontSize,
            hjust = as.numeric(options[["yAxisPosition"]] %||% 0)
        ),
        # Axis Labels
        axis.text.x = ggplot2::element_text(
            size = xAxisLabelFontSize,
            angle = xAxisLabelRotation
        ),
        axis.text.y = ggplot2::element_text(
            size = yAxisLabelFontSize,
            angle = yAxisLabelRotation)
    ))
}

vijHelpMessage = function(self, htmlText) {
    # Hide other results
    for (resName in names(self$results)) {
        aResult <- get0(resName, self$results)
        if (!is.null(aResult))
            aResult$setVisible(FALSE)
    }
    # Display Help message
    helpMsg <- paste(
        "<style>.block {border: 2px solid gray;border-radius: 15px;background-color: WhiteSmoke;padding: 5px 20px;text-align: justify;}</style>",
        "<div class=\"block\">",
        htmlText,
        "</div>")
    helpHtml <- jmvcore::Html$new(self$options, name = '.help', content = helpMsg)
    self$results$insert(1, helpHtml)
}

vijErrorMessage = function(self, errorMessage) {
    return(jmvcore::reject(errorMessage))
}

vijWarningMessage = function(self, warningMessage, name = NULL, pos = 1) {
    name <- name %||% rlang::hash(warningMessage)
    warningNotice <- jmvcore::Notice$new(self$options, type = jmvcore::NoticeType$WARNING, name = name, content = warningMessage)
    self$results$insert(pos, warningNotice)
}

vijDebugMessage = function(self, debugMessage, name = NULL, title = "Debug") {
    name <- name %||% rlang::hash(debugMessage)
    debugMsg <- jmvcore::Preformatted$new(self$options, name = name, title = title)
    debugMsg$setContent(debugMessage)
    self$results$insert(1, debugMsg)
}

# Debug helper: forces the plot to build now (instead of waiting for jmvcore's own
# print(), which swallows warnings/messages via suppressWarnings/suppressMessages),
# surfacing any ggplot2 warning or message (e.g. cli_inform() diagnostics like
# "Ignoring unknown labels") as a Notice in the results pane. Called for its side
# effect, e.g. `vijDebugPlot(self, plot); return(plot)` at the end of .plot().
# No-op on a release version (a plain x.y.z DESCRIPTION Version); only active on a
# development version (x.y.z.w), so it never surfaces to end users of a published module.
vijDebugPlot = function(self, p) {
    if (length(unclass(utils::packageVersion("vijPlots"))[[1]]) < 4)
        return(invisible(NULL))
    withCallingHandlers({
        ggplot2::ggplot_build(p)
    }, warning = function(w) {
        # Benign macOS/R quirk: strptime() (used internally by scale_x_date's
        # date_breaks for month/year/... steps) validates the *system* timezone
        # against a tzdata copy that can diverge from the one R itself uses,
        # even for well-known zone names. Doesn't affect the computed breaks.
        if (!grepl("^unknown timezone", conditionMessage(w)))
            vijDebugMessage(self, conditionMessage(w), title = "Debug Plot")
        invokeRestart("muffleWarning")
    }, message = function(m) {
        vijDebugMessage(self, conditionMessage(m))
        invokeRestart("muffleMessage")
    })
    invisible(NULL)
}
