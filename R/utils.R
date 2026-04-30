vijScale = function(pal, type = "fill", drop = TRUE) {
    themePal <- get('theme', envir = parent.frame())$palette
    palette <- vijPalette(pal, type, themePal)
    return(discrete_scale(aesthetics = type, palette = palette, na.value="gray", drop = drop))
}

vijPalette = function(pal, type = "fill", themePal = NULL) {
    palType <- strsplit(pal, "::")[[1]][1]
    palName <- strsplit(pal, "::")[[1]][2]
    if (is.na(palName)) {
        palName <- palType
        palType <- "brewer"
    }
    if (is.null(themePal)) { # Function is called directly (from plot).
         themePal <- get('theme', envir = parent.frame())$palette
    }
    if (palName == "jmv") {
        jmvPalette <- function(n) jmvcore::colorPalette(n, pal = themePal, type = type)
        attr(jmvPalette,"nlevels") <- 5
        return(jmvPalette)
    } else if (palType == "brewer") {
        return(scales::pal_brewer(palette = palName))
    } else if (palType == "viridis") {
        return(scales::pal_viridis(option = palName))
    } else if (palType == "dichromat") {
        return(scales::pal_dichromat(palName))
    } else if (palType == "tidy") {
        if (palName == "friendly") {
            tidyColors <- c("#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00")
        } else if (palName == "seaside") {
            tidyColors <- c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500")
        } else if (palName == "apple") {
            tidyColors <- c("#ff3b30", "#ff9500", "#ffcc00", "#4cd964", "#5ac8fa", "#007aff", "#5856d6")
        } else if (palName == "ibm") {
            tidyColors <- c("#5B8DFE", "#725DEE", "#DD227D", "#FE5F00", "#FFB109")
        } else if (palName == "candy") {
            tidyColors <- c("#9b5de5", "#f15bb5", "#fee440", "#00bbf9", "#00f5d4")
        } else if (palName == "alger") {
            tidyColors <- c("#000000", "#1A5B5B", "#ACC8BE", "#F4AB5C", "#D1422F")
        } else if (palName == "rainbow") {
            tidyColors <- c("#FF7777", "#FFAB74", "#FFE577", "#DBF47B", "#91E480", "#7CC9E5", "#7DA8E6", "#887DE6", "#BC7BE4")
        } else if (palName == "metro") {
            tidyColors <- c("#4DACD6","#4FAE62","#F6C54D","#E37D46","#C02D45")
        }
        tidyPalette <- grDevices::colorRampPalette(tidyColors)
        attr(tidyPalette,"nlevels") <- length(tidyColors)
        return(tidyPalette)
    } else if (palType == "custom") {
        if (palName == "lemovice")
            customColors <- c("#16144e", "#00dc8c", "#5fcdcd", "#007387", "#efbe7c", "#8c87a4", "#ff6e5a", "#bc6479", "#8faadc", "#006d4d")
        else if (palName == "tidyplots")
            customColors <- c("#0072B2","#56B4E9","#009E73","#F5C710","#E69F00","#D55E00") # unused
        return(scales::pal_manual(values = customColors, type = "colour"))
    } else {
        return(NULL)
    }
}

vijTitlesAndLabels = function(options, defaults = list(), plotType = '') {
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
    y <- options[["yAxisText"]] %||% ''
    if (horizontal)
        y <- options[["xAxisText"]] %||% ''
    else
        y <- options[["yAxisText"]] %||% ''
    if (y == "")
        y <- defaults$y

    return(ggplot2::labs(title = title, subtitle = subtitle, caption = caption, fill = legend, color = legend, shape = legend, size = sizeLegend, x = x, y = y))
}

vijTitleAndLabelFormat = function(options, showLegend = TRUE) {
    horizontal <- options[["horizontal"]]  %||% FALSE
    if (showLegend) {
        legendPosition  <- options$legendPosition
        legendFontSize <- as.integer(options$legendFontSize)
    } else {
        legendPosition <- "none"
        legendFontSize <- 14
    }
    xAxisLabelFontSize <- options[["xAxisLabelFontSize"]] %||% 12
    xAxisLabelRotation <- options[["xAxisLabelRotation"]] %||% 0
    yAxisLabelFontSize <- options[["yAxisLabelFontSize"]] %||% 12
    yAxisLabelRotation <- options[["yAxisLabelRotation"]] %||% 0
    return(ggplot2::theme(
        # Title, subtitle and caption
        plot.title = element_text(
            size = options$titleFontSize,
            face = options$titleFontFace,
            hjust = as.numeric(options$titleAlign)),
        plot.subtitle = element_text(
            size = options$subtitleFontSize,
            face = options$subtitleFontFace,
            hjust = as.numeric(options$subtitleAlign),
            margin = margin(-5, 0, 15, 0)),
        plot.caption = element_text(
            size = options$captionFontSize,
            face = options$captionFontFace,
            hjust = as.numeric(options$captionAlign)),
        # Legend
        legend.title=element_text(
            size = (legendFontSize + 1)),
        legend.text=element_text(
            size = legendFontSize),
        legend.position = legendPosition,
        legend.box = "vertical", # for legend at bottom
        legend.margin = margin(b=0), # for multiple legends
        # Facet Label ~ subtitle
        strip.text = element_text(
            size = options$subtitleFontSize,
            face = options$subtitleFontFace,
            hjust = as.numeric(options$subtitleAlign)),
        # Axis Titles
        axis.title.x = element_text(
            size = options[["xAxisFontSize"]] %||% 14,
            hjust = as.numeric(options[["xAxisPosition"]] %||% 0)
        ),
        axis.title.y = element_text(
            size = options[["yAxisFontSize"]] %||% 14,
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
    # Hide other results
    for (resName in names(self$results)) {
        aResult <- get0(resName, self$results)
        if (!is.null(aResult))
            aResult$setVisible(FALSE)
    }
    # Display Error message
    errorNotice <- jmvcore::Notice$new(self$options, type = jmvcore::NoticeType$ERROR, name = '.warning', content = errorMessage)
    self$results$insert(1, errorNotice)
}

vijWarningMessage = function(self, warningMessage, name = '.warning') {
    warningNotice <- jmvcore::Notice$new(self$options, type = jmvcore::NoticeType$WARNING, name = name, content = warningMessage)
    self$results$insert(1, warningNotice)
}
