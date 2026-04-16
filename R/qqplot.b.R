
# This file is a generated template, your changes will not be overwritten

qqplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "qqplotClass",
    inherit = qqplotBase,
    private = list(
        .init = function() {
            # Stretchable dimensions
            width <- 450
            height <- 450
            # Fixed dimensions
            fixed_height <- 50 # X-Axis legend
            fixed_width <- 75 # Y-Axis legend
            # Legend
            if (!is.null(self$options$group)) {
                if (self$options$legendPosition %in% c('top','bottom'))
                    fixed_height <- fixed_height + 50
                else
                    fixed_width <- fixed_width + 100
            }
            # Set the image dimensions
            image <- self$results$plot
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
            # Show help
            if (is.null(self$options$dep)) {
                private$.showHelpMessage()
            }
        },
        .run = function() {
            depVar <- self$options$dep
            groupVar <- self$options$group

            if (is.null(depVar) || nrow(self$data) == 0)
                return(FALSE)

            data <- jmvcore::select(self$data, c(depVar,groupVar))
            data[[depVar]] <- jmvcore::toNumeric(data[[depVar]])

            data <- jmvcore::naOmit(data)

            if (self$options$transLog)
                data[[depVar]] <- log(data[[depVar]])

            if (self$options$standardize) {
                data[[depVar]] <- (data[[depVar]] - mean(data[[depVar]]))/sd(data[[depVar]])
            }
            image <- self$results$plot
            image$setState(data)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            depVar <- self$options$dep
            depVar <- ensym(depVar)
            groupVar <- self$options$group
            if( !is.null(groupVar) )
                groupVar <- ensym(groupVar)

            distrib <- self$options$distrib

            if (self$options$detrend)
                detrend = TRUE
            else
                detrend = FALSE

            identity = (self$options$refType == "identity")

            plotData <- image$state

            # Check data compatibility with distribution
            varMin <- min(plotData[[depVar]])
            varMax <- max(plotData[[depVar]])
            if (self$options$transLog && !is.finite(varMin))
                errorMessage <- .("Natural Log Tranform requires positive (>0) data.")
            else if (varMin <= 0 && distrib %in% c("lnorm", "chisq", "f", "gamma", "weibull"))
                errorMessage <- jmvcore::format(.("{distrib} distribution requires positive (>0) data."), distrib = private$.distTitleName(distrib))
            else if (varMin < 0 && distrib == "exp")
                errorMessage <- .("Exponential distribution requires non-negative (≥0) data.")
            else if (distrib == "beta" && (varMin <=0 || varMax >= 1))
                errorMessage <- .("Beta distribution requires data between 0 and 1.")
            else
                errorMessage <- NULL

            if (!is.null(errorMessage)) {
                vijErrorMessage(self, errorMessage)
                return(TRUE)
            }

            # Parameter estimations
            paramErrorMessage <- NULL
            if (self$options$paramMethod == "paraEstimate") {
                if (self$options$paramEstMethod == "mle") {
                    try(
                        MASS::fitdistr(x = plotData[[depVar]], densfun = private$.corresp(distrib), start = private$.initVal(distrib))$estimate -> params,
                        silent = TRUE
                    ) -> tryResult
                } else { # paramEstMethod == "mme"
                    try(
                        private$.distParameters(distrib, plotData[[depVar]]) -> params,
                        silent = TRUE
                    ) -> tryResult
                }
                if (class(tryResult) == "try-error" || is.null(params)) {
                    paramErrorMessage <- .("Unable to estimate the distribution parameters.")
                }
            } else {
                params <- private$.userParams(distrib, as.numeric(self$options$param1), as.numeric(self$options$param2))
                if (is.null(params)) {
                    paramErrorMessage <- .("Wrong parameter values.")
                }
            }

            if (!is.null(paramErrorMessage)) {
                vijErrorMessage(self, paramErrorMessage)
                return(TRUE)
            }

            # Everthing is OK
            self$results$paramTable$setVisible(TRUE)
            self$results$plot$setVisible(TRUE)

            # Define the title of the plot (it will be set at the end)
            if (detrend) {
                plotTitle <- jmvcore::format(.("Detrended {distribStr} {typeStr} Plot of {varStr}"),
                                             distribStr = private$.distTitleName(distrib),
                                             typeStr = ifelse(self$options$type == "PP", .("P-P"), .("Q-Q")),
                                             varStr = ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep)
                                        )
            } else {
                plotTitle <- jmvcore::format(.("{distribStr} {typeStr} Plot of {varStr}"),
                                             distribStr = private$.distTitleName(distrib),
                                             typeStr = ifelse(self$options$type == "PP", .("P-P"), .("Q-Q")),
                                             varStr = ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep)
                )
            }

            # Populate the Parameters table
            self$results$paramTable$getColumn('var')$setTitle(private$.distTitleName(distrib))
            for(i in seq_along(params)) {
                self$results$paramTable$addColumn(names(params[i]), type = 'number', format = 'zto')
            }
            self$results$paramTable$setRow(rowNo=1, params)
            self$results$paramTable$setCell(rowNo=1, col = 1, ifelse(self$options$transLog, paste0("LN(",self$options$dep,")"), self$options$dep))
            if (self$options$standardize)
                self$results$paramTable$setNote("1", .("Standardized values"), init=TRUE)
            if (self$options$paramMethod == "paraEstimate")
                self$results$paramTable$setTitle(paste0(.("Parameter Estimates"),ifelse(self$options$paramEstMethod == "mle", " (MLE)", " (MME)")))
            else
                self$results$paramTable$setTitle("Parameter Values")

            # Do the plot
            if (is.null(groupVar))
                plot <- ggplot(data = plotData, mapping = aes(sample = !!depVar))
            else
                plot <- ggplot(data = plotData, mapping = aes(sample = !!depVar, color = !!groupVar, fill = !!groupVar))

            if (self$options$type == "QQ") {
                if (self$options$band) {
                    if (is.null(groupVar))
                        plot <- plot + qqplotr::geom_qq_band(distribution = distrib, dparams = params, bandType = self$options$methodQQ,
                                                         identity = identity, detrend = detrend, alpha=0.5, fill = "darkgrey")
                    else
                        plot <- plot + qqplotr::geom_qq_band(distribution = distrib, dparams = params, bandType = self$options$methodQQ,
                                                             identity = identity, detrend = detrend, alpha=0.5)
                }
                if (self$options$refLine) {
                    if (is.null(groupVar) || identity)
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, dparams = params, identity = identity,
                                                             detrend = detrend, color = "darkgrey")
                    else
                        plot <- plot + qqplotr::stat_qq_line(distribution = distrib, dparams = params, identity = identity,
                                                             detrend = detrend)
                }
                plot <- plot + qqplotr::stat_qq_point(distribution = distrib, identity = identity, dparams = params,
                                                      detrend = detrend, key_glyph = draw_key_rect, show.legend = TRUE)
                if (detrend) {
                    if (self$options$standardize)
                        yLab <- .("Standardized Sample Quantiles Deviation")
                    else
                        yLab <- .("Sample Quantiles Deviation")
                } else {
                    if (self$options$standardize)
                        yLab <- .("Standardized Sample Quantiles")
                    else
                        yLab <- .("Sample Quantiles")
                }
                xLab <- .("Theoretical Quantiles")
            } else { # PP
                if (self$options$band) {
                    if (is.null(groupVar))
                        plot <- plot + qqplotr::stat_pp_band(distribution = distrib, dparams = params, bandType = self$options$methodPP,
                                                             identity = identity, detrend = detrend, alpha=0.5, fill = "darkgrey")
                    else
                        plot <- plot + qqplotr::stat_pp_band(distribution = distrib, dparams = params, bandType = self$options$methodPP,
                                                             identity = identity, detrend = detrend, alpha=0.5)
                }
                if (self$options$refLine) {
                    if (detrend)
                        plot <- plot + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "darkgrey")
                    else
                        plot <- plot + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "darkgrey")
                }
                plot <- plot + qqplotr::stat_pp_point(distribution = distrib, dparams = params,  detrend = detrend,
                                                      key_glyph = draw_key_rect, show.legend = TRUE)
                xLab <- .("Theoretical Probabilities")
                yLab <-  ifelse(detrend, .("Sample Probability Deviation"), .("Sample Probabilities"))
            }

            # Theme and colors
            plot <- plot + ggtheme + vijScale(self$options$colorPalette, "color", drop = FALSE) + vijScale(self$options$colorPalette, "fill", drop = FALSE)

            # Legend
            if (is.null(groupVar))
                plot <- plot + guides(fill = "none")
            else
                plot <- plot + guides(fill = guide_legend(override.aes = list(alpha = 1)))

            # Axis Limits
            if (self$options$yAxisRangeType == "manual")
                yLim <- c(self$options$yAxisRangeMin, self$options$yAxisRangeMax)
            else
                yLim <- NULL
            if (self$options$xAxisRangeType == "manual")
                xLim <- c(self$options$xAxisRangeMin, self$options$xAxisRangeMax)
            else
                xLim <- NULL
            plot <- plot + coord_cartesian(ylim = yLim, xlim = xLim)

            # Ticks
            if (self$options$xTicks > 0) {
                plot <- plot  + scale_x_continuous(breaks = scales::breaks_extended(self$options$xTicks + 1))
            }
            if (self$options$yTicks > 0) {
                plot <- plot  + scale_y_continuous(breaks = scales::breaks_extended(self$options$yTicks + 1))
            }

            # Titles & Labels
            defaults <- list(title = plotTitle, x = xLab , y = yLab, legend = groupVar)
            plot <- plot + vijTitlesAndLabels(self$options, defaults) + vijTitleAndLabelFormat(self$options)

            # Legend spacing
            plot <- plot + theme(legend.key.spacing.y = unit(1, "mm"), legend.byrow = TRUE)

            return (plot)
        },

        # Mod from https://github.com/aloy/qqplotr/blob/master/R/stat_pp_point.R
        .corresp = function(distName) {
            switch(
                distName,
                beta = "beta",
                cauchy = "cauchy",
                chisq = "chi-squared",
                exp = "exponential",
                f = "f",
                gamma = "gamma",
                lnorm = "log-normal",
                logis = "logistic",
                norm = "normal",
                weibull = "weibull",
                NULL
            )
        },

        .initVal = function(distName) {
            switch(
                distName,
                beta = list(shape1 = 1, shape2 = 1),
                chisq = list(df = 1),
                f = list(df1 = 1, df2 = 2),
                NULL
            )
        },

        .distTitleName = function(distName) {
            switch(
                distName,
                beta = .("Beta"),
                cauchy = .("Cauchy"),
                chisq = .("Chi-squared"),
                exp = .("Exponential"),
                f = "F",
                gamma = .("Gamma"),
                lnorm = .("Log-normal"),
                logis = .("Logistic"),
                norm = .("Normal [distribution]"),
                t = .("Student"),
                unif = .("Uniform"),
                weibull = .("Weibull"),
                NULL
            )
        },

        .userParams = function(distName, p1, p2) {
            params <- NULL
            if (distName == "beta" && p1 > 0 && p2 > 0) {
                params <- list(shape1 = p1, shape2 = p2)
            } else if (distName == "cauchy" && p2 > 0) {
                params <- list(location = p1, scale = p2)
            } else if (distName == "chisq" && p1 > 0) {
                params <- list(df = p1)
            } else if (distName == "exp" && p1 > 0) {
                params <- list(rate = p1)
            } else if (distName == "f" && p1 > 0 && p2 > 0) {
                params <- list(df1 = p1, df2 = p2)
            } else if (distName == "gamma" && p1 > 0 && p2 > 0) {
                params <- list(shape = p1, rate = p2)
            } else if (distName == "lnorm" && p2 > 0) {
                params <- list(meanlog = p1, sdlog = p2)
            } else if (distName == "logis" && p2 > 0) {
                params <- list(location = p1, scale = p2)
            } else if (distName == "norm" && p2 > 0) {
                params <- list(mean = p1, sd = p2)
            } else if (distName == "t" && p1 > 0) {
                params <- list(df = p1)
            } else if (distName == "unif" && p1 < p2) {
                params <- list(min = p1, max = p2)
            } else if (distName == "weibull" && p1 > 0 && p2 > 0) {
                params <- list(shape = p1, scale = p2)
            }
            return(params)
        },
        .distParameters = function(distName, aVar) {
            m <- mean(aVar)
            s <- sd(aVar)
            if (distName == "beta") {
                shape1 = m*(m*(1-m)/s**2 - 1)
                shape2 = (1-m)*(m*(1-m)/s**2 - 1)
                params <- list(shape1 = shape1, shape2 = shape2)
            } else if (distName == "exp") {
                rate <- 1/m
                params <- list(rate = rate)
            } else if (distName == "gamma") {
                shape = m**2/s**2
                rate = m/s**2
                params <- list(shape = shape, rate = rate)
            } else if (distName == "lnorm") {
                meanlog <- mean(log(aVar))
                sdlog <- sd(log(aVar))
                params <- list(meanlog = meanlog, sdlog = sdlog)
            } else if (distName == "logis") {
                location <- m
                scale <- sqrt(3)*s/pi
                params <- list(location = location, scale = scale)
            } else if (distName == "norm") {
                params <- list(mean = m, sd = s)
            } else if (distName == "unif") {
                params <- list(min = min(aVar), max = max(aVar))
            } else if (distName %in% c("cauchy","chisq", "f", "weibull","t")) {
                params <- NULL
            }
            return(params)
        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module uses <a href = "https://CRAN.R-project.org/package=qqplotr" target="_blank">qqplotr R package<a/> by Alexandre Almeida, Adam Loy and Heike Hofmann. In-depth information can be found in the package documentation on CRAN site.</p>
<p><strong>Reference line:</strong> Draws either the <em>identity line</em> (y = x) or the commonly-used <em>Q-Q line</em> that intercepts two data quantiles (Q<sub>0.25</sub> and Q<sub>0.75</sub>). P-P plot only supports identity line.</p>
<p><strong>Confidence band:</strong> Draws a confidence band around the reference line. qqplotr package provides several methods to compute the confidence band:</p>
<ul>
<li><strong><em>Pointwise</em></strong> constructs pointwise confidence bands based on Normal confidence intervals;</li>
<li><strong><em>Bootstrap</em></strong> creates pointwise confidence bands based on a parametric bootstrap;</li>
<li><strong><em>Kolmogorov-Smirnov</em></strong> band is based on the Kolmogorov-Smirnov test;</li>
<li><strong><em>Tail-Sensitive</em></strong> constructs a tail-sensitive confidence bands but is only implemented for Normal Q-Q plots;</li>
<li><strong><em>Equal Local Levels (ELL)</em></strong> constructs simultaneous bands using the equal local levels.</li>
</ul>
<p>P-P plots only support "ELL" and "Bootstrap" methods.</p>
<p><strong>Detrended plot:</strong> The objects are <em>detrended</em> according to the reference line. This procedure may help reducing visual bias caused by the orthogonal distances from the points to the reference line.</p>
<p><strong>Parameter values:</strong> The distribution parameters can be estimated from data using:</p>
<ul>
<li><strong>Maximum Likelihood Method</strong> (using MASS package) : it should work with all distributions but "t" and "uniform";</li>
<li><strong>Method of Moments</strong> for moment based parameters : it should work with "normal", "log-normal", "Beta", "Exponential", "Gamma", "Logistic" and "Uniform" distributions;</li>
</ul>
<p>or entered by user:</p>
<ul>
<li><strong>Parameter 1:</strong> mean (Normal), meanlog (Log-normal), shape1 (Beta), location (Cauchy, Logistic), df (Chi-squared, Student), df1 (F), rate (Exponential), shape (Gamma, Weibull) and min (Uniform);</li>
<li><strong>Parameter 2:</strong> sd (normal), sdlog (Log-normal), shape2 (Beta), scale (Cauchy, Logistic), df2 (F), rate (Gamma, Weibull) and max (Uniform).</li>
</ul>
</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
