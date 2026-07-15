
# This file is a generated template, your changes will not be overwritten

likertplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "likertplotClass",
    inherit = likertplotBase,
    private = list(
        .init = function() {
            if (length(self$options$liks) == 0) {
                private$.showHelpMessage()
                return()
            } else {
                if (is.null(self$options$group))
                    self$results$comp$setVisible(FALSE)
            }
            # Stretchable dimensions
            if (!is.null(self$options$group))
                nbOfBars<- nlevels(self$data[[self$options$group]]) * max(1,length(self$options$liks))
            else
                nbOfBars <- max(1,length(self$options$liks))
            width <- 500
            height <- max(200, nbOfBars*50)
            # Fixed dimension
            fixed_width <- 75 # Y-Axis legend
            fixed_height <- 50 # X-Axis legend
            # Set the image dimensions
            image <- self$results$plot
            if (is.null(image[['setSize2']])) { # jamovi < 2.7.16
                image$setSize(width + fixed_width, height + fixed_height)
            } else {
                image$setSize2(width, height, fixed_width, fixed_height)
            }
        },
        .run = function() {
            if (length(self$options$liks) == 0 || nrow(self$data) == 0)
                return()

            mainData <- self$data[c(self$options$liks, self$options$group)]

            # Check if ordered factor
            for (ques in self$options$liks) {
                varAttrib <- attr(mainData[[ques]],"class",TRUE)
                if ( ("factor" %in% varAttrib)  && !("ordered" %in% varAttrib) ) {
                    vijErrorMessage(self, .("Likert Plot requires ordinal (or numeric) variables"))
                    return(TRUE)
                }
            }

            # Check if canBeNumeric (if median/mean/tests/to integer / tidy up requested)
            canbeNum <- TRUE
            for (ques in self$options$liks)
                canbeNum <- canbeNum && jmvcore::canBeNumeric(mainData[[ques]])

            if (!canbeNum && self$options$frequencyTable && (self$options$showMedian || self$options$showMean))
                errorMessage <- .("Median and mean require numeric variables")
            else if (!canbeNum && (self$options$showMannU || self$options$showKW || self$options$showPostHoc))
                errorMessage <- .("Comparison tests require numeric variables")
            else if (!canbeNum && self$options$toInteger)
                errorMessage <- .("Cannot convert text variables to integers")
            else if (!canbeNum && self$options$tidyUp)
                errorMessage <- .("Cannot tidy up text variables")
            else
                errorMessage <- NULL

            if (!is.null(errorMessage)) {
                vijErrorMessage(self, errorMessage)
                return(TRUE)
            }

            #### Convert to integer ####
            if (self$options$toInteger) {
                for (var in self$options$liks) {
                    mainData[[var]] <- factor(jmvcore::toNumeric(mainData[[var]]))
                    attr(mainData[[var]], "values") <- as.integer(levels(mainData[[var]]))
                    attr(mainData[[var]], "class") <- c("ordered","factor")
                }
            }

            #### Tidy up levels ####
            if (self$options$tidyUp) {
                all_values <- c()
                level_value <- list()
                # Save all levels and values from the variables
                for (ques in self$options$liks) {
                    levels <- levels(mainData[[ques]])
                    values <- attr(mainData[[ques]], "values", TRUE)
                    all_values <- c(all_values, values)
                    for (i in seq_along(levels)){ # for each value, save the associate level (once)
                        if(is.null(level_value[[as.character(values[i])]]))
                            level_value[[as.character(values[i])]] <- levels[i]
                    }
                }
                # Sort the value and the corresponding levels
                tidyValues <- sort(unique(all_values))
                tidyLevels <- unlist(level_value[as.character(tidyValues)], use.names=FALSE)
                # Set the variable again as factors with tidy levels/values
                for (ques in self$options$liks) {
                    mainData[[ques]] <- factor(jmvcore::toNumeric(mainData[[ques]]), levels = tidyValues, labels = tidyLevels)
                    attr(mainData[[ques]], "values") <- tidyValues
                    attr(mainData[[ques]], "class") <- c("ordered","factor")
                    attr(mainData[[ques]], "jmv-retain-unused") <- TRUE
                }
            }

            # Missing group cases
            if (!is.null(self$options$group)) {
                if (!self$options$ignoreNA) {
                    # change NA to "NA"
                    if (anyNA(mainData[[self$options$group]]))
                        mainData[[self$options$group]] <- forcats::fct_na_value_to_level(mainData[[self$options$group]], level="NA")
                }
            }
            # Cleaning the group variable name (it would crash gglikert)
            if (!is.null(self$options$group)) {
                groupingVar <- jmvcore::toB64(self$options$group)
                #names(mainData)[length(names(mainData))] <- groupingVar
                names(mainData)[names(mainData) == self$options$group] <- groupingVar
            } else {
                groupingVar <- NULL
            }

            # Data for table
            ggLikertData <- ggstats::gglikert_data(tibble::as_tibble(mainData), include = self$options$liks, sort = self$options$sorting)
            questions <- levels(ggLikertData[['.question']])
            nq <- length(questions)
            if (!is.null(groupingVar)) {
                groups <- levels(ggLikertData[[groupingVar]])
                ng <- length(groups)
            } else {
                ng <- 0
            }

            #### Compute frenquencies by question/group ####
            freq_wide <- private$.computeFrequencies(ggLikertData, groupingVar, ng)

            #### Frequency table ####
            if (self$options$frequencyTable) {
                if (self$options$frequencies == "counts") {
                    fType <- 'integer'
                    fmt <- ''
                } else {
                    fType <- 'number'
                    fmt <- 'pc'
                }
                # Set columns
                if (ng > 0) {
                    self$results$frequencies$addColumn(self$options$group, type = "text", title = private$.getVarName(self$options$group))
                }
                self$results$frequencies$addColumn("Sum", type = "integer", title = "N")
                answers_levels <- levels(ggLikertData[['.answer']])
                for (col in answers_levels) {
                    self$results$frequencies$addColumn(col, type = fType, format = fmt, title = col)
                }
                if (self$options$showMedian)
                    self$results$frequencies$addColumn("Median", type = "number", title = .("Median"))
                if (self$options$showMean) {
                    self$results$frequencies$addColumn("Mean", type = "number")
                    self$results$frequencies$addColumn("SD", type = "number", title = .("SD"))
                }

                # Populate the table
                if (ng == 0) { # Freq table without grouping variable
                    for (ques in questions) {
                        row_data <- dplyr::filter(freq_wide, .question == ques)
                        values <- as.list(row_data)
                        values[".question"] <- private$.getVarName(ques)
                        numericData <- jmvcore::toNumeric(mainData[[ques]])
                        if (self$options$showMedian)
                            values['Median'] <- as.numeric(median(numericData, na.rm = TRUE))
                        if (self$options$showMean) {
                            values["Mean"] <- mean(numericData, na.rm = TRUE)
                            values["SD"] <- sd(numericData, na.rm = TRUE)
                        }
                        self$results$frequencies$addRow(rowKey = ques, values = values)
                    }
                } else { # Freq table by group
                    group_sym <- rlang::ensym(groupingVar)
                    for (ques in questions) {
                        firstGroup <- TRUE
                        for (group in groups) {
                            groupAndQues <- paste0(group, ques)
                            row_data <- dplyr::filter(freq_wide, .question == ques & !!group_sym == group)
                            values <- as.list(row_data)
                            if (firstGroup)
                                values[".question"] <- private$.getVarName(ques)
                            else
                                values[".question"] <- " "
                            values[[self$options$group]] <- group
                            numericData <- jmvcore::toNumeric(mainData[[ques]])[mainData[[groupingVar]] == group]

                            if (self$options$showMedian)
                                values["Median"] <- as.numeric(median(numericData, na.rm = TRUE))
                            if (self$options$showMean) {
                                values["Mean"] <- mean(numericData, na.rm = TRUE)
                                values["SD"] <- sd(numericData, na.rm = TRUE)
                            }
                            self$results$frequencies$addRow(rowKey = groupAndQues, values = values)
                            if (firstGroup)
                                self$results$frequencies$addFormat(rowKey = groupAndQues, 1, jmvcore::Cell.BEGIN_GROUP)
                            firstGroup <- FALSE
                        }
                        self$results$frequencies$addFormat(rowKey = groupAndQues, 1, jmvcore::Cell.END_GROUP)
                    }
                }
            } # End Frenquency Table

            # p correction method
            adjustMethod <- self$options$adjustMethod
            adjustMethodStr <- paste0(toupper(substring(adjustMethod, 1, 1)), substring(adjustMethod, 2))
            if (adjustMethod == "BH")
                adjustMethodStr <- "Benjamini-Hochberg"
            else if (adjustMethod == "BY")
                adjustMethodStr <- "Benjamini-Yekutieli"

            #### Mann Whitney U ####
            if ( ng > 1 && self$options$showMannU) {
                if (ng != 2) {
                    self$results$comp$uTestTable$setNote("p","Mann-Whitney tests require two groups")
                    for (ques in questions) { # Empty table
                        self$results$comp$uTestTable$setRow(rowKey = ques,
                                                            values = list("ques" = private$.getVarName(ques), statistic = NULL, p.value = NULL, adjusted.p = NULL))
                    }
                } else {
                    p <- c()
                    for (ques in questions) {
                        mannU <- private$.mannU(ques, groupingVar, mainData)
                        mannU[["ques"]] <- private$.getVarName(ques)
                        self$results$comp$uTestTable$setRow(rowKey = ques, values = mannU)
                        p <- c(p, mannU[["p.value"]])
                    }
                    if (self$options$pValue == "overall") {
                        self$results$comp$uTestTable$addColumn(name = "adjusted.p", title = .("Adj. p"), type = 'number', format = 'zto,pvalue')
                        adjustedp <- p.adjust(p, method = adjustMethod)
                        for (i in 1:nq) {
                            self$results$comp$uTestTable$setCell(rowNo = i, col = "adjusted.p", ifelse(is.finite(adjustedp[i]),adjustedp[i],NA))
                        }
                        self$results$comp$uTestTable$setNote("adj",
                            jmvcore::format(.("p-values are adjusted using {method} method."), method = adjustMethodStr))
                    }
                }
            }

            #### Kruskal-Wallis tests ####
            if (ng > 1 && self$options$showKW) {
                p <- c()
                for (ques in questions) {
                    res <- private$.kruskalW(ques, groupingVar, mainData)
                    res[["ques"]] <- private$.getVarName(ques)
                    self$results$comp$kwTable$setRow(rowKey = ques, values = res)
                    p <- c(p, res[["p.value"]])
                }
                if (self$options$pValue == "overall") {
                    self$results$comp$kwTable$addColumn(name = "adjusted.p", title = .("Adj. p"), type = 'number', format = 'zto,pvalue')
                    adjustedp <- p.adjust(p, method = adjustMethod)
                    for (i in 1:nq) {
                        self$results$comp$kwTable$setCell(rowNo = i, col = "adjusted.p", ifelse(is.finite(adjustedp[i]),adjustedp[i],NA))
                    }
                    self$results$comp$kwTable$setNote("adj",
                        jmvcore::format(.("p-values are adjusted using {method} method."), method = adjustMethodStr))
                }
            }

            #### Pairwise comparison table ####
            if (ng > 1 && self$options$showPostHoc) {
                # Set title and statistic column title
                self$results$comp$pwTable$setTitle(switch(self$options$postHoc,
                                                          "conover" = .("Conover's Pairwise Comparisons"),
                                                          "dunn" = .("Dunn's Pairwise Comparisons"),
                                                          "dscf" = .("Dwass-Steel-Critchlow-Fligner Pairwise Comparisons")
                                                    )
                )
                statString <- switch(self$options$postHoc,
                                     "conover" = "T",
                                     "dunn" = "Z",
                                     "dscf" = "W*"
                )

                # Compute tests
                res.statistics <- list()
                res.p.values <- list()
                res.p.adjusted <- list()
                for (ques in questions) {
                    if (self$options$postHoc == "conover") {
                        testRes <- private$.conoverTest(jmvcore::toNumeric(mainData[[ques]]), mainData[[groupingVar]])
                    } else if (self$options$postHoc == "dunn") {
                        testRes <- private$.dunnTest(jmvcore::toNumeric(mainData[[ques]]), mainData[[groupingVar]])
                    } else if (self$options$postHoc == "dscf") {
                        testRes <- private$.dscfAllPairsTest(mainData[[ques]], mainData[[groupingVar]])
                    }
                    res.p.values[[ques]] <- testRes$p.values
                    res.statistics[[ques]] <- testRes$statistics
                    # Compute groupwise adjusted p
                    if (self$options$pValue == "group" && self$options$postHoc != "dscf" && length(res.p.values[[ques]]) > 0) {
                        res.p.adjusted[[ques]] <- p.adjust(res.p.values[[ques]], method = adjustMethod)
                    } else {
                        res.p.adjusted[[ques]] <- list()
                    }
                }

                # Compute overall adjusted p
                pvalues <- c()
                if (self$options$pValue == "overall" && self$options$postHoc != "dscf") {
                	# Gather the pValues question-wise
                    for (ques in questions) {
                        if (length(res.p.values[[ques]]) > 0) {
                            pvalues <- c(pvalues, unlist(res.p.values[[ques]]))
                        }
                    }
                    # Adjust the pValues then split them back by question
                    if (length(pvalues) > 0) {
                        pvalues_adj_flat <- p.adjust(pvalues, method = adjustMethod)
                        idx <- 0
                        for (ques in questions) {
                            k <- length(res.p.values[[ques]])
                            if (k > 0) {
                                res.p.adjusted[[ques]] <- pvalues_adj_flat[(idx + 1):(idx + k)]
                                names(res.p.adjusted[[ques]]) <- names(res.p.values[[ques]])
                                idx <- idx + k
                            }
                        }
                    }
                }

                self$results$comp$pwTable$setStatus('running')

                # Add table's columns
                for (ques in questions) {
                    superTitle <- private$.getVarName(ques)
                    self$results$comp$pwTable$addColumn(name = paste(ques, "stat"), title = statString, superTitle = superTitle, type = 'number')
                    self$results$comp$pwTable$addColumn(name = paste(ques, "p"), title = "p",
                                                        superTitle = superTitle, type = 'number', format = 'zto,pvalue')
                    if (self$options$pValue != "none" && self$options$postHoc != "dscf")
                        self$results$comp$pwTable$addColumn(name = paste(ques, "p.adj"), title = .("Adj. p"), superTitle = superTitle, type = 'number', format = 'zto,pvalue')
                }

                # Populate table
                for (i in 1:(ng-1)) {
                    for (j in (i+1):ng) {
                        values <- list("group1" = groups[i], "group2" = groups[j])
                        compStr1 <- paste(groups[i], "-", groups[j])
                        compStr2 <- paste(groups[j], "-", groups[i])
                        for (ques in questions) {
                            # let test both compStr1 and compStr2
                            # because conover.test and dunn.test don't keep the factor order
                            stats1 <- (res.statistics[[ques]])[[compStr1]]
                            stats2 <- (res.statistics[[ques]])[[compStr2]]
                            if (!is.null(stats1)) {
                                values[paste(ques, "stat")] <- stats1
                                values[paste(ques, "p")] <- (res.p.values[[ques]])[[compStr1]]
                                if (self$options$pValue != "none") {
                                    values[paste(ques, "p.adj")] <- (res.p.adjusted[[ques]])[compStr1]
                                }
                            } else if (!is.null(stats2)) {
                                values[paste(ques, "stat")] <- -stats2
                                values[paste(ques, "p")] <- (res.p.values[[ques]])[[compStr2]]
                                if (self$options$pValue != "none") {
                                    values[paste(ques, "p.adj")] <- (res.p.adjusted[[ques]])[compStr2]
                                }
                            } else {
                                values[paste(ques, "stat")] <- NA
                                values[paste(ques, "p")] <- NA
                                if (self$options$pValue != "none") {
                                    values[paste(ques, "p.adj")] <- NA
                                }
                            }
                        }
                        self$results$comp$pwTable$addRow(rowKey = paste0(i,"/",j), values = values)
                    }
                }

                # Add Note
                if (self$options$pValue == "overall" && self$options$postHoc != "dscf" && length(pvalues) > 0){
                    self$results$comp$pwTable$setNote("adj",
                        jmvcore::format(.("p-values are adjusted overall using {method} method."), method = adjustMethodStr))
                }
                if (self$options$pValue == "group" && self$options$postHoc != "dscf") {
                    self$results$comp$pwTable$setNote("adj",
                        jmvcore::format(.("p-values are adjusted groupwise using {method} method."), method = adjustMethodStr))
                }
                if (self$options$postHoc == "dscf") {
                    self$results$comp$pwTable$setNote("adj",.("DSCF p-values are adjusted groupwise."))
                }
                self$results$comp$pwTable$setStatus('complete')
            }

            #### Set image data ####
            plotData <- list()
            plotData$mainData <- mainData
            plotData$variableLabels <- sapply(self$options$liks, FUN = private$.getVarName) #, USE.NAMES = FALSE)
            if (!is.null(self$options$group))
                plotData$groupLabel <- private$.getVarName(self$options$group)
            image <- self$results$plot
            image$setState(plotData)

        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            mainData <- image$state$mainData

            if (self$options$reverseLikert ) { #&& !self$options$toInteger) {
                for (var in self$options$liks)
                    mainData[[var]] <- forcats::fct_rev(mainData[[var]])
            }

            # Variable & group Labels
            variable_labels <- image$state$variableLabels
            if (!is.null(self$options$group)) {
                groupingVar <- names(mainData)[length(names(mainData))]
            } else {
                groupingVar <- NULL
            }

            if (!is.null(groupingVar)) {
                # Remove cases with missing group or change NA to "NA"
                if (self$options$ignoreNA)
                    mainData <- subset(mainData, !is.na(mainData[groupingVar]))
            }

            # options
            accuracy <- as.numeric(self$options$accuracy)
            hLabelWrap <- as.numeric(self$options$hLabelWrap)
            vLabelWrap <- as.numeric(self$options$vLabelWrap)
            if (self$options$hideLabelsBelow)
                hideLabelsBelow <- 0.05
            else
                hideLabelsBelow <- 0.01
            # Doing the plot
            if( self$options$type == 'centered' ) {
                # Group setup
                if( ! is.null(groupingVar) ) {
                    if( self$options$groupBy == "variable" ) {
                        yOption <- groupingVar
                        facetRows <- vars(.question)
                    } else {
                        yOption <- ".question"
                        facetRows <- vars(!!ensym(groupingVar))
                    }
                } else {
                    yOption <- ".question"
                    facetRows <- NULL
                }
                # Do Likert Plot (centered)
                plot <- ggstats::gglikert(tibble::as_tibble(mainData), include = self$options$liks,
                                          sort = self$options$sorting,
                                          add_labels = self$options$addLabels,
                                          labels_size = self$options$labelSize / .pt , #0.8*textSize / .pt ,
                                          labels_accuracy = accuracy,
                                          labels_hide_below = hideLabelsBelow,
                                          labels_color = self$options$labelColor,
                                          facet_label_wrap = vLabelWrap,
                                          y_label_wrap = hLabelWrap,
                                          add_totals = self$options$addTotals,
                                          y = yOption, facet_rows = facetRows,
                                          variable_labels = variable_labels)
            } else {
                # Group setup
                if( ! is.null(groupingVar) ) {
                    if( self$options$groupBy == "variable" ) {
                        yOption <- groupingVar
                        facetRows <- vars(.question)
                    } else {
                        yOption <- ".question"
                        facetRows <- vars(!!ensym(groupingVar))
                    }
                } else {
                    yOption <- ".question"
                    facetRows <- NULL
                }
                # Do Likert Plot (stacked)
                plot <- ggstats::gglikert_stacked(tibble::as_tibble(mainData), include = self$options$liks,
                                                  sort = self$options$sorting,
                                                  add_labels = self$options$addLabels,
                                                  labels_size = self$options$labelSize / .pt , #0.8*textSize / .pt ,
                                                  labels_accuracy = accuracy,
                                                  labels_hide_below = hideLabelsBelow,
                                                  labels_color = self$options$labelColor,
                                                  y_label_wrap = hLabelWrap,
                                                  add_median_line = self$options$addMedianLine,
                                                  y = yOption,
                                                  variable_labels = variable_labels)
                plot <- plot + facet_grid(rows = facetRows, labeller = label_wrap_gen(vLabelWrap))
            }

            # removed in 1.0 (0.11.6)
            #plot <- plot + theme(text = element_text(size=textSize))

            if (self$options$reverseLikert)
                plot <- plot + scale_fill_brewer(palette = self$options$plotColor, direction = -1)
            else
                plot <- plot + scale_fill_brewer(palette = self$options$plotColor)

            #plot <- plot + theme(strip.text = element_text(size = 18, colour = "red", angle = 0), strip.position = "top")

            # Title & subtitle
            plot <- plot + vijTitlesAndLabels(self$options) + vijTitleAndLabelFormat(self$options, showLegend = TRUE)

            # Adjust strip (= Facet = Group) text (vijTitleAndLabelFormat uses subtittle format)
            plot <- plot + theme(strip.text = element_text(size = self$options$groupSize, face = "plain", hjust = 0.5))

            return(plot)
        },
        .getVarName = function(aVar) {
            if (self$options$descAsVarName) {
                aVarName <- attr(self$data[[aVar]], "jmv-desc", TRUE)
                if (!is.null(aVarName))
                    return(aVarName)
                else
                    return(aVar)
            } else {
                return(aVar)
            }
        },
        .computeFrequencies = function(ggLikertData, groupingVar, ng) {
            ggLikertData <- ggLikertData |>
                dplyr::filter(!is.na(.answer))
            if (ng == 0) { # --- Cas SANS variable de groupe ---
                freq_df <- ggLikertData |>
                    dplyr::count(.question, .answer, .drop = FALSE) |>
                    dplyr::group_by(.question) |>
                    dplyr::mutate(Sum = sum(n))

                if (self$options$frequencies == "percentages") {
                    freq_df <- freq_df |> dplyr::mutate(n = ifelse(Sum == 0, 0, n / Sum))
                }

                freq_wide <- freq_df |>
                    tidyr::pivot_wider(names_from = .answer, values_from = n, values_fill = 0)

            } else { # --- Cas AVEC variable de groupe ---
                group_sym <- rlang::sym(groupingVar)
                freq_df <- ggLikertData |>
                    dplyr::count(.question, !!group_sym, .answer, .drop = FALSE) |>
                    dplyr::group_by(.question, !!group_sym) |>
                    dplyr::mutate(Sum = sum(n))

                if (self$options$frequencies == "percentages") {
                    freq_df <- freq_df |> dplyr::mutate(n = ifelse(Sum == 0, 0, n / Sum))
                }

                freq_wide <- freq_df |>
                    tidyr::pivot_wider(names_from = .answer, values_from = n, values_fill = 0)
            }

            return(freq_wide)
        },
        .mannU = function(var, group, data, level1=1, level2=2) { # Two groups
            variable <- jmvcore::toNumeric(data[[var]])
            gLevels <- levels(data[[group]])
            group1 <- na.omit(variable[data[[group]] == gLevels[level1]])
            group2 <- na.omit(variable[data[[group]] == gLevels[level2]])
            n1 <- length(group1)
            n2 <- length(group2)
            if (n1 > 0 && n2 > 0) {
                res <- wilcox.test(group1, group2)
                statistic <- min(res$statistic, n1 * n2 - res$statistic)
                return(list('statistic' = statistic, 'p.value' = res$p.value))
            } else {
                return(list('statistic' = NA, 'p.value' = NA))
            }
        },
        .kruskalW = function(var, group, data) {
            variable <- jmvcore::toNumeric(data[[var]])
            group <- data[[group]]
            tryCatch(
                kruskal.test(variable, group),
                error = function(e) list(p.value = NA)
            )
        },
        # Modified from https://github.com/cran/PMCMRplus/blob/master/R/dscfAllPairsTest.R
        .dscfAllPairsTest = function(x, g){
            OK <- complete.cases(x, g)
            x <- x[OK]
            g <- g[OK]
            # vijPlots: drop empty levels
            g <- droplevels(g)
            k <- nlevels(g)
            # vijPlots: return empty result if less than 2 groups
            if (k < 2) {
                return( list(p.values = list(), statistics = list()) )
            }
            n <- tapply(x, g, length)
            glev <- levels(g)
            ## Function to get ties for tie adjustment
            getties <- function(x){
                t <- table(x)
                C <- sum((t^3 - t) / 12)
                C
            }
            ## function for pairwise comparisons
            compare.stats <-function(i, j){
                m <- n[j]
                nn <- n[i]
                xraw <- c(x[g==glev[i]], x[g==glev[j]])
                rankx <- rank(xraw)
                lev <- c(g[g==glev[i]], g[g==glev[j]])

                ## make sure to drop unneeded levels
                id <- !glev %in% c(glev[i], glev[j])
                exclude <- glev[id]
                lev <- droplevels(lev, exclude = exclude)

                R <- tapply(rankx, lev, sum)
                # vijPlots
                U <- c(m*nn + (m * (m + 1) / 2), m * nn + (nn * (nn + 1) / 2)) - R
                #Umn <- min(U)
                Umn <- U[1]
                S <- m + nn
                VAR <- (m * nn / (S * (S - 1))) * ((S^3 - S) / 12 - getties(rankx))
                PSTAT <- sqrt(2) * (Umn - m * nn / 2) / sqrt(VAR)
                PSTAT
            }
            PSTAT <- pairwise.table(compare.stats,levels(g), p.adjust.method="none")
            PVAL <- ptukey(abs(PSTAT), nmeans = k, df = Inf, lower.tail = FALSE)
            # vijPlots : change the format of result returned
            # pairwise.table() = (k-1)×(k-1) triangular matrix
            p.values = list()
            statistics = list()
            for(i in 1:(k-1)) {
                for(j in i:(k-1)) {
                    gname <- paste(colnames(PSTAT)[i],"-",rownames(PSTAT)[j])
                    p.values[gname] <- PVAL[j,i]
                    statistics[gname] <- - PSTAT[j,i] # i - j
                }
            }
            return(list(p.values = p.values, statistics = statistics))
        },
        # Modified from https://github.com/cran/PMCMRplus/blob/master/R/kwAllPairsConoverTest.R
        .conoverTest = function(x, g,  p.adjust.method = "none"){
            ## Kruskal-Wallis functions
            gettiesKruskal <- function(x) {
                n <- length(x)
                t <- table(x)
                C <- 1 - sum(t^3 - t) / (n^3 - n)
                C <- min(1, C)
                return(C)
            }
            HStat <- function(r, g) {
                ni <- tapply(!is.na(r), g, length)
                N <- sum(ni)

                H <- (12 / (N * (N + 1))) *
                    sum(tapply(r, g, "sum") ^ 2 / ni) - 3 * (N + 1)
                H
            }
            # Main computation
            OK <- complete.cases(x, g)
            x <- x[OK]
            g <- g[OK]
            # vijPlots: drop empty levels
            g <- droplevels(g)
            k <- nlevels(g)
            # vijPlots: return empty result if less than 2 groups
            if (k < 2) {
                return( list(p.values = list(), statistics = list()) )
            }
            x.rank <- rank(x)
            R.bar <- tapply(x.rank, g, mean,na.rm=T)
            R.n <- tapply(!is.na(x), g, length)
            # g.unique <- unique(g)
            # k <- length(g.unique)
            n <- sum(R.n)

            ## Kruskal-Wallis statistic
            H <- HStat(x.rank, g)
            C <- gettiesKruskal(x.rank)
            H.cor <- H / C

            if (C == 1) {
                S2 <- n * (n + 1) / 12
            } else {
                # warning("Ties are present. Quantiles were corrected for ties.")
                S2 <-   ( 1 / (n - 1)) * (sum(x.rank^2) - (n * (((n + 1)^2) / 4)))
            }

            compare.stats <- function(i,j) {
                dif <- R.bar[i] - R.bar[j]
                B <- (1 / R.n[i] + 1 / R.n[j])
                D <- (n - 1 - H.cor) / (n - k)
                tval <- dif / sqrt(S2 * B * D)
                return(tval)
            }
            PSTAT <- pairwise.table(compare.stats, levels(g), p.adjust.method = "none" )

            compare.levels <- function(i,j) {
                dif <- abs(R.bar[i] - R.bar[j])
                B <- (1 / R.n[i] + 1 / R.n[j])
                D <- (n - 1 - H.cor) / (n - k)
                tval <- dif / sqrt(S2 * B * D)
                pval <- 2 * pt(abs(tval), df=n - k, lower.tail=FALSE)
                return(pval)
            }

            PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method = p.adjust.method )

            # vijPlots : change the format of result returned
            p.values = list()
            statistics = list()
            for(i in 1:(k-1)) {
                for(j in i:(k-1)) {
                    gname <- paste(colnames(PSTAT)[i],"-",rownames(PSTAT)[j])
                    p.values[gname] <- PVAL[j,i]
                    statistics[gname] <- - PSTAT[j,i] # i - j
                }
            }

            return( list(p.values = p.values, statistics = statistics) )
        },
        # Modified from https://github.com/cran/PMCMRplus/blob/master/R/kwAllPairsDunnTest.R
        .dunnTest = function(x, g,  p.adjust.method = "none"){
            gettiesDunn <- function(x){
                n <- length(x)
                t <- table(x)
                C <- sum(t^3 - t) / (12 * (n - 1))
                return(C)
            }
            OK <- complete.cases(x, g)
            x <- x[OK]
            g <- g[OK]
            # vijPlots: drop empty levels
            g <- droplevels(g)
            k <- nlevels(g)
            # vijPlots: return empty result if less than 2 groups
            if (k < 2) {
                return( list(p.values = list(), statistics = list()) )
            }
            x.rank <- rank(x)
            R.bar <- tapply(x.rank, g, mean,na.rm=T)
            R.n <- tapply(!is.na(x), g, length)
            # g.unique <- unique(g)
            # k <- length(g.unique)
            n <- sum(R.n)

            ## get the ties
            C <- gettiesDunn(x.rank)
            # if (C != 0) warning("Ties are present. z-quantiles were corrected for ties.")
            compare.stats <- function(i,j) {
                dif <- R.bar[i] - R.bar[j] # vijplots: remove abs
                A <- n * (n+1) / 12
                B <- (1 / R.n[i] + 1 / R.n[j])
                zval <- dif / sqrt((A - C) * B)
                return(zval)
            }
            PSTAT <- pairwise.table(compare.stats,levels(g), p.adjust.method="none" )
            compare.levels <- function(i,j) {
                dif <- abs(R.bar[i] - R.bar[j])
                A <- n * (n+1) / 12
                B <- (1 / R.n[i] + 1 / R.n[j])
                zval <- dif / sqrt((A - C) * B)
                pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
                return(pval)
            }
            PVAL <- pairwise.table(compare.levels,levels(g), p.adjust.method = p.adjust.method)

            # vijPlots: change the format of result returned
            p.values = list()
            statistics = list()
            for(i in 1:(k-1)) {
                for(j in i:(k-1)) {
                    gname <- paste(colnames(PSTAT)[i],"-",rownames(PSTAT)[j])
                    p.values[gname] <- PVAL[j,i]
                    statistics[gname] <- - PSTAT[j,i] # i - j
                }
            }

            return( list(p.values = p.values, statistics = statistics) )
        },
        .showHelpMessage = function() {
            helpMsg <- .('<h3>Data & Sorting</h3>
<ul>
<li>Likert variables must be of ordinal measure-type. (Continuous measure-type works as well.)</li>
<li>If you plan to compute mean/median/sd or to use comparison tests, they must be of integer data-type.</li>
<li><strong>Tidy up levels:</strong> when checked, try to fix the labels order in table and graph (when some variables miss some levels).</li>
<li><strong>Convert variables to integer:</strong> when checked, the level labels are ignored and only integer values are used.</li>
<li><strong>Sort Variables by Median:</strong> orders the list of Likert variables by median.</li>
</ul>
<h3>Comparison Tests</h3>
<p>When using a group variable, several tests are available:</p>
<ul>
<li><strong>Mann-Whitney U:</strong> Two group comparison</li>
<li><strong>Kruskal-Wallis:</strong> n group comparison</li>
<li><strong>Post Hoc Tests:</strong> Dunn, Conover and Dwass-Steel-Critchlow-Fligner (DSCF) pairwise comparisons tests.</li>
</ul>
<p>The p-values can be adjusted <strong>groupwise</strong> (for each question) for post hoc tests or <strong>overall</strong> (groupwise and questionwise) for Mann-Whitney U, Kruskal-Wallis and post hoc tests. DSCF p-values are already adjusted (groupwise); no other adjustment is possible.</p>
<p>A sample file is included at Open > Data Library > vijPlots > Likert</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
