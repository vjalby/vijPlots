correspClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "correspClass",
    inherit = correspBase,
    #### Active bindings ---- from jmv/conttables.b.R
    active = list(
        countsName = function() {
            if ( ! is.null(self$options$counts)) {
                return(self$options$counts)
            } else if ( ! is.null(attr(self$data, "jmv-weights-name"))) {
                return (attr(self$data, "jmv-weights-name"))
            }
            NULL
        }
    ),
    private = list(
        .getVarName = function(aVar) {
            if (self$options$descAsVarName && !is.null(aVar)) {
                aVarName <- attr(self$data[[aVar]], "jmv-desc", TRUE)
                if (!is.null(aVarName))
                    return(aVarName)
                else
                    return(aVar)
            } else {
                return(aVar)
            }
        },
        .getData = function() {
            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            if (is.null(rowVarName) || is.null(colVarName))
                return(NULL)

            data <- jmvcore::select(self$data, c(rowVarName, colVarName))

            # Weight data
            countsName <- self$options$counts
            if (!is.null(countsName)) {
                # vijPlots/Mosaic weights
                data[['.COUNTS']] <- jmvcore::toNumeric(self$data[[countsName]])
            } else if (!is.null(attr(self$data, "jmv-weights"))) {
                # jamovi built-in weights
                data[['.COUNTS']] <- jmvcore::toNumeric(attr(self$data, "jmv-weights"))
            } else {
                # no weights
                data[['.COUNTS']] <- as.integer(rep(1, nrow(data)))
            }

            data <- jmvcore::naOmit(data)
            return(data)
        },
        .getProfile = function(contingencyTable, supplementaryRows, supplementaryCols) {
            # This function is tricky because of supplementaryPoints. Maybe it's possible to simplify it.
            # The idea is to build the contingencyTable of active columns and rows without deleting the supplementary ones
            # but setting them to 0 then compute the % for the supplementary rows based on active margins
            # then adds then back to the main table...
            rowProfiles <- contingencyTable                     # copy contingencyTable
            rowProfiles[supplementaryRows,] <- 0                # set supplementary rows to 0
            rowProfiles <- stats::addmargins(rowProfiles, margin=1)    # Add margin row (sum)
            rowProfiles[supplementaryRows,] <-contingencyTable[supplementaryRows,]  # set supplementary rows back
            rowProfiles[,supplementaryCols] <- 0                # Empty supplementary columns
            # Compute margin
            tmpRowProfiles <- stats::addmargins(rowProfiles, margin=2)
            rowMargins <- tmpRowProfiles[-nrow(tmpRowProfiles),ncol(tmpRowProfiles)]
            rowMargins[supplementaryRows]<-0
            rowMargins <- stats::addmargins(as.matrix(rowMargins), margin = 1)
            #
            rowProfiles <- proportions(rowProfiles, margin = 1)             # Compute % per lines
            rowProfiles <- stats::addmargins(rowProfiles,margin=2)                 # Add margin column (sum)
            supplCols <- as.matrix(contingencyTable[,supplementaryCols, drop = FALSE])    # Table of supplementary Cols
            supplCols[supplementaryRows,] <- 0                              # set supplementary rows to 0
            supplCols <- stats::addmargins(supplCols, margin = 1)                  # add margin
            supplCols <-supplCols / rowMargins[,1]                          # compute % per lines
            supplCols[supplementaryRows,] <- 0                              # Remplace NaN by 0
            rowProfiles[,supplementaryCols] <- supplCols                    # Replace supplementary cols in row profiles table
            #
            rownames(rowProfiles)[nrow(rowProfiles)] <- .("Mass")
            colnames(rowProfiles)[ncol(rowProfiles)] <- .("Active Margin")
            return(rowProfiles)
        },
        .getContingencyTable = function(contingencyTable, supplementaryRows, supplementaryCols) {
            savedSupplementaryRows <- contingencyTable[supplementaryRows,, drop = FALSE]
            savedSupplementaryCols <- contingencyTable[,supplementaryCols, drop = FALSE]
            contingencyTable[supplementaryRows,] <- 0                # set supplementary rows to 0
            contingencyTable[,supplementaryCols] <- 0                # Empty supplementary columns
            contingencyTable <- stats::addmargins(contingencyTable, margin=c(1,2))    # Add margin row (sum)
            # Set the supplementary rows and columns back
            contingencyTable[supplementaryRows,1:(ncol(contingencyTable)-1)] <- savedSupplementaryRows
            contingencyTable[1:(nrow(contingencyTable)-1),supplementaryCols] <- savedSupplementaryCols
            # Delete values and margins for supplementary rows/columns
            for (i in supplementaryRows) {
                for (j in supplementaryCols) {
                    contingencyTable[i,j] <- NA
                }
            }
            contingencyTable[supplementaryRows,ncol(contingencyTable)] <- NA
            contingencyTable[nrow(contingencyTable), supplementaryCols] <- NA
            return(contingencyTable)
        },
        .fillProfileTable = function(profileTable, profiles, suppl, rowName, colName) {
            profileTable$addColumn("row", type = "text", title = rowName)
            for (j in seq(ncol(profiles))) {
                profileTable$addColumn(colnames(profiles)[j], type = "number", format = "zto", superTitle = colName)
            }
            for (i in seq(nrow(profiles))) {
                profileTable$addRow(i, values = profiles[i,])
                profileTable$setCell(rowNo = i, "row", rownames(profiles)[i])
            }
            profileTable$addFormat(rowNo = nrow(profiles), 1, jmvcore::Cell.BEGIN_END_GROUP)
            if (suppl)
                profileTable$setNote("supp", paste("* :", .("Supplementary rows/columns")))
        },
        .fillSummaryTable = function(table, items, labelCol, labelTitle, coord, coordSup, marge,
                                      supplementary, suppText, nDim, normalizationString) {
            # table = table to fill
            # item = row/col names
            # labelCol = "row" or "col"
            # labelTitle = col/rowVarNameString
            dimN <- function(n) jmvcore::format(.("Dim {n}"), n = n)
            table$addColumn(name = "id", title = "#", type = "integer")
            table$addColumn(name = labelCol, title = labelTitle, type = "text")
            table$addColumn(name = "margin", title = .("Mass"), type = "number", format = "zto")
            for (i in seq(nDim))
                table$addColumn(name = paste0("score",i), title = dimN(i), superTitle = paste(.("Coordinates"),"†"), type = "number", format = "zto")
            table$addColumn(name = "inertia", title = .("% Inertia"), type = "number", format = "zto")
            for (i in seq(nDim))
                table$addColumn(name = paste0("contrib",i), title = dimN(i), superTitle = .("Contributions"), type = "number", format = "zto")
            table$addColumn(name = "qlt", title = "QLT", type = "number", format = "zto")
            for (i in seq(nDim))
                table$addColumn(name = paste0("cos",i), title = dimN(i), superTitle = "CO2", type = "number", format = "zto")
            # Populate Summary table
            for (i in seq_along(items)) {
                anItem <- items[i]
                if (anItem %in% rownames(coord$coord)) { # Active row/col
                    theValues <- list(id = i, margin = marge[anItem], inertia = coord$inertia[anItem],
                                       qlt = sum(coord$cos2[anItem,1:nDim]))
                    theValues[[labelCol]] <- anItem
                    for (j in seq(nDim)) {
                        theValues[[paste0("score",j)]] <- coord$coord[anItem,j]
                        theValues[[paste0("contrib",j)]] <- coord$contrib[anItem,j]
                        theValues[[paste0("cos",j)]] <- coord$cos2[anItem,j]
                    }
                } else { # Supplementary row/col
                    theValues <- list(id = i, margin = "", inertia = "",
                                       qlt = sum(coordSup$cos2[anItem,1:nDim], na.rm = TRUE))
                    theValues[[labelCol]] <- anItem
                    for (j in seq(nDim)) {
                        theValues[[paste0("score",j)]] <- coordSup$coord[anItem,j]
                        theValues[[paste0("contrib",j)]] <- ""
                        theValues[[paste0("cos",j)]] <- coordSup$cos2[anItem,j]
                    }
                }
                table$addRow(i, values = theValues)
            }
            if (!is.null(supplementary))
                table$setNote("supp", paste("* :", suppText))
            table$setNote("norm", paste("† :", normalizationString))
        },
        .parseSupplementary = function(optionValue, nmax, parseErrorMsg, rangeErrorMsg) {
            if (is.null(optionValue) || optionValue == "0" || optionValue == "")
                return(NULL)
            supp <- as.integer(unlist(strsplit(optionValue, ",")))
            if (any(is.na(supp))) {
                vijErrorMessage(self, parseErrorMsg)
            } else {
                supp <- sort(unique(supp))
                if (!all(supp %in% 1:nmax))
                    vijErrorMessage(self, jmvcore::format(rangeErrorMsg, nmax = nmax))
            }
            supp
        },
        .init = function() {
            #
            if ((self$options$mode == "obsTable" && (is.null(self$options$rows) || is.null(self$options$cols))) ||
                (self$options$mode == "contTable" && (is.null(self$options$rowLabels) || length(self$options$columns) < 3)) ) {
                private$.showHelpMessage()
            } else if (self$options$mode == "obsTable") {
                # Weight message
                countsName <- self$countsName
                if (!is.null(countsName)) {
                    warningMessage <- ..('The data is weighted by the variable {}.', countsName)
                    vijWarningMessage(self, warningMessage, '.weights')
                }
            }
        },
        .run = function() {
            if (self$options$mode == "obsTable") {
                data <- private$.getData()
                if (is.null(data) || nrow(data) == 0) {
                    return(FALSE)
                }

                if (any(data$.COUNTS < 0)) {
                    vijErrorMessage(self, .('Counts may not be negative.'))
                }
                if (any(is.infinite(data$.COUNTS))) {
                    vijErrorMessage(self, .('Counts may not be infinite.'))
                }

                rowVarName <- self$options$rows
                colVarName <- self$options$cols

                # Set variable names
                rowVarNameString <- private$.getVarName(rowVarName)
                colVarNameString <- private$.getVarName(colVarName)

                #Contingency Table (base)
                formula <- jmvcore::composeFormula('.COUNTS', c(rowVarName, colVarName))
                contingencyTable <- stats::xtabs(formula, data)
            } else { # self$options$mode == "contTable"
                if (is.null(self$options$rowLabels) || length(self$options$columns) < 3 || nrow(self$data) < 3)
                    return(FALSE)

                contingencyTable <- jmvcore::select(self$data,self$options$columns)
                for (colName in self$options$columns)
                    contingencyTable[[colName]] <- jmvcore::toNumeric(contingencyTable[[colName]])

                if (anyNA(contingencyTable)) {
                    vijErrorMessage(self, .("Some values are missing from the contingency table."))
                }
                if (any(contingencyTable < 0)) {
                    vijErrorMessage(self, .('Counts may not be negative.'))
                }
                row.names(contingencyTable) <- self$data[[self$options$rowLabels]]
                contingencyTable <- as.matrix(contingencyTable)
                # Set variable names
                rowVarName <- self$options$rowLabels
                rowVarNameString <- rowVarName
                colVarName <- self$options$columnTitle
                colVarNameString <- colVarName
            }

            #### Supplementary Rows & Column ####

            supplementaryRows <- private$.parseSupplementary(
                self$options$supplementaryRows, nrow(contingencyTable),
                .("Supplementary row numbers must be a list of numbers, e.g. 1,2,9"),
                .("Supplementary row numbers must be between 1 and {nmax}.")
            )
            supplementaryCols <- private$.parseSupplementary(
                self$options$supplementaryCols, ncol(contingencyTable),
                .("Supplementary column numbers must be a list of numbers, e.g. 1,2,9"),
                .("Supplementary column numbers must be between 1 and {nmax}.")
            )
            # Modify the supplementary row/col names
            for (i in supplementaryRows)
                rownames(contingencyTable)[i] <- paste(rownames(contingencyTable)[i], "*")
            for (j in supplementaryCols)
                colnames(contingencyTable)[j] <- paste(colnames(contingencyTable)[j], "*")

            #### Normalisation ####

            normalizationString <- switch(self$options$normalization,
                                          principal = .("Principal normalization"),
                                          symmetric = .("Symmetric normalization"),
                                          rowprincipal = .("Row principal normalization"),
                                          colprincipal = .("Column principal normalization"),
                                          standard = .("Standard normalization")
            )

            #### Contingency Table (with supplementary rows/columns ####

            fullTable <- private$.getContingencyTable(contingencyTable, supplementaryRows, supplementaryCols)
            rownames(fullTable)[nrow(fullTable)] <- .("Active Margin")
            colnames(fullTable)[length(colnames(fullTable))] <- .("Active Margin")
            self$results$contingency$addColumn("row", type="text", title = rowVarNameString)
            for (col in colnames(fullTable)) {
                if (col != .("Active Margin"))
                    self$results$contingency$addColumn(col, type="integer", superTitle = colVarNameString)
                else
                    self$results$contingency$addColumn(col, type="integer")
            }
            for (i in seq(nrow(fullTable))) {
                self$results$contingency$addRow(i, values = fullTable[i,])
                self$results$contingency$setCell(rowNo = i, "row", rownames(fullTable)[i])
            }
            self$results$contingency$addFormat(rowNo = nrow(fullTable), 1, jmvcore::Cell.BEGIN_END_GROUP)
            # Change NaN/NA to NULL. Is there another way to have empty cells ?
            for (i in seq(nrow(fullTable))) {
                for (j in seq(ncol(fullTable))) {
                    if (is.na(fullTable[i,j]))
                        self$results$contingency$setCell(rowNo = i, colnames(fullTable)[j], NULL)
                }
            }
            if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                self$results$contingency$setNote("supp",paste("* :", .("Supplementary rows/columns")))

            #### Row and Column Profile Tables ####

            if(self$options$showProfiles) {
                hasSupp <- !is.null(supplementaryRows) || !is.null(supplementaryCols)
                # Row Profiles
                rowProfiles <- private$.getProfile(contingencyTable, supplementaryRows, supplementaryCols)
                private$.fillProfileTable(self$results$rowProfiles, rowProfiles, hasSupp, rowVarNameString, colVarNameString)
                # Column Profiles
                colProfiles <- t(private$.getProfile(t(contingencyTable),supplementaryCols, supplementaryRows))
                private$.fillProfileTable(self$results$colProfiles, colProfiles, hasSupp, rowVarNameString, colVarNameString)
            }

            #### Chi-Squared test ####

            activeContingencyTable <- contingencyTable
            if (!is.null(supplementaryRows))
                activeContingencyTable <- activeContingencyTable[-supplementaryRows,, drop = FALSE]
            if (!is.null(supplementaryCols))
                activeContingencyTable <- activeContingencyTable[,-supplementaryCols, drop = FALSE]

            if (any(rowSums(activeContingencyTable) == 0) ||
                any(colSums(activeContingencyTable) == 0)) {
                vijErrorMessage(self, .("Some categories have zero counts and must be removed."))
            }

            chisqres <- tryCatch(
                            suppressWarnings(stats::chisq.test(activeContingencyTable)),
                            error = function (e) NULL
                        )

            if (is.null(chisqres) || !is.finite(chisqres$statistic) ) {
                vijErrorMessage(self, .("Unable to compute the χ2 statistic."))
            }
            if (chisqres$statistic <= .Machine$double.eps) {
                vijErrorMessage(self, .("The χ2 statistic is equal to zero."))
            }

            # Check solution dimension
            maxDim = min(nrow(contingencyTable)-length(supplementaryRows), ncol(contingencyTable)-length(supplementaryCols)) - 1
            if (maxDim < 2) {
                vijErrorMessage(self, .("Not enough data to compute CA."))
            }
            nDim <-self$options$dimNum
            if (nDim > maxDim) {
                errorMessage <- jmvcore::format(.("Number of dimensions must be less than or equal to {maxDim}."), maxDim = maxDim)
                vijErrorMessage(self,errorMessage)
            }

            #### Compute CA ####
            if (is.null(supplementaryRows))
                suprow <- NULL
            else
                suprow <- supplementaryRows

            if (is.null(supplementaryCols))
                supcol <- NULL
            else
                supcol <- supplementaryCols

            res <- tryCatch(
                    private$.ca(contingencyTable, row.sup = suprow, col.sup = supcol, ncp = nDim, norm = self$options$normalization),
                    error = function (e) NULL
                )

            if (is.null(res) ) {
                vijErrorMessage(self, .("Unable to compute correspondence analysis for the selected data."))
            }

            #### Inertia Table ####
            # rows is set to 1 in yaml to force jamovi to refresh the table note after a reject().
            # we need to delete this (empty) row before to populate the table.
            self$results$eigenvalues$deleteRows()
            # Populate the inertia table
            for (i in seq_along(res$sv)) {
                self$results$eigenvalues$addRow(i, values = list(
                    dim = i,
                    singular = res$sv[i],
                    inertia = res$eig[i,1],
                    proportion = res$eig [i,2],
                    cumulative = res$eig [i,3]
                ))
            }
            # Add total row
            self$results$eigenvalues$addRow(rowKey="Total", values = list(
                dim = "Total",
                singular = "",
                inertia = sum(res$eig[,1]),
                proportion = 1,
                cumulative = 1
            ))
            self$results$eigenvalues$addFormat(rowKey="Total", 1, jmvcore::Cell.BEGIN_END_GROUP)
            # Chi-squared test
            chisqNote <- jmvcore::format(.("χ² = {chisq}, df = {df}, p-value = {pval}"),
                                         chisq = round(chisqres$statistic,2),
                                         df = chisqres$parameter,
                                         pval = format.pval(chisqres$p.value, eps = 0.001)
            )
            self$results$eigenvalues$setNote(key = "chisq", note = chisqNote, init = FALSE)

            #### Summary Tables ####

            if(self$options$showSummaries) {
                private$.fillSummaryTable(self$results$rowSummary, rownames(contingencyTable), "row", rowVarNameString,
                                           res$row, res$row.sup, res$call$marge.row, supplementaryRows,
                                           .("Supplementary rows"), nDim, normalizationString)
                private$.fillSummaryTable(self$results$colSummary, colnames(contingencyTable), "col", colVarNameString,
                                           res$col, res$col.sup, res$call$marge.col, supplementaryCols,
                                           .("Supplementary columns"), nDim, normalizationString)
            }

            # Check axis values
            xaxis <- self$options$xaxis
            yaxis <- self$options$yaxis
            if (xaxis > nDim || yaxis > nDim) {
                errorMessage <- jmvcore::format(.("Axis numbers must be less than or equal to the number of dimensions ({nDim})."), nDim = nDim)
                vijErrorMessage(self, errorMessage)
            }
            if (xaxis == yaxis) {
                vijErrorMessage(self, .("Axis numbers cannot be equal."))
            }
            if (res$sv[max(xaxis, yaxis)] < .Machine$double.eps) {
                message <- jmvcore::format(.("The singular value for dimension {n} is close to zero. The plots may not be accurate."), n = max(xaxis, yaxis))
                pos <- ifelse(is.null(self$countsName), 1, 2)
                vijWarningMessage(self, message, pos = pos)
            }

            #### Plots ####

            res$rowVarNameString <- rowVarNameString
            res$colVarNameString <- colVarNameString

            rowplot <- self$results$rowplot
            rowplot$setState(res)
            colplot <- self$results$colplot
            colplot$setState(res)
            biplot <- self$results$biplot
            biplot$setState(res)
        },
        .ca = function(contingencyTable, ncp = 2, row.sup = NULL, col.sup = NULL, norm = "principal") {
            res <- FactoMineR::CA(contingencyTable, ncp = ncp, row.sup = row.sup, col.sup = col.sup, graph = FALSE)
            res$sv <- sqrt(res$eig[,1]) # singular values
            res$eig[,2:3] <- res$eig[,2:3] / 100
            res$col$contrib <- res$col$contrib / 100
            res$row$contrib <- res$row$contrib / 100
            res$row$inertia <- res$row$inertia / sum(res$eig[,1])
            res$col$inertia <- res$col$inertia / sum(res$eig[,1])
            names(res$row$inertia) <- rownames(res$row$coord)
            names(res$col$inertia) <- rownames(res$col$coord)
            if (norm == "symmetric") {
                res$col$coord <- sweep(res$col$coord, 2, sqrt(res$sv[1:ncp]), FUN = "/")
                res$row$coord <- sweep(res$row$coord, 2, sqrt(res$sv[1:ncp]), FUN = "/")
                if (!is.null(col.sup))
                    res$col.sup$coord <- sweep(res$col.sup$coord, 2, sqrt(res$sv[1:ncp]), FUN = "/")
                if (!is.null(row.sup))
                    res$row.sup$coord <- sweep(res$row.sup$coord, 2, sqrt(res$sv[1:ncp]), FUN = "/")
            } else if (norm == "rowprincipal") {
                res$col$coord <- sweep(res$col$coord, 2, res$sv[1:ncp], FUN = "/")
                if (!is.null(col.sup))
                    res$col.sup$coord <- sweep(res$col.sup$coord, 2, res$sv[1:ncp], FUN = "/")
            } else if (norm == "colprincipal") {
                res$row$coord <- sweep(res$row$coord, 2, res$sv[1:ncp], FUN = "/")
                if (!is.null(row.sup))
                    res$row.sup$coord <- sweep(res$row.sup$coord, 2, res$sv[1:ncp], FUN = "/")
            } else if (norm == "standard") {
                res$col$coord <- sweep(res$col$coord, 2, res$sv[1:ncp], FUN = "/")
                if (!is.null(col.sup))
                    res$col.sup$coord <- sweep(res$col.sup$coord, 2, res$sv[1:ncp], FUN = "/")
                res$row$coord <- sweep(res$row$coord, 2, res$sv[1:ncp], FUN = "/")
                if (!is.null(row.sup))
                    res$row.sup$coord <- sweep(res$row.sup$coord, 2, res$sv[1:ncp], FUN = "/")
            }
            return(res)
        },
        .caplot = function(plotType, image, ggtheme, theme) {
            if (is.null(image$state))
                return(FALSE)

            # Plot data
            res <- image$state
            # Supplementary Row & Column Colors
            # 1 = row, 2 = rowsup, 3 = column, 4 = colsup
            if (plotType != 'column') { # rowplat and biplot
                if (!is.null(res$row.sup$coord)) {
                    ptcoord <- as.data.frame(rbind(
                        cbind(res$row$coord, "sup" = 1),
                        cbind(res$row.sup$coord, "sup" = 2)
                    ))
                } else {
                    ptcoord <- as.data.frame(
                        cbind(res$row$coord, "sup" = 1)
                    )
                }
            } else {
                ptcoord <- NULL #NA
            }
            if (plotType != 'row') { # colplot and biplot
                if (!is.null(res$col.sup$coord)) {
                    ptcoord <- as.data.frame(rbind(
                        ptcoord,
                        cbind(res$col$coord, "sup" = 3),
                        cbind(res$col.sup$coord, "sup" = 4)
                    ))
                } else {
                    ptcoord <- as.data.frame(rbind(
                        ptcoord,
                        cbind(res$col$coord, "sup" = 3)
                    ))
                }
            }
            ptcoord$sup <- factor(ptcoord$sup, levels = c(1,2,3,4))
            # ptcoord dataframe containt the row and column coordinates
            # ptcoord$sup is the type of point (1 = row, 2 = rowsup, 3 = column, 4 = colsup)

            # Plot inertia
            percentInertia <- round(100*res$eig[,2], 1)
            # Plot axis
            xaxis <- self$options$xaxis
            xaxisdim <- paste("Dim", xaxis)
            dim1name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = xaxis, perc = percentInertia[xaxis])
            yaxis <- self$options$yaxis
            yaxisdim <- paste("Dim", yaxis)
            dim2name <- jmvcore::format(.("Dimension {n} ({perc} %)"), n = yaxis, perc = percentInertia[yaxis])

            # Building the plot
            plot <-  ggplot2::ggplot(ptcoord, ggplot2::aes(x = .data[[xaxisdim]], y = .data[[yaxisdim]], color = .data[["sup"]], shape = .data[["sup"]]))
            plot <- plot + ggplot2::geom_point()
            plot <- plot + ggrepel::geom_text_repel(ggplot2::aes(label = rownames(ptcoord)), show.legend = FALSE, size = self$options$labelSize/ggplot2::.pt, seed = 123)
            plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::geom_vline(xintercept = 0, linetype = 2)

            # Apply jmv theme
            plot <- plot + ggtheme

            # Set point colors
            plot <- plot +
                ggplot2::scale_color_manual(
                    values=c("1" = self$options$rowColor, "2" = self$options$supColor, "3" = self$options$colColor, "4" = self$options$supColor),
                    breaks=c("1", "3", "2", "4")) + ggplot2::labs(color = "") +
                ggplot2::scale_shape_manual(values = c(19, 19, 17, 17), breaks = c("1","2","3","4")) +
                ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
            plot <- plot + ggplot2::guides(color = "none", shape = "none")

            # Plot frame
            plot <- plot + ggplot2::theme(axis.line = ggplot2::element_line(linewidth = 0), panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1))

            # Plot title
            title <- switch(plotType,
                            row = jmvcore::format(.("Row Points for {rows}"), rows = res$rowVarNameString),
                            column = jmvcore::format(.("Column Points for {cols}"), cols = res$colVarNameString),
                            biplot = jmvcore::format(.("Row and Column Points for {rows} and {cols}"),
                                                     rows = res$rowVarNameString,
                                                     cols = res$colVarNameString)
            )
            # Plot subtitle
            subtitle <- switch(self$options$normalization,
                               principal = .("Principal normalization"),
                               symmetric = .("Symmetric normalization"),
                               rowprincipal = .("Row principal normalization"),
                               colprincipal = .("Column principal normalization"),
                               standard = .("Standard normalization")
            )

            # Titles & Labels
            defaults <- list(title = title, subtitle = subtitle, y = dim2name, x = dim1name)
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = plotType, plot = plot) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)

            vijDebugPlot(self, plot)

            return(plot)
        },
        .rowplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'row', image, ggtheme, theme))
        },
        .colplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'column', image, ggtheme, theme))
        },
        .biplot = function(image, ggtheme, theme, ...) {
            return(private$.caplot(plotType = 'biplot', image, ggtheme, theme))
        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module computes <strong>Correspondence Analysis (CA)</strong> for two categorical variables. Computations are based on <a href = "https://CRAN.R-project.org/package=FactoMineR" target="_blank">FactoMineR<a/> package by F. Husson, J. Josse, S. Le, J. Mazet.</p>
<p>The data can be</p>
<ul>
<li>an <strong>Observation table</strong> (raw data), possibly weighted using <em>jamovi</em> built-in weight system or using the "Counts" variable</li>
<li>or a <strong>Contingency table</strong></li>
</ul>
<p><strong>Supplementary row or column</strong> numbers may be entered as integer lists : 1,3,6</p>
<p>Four normalizations (scaling of row and column scores before plotting) are available :</p>
<ul>
<li><strong>Principal:</strong> Row and column scores are scaled by eigenvalues.</li>
<li><strong>Symmetric:</strong> Row and column scores are scaled by the square root of eigenvalues. </li>
<li><strong>Row Principal:</strong> Only row scores are scaled by eigenvalues.</li>
<li><strong>Column Principal:</strong> Only column scores are scaled by eigenvalues.</li>
<li><strong>Standard:</strong> The raw coordinates without normalization.</li>
</ul>
<p>A sample file is included at Open > Data Library > vijPlots > Smoking</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
