
# This file is a generated template, your changes will not be overwritten

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
        .getData = function() {
            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            if (is.null(rowVarName) || is.null(colVarName))
                return(NULL)
            data <- private$.cleanData()
            data <- jmvcore::naOmit(data[,c(rowVarName,colVarName,'.COUNTS')])
            return(data)
        },
        .getProfile = function(contingencyTable, supplementaryRows, supplementaryCols) {
            # This function is tricky because of supplementaryPoints. Maybe it's possible to simplify it.
            # The idea is to build the contingencyTable of active columns and rows without deleting the supplementary ones
            # but setting them to 0 then compute the % for the supplementary rows based on active margins
            # then adds then back to the main table...
            rowProfiles <- contingencyTable                     # copy contingencyTable
            rowProfiles[supplementaryRows,] <- 0                # set supplementary rows to 0
            rowProfiles <- addmargins(rowProfiles, margin=1)    # Add margin row (sum)
            rowProfiles[supplementaryRows,] <-contingencyTable[supplementaryRows,]  # set supplementary rows back
            rowProfiles[,supplementaryCols] <- 0                # Empty supplementary columns
            # Compute margin
            tmpRowProfiles <- addmargins(rowProfiles, margin=2)
            rowMargins <- tmpRowProfiles[-nrow(tmpRowProfiles),ncol(tmpRowProfiles)]
            rowMargins[supplementaryRows]<-0
            rowMargins <- addmargins(as.matrix(rowMargins), margin = 1)
            #
            rowProfiles <- proportions(rowProfiles, margin = 1)             # Compute % per lines
            rowProfiles <- addmargins(rowProfiles,margin=2)                 # Add margin column (sum)
            supplCols <- as.matrix(contingencyTable[,supplementaryCols])    # Table of supplementary Cols
            supplCols[supplementaryRows,] <- 0                              # set supplementary rows to 0
            supplCols <- addmargins(supplCols, margin = 1)                  # add margin
            supplCols <-supplCols / rowMargins[,1]                          # compute % per lines
            supplCols[supplementaryRows,] <- 0                              # Remplace NaN by 0
            rowProfiles[,supplementaryCols] <- supplCols                    # Replace supplementary cols in row profiles table
            #
            rownames(rowProfiles)[nrow(rowProfiles)] <- .("Mass")
            colnames(rowProfiles)[ncol(rowProfiles)] <- .("Active Margin")
            return(rowProfiles)
        },
        .getContingencyTable = function(contingencyTable, supplementaryRows, supplementaryCols) {
            savedSupplementaryRows <- contingencyTable[supplementaryRows,]
            savedSupplementaryCols <- contingencyTable[,supplementaryCols]
            contingencyTable[supplementaryRows,] <- 0                # set supplementary rows to 0
            contingencyTable[,supplementaryCols] <- 0                # Empty supplementary columns
            contingencyTable <- addmargins(contingencyTable, margin=c(1,2))    # Add margin row (sum)
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
                    self$results$contingency$addColumn(".", type="text")
                    self$results$rowProfiles$addColumn(".", type="text")
                    self$results$colProfiles$addColumn(".", type="text")
                    self$results$rowSummary$addColumn(".", type="text")
                    self$results$colSummary$addColumn(".", type="text")
                    self$results$eigenvalues$addRow(".")
                    self$results$eigenvalues$setNote("chisq", NULL)
                    return()
                }

                rowVarName <- self$options$rows
                colVarName <- self$options$cols
                countsVarName <- self$countsName

                # Set variable names
                rowVarNameString <- private$.getVarName(rowVarName)
                colVarNameString <- private$.getVarName(colVarName)

                #Contingency Table (base)

                if (!is.null(countsVarName)) {
                    formula <- jmvcore::composeFormula('.COUNTS', c(rowVarName, colVarName))
                    contingencyTable <- xtabs(formula, data)
                } else {
                    contingencyTable <- table(self$data[[rowVarName]], self$data[[colVarName]])
                }
            } else { # self$options$mode == "contTable"
                if (is.null(self$options$rowLabels) || length(self$options$columns) < 3)
                    return()
                contingencyTable <- self$data[,self$options$columns]
                row.names(contingencyTable) <- self$data[[self$options$rowLabels]]
                contingencyTable <- as.matrix(contingencyTable)
                # Set variable names
                rowVarName <- self$options$rowLabels
                rowVarNameString <- rowVarName
                colVarName <- self$options$columnTitle
                colVarNameString <- colVarName
            }

            #self$results$text$setContent(contingencyTable)

            #### Supplementary Rows & Column ####

            # Rows
            if (is.null(self$options$supplementaryRows) || self$options$supplementaryRows == "0" || self$options$supplementaryRows == "") {
                supplementaryRows <- NULL
            } else {
                supplementaryRows <- as.integer(unlist(strsplit(self$options$supplementaryRows,",")))
                if (any(is.na(supplementaryRows))) {
                    vijErrorMessage(self, .("Supplementary row numbers must be a list of numbers, e.g. 1,2,9"))
                    return(TRUE)
                } else {
                    supplementaryRows <- sort(unique(supplementaryRows))
                    nmax <- nrow(contingencyTable) #nlevels(self$data[[rowVarName]])
                    if (!all(supplementaryRows %in% 1:nmax)) {
                        errorMessage <- jmvcore::format(.("Supplementary row numbers must be between 1 and {nmax}."), nmax = nmax)
                        vijErrorMessage(self, errorMessage)
                        return(TRUE)
                    }
                }
            }
            # Columns
            if (is.null(self$options$supplementaryCols) || self$options$supplementaryCols == "0" || self$options$supplementaryCols == "") {
                supplementaryCols <- NULL
            } else {
                supplementaryCols <- as.integer(unlist(strsplit(self$options$supplementaryCols,",")))
                if (any(is.na(supplementaryCols))) {
                    vijErrorMessage(self,.("Supplementary column numbers must be a list of numbers, e.g. 1,2,9"))
                    return(TRUE)
                } else {
                    supplementaryCols <- sort(unique(supplementaryCols))
                    nmax <- ncol(contingencyTable) #nlevels(self$data[[colVarName]])
                    if (!all(supplementaryCols %in% 1:nmax)) {
                        errorMessage <- jmvcore::format(.("Supplementary column numbers must be between 1 and {nmax}."), nmax=nmax)
                        vijErrorMessage(self, errorMessage)
                        return(TRUE)
                    }
                }
            }
            # Modify the supplementary row/col names
            for (i in supplementaryRows)
                rownames(contingencyTable)[i] <- paste(rownames(contingencyTable)[i], "*")
            for (j in supplementaryCols)
                colnames(contingencyTable)[j] <- paste(colnames(contingencyTable)[j], "*")

            #### Dimensions and axes  ####

            # Solution dimension
            maxDim = min(nrow(contingencyTable)-length(supplementaryRows), ncol(contingencyTable)-length(supplementaryCols)) - 1
            nDim <-self$options$dimNum
            if (nDim > maxDim) {
                errorMessage <- jmvcore::format(.("Number of dimensions must be less than or equal to {maxDim}."), maxDim = maxDim)
                vijErrorMessage(self,errorMessage)
                return(TRUE)
            }
            # Axis
            xaxis <- self$options$xaxis
            yaxis <- self$options$yaxis
            if (xaxis > nDim || yaxis > nDim) {
                errorMessage <- jmvcore::format(.("Axis numbers must be less than or equal to the number of dimensions ({nDim})."), nDim = nDim)
                vijErrorMessage(self, errorMessage)
                return(TRUE)
            }
            if (xaxis == yaxis) {
                vijErrorMessage(self, .("Axis numbers cannot be equal."))
                return(TRUE)
            }

            #### Normalisation ####
            normalizationString <- switch(self$options$normalization,
                                          principal = .("Principal normalization"),
                                          symmetric = .("Symetric normalization"),
                                          rowprincipal = .("Row principal normalization"),
                                          colprincipal = .("Column principal normalization"),
                                          standard = .("Standard normalization")
            )

            #### Contingency Table (with supplementary rows/columns ####

            fullTable <- private$.getContingencyTable(contingencyTable, supplementaryRows, supplementaryCols)
            rownames(fullTable)[nrow(fullTable)] <- .("Active Margin")
            colnames(fullTable)[length(colnames(fullTable))] <- .("Active Margin")
            self$results$contingency$addColumn(rowVarName, type="text", title = rowVarNameString)
            for (col in colnames(fullTable)) {
                if (col != .("Active Margin"))
                    self$results$contingency$addColumn(col, type="integer", superTitle = colVarNameString)
                else
                    self$results$contingency$addColumn(col, type="integer")
            }
            for (i in seq(nrow(fullTable))) {
                self$results$contingency$addRow(i, values = fullTable[i,])
                self$results$contingency$setCell(rowNo = i, rowVarName, rownames(fullTable)[i])
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
                # Row Profiles
                rowProfiles <- private$.getProfile(contingencyTable, supplementaryRows, supplementaryCols)
                self$results$rowProfiles$addColumn(rowVarName, type = "text", title = rowVarNameString)
                for (j in seq(ncol(rowProfiles))) {
                    self$results$rowProfiles$addColumn(colnames(rowProfiles)[j], type = "number", format = "zto", superTitle = colVarNameString)
                }
                for (i in seq(nrow(rowProfiles))) {
                    self$results$rowProfiles$addRow(i, values = rowProfiles[i,])
                    self$results$rowProfiles$setCell(rowNo = i, rowVarName, rownames(rowProfiles)[i])
                }
                self$results$rowProfiles$addFormat(rowNo = nrow(rowProfiles), 1, jmvcore::Cell.BEGIN_END_GROUP)
                if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                    self$results$rowProfiles$setNote("supp", paste("* :", .("Supplementary rows/columns")))
                # Column Profiles
                colProfiles <- t(private$.getProfile(t(contingencyTable),supplementaryCols, supplementaryRows))
                self$results$colProfiles$addColumn(rowVarName, type = "text", title = rowVarNameString)
                for (j in seq(ncol(colProfiles))) {
                    self$results$colProfiles$addColumn(colnames(colProfiles)[j], type = "number", format = "zto", superTitle = colVarNameString)
                }
                for (i in seq(nrow(colProfiles))) {
                    self$results$colProfiles$addRow(i, values = colProfiles[i,])
                    self$results$colProfiles$setCell(rowNo = i, rowVarName, rownames(colProfiles)[i])
                }
                self$results$colProfiles$addFormat(rowNo = nrow(colProfiles), 1, jmvcore::Cell.BEGIN_END_GROUP)
                if (!is.null(supplementaryRows) || !is.null(supplementaryCols))
                    self$results$colProfiles$setNote("supp",paste("* :", .("Supplementary rows/columns")))
            }

            #### Chi-Squared test ####

            activeContingencyTable <- contingencyTable
            if (!is.null(supplementaryRows))
                activeContingencyTable <- activeContingencyTable[-supplementaryRows,]
            if (!is.null(supplementaryCols))
                activeContingencyTable <- activeContingencyTable[,-supplementaryCols]
            chisqres <- chisq.test(activeContingencyTable)
            if (round(chisqres$statistic,2) == 0)
                return()

            #### Compute CA ####
            if (is.null(supplementaryRows))
                suprow <- NULL
            else
                suprow <- supplementaryRows
            if (is.null(supplementaryCols))
                supcol <- NULL
            else
                supcol <- supplementaryCols

            res <- private$.ca(contingencyTable, row.sup = suprow, col.sup = supcol, ncp = nDim, norm = self$options$normalization)

            #### Inertia Table ####
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
            self$results$eigenvalues$setNote("chisq",
                                             paste0("X-squared = ", round(chisqres$statistic,2), ", df = ", chisqres$parameter, ",
                               p-value = ",format.pval(chisqres$p.value, eps = 0.001)),
                                             init = FALSE)
            #### Summary Tables ####

            if(self$options$showSummaries) {
                # Row Summary Table
                self$results$rowSummary$addColumn(name = "id", title = "#", type = "integer")
                self$results$rowSummary$addColumn(name = "row", title = rowVarNameString, type = "text")
                self$results$rowSummary$addColumn(name = "margin", title = "Mass", type = "number", format = "zto")
                for (i in seq(nDim))
                    self$results$rowSummary$addColumn(name = paste0("score",i), title = paste("Dim",i), superTitle = paste(.("Coordinates"),"†"), type = "number", format = "zto")
                self$results$rowSummary$addColumn(name = "inertia", title = "% Inertia", type = "number", format = "zto")
                for (i in seq(nDim))
                    self$results$rowSummary$addColumn(name = paste0("contrib",i), title = paste("Dim",i), superTitle = "Contributions", type = "number", format = "zto")
                self$results$rowSummary$addColumn(name = "qlt", title = "QLT", type = "number", format = "zto")
                for (i in seq(nDim))
                    self$results$rowSummary$addColumn(name = paste0("cos",i), title = paste("Dim",i), superTitle = "CO2", type = "number", format = "zto")
                # Populate Row Summary
                for (i in seq_len(nrow(contingencyTable))) {
                    aRow <- rownames(contingencyTable)[i]
                    if (aRow %in% rownames(res$row$coord)) {
                        theValues = list(
                            id = i,
                            row = aRow,
                            margin = res$call$marge.row[aRow],
                            inertia = res$row$inertia[aRow],
                            qlt = sum(res$row$cos2[aRow,1:nDim])
                        )
                        for (j in seq(nDim)) {
                            theValues[[paste0("score",j)]] <- res$row$coord[aRow,j]
                            theValues[[paste0("contrib",j)]] <- res$row$contrib[aRow,j]
                            theValues[[paste0("cos",j)]] <- res$row$cos2[aRow,j]
                        }
                    } else {
                        # Supplementary row
                        theValues = list(
                            id = i,
                            row = aRow,
                            margin = "",
                            inertia = "",
                            qlt = sum(res$row.sup$cos2[aRow,1:nDim])
                        )
                        for (j in seq(nDim)) {
                            theValues[[paste0("score",j)]] <- res$row.sup$coord[aRow,j]
                            theValues[[paste0("contrib",j)]] <- ""
                            theValues[[paste0("cos",j)]] <- res$row.sup$cos2[aRow,j]
                        }

                    }
                    self$results$rowSummary$addRow(i, values = theValues)
                }
                if (!is.null(supplementaryRows))
                    self$results$rowSummary$setNote("supp",paste("* :", .("Supplementary rows")))
                self$results$rowSummary$setNote("norm", paste("† :", normalizationString))

                # Column Summary Table
                self$results$colSummary$addColumn(name = "id", title = "#", type = "integer")
                self$results$colSummary$addColumn(name = "col", title = colVarNameString, type = "text")
                self$results$colSummary$addColumn(name = "margin", title = "Mass", type = "number", format = "zto")
                for (i in seq_len(nDim))
                    self$results$colSummary$addColumn(name = paste0("score",i), title = paste("Dim",i), superTitle = paste(.("Coordinates"),"†"), type = "number", format = "zto")
                self$results$colSummary$addColumn(name = "inertia", title = "% Inertia", type = "number", format = "zto")
                for (i in seq_len(nDim))
                    self$results$colSummary$addColumn(name = paste0("contrib",i), title = paste("Dim",i), superTitle = "Contributions", type = "number", format = "zto")
                self$results$colSummary$addColumn(name = "qlt", title = "QLT", type = "number", format = "zto")
                for (i in seq_len(nDim))
                    self$results$colSummary$addColumn(name = paste0("cos",i), title = paste("Dim",i), superTitle = "CO2", type = "number", format = "zto")
                # Populate Col Summary
                for (i in seq_len(ncol(contingencyTable))) {
                    aCol <- colnames(contingencyTable)[i]
                    if (aCol %in% rownames(res$col$coord)) {
                        theValues = list(
                            id = i,
                            col = aCol,
                            margin = res$call$marge.col[aCol],
                            inertia = res$col$inertia[aCol],
                            qlt = sum(res$col$cos2[aCol,1:nDim])
                        )
                        for (j in seq(nDim)) {
                            theValues[[paste0("score",j)]] <- res$col$coord[aCol,j]
                            theValues[[paste0("contrib",j)]] <- res$col$contrib[aCol,j]
                            theValues[[paste0("cos",j)]] <- res$col$cos2[aCol,j]
                        }
                    } else {
                        # Supplementary col
                        theValues = list(
                            id = i,
                            col = aCol,
                            margin = "",
                            inertia = "",
                            qlt = sum(res$col.sup$cos2[aCol,1:nDim])
                        )
                        for (j in seq(nDim)) {
                            theValues[[paste0("score",j)]] <- res$col.sup$coord[aCol,j]
                            theValues[[paste0("contrib",j)]] <- ""
                            theValues[[paste0("cos",j)]] <- res$col.sup$cos2[aCol,j]
                        }
                    }
                    self$results$colSummary$addRow(i, values = theValues)
                }
                if (!is.null(supplementaryCols))
                    self$results$colSummary$setNote("supp", paste("* :", .("Supplementary columns")))
                self$results$colSummary$setNote("norm", paste("† :", normalizationString))
            }
            if (length(res$sv) < 2)
                return()
            if (res$sv[2] < .Machine$double.eps)
                return()

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
                ptcoord <- NA
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
            dim1name <- paste0(.("Dimension"), " ", xaxis, " (", percentInertia[xaxis], '\u2009%)')
            yaxis <- self$options$yaxis
            yaxisdim <- paste("Dim", yaxis)
            dim2name <- paste0(.("Dimension"), " ", yaxis, " (", percentInertia[yaxis], '\u2009%)')

            # Building the plot
            plot <-  ggplot(ptcoord, aes(x = ptcoord[,xaxisdim], y = ptcoord[,yaxisdim], color = ptcoord$sup, shape = ptcoord$sup))
            plot <- plot + geom_point()
            plot <- plot + ggrepel::geom_text_repel(aes(label = rownames(ptcoord)), show.legend = FALSE, size = self$options$labelSize/.pt)
            plot <- plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)

            # Apply jmv theme
            plot <- plot + ggtheme

            # Set point colors
            plot <- plot +
                scale_color_manual(
                    values=c("1" = self$options$rowColor, "2" = self$options$supColor, "3" = self$options$colColor, "4" = self$options$supColor),
                    breaks=c("1", "3", "2", "4")) + labs(color = "") +
                    #labels = c(self$options$rows, self$options$cols, .("Suppl. Row"), .("Suppl. Column"))) + labs(color = "") +
                scale_shape_manual(values = c(19, 19, 17, 17), breaks = c("1","2","3","4")) +
                theme(legend.text = element_text(size=10))
            plot <- plot + guides(color = "none", shape = "none")

            # Plot frame
            plot <- plot + theme(axis.line = element_line(linewidth = 0), panel.border = element_rect(color = "black", fill = NA, size = 1))

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
            plot <- plot + vijTitlesAndLabels(self$options, defaults, plotType = plotType) + vijTitleAndLabelFormat(self$options, showLegend = FALSE)


            #self$results$text$setContent(plot) # show debug message
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

        #### Helper functions ---- modified from jmv/conttables.b.R
        .cleanData = function(B64 = FALSE) {

            data <- self$data

            rowVarName <- self$options$rows
            colVarName <- self$options$cols
            countsName <- self$options$counts

            columns <- list()

            if ( ! is.null(rowVarName)) {
                columns[[rowVarName]] <- as.factor(data[[rowVarName]])
            }
            if ( ! is.null(colVarName)) {
                columns[[colVarName]] <- as.factor(data[[colVarName]])
            }

            if ( ! is.null(countsName)) {
                columns[['.COUNTS']] <- jmvcore::toNumeric(data[[countsName]])
            } else if ( ! is.null(attr(data, "jmv-weights"))) {
                columns[['.COUNTS']] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
            } else {
                columns[['.COUNTS']] <- as.integer(rep(1, nrow(data)))
            }

            if (B64)
                names(columns) <- jmvcore::toB64(names(columns))

            attr(columns, 'row.names') <- paste(seq_len(length(columns[[1]])))
            class(columns) <- 'data.frame'

            columns
        },
        .showHelpMessage = function() {
            helpMsg <- .('<p>This module computes <strong>Correspondence Analysis (CA)</strong> for two categorical variables. Computations are based on <a href = "https://CRAN.R-project.org/package=FactoMineR" target="_blank">FactoMineR<a/> package by F.&nbsp;Husson, J.&nbsp;Josse, S.&nbsp;Le, J.&nbsp;Mazet.</p>
<p>The data can be</p>
<ul>
<li>an <strong>Observation table</strong> (raw data), possibly weighted using <em>jamovi</em> built-in weight system or using the "Counts" variable</li>
<li>or a <strong>Contingency table</strong></li>
</ul>
<p><strong>Supplementary row or column</strong> numbers may be entered as integer lists : 1,3,6</p>
<p>Four normalizations (scaling of row and column scores before plotting) are avalaible :</p>
<ul>
<li><strong>Principal:</strong> Row an columns scores are scaled by eigenvalues.</li>
<li><strong>Symmetric:</strong> Row an columns scores are scaled by the square root of eigenvalues. </li>
<li><strong>Row Principal:</strong> Only row scores are scaled by eigenvalues.</li>
<li><strong>Column Principal:</strong> Only column scores are scaled by eigenvalues.</li>
<li><strong>Standard:</strong> The raw coordinates without normalization.</li>
</ul>
<p>A sample file is included at Open > Data Library > vijPlots > Smoking</p>')
            vijHelpMessage(self, helpMsg)
        }
    )
)
