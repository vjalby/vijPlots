# Frequency table (barplot, piechart): one block of rows per facet x outer level, the inner levels
# as rows, with counts, % and cumulative % within the block, and a Total row per block.
# - self: the analysis; its results$frequencies Table has the columns facet, inner, counts, percent,
#   cumPercent (and outer when used). Also needed by .() to translate "Total" and the note.
# - plotData: aggregated data (one row per combination, weights in .COUNTS, no zero weight)
# - inner, outer, facet: column names in plotData (outer/facet NULL when not used)
# - nMissing: observations left out because of missing values (ignoreNA checked), shown as a note
vijFrequencyTable = function(self, plotData, inner, outer = NULL, facet = NULL, nMissing = 0) {
    table <- self$results$frequencies
    # Weighted counts may be decimal: switch the "counts" column (declared integer, which would round
    # them) to number. This is the approach of jmv::descriptives (its frequency tables): jmvcore has no
    # setter for a column type, so the private field of the column is changed directly
    if (any(abs(plotData$.COUNTS - round(plotData$.COUNTS)) > sqrt(.Machine$double.eps))) {
        countsColumn <- table$columns[['counts']]
        countsColumn$.__enclos_env__$private$.type <- 'number'
    }
    # Levels of a variable completed with NA if necessary
    levelsOf <- function(name, drop = FALSE) {
        if (is.null(name)) {
            return(list(NULL))
        } else {
            x <- plotData[[name]]
            values <- if (drop) levels(droplevels(x)) else levels(x)
            if (anyNA(x)) values <- c(values, NA)
            return(as.list(values))
        }
    }
    # Label of a value: "" when no such variable (NULL), "(Missing)" for missing values
    labelNA <- vijLabelNA(self)
    labelOf <- function(value) if (is.null(value)) "" else labelNA(value)
    # Columns of the block's first row (facet/outer only exist when used)
    blockColumns <- c(if (!is.null(facet)) 'facet', if (!is.null(outer)) 'outer',
                      'inner', 'counts', 'percent', 'cumPercent')
    # Only facets with data (as facet_wrap)
    facetLevels <- levelsOf(facet, drop = TRUE)
    outerLevels <- levelsOf(outer)
    innerLevels <- levelsOf(inner)
    rowNo <- 0
    for (facetLevel in facetLevels) {
        # Rows of this facet (may be NA). All rows when no facet
        facetData <- if (is.null(facetLevel)) plotData else dplyr::filter(plotData, .data[[facet]] %in% facetLevel)
        for (outerLevel in outerLevels) {
            # Rows of this block (may be NA). All the facet's rows when no outer variable
            blockData <- if (is.null(outerLevel)) facetData else dplyr::filter(facetData, .data[[outer]] %in% outerLevel)
            counts <- vapply(innerLevels, function(level) sum(blockData$.COUNTS[blockData[[inner]] %in% level]), numeric(1))
            total <- sum(counts)
            # An outer level without data in this facet has no bars: skip it
            if (total == 0)
                next
            blockValues <- list(facet = labelOf(facetLevel), outer = labelOf(outerLevel))
            cumCounts <- cumsum(counts)
            for (i in seq_along(innerLevels)) {
                rowNo <- rowNo + 1
                table$addRow(rowKey = rowNo, values = c(blockValues, list(
                    inner = labelOf(innerLevels[[i]]), counts = counts[[i]],
                    percent = counts[[i]] / total, cumPercent = cumCounts[[i]] / total)))
                # Blocks marked as in jmv::conttables: BEGIN_GROUP on the block's first row (jamovi
                # renders it as a top padding), END_GROUP on its last row before the Total
                if (i == 1)
                    for (col in blockColumns)
                        table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.BEGIN_GROUP)
                if (i == length(innerLevels))
                    for (col in blockColumns)
                        table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.END_GROUP)
            }
            rowNo <- rowNo + 1
            table$addRow(rowKey = rowNo, values = c(blockValues, list(
                inner = .("Total"), counts = total, percent = 1, cumPercent = "")))
            # The Total row is a group of its own
            for (col in c('inner', 'counts', 'percent', 'cumPercent'))
                table$addFormat(rowKey = rowNo, col = col, jmvcore::Cell.BEGIN_END_GROUP)
        }
    }
    # Observations left out because of missing values (ignoreNA checked), overall
    if (nMissing > 0)
        # formatted with jamovi's decimal symbol: in .run(), R's OutDec is not set (see CLAUDE.md)
        table$setNote('missing', jmvcore::format(.('Missing values: {n}'),
                                                 n = vijFormatCount(self$options[['decSymbol']])(nMissing)))
}

# Warning shown when the data is weighted (by the "counts" option or jamovi's weights)
vijWeightsMessage = function(self, countsName = NULL) {
    countsName <- countsName %||% attr(self$data, "jmv-weights-name")
    if (!is.null(countsName))
        vijWarningMessage(self, ..('The data is weighted by the variable {}.', countsName), '.weights')
}

# Data of the variables varNames, with their weights in .COUNTS (barplot, piechart, mosaic):
# - weights from the variable countsName, else jamovi's weights, else 1
# - a missing, negative or infinite weight is an error, even on a row with a missing value elsewhere
# - missing values left out when ignoreNA is checked; nMissing = their weighted number
# - all weights zero is an error; a zero weight means no observation, so those rows are dropped
# Returns list(data, nMissing), or NULL when no row is left.
vijWeightedData = function(self, varNames, countsName = NULL, ignoreNA = TRUE) {
    data <- jmvcore::select(self$data, varNames)

    # Weight data
    if (!is.null(countsName)) {
        # vijPlots weights
        data[['.COUNTS']] <- jmvcore::toNumeric(self$data[[countsName]])
    } else if (!is.null(attr(self$data, "jmv-weights"))) {
        # jamovi built-in weights
        data[['.COUNTS']] <- jmvcore::toNumeric(attr(self$data, "jmv-weights"))
    } else {
        # no weights
        data[['.COUNTS']] <- as.integer(rep(1, nrow(data)))
    }

    # Validate .COUNTS on every row, before leaving out missing values
    if (any(is.na(data$.COUNTS)))
        vijErrorMessage(self, .('Counts may not contain missing values.'))
    if (any(data$.COUNTS < 0))
        vijErrorMessage(self, .('Counts may not be negative.'))
    if (any(is.infinite(data$.COUNTS)))
        vijErrorMessage(self, .('Counts may not be infinite.'))

    # Excluded observations (weighted)
    if (ignoreNA) {
        isIncomplete <- !stats::complete.cases(data)
        nMissing <- sum(data$.COUNTS[isIncomplete])
        data <- jmvcore::naOmit(data)
    } else {
        nMissing <- 0
    }
    if (nrow(data) == 0)
        return(NULL)

    # Not all zero, among the observations actually plotted
    if (sum(data$.COUNTS) == 0)
        vijErrorMessage(self, .('Counts may not all be zero.'))
    # A zero weight means no observation: drop those rows, treated everywhere like no data
    data <- dplyr::filter(data, .data$.COUNTS > 0)

    list(data = data, nMissing = nMissing)
}

# One-hot encoding of a multi-valued variable (mrfrequencies, mrcrosstabs): each value of aCol holds
# several responses separated by `separator` (e.g. "a;b").
# - init = TRUE: returns only the distinct responses (the columns, for the table's init)
# - otherwise: a data.frame with one row per case and one 0/1 column per response (1 = the case chose it)
# - na: a missing or empty value (only separators, e.g. ";") is a missing case (NA row), not a case with
#   no response
vijOneHotEncoding = function(aCol, separator, na = TRUE, init = FALSE) {
    # List of values
    uniqueValues <- unique(unlist(strsplit(levels(aCol), split = separator, fixed = TRUE)))
    uniqueValues <- trimws(uniqueValues)
    uniqueValues <- unique(uniqueValues)  # trimming may create duplicates
    uniqueValues <- uniqueValues[uniqueValues != ""]
    # Return the list of values/columns for table init
    if (init)
        return(uniqueValues)
    # No distinct value found: an empty (0-column) table, still with one row per case
    if (length(uniqueValues) == 0)
        return(as.data.frame(matrix(nrow = length(aCol), ncol = 0)))
    # Build the encoded table
    rawValues <- as.character(aCol)
    # split then clean (heading/trailing spaces) the values
    splitted <- lapply(
        strsplit(rawValues, split = separator, fixed = TRUE),
        trimws
    )
    # build the 0/1 table: one row per case, one column per value (1 = the case chose that value).
    # Filled column by column, so that it keeps one row per case even with a single case
    onehotDF <- data.frame(row.names = seq_along(splitted))
    for (opt in uniqueValues) {
        chosen <- vapply(splitted, function(x) opt %in% x, logical(1))
        onehotDF[[opt]] <- as.integer(chosen)
    }
    # NA/empty rawvalues set to NA (a value containing only separators, e.g. ";",
    # splits into all-empty pieces after trimws() and must count as missing too —
    # trimws(rawValues) == "" alone misses that since it never removes separators)
    if (na) {
        isMissing <- is.na(aCol) | vapply(splitted, function(x) all(x == ""), logical(1))
        onehotDF[isMissing, ] <- NA
    }
    return(onehotDF)
}
