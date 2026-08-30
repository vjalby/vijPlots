# vijPlots 1.3.1 (2026-08-31)

* Mosaic Plot
* Facet label options (font, size, alignment, background style)
* Histogram: option to show mean/median reference lines, with an optional value label
* Multiple Response Plots: the "Counted value" option now accepts any text (e.g. Y/N)
* Line/Area Chart: manual X-axis date range can now be entered as text, and fixed breaks falling outside the specified range
* Multiple Response Frequencies/Crosstabs: fixed a value containing only the separator (e.g. ";") not being treated as missing
* Two new color palettes based on the Carbon Design System (Carbon:Dark, Carbon:Light)
* Improved error handling

# vijPlots 1.3.0 (2026-08-17)

* Code optimization and fixes everywhere
* MCA (Burt method): Fixed variable discrimination computation
* Histogram: Stacking group densities and normal curves when Grouping:stacked option is selected
* Histogram: Options to hide bins and to show lines
* Scatter Plot: Option to use different shapes. Option to set label text size
* Fixed stacking order in barplot/barchart/mrcrosstab
* Support for weights in barplot/piechart
* Improved ggplot2 4.0 support

# vijPlots 1.2.2 (2026-07-31)

* Fixed compatibility with ggplot2 4.0 / jamovi 28.1
* Option to ignore missing groups in Raincloud plot
* typo / translations fixes

# vijPlots 1.2.1 (2026-06-08)

* Improved x-axis limits for line and area plots

# vijPlots 1.2.0 (2026-05-31)

* Option to use contingency table (instead of observation table) for Correspondence analysis
* Bar chart for quantitative variables with error bars

# vijPlots 1.1.0 (2026-03-21)

* Axis tick option

# vijPlots 1.0.0 (2026-03-01)

* Axis options to set axis label sizes, rotation, range
* Regression line in scatter plot
* Density line in histogram
* Faceting added to scatter plot and box plot
* Bar plot reworked with labels and percent by group/category
* Pie chart label options
* Option to show % labels in MR Frequencies & Crosstabs plots
* Single color option for scatter plot without group
* Option to choose 1 color from palette for bar plot, box plot, MR plot without group
* Unused levels are now retained in boxplot, barplot, raincloud (if checked in variable settings)
* More color palettes
* Tidy up Option UI
* French translation completed
* Spanish translation completed
* Plot sizing options removed (built in jamovi 2.7.16+)
* Module icon (with jamovi 2.7.19+)

# vijPlots 0.11.1 (2025-12-21, beta)

* Multiple Response Frequencies/crosstab can handle multi-valued variables

# vijPlots 0.11.0 (2025-12-16, beta)

* Multiple Correspondence Analysis (using FactoMineR)
* Update Correspondence Analysis (using FactoMineR)
* Option to save PCA scores in Principal Component Analysis
* Viridis and Dichromat palettes added
* Title/Subtitle/Caption options for all plots
* Legend and axis label options for all plots
* Small fixes and improvements

# vijPlots 0.10.0 (2025-09-03, beta)

* Principal Component Analysis
* Plot Titles & Subtitle in Correspondence Analysis
* "Use variable's description as name" for Likert, CA, PCA
* Moved vijPlots menu to Jamovi 2.7 "Plots" ribbon

# vijPlots 0.9.5 (2025-07-20, beta)

* Table and tests (KW, post hoc) added to Likert Plot
* Improve data handling for Likert Plot
* Principal Component Plot (work in progress)

# vijPlots 0.9.1 (2025-06-06, beta)

* Correspondence (analysis) Plot
* Improve text size computation for Likert Plot

# vijPlots 0.8.0 (2025-03-01, beta)

* Q-Q & P-P Plots (using qqplotr package)
* Raincloud plot
* Improved Boxplot: Labels (for outliers), staples, notched box, horizontal plot, legend at bottom, NA's, custom plot size
* Improved Likert plot: Fix bugs with "by group" total computation and with "reverse staking order", fix missing values exclusion, add % accuracy setting, ignore group NA setting.
* Fix label position in Scatterplot

# vijPlots 0.7.0 (2024-12-11, beta)

* Line Chart
* Area Chart
* Option "Auto" for text color in Barchart and Piechart using ggstats::hex_bw()
* Sort (by median) in Boxplot
* Color Options in Multiple Response Frequencies & Crosstab
* Option to convert variables to integer in Likert Plot.
* Likert Plot can plot a single variable

# vijPlots 0.6.0 (2024-11-10, beta)

* Lollipop plot
* Improve normal curve in histogram using ggh4x

# vijPlots 0.5.0 (2024-11-01, beta)

* French translation
* bug fixes

# vijPlots 0.4.0 (2024-10-27, alpha)

* barplot and scatterplot geom wrappers added.
* merged with vijMR (multiple response) and vijLikert

# vijPlots 0.2.0 (2024-10-21, alpha)

* boxplot and pie chart geom wrappers added.

# vijPlots 0.1.0 (2024-10-15, alpha)

* First public release with histogram geom wrapper.
