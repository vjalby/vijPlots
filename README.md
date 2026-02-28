# vijPlots

[jamovi](https://www.jamovi.org) module as ggplot2 wrapper to generate 
basic stats plots (histogram, boxplot, barplot, piechart,...) 
with many options along with Likert and multiple response barplots, 
Principal component analysis and (Multiple) Correspondence analysis.

## Histogram

![](img/hist.jpg)

## Box Plot

![](img/box.jpg)

## Scatter Plot

![](img/scatter.jpg)

## Lollipop Plot

![](img/lollipop.jpg)

## Bar Plot

![](img/bar.jpg)

## Pie Chart

![](img/pie.jpg)

## Likert Plot

![](img/likert.jpg)

## Multiple Responses

![](img/mr.jpg)

## Line Charts

![](img/linechart1.jpg)

![](img/linechart2.jpg)

## Area Chart

![](img/areachart.jpg)

## Raincloud Plot

![](img/raincloud.jpg)

## QQ Plot

![](img/qqplot.jpg)

## Principal Component Analysis

![](img/principal.jpg)

## Correspondence Analysis

![](img/correspondence.jpg)

## Multiple Correspondence Analysis

![](img/mca.jpg)

## Version history

### 2026-02-27 / 0.99.4

-   Axis options to set axis label sizes, rotation, range
-   Regression line in scatter plot
-   Density line in histogram
-   Faceting added to scatter plot
-   Bar plot reworked with labels and percent by group/category
-   Pie chart label options
-   Option to show % labels in MR Frequencies & Crosstabs plots
-   Single color option for scatter plot without group
-   Option to choose 1 color from palette for bar plot, box plot, MR plot without group
-   Unused levels are now retained in boxplot, barplot, raincloud (if checked in variable settings)
-   More color palettes
-   Tidy up Option UI
-   French translation completed
-   Spanish translation (second draft)
-   Plot sizing options removed (built in jamovi 2.7.16+)
-   Module icon (with jamovi 2.7.19+)

### 2025-12-21 / 0.11.1 (beta)

-   Multiple Response Frequencies/crosstab can handle multi-valued variables

### 2025-12-16 / 0.11.0 (beta)

-   Multiple Correspondence Analysis (using FactoMineR)
-   Update Correspondence Analysis (using FactoMineR)
-   Option to save PCA scores in Principal Component Analysis
-   Viridis and Dichromat palettes added
-   Title/Subtitle/Caption options for all plots
-   Legend and axis label options for all plots
-   Small fixes and improvements

### 2025-09-03 / 0.10.0 (beta)

-   Principal Component Analysis
-   Plot Titles & Subtitle in Correspondence Analysis
-   "Use variable's description as name" for Likert, CA, PCA
-   Moved vijPlots menu to Jamovi 2.7 "Plots" ribbon

### 2025-07-20 / 0.9.5 (beta)

-   Table and tests (KW, post hoc) added to Likert Plot
-   Improve data handling for Likert Plot
-   Principal Component Plot (work in progress)

### 2025-06-06 / 0.9.1 (beta)

-   Correspondence (analysis) Plot
-   Improve text size computation for Likert Plot

### 2025-03-01 / 0.8.0 (beta)

-   Q-Q & P-P Plots (using qqplotr package)
-   Raincloud plot
-   Improved Boxplot: Labels (for outliers), staples, notched box, horizontal plot, legend at bottom, NA's, custom plot size
-   Improved Likert plot: Fix bugs with "by group" total computation and with "reverse staking order", fix missing values exclusion, add % accuracy setting, ignore group NA setting.
-   Fix label position in Scatterplot

### 2024-12-11 / 0.7.0 (beta)

-   Line Chart
-   Area Chart
-   Option "Auto" for text color in Barchart and Piechart using ggstats::hex_bw()
-   Sort (by median) in Boxplot
-   Color Options in Multiple Response Frequencies & Crosstab
-   Option to convert variables to integer in Likert Plot.
-   Likert Plot can plot a single variable

### 2024-11-10 / 0.6.0 (beta)

-   Lollipop plot
-   Improve normal curve in histogram using ggh4x

### 2024-11-01 / 0.5.0 (beta)

-   French translation
-   bug fixes

### 2024-10-27 / 0.4.0 (alpha)

-   barplot and scatterplot geom wrappers added.
-   merged with vijMR (multiple response) and vijLikert

### 2024-10-21 / 0.2.0 (alpha)

-   boxplot and pie chart geom wrappers added.

### 2024-10-15 / 0.1.0 (alpha)

-   First public release with histogram geom wrapper.

## References

-   Larmarange J. (2025). ggstats: Extension to 'ggplot2' for Plotting Stats. R package version 0.8.0, <https://github.com/larmarange/ggstats>.
-   Almeida, A., Loy, A., Hofmann, H. (2023). qqplotr: Quantile-Quantile Plot Extensions for 'ggplot2'. R package version 0.0.6, <https://github.com/aloy/qqplotr>.
-   Bernaards, C., Gilbert, P., Jennrich, R. (2025), GPArotation: Gradient Projection Factor Rotation. R package version 2025.3.1, <https://cran.r-project.org/package=GPArotation>.
-   Greenacre, M. (2010), Biplots in Practice, Fundación BBVA. <https://www.fbbva.es/en/publicaciones/biplots-in-practice-7/>.
-   Husson, F., Josse, J., Le, S., Mazet, J. (2025). FactoMineR: Multivariate Exploratory Data Analysis and Data Mining. R package version 2.12, <https://cran.r-project.org/package=FactoMineR>
-   Engler, J.B. (2026). tidyplots: Tidy Plots for Scientific Papers, R package version 0.4.0, <https://CRAN.R-project.org/package=tidyplots>
