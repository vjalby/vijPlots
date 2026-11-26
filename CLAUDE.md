# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**vijPlots** is an R package / jamovi module: a `ggplot2` wrapper providing point-and-click
statistical plots inside [jamovi](https://www.jamovi.org). It ships 15 analyses, available in
jamovi's **Plots** ribbon tab: `histogram`, `boxplot`, `raincloud`, `scatterplot`, `barplot`,
`barchart`, `lollipop`, `qqplot`, `piechart`, `mosaic`, `likertplot`, `mrfrequencies`,
`mrcrosstabs`, `linechart`, `areachart`.

Three more analyses — `principal` (PCA), `corresp` (Correspondence Analysis), `multcorresp`
(Multiple Correspondence Analysis) — still ship in the package but are hidden from the ribbon
(`hidden: true` in `jamovi/0000.yaml`, see that file's "analyses" section): they were transferred
to a separate **vijMulti** module and are kept here only so documents created before the transfer
keep working. Don't add them to menus/README/docs as active features; do keep their `.b.R`/
`.a.yaml`/`.u.yaml`/`.r.yaml` and tests working, since old `.omv` files still call them. They're
slated for eventual removal, so don't spend effort on refactoring/polish/syncing them with vijMulti
(e.g. audit "by design" comments, style cleanups) — fix only what breaks old documents.

### Target jamovi version

Development now targets jamovi's **current** channel (28.2), having moved off the **stable**
channel (2.7.37) referenced in older notes/commits. As of jamovi 28.1, the bundled `jmvcore` no
longer calls the deprecated `ggplot2::element_line(size = ...)`, so the "The `size` argument of
`element_line()` is deprecated" warning that used to be expected/ignorable on 2.7.37 should no
longer appear — if it does show up again, treat it as a real regression, not a known non-issue.
The same warning showed up when running the tests, from the `jmvcore` installed in the R library
(2.7.7, which still calls `element_line(size = ...)` in `jmvcore::theme_default()` while rendering).
It disappeared once that library was updated to `jmvcore` 28.3 (2026-10-26): `test-likertplot.R` now
runs with 0 warnings. So if it comes back in the tests, first check `packageVersion("jmvcore")`.

## Working style

Never start or propose a parallel session or a git worktree for vijPlots work (user's rule,
2026-10-17): do everything in the current session, on `main`, in the main checkout.

Never run `git commit` (nor amend, reset or push) in vijPlots (user's rule, 2026-10-19): leave
changes in the working tree, the user commits themself. At most, propose a commit message.

Before editing code or YAML, show the planned changes (excerpt or diff, per file) and wait for
the user's go-ahead (user's preference, 2026-10-17). Simple actions the user asks for directly
(deleting a named file, running tests) don't need this step. Nor does `CLAUDE.md` itself: edit it
without asking (user's preference, 2026-11-03).

## Commands

```r
devtools::load_all(".")                                   # load the package for interactive dev
devtools::document()                                       # regenerate man/ from roxygen comments — stubs only (empty @param text); man/ isn't tracked in git and isn't actively maintained, since vijPlots is used via jamovi's GUI, not `?fun`
jmvtools::prepare()                                         # regenerate R/*.h.R from jamovi/*.yaml after editing options
jmvtools::install()                                         # build & install the .jmo into a local jamovi (requires options(jamovi_home=...), see build.R)
```

`inst/` (and `R/*.h.R`) are fully build-generated — regenerated wholesale by `jmvtools::prepare()`/
`jmvtools::install()`. Don't fuss over preserving, restoring, or diffing uncommitted changes under
`inst/` before running either command: `jmvtools::prepare()` has been observed to delete files
there outright (e.g. `inst/i18n/fr.json`) as part of its regeneration pass rather than just
overwrite them. If you need to keep in-progress edits to a tracked file under `inst/`, commit or
copy it out *before* running `prepare()`/`install()`, since anything lost this way isn't
recoverable from the working tree afterward.

**Releases.** The user handles the version/date/README/NEWS changes and the example files
(`data/*.omv`) themself, then commits "Version X.Y.Z". Before that, `DESCRIPTION` goes from the
4-part dev version to `X.Y.Z` (in sync with `0000.yaml`'s `version:` and date), and the `.jmo` is
rebuilt. After the release commit, remind the user to create a git tag, prefixed with `v`
(`v1.3.3`, `v1.4.0`): a plain lightweight tag, not an annotated one (user's choice, 2026-10-16 —
`v1.3.3` itself is annotated, but don't redo it): `git tag vX.Y.Z <commit>` then
`git push origin vX.Y.Z`. vijPlots 1.4.0 was released on 2026-11-08 (tag `v1.4.0`, commit
`3058449`); development of 1.5 needs `DESCRIPTION` back to a 4-part version (e.g. `1.4.0.1`).

Testing (testthat edition 3, run from the package root after `devtools::load_all(".")`):

```r
Sys.setenv(NOT_CRAN = "true")                              # required, see "Testing" below
testthat::test_file("tests/testthat/test-histogram.R")
testthat::test_dir("tests/testthat")
```

There is no CI workflow in this repo (no `.github/`) and no `renv`/`air.toml` — don't assume
either is present or run commands that depend on them.

**Building and testing for jamovi 2.7** (2026-11-07). jamovi 2.7.41 is installed next to jamovi 28
(`/Applications/_Applications/jamovi 2.7.41.app`). The compiler builds with the R of the jamovi given
as `--home` (2.7.41: R 4.5.0, ggplot2 3.5.2, jmvcore 2.7.35, rlang 1.1.6; ggstats 0.9.0 and ggrepel
0.9.6 come from jamovi's own `jmv` module, not from the `.jmo`). `jmvtools::install(home = ...)` breaks
on the space in the path and would overwrite the jamovi 28 `.jmo`, so call the compiler directly:
`system2(node:::node(), c(jmvtools:::jmcPath(), "--build", ".", "--home", shQuote("/Applications/_Applications/jamovi 2.7.41"), "--jmo", "vijPlots_X.Y.Z-jamovi2.7.jmo"))`
(dependencies go to `build/R4.5.0-arm64-macos`). The test suite can then run with jamovi 2.7's R
(`R_HOME=<app>/Contents/Frameworks/R.framework/Versions/Current/Resources`, `.libPaths()` = that
build dir + `<app>/Contents/Resources/modules/base/R` + `.../jmv/R`, `CI=true` since snapshots
can't match, `test_dir(package = "vijPlots", load_package = "installed")`). Known differences there,
not vijPlots bugs: `likertplot`'s Mann-Whitney p-values (R 4.6's `wilcox.test()` computes exact
p-values with ties, R 4.5 warns and uses the normal approximation; the tests' expected values are
R 4.6's), and an `is.ggproto()`
deprecation warning raised by ggh4x 0.3.0's `facetted_pos_scales()` in `mosaic` facets.

## Architecture

### Five files per analysis

Each analysis `<name>` (e.g. `histogram`) is defined by:

- `R/<name>.b.R` — the backend, an R6 class `<name>Class` (`inherit = <name>Base`) implementing
  `private$.init()` (sets the plot's pixel size, often dynamically from facet/group levels),
  `private$.run()` (validates required variables, prepares `plotData`, calls
  `image$setState(plotData)`), and `private$.plot(image, ggtheme, theme, ...)` (builds and
  `return()`s the `ggplot` object from `image$state`).
- `R/<name>.h.R` — **auto-generated by `jmvtools::prepare()` from the `.yaml` files below; never
  edit by hand.** Defines `<name>Options`, `<name>Results`, `<name>Base`, and the exported
  `<name>(...)` wrapper function that end users (and tests) call directly.
- `jamovi/<name>.a.yaml` — option definitions (types, defaults, permitted variable types).
- `jamovi/<name>.u.yaml` — UI layout (controls, `enable:` conditions referencing other options).
- `jamovi/<name>.r.yaml` — declares the result items (e.g. `plot`) that `self$results$<item>`
  refers to in the `.b.R`.

All 18 analyses are additionally registered in `jamovi/0000.yaml` (title, menu placement, `ns:
vijPlots`). To scaffold a new analysis, see `build.R` for the `jmvtools::addAnalysis(name=...,
title=...)` pattern used for the existing ones.

### Shared utilities (`R/utils.R`, `R/utilsPlots.R`)

`R/utils.R` and `tests/testthat/test-utils.R` are **identical in vijPlots and vijMulti** (split
2026-11-22): edit them here, then copy them to `../vijMulti/` and check with the two
`identical(readLines(...))` lines of `build.R`. They hold the generic presentation helpers, some
unused by vijMulti (`vijVar()`, `vijOneColorOfPalette()`, `vijFormatCount()`, `lastColor` of
`vijColorScale()`). Helpers tied to vijPlots' own features go to `R/utilsPlots.R` (tests in
`test-utilsPlots.R`): `vijGridLines()`, `vijHideAxis()`, `vijNudgePosition()`. A new helper goes to
`utils.R` if it is generic, to `utilsPlots.R` if it depends on vijPlots-only options or plots.

All prefixed `vij*` for easy grepping: `vijColorScale()`/`vijColorPalette()`/`vijColorPaletteNlevels()`
build palette scales from the `colorPalette` option; `vijTitlesAndLabels()`/`vijTitleAndLabelFormat()`
build `labs()` and axis/legend theme from the title/subtitle/caption/axis options every module
exposes; `vijHelpMessage()`/`vijErrorMessage()`/`vijWarningMessage()`/`vijDebugMessage()` write to
`self$results$todo`; `vijDebugPlot(self, plot)` — called at the end of every `.plot()` — forces an
eager build to surface any ggplot2 warning/message as a Notice instead of letting jmvcore silently
swallow it (see gotcha below). `vijNudgePosition(position, x)` returns a ggplot2 position (e.g.
`position_jitter()`, `position_dodge()`) followed by a horizontal nudge `x` (see "Module size").

### Data helpers (`R/utilsData.R`)

Data preparation/tabulation shared by several analyses (unlike `utils.R`, which is
presentation-only and shared with vijMulti): `vijFrequencyTable(self, plotData, inner, outer,
facet, nMissing)` fills the optional frequency table (`self$results$frequencies`) of
`barplot`/`piechart` from the aggregated plot data. It takes `self` because jmvcore's `.()`
translation function looks `self` up in its caller. When weighted counts are decimal, it switches
the `counts` column (declared `integer` in the `.r.yaml`, which would round them) to `number` by
setting the column's private `.type` field (`table$columns[['counts']]$.__enclos_env__$private$.type`):
this is deliberate, the approach of `jmv::descriptives` for its frequency tables, as jmvcore has no
setter for a column type — not a mistake to "fix" (vijMulti's CA builds its columns in `.run()`
instead, with the type chosen at creation). `vijWeightedData(self, varNames, countsName, ignoreNA)` builds the
weighted data of `barplot`/`piechart`/`mosaic` (`.COUNTS` from the `countsName` variable, else jamovi's
weights, else 1; missing/negative/infinite weights rejected on every row, as in
`jmv::conttables`; missing values left out when `ignoreNA`, counted in `nMissing`; zero-weight
rows dropped) and returns `list(data, nMissing)`, or `NULL` when no row is left;
`vijWeightsMessage(self, countsName)` shows the "data is weighted" notice from `.init()`.
`vijOneHotEncoding(aCol, separator, na, init)` turns the multi-valued variable of `mrfrequencies`/
`mrcrosstabs` ("onevar" mode, e.g. `"a;b"`) into one 0/1 column per distinct response, one row per
case (a missing or separator-only value gives an `NA` row when `na`), or returns just the distinct
responses when `init` (for the tables' `.init()`); it takes no `self`. It fills the data.frame column
by column on purpose: the former nested `vapply()` came out as a plain vector, not a one-row matrix,
when the data had a single row (fixed 2026-10-24). `vijLumpLevels(f, w, method, prop, otherLevel)`
lumps the small categories of `piechart`/`barplot` (see "combining small categories" below) and returns the
factor, or `NULL` when nothing is lumped; it takes no `self` (`otherLevel` is translated by the caller).
`vijDropMissing(data, names)` leaves out the rows with a missing value in one of the columns `names` (`NULL`
names skipped: data unchanged), keeping row names and custom column attributes (see the gotcha on
row-subsetting below); used by `boxplot`, `barchart`, `raincloud`, `linechart`, `areachart`, `scatterplot`.
Written as successive calls (values, then groups under `if (ignoreNA)`), never with an `if` inside the
arguments (user's preference, 2026-11-24). Missing *numeric* values are left out in `.run()`, not by
ggplot2 (whose "Removed rows" warnings `vijDebugPlot()` shows): `naOmit()` in `histogram`/`qqplot`/
`lollipop`, `vijDropMissing()` in `barchart`/`scatterplot`/`raincloud`/`areachart`
(one variable; several: `values_drop_na` when pivoting in `.plot()`, the row may hold other values).
Exceptions: `boxplot` keeps them (several variables per row; `na.rm = TRUE` layers), `linechart` too (a
gap in the line). Since 2026-11-24, `raincloud`'s jittered points move sideways when the variable has
missing values (the seeded jitter no longer draws for the NA rows); areas unchanged.
`vijToNumeric(x)` = `jmvcore::toNumeric()` with `±Inf` turned into `NA`, used for every numeric variable
of the 9 analyses above (2026-11-24), so infinite values follow the missing-value handling: same plots
as with `NA` (checked on 10 cases). jamovi can produce them (a computed `BOXCOX(A, -1)` gives `-Inf` where
`A = 0`; division by zero, `LN(0)`, `EXP(1000)` give a missing value), and before, `histogram` drew no bins
(binwidth from `range()`), `qqplot` failed, `lollipop`'s mean was `-Inf`, `linechart` plunged to the
panel's edge. `na.rm = TRUE` is no substitute: geoms with no stat keep `±Inf` (drawn at the edge), and our
own `mean()`/`sd()`/`range()` give `±Inf`/`NaN`. jmvcore's `rejectInf` (default TRUE for `Variables`, FALSE
for `Variable`) never fires in jamovi: `Analysis$init()` checks the data then, but jamovi only gives it the
header (empty columns), and `.run()` reads the rows unchecked; it fires only on an R call with `data =`
(so tests can't put `±Inf` in `boxplot`/`linechart` `vars`). Weights keep their own error on infinite values. Options
are passed explicitly (user's preference), `self` is only used for `self$data`, errors and `.()`.

### Non-obvious runtime gotchas

- Keep validation and expensive/one-off computation (parameter estimation, model fitting, data
  compatibility checks) in `.run()`, not `.plot()`. `.run()` executes once per actual analysis run
  and its result is cached via `image$setState(...)`, but `.plot()` can be invoked multiple times
  per run (resizing, re-render) — anything placed there re-executes on every call. `qqplot.b.R` was
  refactored (2026-08-22) to move its distribution-compatibility checks and MLE/MME parameter
  estimation from `.plot()` into `.run()`, storing the computed `params` alongside `data` in the
  state list (`image$setState(list(data = data, params = params))`) so `.plot()` just reads them
  back — the pattern to follow for any analysis that fits/estimates something before plotting.
  `boxplot.b.R` was similarly refactored (2026-08-25) to move its outlier detection (the
  `private$.isOutlier()` loop building the `.outliers_<var>` columns, needed only when a `label`
  variable is set) from `.plot()` into `.run()`, appending those columns to `data` before
  `image$setState(data)` so `.plot()` just reads them back from `plotData` — same rationale as
  `qqplot.b.R`, plus it means every `.plot()` re-render no longer redoes the outlier pass.
- What goes into `image$setState()` is saved into the user's `.omv`, once per analysis, as
  jamovi's docs say (tuts0301-image-state-performance). Checked 2026-11-08 by decoding the committed
  `data/vijPlots.omv` (protobuf `state` field of each results element, gzip-`memCompress`ed RDS): 18
  states, e.g. `barplot` 6 rows, `piechart` 10, `barchart` 10 and 5, `lollipop` 5, `mosaic` 10, vs
  148 rows for `histogram`. A file can also hold none: on jamovi 28.17, an analysis loaded from a
  file loses its state and gets one again only once it is re-run in the session. Seen 2026-11-08:
  the demo saved at 09:10 had 18 states; reopened, given the decimal point (plots redrawn) and
  saved at 09:21, it had 0; a copy where the user re-ran the first 3 analyses had exactly those 3.
  jamovi 28.17's server drops any state read from a file (`Analysis.drop_state()`, a
  security measure: R would restore arbitrary code), so on jamovi 28 a reopened analysis re-runs
  `.run()` before anything needs the state; jamovi 2.7.41's server has no such step. Don't
  conclude "no state in the file" from small `analysis` entries: compressed states are small
  (a few hundred bytes for the summary plots). The summary-shaped plots (`barchart`, `barplot`, `piechart`, `lollipop`) therefore aggregate in
  `.run()` and store one row per bar/slice/lollipop, not one row per case (`lollipop`'s "Values"
  mode excepted: it draws each value). The plots that genuinely need the raw rows (`histogram`,
  `scatterplot`, `boxplot`, `raincloud`, `qqplot`) deliberately keep the selected columns in the
  state: this is the same approach as jamovi's own jmvPlots module (its `jmvhist.b.R`, `scat.b.R`
  and `jmvbox.b.R` all `image$setState(df)` the selected rows in `.run()`, with no `requiresData`
  on the Image — checked 2026-10-13). An audit suggesting to switch them to `requiresData: true`
  + reading `self$data` in `.plot()` is a file-size vs redraw-time trade-off already decided, not
  a defect — don't "fix" it.
- Row-subsetting a data.frame with base `[` (`df <- df[keep, ]`, e.g. to drop `NA`-group rows)
  silently drops each column's *custom* attributes — `levels`/`class` on a factor survive, but an
  attribute like the `"values"` numeric-coding attribute jmvcore attaches to labelled factor
  columns does not (confirmed this isn't data.frame-specific: plain vector subsetting, `f[keep]`,
  drops it too). `jmvcore::toNumeric()` reads that `"values"` attribute to convert such a factor to
  its underlying numeric codes, silently falling back to returning the (non-numeric) factor
  unchanged when it's missing — which then surfaces downstream as a confusing
  `stats::median()`/`wilcox.test()` error ("need numeric data" / "'x' must be numeric"), not an
  error at the subsetting line itself. Bit `likertplot.b.R`'s `ignoreNA` handling (2026-08-23) when
  its NA-row filter was moved from `.plot()` into `.run()`, ahead of the Mann-Whitney/Kruskal-Wallis/
  pairwise tests that call `toNumeric()` on the filtered columns — fixed by switching to
  `dplyr::filter()`. But `dplyr::filter()` has the opposite trap (checked 2026-11-24, dplyr 1.2.1):
  it keeps custom column attributes, yet replaces *integer* row names by `1..n` (character ones
  survive). Row names are jamovi's spreadsheet row numbers: `jmvcore::select()` keeps them, and jmv
  uses them (`descriptives`' extreme values, `setRowNums()` of output variables), so a future
  "row number" label would be wrong after `dplyr::filter()`. To drop missing values on some columns,
  use `vijDropMissing()` (`utilsData.R`): base `[` (keeps row names) then each column's attributes
  restored, exactly what `jmvcore::naOmit()` does on all columns (a test checks they're identical).
  `naOmit()` also adds `na.action`/`nRowsOmitted` to the data.frame: read by nothing in jmv/jmvcore/
  vijPlots, so `vijDropMissing()` doesn't (user's decision, 2026-11-24).
- The generated `<name>(...)` wrapper's `for (v in group) ...` / `for (v in facet) ...` lines are
  **not** guarded by `missing()`. Calling it with `data=` supplied directly (as tests do) and
  omitting `group`/`facet` throws `"argument is missing, with no default"`. Always pass them
  explicitly (even `NULL`) in that calling style.
- `jmvcore::select(df, character(0))` errors on the currently installed `jmvcore`, reproducible
  standalone outside vijPlots entirely. This means any analysis run with zero required variables
  selected crashes inside the base `Analysis$init()`, *before* the module's own
  `if (is.null(self$options$<var>)) return(FALSE)` guard in `.run()` is ever reached. Don't write
  "no variable selected" tests that go through a full `analysis$run()`/wrapper call.
- A separate, unrelated gotcha bites when a `.a.yaml` option has no `default:` key:
  `jmvtools::prepare()` then generates that argument with **no default value** in the wrapper
  function signature (not just missing a `missing()` guard). Omitting it in a direct `data=` call
  throws `"argument \"<name>\" is missing, with no default"` as soon as the wrapper builds
  `<name>Options$new(...)`. This bit `piechart`'s `labType`, `multcorresp`'s
  `discrimSubtitleText`/`discrimCaptionText`, `principal`'s `screeSubtitleText`, and the `mode`
  option in `areachart`/`corresp`/`mrcrosstabs`/`mrfrequencies` — all now fixed with an explicit
  `default:`, and as of 2026-08-10 a full scan of every `jamovi/*.a.yaml` found no more `List`/
  `Bool`/`Number`/`Integer`/`String` option missing one. Still worth checking a new/edited
  analysis's generated `<name>.h.R` signature for arguments without a default before writing its
  test file (and passing those explicitly), since nothing prevents a future option from
  reintroducing this.
- `jmvcore::Analysis$.render()` always wraps the real plot print in
  `suppressWarnings(suppressMessages(print(result)))` — any ggplot2 warning/message raised while
  rendering through the normal jamovi/testthat pipeline is silently swallowed, including the
  `cli_inform()`-based "Ignoring unknown labels"/"Scale ... already present" diagnostics (these are
  `message`-class conditions, not `warning`-class, despite reading like warnings — a `warning=`
  handler alone won't catch them). `vijDebugPlot(self, plot)` forces an eager
  `ggplot2::ggplot_build()` to catch and surface them as Notices in the results pane instead — but
  only when `DESCRIPTION`'s `Version` has a 4th component (e.g. `1.3.0.0`); it's a no-op on the
  plain `x.y.z` release version. Bump to a 4-part version locally to see these during testing, and
  use a 3-part version before publishing. `jamovi/0000.yaml`'s own `version:` field is a separate
  number and must always stay strict 3-part (jamovi module registry requirement) — never give it a
  4th component.
- `self$options[['decSymbol']]` (read by 7 analyses for `scales::label_percent(decimal.mark = ...)`)
  is **not** a module option and must not be added to any `.a.yaml`: it is an active binding of
  `jmvcore::Options`, like `theme`/`palette`/`ppi`, fed by jamovi's global decimal-symbol
  preference. It defaults to `"."` and is never `NULL` (checked on jmvcore 2.7.38) — reviewers
  keep flagging it as an undefined option, it is not. Its one real limitation is jmvcore-side:
  `Options$compProtoBuf()` only tracks `theme`, `palette` and declared options, so a change of
  decimal symbol is never reported as an option change — an already-rendered plot is not redrawn
  until another option changes, and listing `decSymbol` in a `clearWith:` has no effect (tried on
  `mrfrequencies`/`mrcrosstabs`, 2026-10-13).
  Keep passing `decimal.mark = self$options[['decSymbol']]` explicitly to the label formats
  (`scales::label_percent()`/`label_number()`, `vijFormatCount()`), even though it looks
  redundant: while rendering a plot, `jmvcore::Analysis$.createImage()` sets R's `OutDec` to the
  decimal symbol, but only jmvcore 28.x (jamovi 28) also sets `scales.decimal.mark`, which
  `scales` functions read when `decimal.mark` is `NULL`; jmvcore 2.7.35 (jamovi 2.7.37, "solid")
  sets `OutDec` only (checked 2026-10-23). So on jamovi 2.7.x, `scales` labels without an explicit
  `decimal.mark` would show a `.` — whereas numbers converted by `as.character()`/`format()`/
  `formatC()` at render time (e.g. `barchart`'s `round()` labels) follow `OutDec` on both versions.
  Neither applies to text formatted in `.run()` (outside rendering). **To revisit once jamovi 28
  is the "solid" version** (expected when jamovi 29 becomes "current"): the explicit arguments
  could then go, if 2.7.x is no longer supported.
- `vijTitlesAndLabels()` takes an optional `plot` argument: when supplied, it inspects
  `plot$mapping`/`layer$mapping` (minus each layer's `aes_params`, which shadow an inherited
  mapping — e.g. `geom_histogram(fill = fixedColor)`) to decide which of `fill`/`colour`/`shape`
  are actually used, so it only sets a `labs()` legend title for aesthetics really present in the
  plot. Skipping this (or passing no `plot`) risks ggplot2's "Ignoring unknown labels" diagnostic.
  Don't use `ggplot2::get_labs()` for this check — it calls a full `ggplot2::ggplot_build()`
  internally (~300x slower in informal benchmarking), doubling the real render cost on every plot.
- Default titles given to `vijTitlesAndLabels()` may be symbols: most `.plot()` functions build
  `defaults <- list(x = ..., y = ..., legend = ...)` from the same `rlang::sym()`/`vijVar()` objects
  they use in `aes()`. `vijTitlesAndLabels()` converts any symbol default to a string on entry
  (fixed 2026-10-10) — keep that conversion, and never pass a symbol straight to `ggplot2::labs()`
  elsewhere: ggplot2 renders a symbol label as a plotmath expression, not as plain text. Two
  consequences were confirmed before the fix: a variable named after a plotmath keyword was
  displayed as the Greek letter (`alpha`, `beta`, `Delta` -> α, β, Δ in axis/legend titles), and
  an expression title is taller than the same text (legend title "Group": 18.35 pt vs 15.86 pt),
  which is why the fix moved 94 visual snapshots by about a pixel (same text, same elements, same
  colours — only positions, and svglite writing one `<text>` per title instead of one per
  character).
- **Index columns with a string, never with a symbol** (done 2026-11-24): `plotData[[self$options$x]]`,
  or a `xName` string kept next to its `x` symbol when the name depends on a branch or is used often
  (`areachart`'s `timeVarName`, `mosaic`'s `categoryName`/`groupName`). `plotData[[sym]]` works (`[[`
  accepts a symbol on a data.frame, a tibble and a list; only `df[sym]` errors), but reviewers kept
  flagging it as an error. The 18 occurrences of `.plot()` (areachart, linechart, boxplot, histogram,
  mosaic, scatterplot; barplot had none left) were replaced with no snapshot change; the lines no test
  ran (`boxplot`'s group sorting, `mosaic`'s facets with "Category %") gave identical SVGs by hand.
- When forcing legend order via `guides(aes = guide_legend(order = N))` on a plot with multiple
  legends, set an explicit `order` on *every* merged aesthetic (e.g. `color`/`fill`/`shape` all
  mapped to the same grouping variable), not just the one you care about positioning — an
  aesthetic left at the default `order = 0` sorts by an internal hash rather than code order, so
  its position relative to an explicitly-ordered one is not deterministic across different
  grouping variables/data (seen in `scatterplot.b.R`'s size-vs-group legend ordering).
- `Table$setNote(key, NULL)` (or `""`) does **not** reliably clear a previously-shown note on the
  jamovi client (confirmed live on jamovi 28.2/jmvcore 2.7.38) unless the table's `rows:` in its
  `.r.yaml` is a non-zero value — a `rows: 0` (fully dynamic) table keeps showing a stale note from
  the last successful run even though its rows/columns correctly reset to empty.
  **Update (2026-10-17): no longer reproduced on jamovi 28.13/jmvcore 28.3** — after a
  `reject()`-interrupted run (weights with a negative value), `barplot`/`piechart`'s `rows: 0`
  frequency table was emptied and its `'missing'` note cleared. The exact fixing version is
  unknown. The `corresp` `rows: 1` + `deleteRows()` workaround is left as is (legacy analysis, see
  Overview). This was root-caused
  and fixed for `corresp`'s `eigenvalues`/`chisq` note (`rows: 1` + an explicit `deleteRows()` before
  repopulating, to work around the phantom placeholder row `rows: 1` itself introduces — see
  `corresp.b.R`'s `.run()`). Note this is specifically a `reject()`-interrupted-run problem, not a
  general one: an `if (...) setNote(key, text)` with no `else setNote(key, NULL)` branch is safe on a
  normal *successful* run — confirmed live that toggling an option so the guard turns false (e.g.
  `principal`'s rotation option going from `Varimax` back to `none`) correctly clears the note,
  presumably via `clearWith` doing its usual client-side job when the run actually completes. The
  failure mode only shows up when that same option change *also* causes an unrelated validation
  `reject()` before `.run()` reaches the `setNote()` line — then the note is left showing the stale
  pre-change text, same as the `chisq` case. `principal.b.R`'s unguarded notes (`summaryTable`/
  `loadingTable`/`obsTable`'s `'rot'`, `loadingTable`/`obsTable`'s `'norm'`, `obsTable`'s `"100"`) are
  left unfixed for now on that basis — the overlap needed to actually trigger it is narrow, and
  fixing it would need the same `rows: N` + `deleteRows()` treatment as `chisq`, which isn't
  worthwhile pre-emptively without a concrete repro. More fundamentally, `rows: N` + `deleteRows()`
  is a per-table workaround for what is really a jmvcore/jamovi-side bug (`setNote(key, NULL)` not
  reliably clearing a `rows: 0` table's note) — it was applied to `corresp` because that one had a
  concrete repro, not adopted as the standard pattern to sprinkle across every `rows: 0` table with
  an unguarded note. The proper fix belongs upstream in jmvcore/jamovi, not module-by-module here.
  `multcorresp.b.R` has the same unfixed pattern, on `eigenvalues`'s `'method'`/`'adjusted'` notes,
  `discrim`'s `'sup'` note, `categories`'s `'normalization'`/`'sup'` notes, and `observations`'s
  `"100"`/`'normalization'` notes — all set after `.run()`'s early reject points (no data, MCA
  failed, `dimNum > res$nd.max`), all on `rows: 0` tables. Same call: not worth fixing pre-emptively.
- Never build a formula by pasting/`reformulate()`-ing a raw jamovi variable name into text: any
  name containing a space or other non-syntactic character (`!`, `+`, `?`, `%`, `-`, and even a
  literal backtick) breaks `stats::reformulate()`/`as.formula()` at parse time, since neither
  backtick-quotes automatically. This crashed `mosaic.b.R`'s `.mosaicData()` (fixed 2026-08-31):
  `stats::xtabs(stats::reformulate(c(categoryName, groupName), response = "freq"), data = subDf)`
  raised a raw, uncaught `str2lang()` parse error whenever the category/group variable name had a
  space. Fixed by building the formula with `jmvcore::composeFormula(lhs, rhs)` instead (it
  backtick-escapes each component correctly, including a literal backtick in the name) — the same
  pattern already used in `corresp.b.R` (`jmvcore::composeFormula('.COUNTS', c(rowVarName,
  colVarName))`) — so prefer that over ad hoc `rlang::expr(lhs ~ !!sym1 + !!sym2)`-style
  workarounds when a formula needs to be built from raw jamovi variable names. Swept all 18
  analyses afterward with a battery of ad hoc (non-testthat) scripts using variable names with
  spaces/`!+?%-`/backticks and with non-ASCII characters (accented Latin, Chinese, Japanese) — all
  passed, so this was an isolated case, not a systemic issue.
- **Name conflicts with the user's variables** (audit of 2026-11-24). Two kinds, both reproduced and fixed:
  an internal column named like a user variable (qqplot stored the group under the variable's own name,
  so a group `x`/`y`/`lower`/`upper` overwrote its point/band columns: now `.GROUP`), and a bare R local
  inside a data-masking call (`mutate()`/`filter()`/`summarise()`, or `aes()` outside `after_stat()`),
  shadowed by a column of the same name: boxplot's `private`/`whiskerCoef`, mosaic's `self`, likertplot's
  `ques`/`group` (answer levels become columns), `vijFrequencyTable()`'s `facetLevel`/`outerLevel`. Inject
  locals with `!!` (or compute outside the mask); internal columns get a dotted upper-case name. Safe as
  is: the index of `.data[[x]]`, `pick(all_of(x))`, function calls (R skips non-function bindings) and
  `after_stat()` (computed data). Each case has a test renaming a variable to the conflicting name.
  A dotted internal name that *does* conflict with a selected variable (`.COUNTS`, `.X`, `.variable`/`.value`,
  histogram's `.value`/`.label` with a mean/median line...) is rejected in `.run()` by
  `vijCheckReservedNames(self, varNames, reserved)` (`utilsData.R`, used by 8 analyses): an error, not a
  warning (the plot would be wrong), and only for the names that really conflict, not every name starting
  with a dot (user's decision, 2026-11-24: e.g. qqplot's `.GROUP` works as a variable name, rejecting it
  would break documents for nothing).
- Missing values kept as a category ("Ignore missing values/groups" unchecked) are labelled
  `"(Missing)"` (translated, fr "(Manquant)"), not ggplot2's default `"NA"` (changed 2026-10-21, in
  all 9 analyses that can keep them: `barplot`, `piechart`, `mosaic`, `boxplot`, `barchart`,
  `raincloud`, `linechart`, `areachart`, `scatterplot`, plus `likertplot`). `vijLabelNA(self)`
  (`utils.R`) returns the labelling function; each `.plot()` creates it once (`labelNA <-
  vijLabelNA(self)`) and passes it to every place a level is shown: `vijColorScale(..., labels =
  labelNA)` (new `labels` argument, default `waiver()`), `scale_x_discrete(labels = )`, other
  discrete scales (`scale_shape_manual`, `scale_linetype_discrete`), `facet_wrap(labeller =
  ggplot2::as_labeller(labelNA))`, hand-built axes (`mosaic`) and text labels (`piechart`'s
  "Group" labels); `vijFrequencyTable()` uses it too. The real `NA` stays in the data: a text level
  would lose the gray `na.value`, and anything converted in `.run()` is saved in the `.omv` in the
  language of the moment. When an aesthetic is mapped to the group on several merged scales
  (`raincloud`: fill/colour/slab_color; `scatterplot`: colour/fill/shape), give all of them the
  same `labels`, or the legend splits. Exception: `likertplot` turns a missing *group* into a real
  text level in `.run()` (it is a group of its own in the frequency table and the comparison
  tests), now `vijLabelNA(self)(NA)` instead of `"NA"` — so its label is saved in the `.omv` in the
  run's language (accepted: harmless, the group never drives the colour). Fixed along the way:
  `linechart` with several variables drew no line at all for a missing group (ggplot2's NA
  linetype is blank): now `na.value = "dotted"`, and `addNA()` in its `interaction()` grouping so
  the variables' NA lines aren't merged into one. "(Missing)" is translated in `fr.po` ("(Manquant)").
- Percent labels and axes use a **thin space** (U+2009) before `%`, not a regular space:
  `scales::label_percent(suffix = "\u2009%", ...)` — the source files hold the character itself — in every analysis
  that formats percentages (`areachart`, `barplot`, `mosaic`, `piechart`, `mrfrequencies`,
  `mrcrosstabs`, plus the legacy ones). It looks like a plain space in an editor: keep the same
  character when writing or moving such code (a find-and-replace on `' %'` with a regular space
  won't match), and normalize it in tests that compare label text
  (`gsub("\u2009", " ", label)`, as in `test-piechart.R`).
- A plot Image with an explicit `clearWith:` list in its `.r.yaml` (currently `mrfrequencies`,
  `mrcrosstabs`, `likertplot`, and the legacy `principal`, `corresp`, `multcorresp`; `qqplot` and
  `piechart`'s lists are on their tables only) is redrawn only when one of the listed options changes. When adding an option
  that affects such a plot, add it to that list too, or toggling it in jamovi leaves the plot
  unchanged. Tests don't catch this: they render once with the option already set, so the snapshot
  looks right (bit the `xGridLines`/`yGridLines` options, 2026-10-20). Images without `clearWith:`
  (all the other analyses) are redrawn on any option change.
- In a `.u.yaml` `enable:`, a Variable option (`columns`, `facet`, `group`...) can't be tested
  positively: `(columns)` or `columns && x` doesn't work (jamovi bug, all analyses). Write
  `!(!columns)`; the negative `!columns` works.
- `barchart`'s observations (`showPoints`, added 2026-10-28) draw the raw values over the bars.
  `.run()` keeps them in the state (`points`: `.X`, group, facet, `.PY`) only when `.showPoints()`
  allows it (mean or median, not stacked with a group); the points layer comes right after
  `geom_col`, under the labels and the error bars. Both `barType`s use
  `private$.positionJitterDodge(single)`, a ggproto `Position` that works out each bar's slot from the
  bars themselves, not from the points ("dodge2": 0.9 / largest number of bars at any x, bars centred;
  "dodge": 0.9 / number of bars at this x), and jitters each bar's points with a seed of its own.
  **Don't use `ggplot2::PositionDodge2`/`position_dodge2()`, nor `ggpp::position_dodge2nudge()` (which
  extends it), for points:** they treat every point as an element to dodge, so with unequal counts
  per bar the slots shrink and the points spread over the whole category. The first version did
  exactly that and passed its tests, which had the same number of points in every bar: the tests now
  use very unequal counts (`penguins`, `ignoreNA = FALSE`), keep it so. **Nor
  `ggplot2::position_jitterdodge()`:** before ggplot2 4.0.0 (its NEWS: "now dodges by `group`") it
  needs a fill/colour aesthetic to dodge by and fails with only `group` ("requires at least one
  aesthetic to dodge by", reproduced on 3.5.2); `minApp: 2.7.1` in `jamovi/0000.yaml` means jamovi 2.7
  users run ggplot2 3.5. It was used in the first versions, and doesn't follow the bars in an incomplete
  category either. The position was tested on ggplot2 3.5.2 in isolation, not the whole module (the
  other installed packages don't load with it); a test checks the layer's position class.
- Random draws in `barchart` (bootstrap CI, jitter) use `withr::with_seed(strtoi(substr(rlang::hash(x),
  1, 7), 16L), ...)` rather than `set.seed()`: same values, and the global RNG is left alone.
  `withr` is in `Imports` for that (it was already installed through ggplot2).
  These seeds depend on the installed `rlang`: 1.3.0 changed every `rlang::hash()` value (its NEWS).
- The 4 snapshots using those draws (`barchart-errorbar-bootstrap`, `barchart-points-dodge2`,
  `barchart-points-dodge-horizontal`, `barchart-points-options`) therefore differ between `rlang`
  versions, whereas their text and bars don't move (bootstrap: by about 0.01 px). They were re-recorded
  on 2026-11-02 with `rlang` 1.3.0, the laptop being on 1.2.0 and the desktop on 1.3.0 (R 4.6.0 and
  4.6.1, not the cause according to that NEWS; `jmvcore` 28.3 on both). If they fail again for no
  apparent reason, compare `packageVersion("rlang")` and `rlang::hash(c(1.5, 2.5))` on both machines
  before accepting anything, not the plot code. An earlier failure of the bootstrap snapshot
  (2026-10-28) was attributed to `jmvcore`, probably wrongly.

### Dependency graph has soft/transitive requirements

Several `Imports:` in `DESCRIPTION` aren't called directly (`pkg::fun()`) anywhere in `R/` — they're
required at runtime by another dependency and would break at render time if removed:
`fitdistrplus` (used internally by `ggh4x::stat_theodensity`, called from `histogram.b.R`) and
`labelled` (an optional dependency of `ggstats::gglikert_data()`, checked via
`rlang::check_installed("labelled")`, called from `likertplot.b.R`). Before removing an
Import that looks unused by a plain grep, check whether the package that *is* called depends on it.
(`GPArotation` used to be in this list — `principal.b.R` called it indirectly via
`getFromNamespace()` — but as of 2026-08-23 it's called directly as `GPArotation::Varimax()` etc.
in a `switch()`, so it's grep-visible like any normal import.)

`Hmisc` was dropped (2026-10-04): `ggplot2::mean_sdl()`/`mean_cl_normal()`/`mean_cl_boot()` all
call `Hmisc::smean.*()` under the hood, so `barchart.b.R`'s error bars now use its own
`private$.meanSd`/`.meanClNormal`/`.meanClBoot` (checked identical to the Hmisc versions) plus
`ggplot2::mean_se()` (Hmisc-free). Don't reintroduce `ggplot2::mean_cl_*`/`mean_sdl`/`median_hilow`
as `stat_summary(fun.data = ...)` anywhere without re-adding `Hmisc` to `Imports`. Likewise
`colorspace` was dropped (2026-10-04) in favour of `scales::col_mix()` (needs `scales >= 1.4.0`)
in `lollipop.b.R`. It was still installed transitively via `FactoMineR` (→ car → pbkrtest → doBy →
forecast) until `FactoMineR` was dropped (2026-11-26); no remaining Import pulls it in.

### Module size (findings of 2026-11-21; `ggpp`, `qqplotr` and `FactoMineR` dropped)

The 1.4.1 `.jmo` is 26.3 MiB compressed, 80 % bundled packages (jamovi's own `jmv` already provides
`ca`, `psych`, `GPArotation`, `car`, `emmeans`: those aren't bundled). Main groups: `FactoMineR` and
its closure (`DT`, `crosstalk`...) 5.8 MiB, legacy CA/MCA only, now dropped; `qqplotr` 6.2 MiB, of which 4.5 MiB
is `opdisDownsampling` and its closure (`pracma`, `foreach`...), called only with `down.sample`, which
`qqplot` never passes (`qqconf`: "ell" bands, `robustbase`: "ts" bands, both needed), now dropped; `labelled` 2.8
MiB (`haven`, `readr`, `vroom`, `tzdb`), needed unconditionally by `ggstats::gglikert_data()`; `ggpp`
2.2 MiB (with `xts`, `lubridate`, `timechange`, `polynom`), now dropped; `ggforce` 1.2 MiB, legacy PCA only.
- **`ggpp` dropped (2026-11-21):** its positions were only a ggplot2 position plus a horizontal nudge.
  `raincloud`'s `position_jitternudge(nudge.from = "jittered")` and `boxplot`'s `position_dodgenudge()`
  (outlier labels, several variables + group) are now `vijNudgePosition(ggplot2::position_jitter(...) /
  position_dodge(...), x)`: a child of the given position whose `compute_layer()` calls the parent's
  (`ggproto_parent()`) then adds `x` (`transform_position()`); `force(x)` reads the nudge when the
  position is created, not at rendering. `layer_data()` identical to ggpp on ggplot2 4.x and 3.5.2
  (ggpp 0.6.0 and 0.5.8-1), no snapshot moved (`boxplot-severalVars-Groups-labels` was recorded with
  ggpp first). The legacy PCA's `position_nudge_center()` (radial nudge from the origin) became
  ggrepel's per-label `nudge_x = 0.2 * x / r`, `nudge_y = 0.01 * y / r` (`r` floored at 1e-5): same
  positions, except that a variable exactly at the origin is no longer nudged. Checked visually by the
  user in jamovi 28 (2026-11-21). The rebuilt `.jmo` went from 26.58 to 24.37 MiB.
- **Not done (user's decisions, 2026-11-21, confirmed 2026-11-26):** porting the legacy CA/MCA to `ca`
  like vijMulti, or copying vijMulti's analyses (results differ: axis signs, inertia table, Burt
  observation coordinates, and the MCA coordinates saved into the data set; old documents re-run on
  jamovi 28, so they would change silently; the aim is to keep old documents as they were); vendoring the used part of `qqplotr`
  into vijPlots (a 248-line draft matched `qqplotr` exactly on 480 cases, but too much code to maintain
  for the gain; superseded by the lighter replacement below). An upstream fix wouldn't help soon: jamovi freezes package versions per major release
  (2.7 bundles qqplotr 0.0.6, 28 bundles 0.0.7).
- **`qqplotr` replaced (2026-11-22; user's decision):** no ggproto, no `qqplotr` in `Imports`. `.run()`
  computes each group's points, reference line and band (`private$.qqData()`/`.qqGroup()`/`.distFun()`:
  pure, adapted from qqplotr 0.0.7's stats, ELL band from `qqconf::get_qq_band()`, "ts" with
  `robustbase`) through `.qqPlotData()` (reads the options, `withr::with_seed(42, ...)` for the "boot"/"ts"
  draws), and stores `list(qq = ...)`; `.plot()` draws it with `geom_ribbon()`/`geom_path(linewidth =
  0.8)`/`geom_point()`, the P-P reference line with one `annotate("segment")`. Same data as qqplotr
  (≤ 2e-15); checked by the user in jamovi 28 and, before qqplotr was removed, by a test comparing both
  SVGs in 76 cases (group, detrend, Q-Q line, every band, 6 distributions). Only differences: the line's
  legend key drawn at 0.8 (qqplotr: the theme's width), `stroke-linejoin` in the band's key, and the P-P
  reference line drawn once instead of once per point (`qqplot-pp`, `qqplot-pp-band*`,
  `qqplot-band-grouped-qqline` were re-recorded for that, identical once these are normalised). **Old
  states:** a state without `qq` (`list(data, params)`, vijPlots <= 1.4.1, restored by jamovi 2.7.41, which
  has no `drop_state()`) is recomputed in `.plot()` by `.qqPlotData()` (tested). A note warns when "ts" is
  used with a non-Normal distribution (qqplotr computed it silently). `qqplotr` stays cited (`refs:`, help
  message), with `qqconf` (`00refs.yaml`: its JSS article). The rebuilt `.jmo` went from 24.37 to 19.43
  MiB (`opdisDownsampling` and its closure gone). Tests with jamovi 2.7.41's R: only the known
  differences listed under "Building and testing for jamovi 2.7".
- **`FactoMineR` replaced (2026-11-26; user's decision):** `R/legacyFactoMineR.R` holds FactoMineR 2.14's
  code (GPL >= 2, credited), reduced to what the legacy `corresp`/`multcorresp` call: `vijLegacyCA()`,
  `vijLegacyMCA()`, `vijLegacySvd()` (`svd.triplet()`), `vijLegacyDisjunctive()` (`tab.disjonctif()`,
  whose C code is rewritten in base R). Goal: old documents give exactly the same results, bugs
  included, so don't "fix" or modernise it. Besides removed branches, only rewrites of equal result
  (`fct.eta2()` without its NA filter, `tryCatch()` for `tryCatch.W.E()`, elements renamed by name);
  one theoretical difference: when both `svd(X)` and `svd(t(X))` fail, FactoMineR's buggy `eigen()`
  fallback returns `NA`s, the port errors ("Unable to compute..."). Results also no longer hold `svd`,
  `call$X`, `v.test`... (unread; smaller `.omv` states). Checked bit-identical to FactoMineR 2.14 on 27
  cases (CA: supplementary rows/columns, weighted table, null eigenvalue, contTable mode; MCA: Indicator
  and Burt, 0-3 supplementary variables, duplicate and `y`/`n` levels, odd names, n = 4, `poison`, `tea`)
  and to 2.11 (jamovi 2.7) up to 1e-16 on supplementary eta2. Shipped `.jmo`s had 2.14 (jamovi 28) and
  2.11 (jamovi 2.7), but the tests had been recorded with 2.16 (local library), which keeps only `ncp`
  eigenvalues: the two `corresp` inertia tests were corrected to 2.14's full table, and 14 snapshots
  re-recorded (ggrepel labels moved by <= 0.2 px), byte-identical to those of FactoMineR 2.14. Freed
  11.5 MiB uncompressed (`DT`, `FactoMineR`, `crosstalk`, `lazyeval`, `scatterplot3d`, `multcompView`,
  `ellipse`, `leaps`, `flashClust`). The `factominer` reference and the help messages ("based on
  FactoMineR") are kept.
- **Compiler side (jamovi-compiler 0.3.5):** the example `.omv` files are bundled twice (copied to
  `data/` for jamovi, and installed by `R CMD INSTALL`, run without `--no-data`, into `R/vijPlots/data/`:
  every module with datasets, `jmv` included), and `--no-data` is ignored for dependencies installed
  as binaries (macOS/Windows), so `opdisDownsampling/data/` (3.5 MB) ships too. A bug report was
  drafted for the user (not posted by Claude). The compiler also copies the whole
  `build/R*-arm64-macos` folder into the module: delete it before rebuilding after dropping an Import,
  or the old packages stay in the `.jmo`.

### `qqplot`: with a group, everything is computed per group (2026-11-23; user's decision, a bug fix)

With a group, the points, the Q-Q line and every band were computed per group (ggplot2's `compute_group()`
did it for qqplotr, and `.qqData()` loops the same way), but the parameters were estimated, and the data
standardized, once on all the data (vijPlots passed `dparams`; qqplotr alone would estimate per group). Each
group was thus compared to the pooled law, and some bands contradicted the reference line: with the Q-Q line,
the bootstrap/ELL bands stayed on the pooled law while each line followed its group; with the identity line,
the "ts" band followed each group (it always uses the sample's robust centre/scale, `params` ignored).
- **Rule now:** with a group, the data are standardized and the parameters estimated **within each group**;
  only parameters entered by the user are common. No option: an "Estimate for each group" checkbox
  (`paramByGroup`, 2026-11-22) was tried and removed the next day, the user preferring one meaning (the
  superposition of the groups' own plots) announced as a bug fix. Old documents with a group and estimated
  parameters (or standardized data) change when re-run. In `.run()`: `byGroup` (group + estimated) gives
  `params` as a list named by non-empty level; `standardizeByGroup` (group + standardize) uses `stats::ave()`;
  per-group checks (n ≥ 2, non-zero variance, estimation) name the group, so a one-case group now errors
  (not with given parameters and no standardization); one table row per group (`group` column added in
  `.run()`, `combineBelow` on `var`).
- Each group is **exactly the plot of that group alone**: a test compares every group's part of `state$qq`
  with `qqplot()` run on the subset, in 9 cases (including given parameters + standardize). That needs the
  seed per group (`withr::with_seed(42, ...)` around each `.qqGroup()` call in `.qqData()`), so that the
  boot/"ts" draws don't depend on the other groups.
- **vs qqplotr** (checked 2026-11-23, iris, 29 cases, qqplotr 0.0.7 called with the same `dparams`): points
  and every band identical (≤ 4e-15), except boot/"ts" with a group, whose draws differ (seed per group;
  identical to qqplotr run on each group alone with `set.seed(42)`). vijPlots <= 1.4.1 set no seed: its
  boot/"ts" bands changed at every redraw.
- **Bands, discussed and left as qqplotr does them (user's decisions, 2026-11-23):** qqconf only knows the
  identity line (its `qq_conf_plot()` draws no Q-Q line); with the Q-Q line, the bootstrap/ELL bands stay
  around the law of `params`, not the line (small gap now that parameters fit each group). Possible fix not
  applied: `lower/upper <- intercept + slope * lower/upper` for boot/ELL (what pointwise/KS do, and
  `car::qqPlot()`'s envelope); a "Q-Q line" bootstrap (each replicate's own quartile line) would be the
  exact version. The "ts" band uses median/Qn, not `params`: no help sentence added (user's choice). Every
  band but "ts" assumes known parameters (conservative when estimated). qqconf's `"pointwise"` band (exact,
  beta order statistics) differs from qqplotr's (normal approximation, wider in the tails); its `"ks"` is
  the same. References: Loy, Follett & Hofmann (2016), Almeida, Loy & Hofmann (2018), Aldor-Noiman et al.
  (2013).
- **"One plot per group"** (`plotPerGroup`, 2026-11-23): the same state, drawn with `facet_wrap()` on the
  group instead of colors (`colorVar` NULL in `.plot()`: single color, gray band, no legend, palette
  disabled). Facets block copied from `histogram` (`facetBy`, `facetNumber`, label options) plus
  `facetFreeScales` (both axes together: x and y both depend on the group; a manual range keeps its axis
  common). A separate `facet` variable, exclusive of `group`, was tried first and dropped: the crossed
  `enable: (!facet)` / `(!group)` on the two `VariablesListBox` targets had no effect in jamovi, so both
  could be filled.
- `MASS::fitdistr()`'s MLE (logistic, Weibull, gamma) raises "NaNs produced" warnings while `optim()`
  explores invalid values, more often on small groups (pre-existing; whether jamovi shows them not
  checked), avoided in the tests.

### Avoid `do.call()`

Prefer a `switch()` (or direct `pkg::fun()` calls) over `do.call(getFromNamespace(x, "pkg"), args)`
or any other pattern that resolves a function to call by name at runtime. A `switch()` on a small,
explicit set of branches is just as flexible for dispatching on an option value, keeps every
callable function spelled out literally (grep-visible, no need for a defensive whitelist check —
see the "Dependency graph" note above for the `principal.b.R` rotation example this was fixed in),
and avoids `do.call`'s NSE/argument-matching quirks entirely.
Accepted (user's decision, 2026-11-25): `do.call()` on a function *object*, e.g. `qqplot.b.R`'s
`do.call(qFunc, c(list(p = probs), params))` (taken from qqplotr; `qFunc` comes from `.distFun()`'s
`switch()`, parameter names vary by distribution) and `do.call(rbind, list_of_dfs)`. Don't "fix" them.

### Checkbox labels

`histogram`'s "Show bins"/"Show lines"/"Show density" deliberately keep the leading verb (despite
jamovi's "name the thing, not the action" convention, applied in `boxplot`/`barplot`) to match
jmvPlots' histogram, which uses the same labels — don't "fix" them in response to an audit. See
the comment in `jamovi/histogram.a.yaml`.

### Translations (`jamovi/i18n/*.po`)

Regenerated via `jmvtools::i18nUpdate("<lang>")` (see `build.R`), which merges newly-extracted
strings into the existing `.po` file by exact string match — reusing one `msgid`/`msgstr` pair
across every `.a.yaml`/`.u.yaml` location where that exact English string occurs, even across
unrelated options. A new option that reuses a common short title (e.g. `title: Label`) silently
inherits whatever translation that string already has elsewhere, which may be wrong in the new
context (plural vs singular, different meaning) — check `grep -n '^msgid "<string>"$'` and read
every `#:` reference before trusting a shared translation. Long strings also wrap across
continuation lines (`msgstr ""` followed by quoted-string lines) — a naive per-line/per-block
regex check for empty translations will misreport every long string as untranslated; parse
`msgid`/`msgstr` by concatenating all consecutive `"..."` lines that follow the keyword.

### `likertplot`: only the plot is sorted and reversed (user's decision, 2026-11-05)

`sorting` and `reverseLikert` apply to the plot only: the frequency table and the comparison tests
keep the selection order, so `gglikert_data()` in `.run()` takes no `sort`. This undoes the alignment of
the table on the plot (`44a6fdf`, 2026-10-25), on purpose: don't align them again. Sorting in `.run()` put
the adjusted p-values of Mann-Whitney/Kruskal-Wallis on the wrong variable (rows in selection order,
p-values in sorted order), and gave a table/plot mismatch with `reverseLikert`. Rejected as too complex: a
`.reverseLevels` helper; reversing the table columns.

### `likertplot`: integer variables are treated as ordinal factors (user's decision, 2026-11-05)

`ggstats` merges the levels of several variables in order of first appearance: `q1 = 3-5`, `q2 = 1-3` gives
`3 4 5 1 2` (wrong centre too). "Tidy up levels" is the remedy, for every type: integer variables
(`commonClass == "integer"`, jamovi "Continuous, Integer") now go through the "Convert to integer" block
(ordered factor + `"values"` attribute, per variable), so `tidyUp` puts them on one sorted scale, and
`reverseLikert` needs no special case. Without `tidyUp` (unchecked by default), partial levels keep the
first-appearance order, as ordinal factors always did: accepted by the user, not a bug to fix again.
`tests/testthat/test-likertlevels.R` (kept, user's decision; 704 tests, order checks skipped without `tidyUp`) covers
partial, non-continuous, negative and zero-less levels, as integers and as labelled ordinal factors.

Two review findings left as they are (user's decisions, 2026-11-05), not bugs to fix again:
- With "Ignore missing groups", levels answered only by the excluded rows stay in the plot and table (empty),
  for integers as for ordinal factors: the scale belongs to the question, not to the rows kept.
- The Likert palettes (`scale_fill_brewer()`) have 11 colours at most: a 12th level gets no colour. Not
  interpolated with `vijExtendPalette()` as in the other analyses: a Likert scale with more than 11 levels
  makes no sense.

### `likertplot`: post hoc tests cite PMCMRplus only (user's decision, 2026-11-14)

The Dunn/Conover/DSCF code is adapted from PMCMRplus, so `pwTable` has `refs: - pmcmrplus` (entry in
`jamovi/00refs.yaml`). The 2026-11-09 audit also suggested citing the three methods' papers (Dunn
1964, Conover & Iman 1979, Critchlow & Fligner 1991): not done, PMCMRplus' documentation cites them.
A `refs:` on a table is shown only while that table is visible (checked in jamovi 28: only with the
post hoc tests checked). jmvcore's `setRefs()` (also in 2.7.35) could set refs from `.run()` if they
ever need to depend on an option.

### Planned for vijPlots 1.5: Pareto curve and lumping in `barplot` (user's decision, 2026-11-08)

Two independent `barplot` options, not implemented yet (feature freeze of 1.4.0):

- **Cumulative curve (Pareto)**: a cumulative-percentage line over the bars, rather than a new
  analysis (`barplot` already has decreasing sorting, aggregated data via `countsName`, weights and
  facets). **Decided (2026-11-25):** a real Pareto only: the curve needs `order: decreasing` (disabled
  otherwise). A general "cumulative curve" (also over an unsorted ordinal variable) was decided first,
  then dropped the same day after a literature search: a Pareto is sorted by definition (Ishikawa,
  Minitab, JMP, SAS, Excel); the cumulative frequencies of an ordinal variable are standard, but drawn
  in a separate step plot in textbooks, the overlay on bars having no name or rules outside Pareto;
  over an unsorted nominal variable the curve means nothing. Could be widened later if users ask.
  No group variable, not stacked, **no facet** (disabled otherwise). Facets were implemented first, with
  the global order (Minitab's default for a Pareto "By" variable), then removed the same day (user's
  decision): a facet's bars didn't decrease, so its curve wasn't a Pareto (hard to read), and they
  needed exceptions (percent axis only, one N per facet, cumulative sums per facet, a category absent
  from a facet). A per-facet order (Minitab's "independent" option) would be the right way if ever asked.
  **Plan for per-facet Pareto curves (2026-11-25, not done: "later", user's decision).** Prototyped outside
  the module (`mpg`, class by drv, percent axis): each facet becomes a real Pareto, each curve ends at 100%
  (`StatProp` already computes the percentages per facet), a category keeps its colour across facets.
  - `.run()`: an axis key `.X` = (facet, category), levels ordered within each facet by decreasing count,
    "Other" then NA last, ties by the global order; `.CUM` per facet with `stats::ave()`. `.X`/`.CUM` become
    reserved names.
  - `.plot()`: bars, labels and curve map `.X` on x; `facet_wrap(scales = "free_x")`;
    `scale_x_discrete(labels = <named vector key -> category, "(Missing)" for NA>)` with `drop = TRUE`
    (with `drop = FALSE` every facet would show every key).
  - Frequency table: its "Cumulative %" must follow the same per-facet order, or it contradicts the curve:
    `vijFrequencyTable()` (shared with `piechart`) needs a per-facet order. The most delicate part.
  - UI: drop `!facet` from the option's `enable`. Tests: per-facet order, Other/NA last, `.CUM` reaching 1
    per facet, axis labels, stable colours, table aligned on the curve, a snapshot.
  - Decisions to take: (A) what triggers the per-facet order: the Pareto checkbox itself (a "curve" option
    reordering the bars: surprising) or a new sorting choice "Decreasing (within facets)", useful without
    the curve; (B) count mode: percent only with a facet (simple), or free y per facet with a secondary % axis
    per panel via `ggh4x::facetted_pos_scales()` (heights no longer comparable, much more complex: not
    recommended); (C) categories absent from a facet: dropped (today, with a facet, every category shows in
    every facet) or kept at 0 at the end.
  - Limits seen on the prototype: bars of different widths across facets (equal panel widths, different bar
    counts; `facet_grid(space = "free_x")` isn't usable with our `facet_wrap()`, ggh4x's `facet_wrap2()`/
    `force_panelsizes()` would be needed); axis labels overlap sooner; a category is harder to compare
    across facets, as its position changes.
  - Cost: about 40-50 lines plus tests, and a change to `vijFrequencyTable()`. Modest gain: wait for a
    user request.
  **Also decided (2026-11-25):**
  - With `yaxis: percent`, bars and curve share the left axis. With `yaxis: count`, the curve is drawn
    in cumulative counts on the left axis (0 to N, the classic Juran/Minitab layout, bars looking small
    as in the `mpg` prototype: accepted) with a right-hand % axis, `sec_axis(~ . / N)`.
  - No 80% reference line for now.
  - Cumulative sums computed in `.run()` (sorting and lumping are already there), in the
    final bar order: "Other" last, then `NA` ("(Missing)" in `.plot()`, after sorting; `fct_infreq()`
    leaves `NA` out of the levels), so the curve ends on them, as a Pareto should. Only when the
    option is checked. No guard for old states: they come from documents without the option (FALSE),
    and checking it clears the state and re-runs `.run()` (the Image has no `clearWith:`, so any
    option change does); keep that true if a `clearWith:` is ever added.
  - An empty level shown on the axis (unused level retained by jamovi, or all weights/counts 0) has no
    row after `dplyr::count()`: the curve crossed its slot rising, when missing values followed. So
    with the curve, `count(.drop = FALSE)` gives it a 0 row in `.run()`, and `.plot()` keeps the 0 rows
    for the curve only (`ggplot()` gets `plotData[plotData$.COUNTS > 0, ]`, the curve layers `data = plotData`),
    not for the bars and labels: in count mode a 0 row draws a flat bar (visible
    with a border colour) and a "0" label. Chosen over a separate curve data.frame built in `.plot()`
    (`tapply()` over the levels, like the frequency table), as easier to extend to per-facet curves.
  - Drawing: one point per bar at its horizontal centre (height = cumulative %), joined by segments,
    starting at the first bar (Minitab), not from 0 at the left edge. No % labels on the points.
  - Vertical bars only: disabled with `horizontal`.
  - The `pareto` CheckBox is a child of `order_decreasing` (kept there rather than in "Plot Options", user's
    decision): jamovi then lays "Decreasing" out as a group (CSS `titled-group`, 5px extra margin), so it
    sits lower than "None"/"Increasing", as "Percent" does under "Count". Giving "Increasing" an empty
    `children: []` to match was tried and looked worse in jamovi: dropped.
  - `yTicks`/`yGridLines` apply to the main (left) axis only, not to the secondary % axis. `yHideAxis`
    should hide both (`axis.line.y.right`/`axis.ticks.y.right` inherit from `vijHideAxis()`'s
    `axis.line.y`/`axis.ticks.y`, unless a theme sets them: to check on the 4 jamovi themes). A manual
    y range below the total (100% or N) cuts the curve (`coord_cartesian()` clips): accepted, the user
    chose the range; the secondary axis follows the left one.
- **Lumping of small categories** ("Group categories below X%", or top n): `forcats::fct_lump_prop()`/
  `fct_lump_n()` (`fct_lump()` itself is superseded), with `w = .COUNTS` and a translated
  `other_level`. `NA` is not lumped (stays "(Missing)"). Sorting by `fct_infreq()` would rank the
  "Other" level by its count: move it last afterwards (`fct_relevel(f, other, after = Inf)`), before
  "(Missing)". Useful beyond the Pareto (variables with many categories). **Done** (2026-11-18),
  see below.

### `piechart`/`barplot`: combining small categories (2026-11-18)

`barplot` (same options, section and code) also lets the user choose the variables: `lumpRows`
(category axis) and `lumpColumns` (group), both unchecked by default (user's choice, 2026-11-19),
each lumped over all the data with the same settings. The checkboxes turn lumping on, so `lump` has
no "None" in `barplot` (`auto`/`prop`, default `prop`; the UI puts them under the checkboxes, as
"Method"); `piechart` keeps "None". Its `"lumped"` attribute holds the *names* of
the lumped variables (piechart's is TRUE/FALSE). The "Other" color applies to the fill variable
(category without a group, group otherwise) when it was lumped: a lumped category with a group is
just an axis tick. It has no effect with "Single color" (fill fixed in `geom_bar`, "Other"
included: user's choice); the UI's `enable:` follows the same rule.

"Small Categories" section (`lump`: none / auto / prop + `lumpProp` %, `lumpLabel`, `lumpColor`).
In `.run()`, the lumping is done on `weighted$data` (one row per case) **before** the existing
`dplyr::count()`, which then sums the "Other" rows: no second count (user's choice, simpler).
The lumping itself is the shared `vijLumpLevels()` (`utilsData.R`).
`auto` = `fct_lump_lowfreq()` (lumps while "Other" stays the smallest: can take a large category
too), `prop` = `fct_lump_prop()`, both with `w = .COUNTS`. Over all the data, not per facet (same
categories in every facet). `NA` is never lumped (forcats leaves it `NA`); the proportions'
denominator includes the missing values' weights when they are kept, like the slices.
- **At least 2 categories with cases** must be lumped (`nlevels(droplevels(g)) <
  nlevels(droplevels(f))`): forcats (`fct_lump_prop`/`_n`/`_lowfreq`, also the superseded
  `fct_lump`) would otherwise rename a single one "Other".
- **Empty levels** (an unused level kept by jamovi's "Retain unused levels in analyses", or a
  category whose counts/weights are all 0: jamovi doesn't trim those, and `vijWeightedData()`
  drops zero-weight rows) are absorbed into "Other" when lumping happens, and shown in the legend
  (`drop = FALSE`) when it doesn't, as with "None". Checked by the user in jamovi (2026-11-18):
  the intended behaviour, don't change it. Rejected: dropping them whenever a mode is chosen (would
  ignore jamovi's option), and lumping only non-empty levels then re-adding the empty ones (a
  0-count category would stay outside "Other").
- "Other" is moved last after sorting, and `attr(plotData, "lumped")` (TRUE/FALSE) tells `.plot()`
  it was lumped. Set just before `setState()` and read first thing in `.plot()` (`isTRUE()`: FALSE
  for older states), so that no data operation can drop it. An attribute rather than a list state
  (user's decision, 2026-11-18): a list would change the state's shape, and jamovi 2.7 restores the
  state saved in an `.omv` (a data.frame for vijPlots <= 1.4.1), so it would need compatibility code;
  a `.OTHER` column was also considered. Its color:
  `vijColorScale(..., lastColor = )` (new optional argument: last level gets that color, the palette
  is computed for `n - 1`; an explicit `NA` level isn't counted in `n`). With the jmv palette, the
  other slices' colors may change, since `jmvcore::colorPalette(n)` depends on `n`.
- Empty `lumpLabel` = translated `.("Other")` ("Autres"), computed in `.run()`: saved in the `.omv`
  in the run's language (same trade-off as `likertplot`'s missing group).
- Tests: `test-utilsData.R` (`vijLumpLevels()`), and the "lump" tests at the end of
  `test-piechart.R`/`test-barplot.R` (state, table, "Other" color read from `layer_data()`, one
  snapshot each).

### Appearance options (findings of 2026-10-29 to 2026-11-21)

**Theme and font options: added, then removed (user's decision, 2026-11-23).** A per-analysis `plotTheme`
(jamovi's 4 themes + ggplot2's Bw/Light/Linedraw/Dark) and `fontFamily` (default/sans/serif/mono) were
added to the 15 visible analyses (commit `d164f3d`, 2026-11-20/21), then removed because jamovi's
developers don't want per-analysis appearance options. Never released, so no `.omv` holds them. Removed
along: `vijPlotTheme()`/`vijFontFamily()` (`utils.R`, copied to vijMulti), the `family =` of the text
layers, `piechart`'s `panel.border` blank (with a global SPSS theme the pie has its frame again, as in
1.4.1), the tests and 3 snapshots; `vijGridLines()` takes the theme name again. `renderedPlot()` stayed,
in `tests/testthat/helper-render.R` (used by `qqplot`/`scatterplot` tests). Don't propose them again
unless jamovi's stance changes. What was learnt, should a font or theme ever come back:

- **Font: what reaches what.** `theme(text = element_text(family = ))` reaches titles, axes, legend and
  strips (`vijTitleAndLabelFormat()` never sets `family`). Not the text layers: `geom_text`/`geom_label`
  follow only `theme(geom = element_geom(family = ))` (ggplot2 4.0 only, not the 3.5 of jamovi 2.7,
  `minApp: 2.7.1`), ggrepel has `family = ""` hard-coded, and `ggstats::gglikert()`'s labels/totals take
  no family argument (settable on the built layers, `layer$aes_params$family`). So every text layer
  needs an explicit `family =` (16 layers in 9 analyses; `histogram`'s two are built in
  `private$.summaryLine()`). Avoid `update_geom_defaults()`: global state in jamovi's long-lived R
  session. Use the generic `"sans"`/`"serif"`/`"mono"` only. vdiffr writes `font-family: sans` whatever
  the family: snapshots don't show the font.
- **Themes.** A complete theme added *after* `ggtheme` replaces jamovi's theme and keeps its palette
  scales. `jmvcore::theme_*(16)[[1]]` gives a jamovi theme regardless of the preferences;
  `jmvcore::theme_spss()` still calls `element_rect(size =)` (deprecation warning, jmvcore 28.3 and GitHub
  main; a bug report was drafted for the user, not posted by Claude). ggplot2 themes need `base_size =
  16` (line widths, ticks and margins derive from it; text sizes are all set by
  `vijTitleAndLabelFormat()`) and jamovi's 15 pt `plot.margin`. Near-duplicates at size 16: Classic ≈
  jamovi, Gray ≈ Hadley (`theme_hadley` is `theme_gray()` with jamovi's margins), Minimal ≈ Minimal
  (jamovi). `likertplot` ignores `ggtheme` (ggstats applies `theme_light()`).
- **Global text size: rejected (user's decision, 2026-11-20).** A `textScale` factor multiplying every
  size was considered: redundant with the per-element sizes every module already exposes (`barplot`:
  title, subtitle, caption, axis titles and labels, legend, facets, value labels), and the displayed
  sizes would no longer be the real ones (14 × 120 % = 16.8).
- **Background colour, `ink`/`paper` (findings of 2026-11-21, nothing done).** ggplot2 4.0's themes take
  `ink`/`paper`/`accent` (derived colours: grid, panel, strips, layer defaults), absent from ggplot2 3.5
  (jamovi 2.7) and from `jmvcore::theme_*()`. In 3.5 the same can be done element by element
  (`plot.background`, `panel.background`, `legend.background`, `legend.key`, `strip.background`; derived
  colours with `scales::col_mix()`): an opaque `plot.background` does show over the device's white. The
  real limit is the ink: text, axes and `textColor` stay dark, so only light backgrounds are readable.
  jamovi's `type: Color` option (ColorPicker, `#rrggbb` string, no alpha) needs jamovi 28.14+
  (`minApp: 28.14.0`) and jmvtools 28.17+ to compile (installed: 28.4). The generated `.h.R` calls
  `jmvcore::OptionColor`, which jamovi 28.17's bundled jmvcore 28.3 has but jamovi 2.7.41's jmvcore
  2.7.35 hasn't, nor the jmvcore "28.3" of the R library (same number, older build: tests would need
  it updated): one Color option would break the whole analysis on 2.7. To revisit with the end of
  jamovi 2.7 support; meanwhile a `String` option holding a colour name or hex code (checked with
  `grDevices::col2rgb()`) would work on both.
- **Transparent background: impossible from the module.** The white comes from jamovi's graphics
  device, not from ggplot2: the exported SVG starts with a `<rect>` larger than the `viewBox`
  filled in white, identical with and without a transparent `plot.background` (tested on
  `barplot`), and the copied PNG is RGB without an alpha channel. It would need a jamovi-side
  change (device opened with `bg = "transparent"`).
- **Vector mode (`mode: vector` of the `Image` results item): not worth it** (tested on
  `barchart`). jamovi's copy gives a PNG anyway (the clipboard holds only raster formats and an HTML
  `<img>` with the PNG in base64), and right-click -> SVG export already works on the PNG plot
  (text drawn as glyph paths, so no font problem). The only gain left is sharpness when zooming on
  screen, against heavier `.omv` files and slower display for plots with many points.

### Hide small % labels (`barplot`, `mrcrosstabs`, 2026-11-02)

`barplot` and `mrcrosstabs` have a `hideLabelsBelow` option ("Hide % below 5%", a child of the "Value
labels" checkbox, enabled only with `showLabels`, percentages, and stacked bars or `labelPosition`
middle). Kept as is (user's decision, 2026-11-03): not extended to `mrfrequencies`, where it would be
of much less use, nor to counts.

- **Rule kept (simple version):** only the *percent* labels are hidden, when the displayed value is
  below 5%, and only when the label sits over its bar (`labelPosition == "middle"`; stacked bars
  always do). Labels above the bar are never hidden. Counts are never hidden. It is done in the
  `label` aesthetic with `na.rm = TRUE` on the text layer, the same mechanism as `piechart`'s
  small-label hiding: `ifelse(after_stat(prop) < threshold, NA_character_, ...)` in `barplot`, in
  `valueLabel()` in `mrcrosstabs`, whose plot has an explicit `clearWith:` listing the option.
- **Why not counts:** with a group variable, `after_stat(prop)` is the share within the group or the
  category (depending on `percentWithin`), which is not the bar's share of the plot, so a
  "count < 5%" test would hide the wrong labels. Tried both on a small-groups CSV and on the demo
  file (Age x Sex), where the denominators behave in opposite ways.
- **The other analyses:** `likertplot` has the same title and child layout, on `ggstats`'
  `labels_hide_below`: unchecked, it still hides below 1%. `piechart` keeps "Hide labels below 5%", on
  every label type, a slice's share being well defined. `mosaic` has no option: a fixed filter
  (`.PERCENTAGE > 0.05 & .XWIDTH > 0.05`) on all its label types, so labels don't overflow small cells.
- The `showLabels` title of `barplot`, `mrcrosstabs` and `mrfrequencies` (one shared `msgid`) was
  renamed "Count labels" -> "Value labels" ("Étiquettes des valeurs"), since it covers counts and
  percentages alike.

### Hide axis (every visible analysis with axes, 2026-11-07)

`barplot`, `mrfrequencies`, `mrcrosstabs`, `barchart`, `lollipop` (hiding + baseline expansion);
`histogram`, `areachart`, `boxplot`, `raincloud`, `linechart`, `scatterplot`, `qqplot` (hiding only);
`mosaic` through its own global `hideAxes` (see below). Not concerned: `piechart` (no axes),
`likertplot` (its own `ggstats` theme).

A "Hide axis" checkbox per axis (`xHideAxis`/`yHideAxis`, under "Grid lines" in the Axes panel), on
the displayed axis like the other Axes options (with `coord_flip()`, theme elements follow the
displayed axes). `vijHideAxis(x, y)` (`utilsPlots.R`), added right after `vijGridLines()` (so after
`ggtheme`), hides the axis line and the ticks but keeps the axis text. Minimal and hadley already
draw no axis line, and minimal no ticks either: the option then changes nothing there.

- **Ticks are transparent, not `element_blank()`, on purpose:** a blank tick also drops its length,
  which brought the labels ~4 px closer to the bars. A transparent tick keeps the layout of the
  shown axis (same panel rectangle, checked on the snapshots). Don't "simplify" it back.
  **With `inherit.blank = TRUE`** (suggested by the user, 2026-11-07): the minimal theme's ticks are
  `element_blank()`, and a plain transparent tick overrode them, moving the labels 4 pt *away* from
  the panel; it now stays blank there. Checked on the 4 jamovi themes (default, minimal, hadley,
  iheartspss); `test-utilsPlots.R` covers it with `calc_element()`. jmvcore's `theme_*()` return a list
  (theme + scales): add it to a plot, as `.plot()` does, not to a theme (`theme + list` adds nothing).
- **Expansion** (`barplot`, `mrfrequencies`, `mrcrosstabs`, `barchart`, `lollipop`: a discrete
  category axis, no vertical grid lines under the bars; the other analyses have the hiding alone,
  see `histogram`/`areachart` below): when the *category* axis is hidden (`xHideAxis` vertical,
  `yHideAxis` horizontal), the value axis gets no lower expansion, so the bars start at the baseline:
  `expansion(mult = c(expandLow, expandHigh))`, `expandLow` 0 or 0.05, `expandHigh` 0.1 with labels
  at the top (never when stacked) else 0.05. **Automatic range only:** a manual range keeps
  `waiver()`, because `coord_cartesian(ylim = )` expands its limits with the scale's expansion (a
  2-8 range showed 1.7-8.3, then 2-8.6 with the axis hidden, before this was fixed).
- **`barchart`: no lower expansion only when nothing is drawn below 0** (bars, error bars `.YMIN`
  when drawn, observations `.PY`): otherwise the bars don't start at the bottom of the panel anyway,
  and the lowest element would touch its edge. The lower margin of negative bars' labels (`expandLow =
  expandHigh`) still comes first. All-negative bars (baseline at the top) are not handled: the 5%
  margin stays. A numeric test (`ggplot_build()` value range) covers each case.
- **`histogram` keeps its lower margin, like `areachart` (user's decision, 2026-11-07):** its x-axis
  is continuous, and with no margin the bins hid the vertical grid lines down to the panel's edge.
  A no-margin rule (`xHideAxis`, automatic y range) was applied, then removed; the expansion code is
  back to what it was before the option. A test checks that hiding the axes leaves the y range
  unchanged, with and without the summary line's label.
- **`lollipop`:** `baseline` = category axis hidden, automatic range, not "Min-Max" (its segments go
  from the minimum to the maximum, not from 0), and every `.Y` **above** 0 (`!any(.Y <= 0)`): a dot
  on 0 (frequent in "Values" mode) would lie on the panel's edge, cut in half, or with `clip = "off"`
  over its category's label (seen by the user with `dotSize = 20`, 2026-11-07). The first version
  switched clipping off instead; no longer needed, the coords are back to what they were. A big dot
  close to 0 can still be cut by the bottom edge, as without the option (5% margin): not handled.
  Numeric test as for `barchart`.
- `mrfrequencies`/`mrcrosstabs` list both options in their plot's `clearWith:`.
- **`areachart` keeps its lower margin (user's decision, 2026-11-07):** with no margin, the areas
  hid the vertical grid lines down to the panel's edge, whereas over a 5% margin they show under
  the areas, as on `linechart`. A no-margin rule (x-axis hidden, automatic range, no value below 0)
  was applied, then removed; 2% was also shown and not kept. A test checks that hiding the axes
  leaves the y range unchanged, in the 3 positions.
- **`scatterplot` with "Plot frame" (`plotBorder`):** the two options combine with nothing added.
  `vijHideAxis()` comes before the frame's `theme(axis.line = element_line(linewidth = 0), panel.border
  = ...)`, which sets the parent `axis.line` and so leaves our blank `axis.line.x/y` alone: the frame
  stays, the hidden axis loses its ticks, its text stays (snapshot `scatterplot-hideaxis-border`).
- `linechart`, `scatterplot` and `qqplot` were first classed "little use" (2026-11-07), then added:
  the axis text stays readable.
- **`mosaic` keeps its own global `hideAxes` (user's decision, 2026-11-07)**, in its main options,
  not the Axes panel: it hides both axes *and* sets the expansion to 0 on both, so that the tiles fill
  the panel, a whole look with no meaning per axis (the tiles touch all four edges; no grid lines
  there). Replacing it by `xHideAxis`/`yHideAxis` would also drop the setting from old `.omv` files.
  It now goes through `vijHideAxis(x = TRUE, y = TRUE)` (it had blank ticks: the category labels
  were ~4 px closer to the tiles); its panel is now that of `mosaic-basic`.

## Testing

Each test file (`tests/testthat/test-<name>.R`) exercises two levels:

1. **State checks** — call the exported `<name>(...)` wrapper and assert on
   `results$plot$state` (the data frame `.run()` staged before rendering).
2. **Visual snapshots** — call `<name>(...)$plot` (a jmvcore `Image` results object) and pass it
   to `expect_plot_snapshot(title, fig)`, defined once in `tests/testthat/helper-vdiffr.R`.
   Printing that `Image` object triggers its real `.render()` pipeline (resolving the actual
   global theme/palette), so this exercises the same code path jamovi itself uses rather than a
   hand-built fake theme. `renderedPlot(results)` (`tests/testthat/helper-render.R`) renders it the
   same way and returns the ggplot drawn, for tests that inspect its layers or theme.

`expect_plot_snapshot()` skips the real `vdiffr` pixel/SVG diff when `Sys.getenv("CI")` is truthy
(rendering is still exercised as a smoke test, just not compared), since SVG output isn't
reproducible across OS/font/graphics-device combinations; the full visual diff only runs locally.
Locally, `vdiffr`/snapshot-based checks also no-op unless `NOT_CRAN` is set truthy — call
`Sys.setenv(NOT_CRAN = "true")` before running tests interactively, or they'll silently skip.
`vdiffr` is deliberately not in `Suggests` (removed 2026-11-21): the jamovi compiler installs
`Suggests` into the `.jmo` like `Imports` (0.24 MiB for nothing at runtime). Install it in the local
R library on each machine (`install.packages("vdiffr")`) to run the visual snapshots.
Review new/changed snapshots with `testthat::snapshot_review("<name>")`, or accept blind with
`testthat::snapshot_accept("<name>")` (or by renaming the generated `*.new.svg` over the
reference file under `tests/testthat/_snaps/<name>/`).
