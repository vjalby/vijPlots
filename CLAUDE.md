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

**Next release (not yet scheduled, requested 2026-10-16): remind the user to create a git tag** for
the released version, prefixed with `v` like the first one (`v1.3.3`, on the "Version 1.3.3"
commit). Use a plain lightweight tag, not an annotated one (user's choice, 2026-10-16 — `v1.3.3`
itself is annotated, but don't redo it): `git tag vX.Y.Z <commit>` then `git push origin vX.Y.Z`.

Testing (testthat edition 3, run from the package root after `devtools::load_all(".")`):

```r
Sys.setenv(NOT_CRAN = "true")                              # required, see "Testing" below
testthat::test_file("tests/testthat/test-histogram.R")
testthat::test_dir("tests/testthat")
```

There is no CI workflow in this repo (no `.github/`) and no `renv`/`air.toml` — don't assume
either is present or run commands that depend on them.

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

### Shared utilities (`R/utils.R`)

All prefixed `vij*` for easy grepping: `vijScale()`/`vijPalette()`/`vijPaletteNlevels()` build
palette scales from the `colorPalette` option; `vijTitlesAndLabels()`/`vijTitleAndLabelFormat()`
build `labs()` and axis/legend theme from the title/subtitle/caption/axis options every module
exposes; `vijHelpMessage()`/`vijErrorMessage()`/`vijWarningMessage()`/`vijDebugMessage()` write to
`self$results$todo`; `vijDebugPlot(self, plot)` — called at the end of every `.plot()` — forces an
eager build to surface any ggplot2 warning/message as a Notice instead of letting jmvcore silently
swallow it (see gotcha below).

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
when the data had a single row (fixed 2026-10-24). Options
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
- What goes into `image$setState()` is saved into the user's `.omv`, once per analysis. The
  summary-shaped plots (`barchart`, `barplot`, `piechart`, `lollipop`) therefore aggregate in
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
  error at the subsetting line itself. `dplyr::filter()` does not have this problem — it preserves
  custom column attributes (and keeps a plain data.frame a data.frame, doesn't coerce to tibble) —
  so prefer `dplyr::filter(df, !is.na(.data[[colName]]))` over `df[!is.na(df[[colName]]), ]`
  whenever a surviving column may later go through `toNumeric()` or otherwise relies on a custom
  attribute. Bit `likertplot.b.R`'s `ignoreNA` handling (2026-08-23) when its NA-row filter was
  moved from `.plot()` into `.run()`, ahead of the Mann-Whitney/Kruskal-Wallis/pairwise tests that
  call `toNumeric()` on the filtered columns — fixed by switching to `dplyr::filter()`.
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
- **To do for vijPlots 1.5 (decided 2026-10-10, postponed 2026-11-03, cosmetic only): stop indexing with a symbol.**
  Several `.plot()` functions read (or assign) a column with `plotData[[sym]]`, where `sym` comes
  from `rlang::sym()`/`vijVar()`. This is *not* a bug: `[[` and `[[<-` accept a symbol on a
  data.frame, a tibble and a list (checked on R 4.6.1, including non-ASCII names; a missing column
  returns `NULL` as with a string) — only single-bracket `df[sym]`/`df[, sym]` errors, and there is
  none in the code. But the idiom is little known and reviewers keep flagging it as an error, so
  the plan is to keep the column name as a string next to its symbol (`timeVarName` / `timeVar`)
  and index with the string. 17 occurrences in 7 analyses: `areachart` (2), `linechart` (2),
  `barplot` (2, one is an assignment), `boxplot` (2), `histogram` (1), `mosaic` (6), `scatterplot`
  (2). `qqplot` is not concerned (its `[[depVar]]` are in `.run()`, where `depVar` is a string).
  When doing it: no visual snapshot may change (any change means a mistake), and check that every
  edited line is actually executed by a test — some sit in option-dependent branches (`boxplot`
  ordering, `mosaic` facets, `scatterplot` range) — rendering by hand those that are not.
- **To do for vijPlots 1.5 (deferred 2026-10-21, postponed 2026-11-03, robustness only): replace the
  base-R group filters.** 11 `df[!is.na(df[[col]]), ]` filters drop the missing groups when "Ignore
  missing groups/values" is checked: `barchart` (3), `boxplot`, `raincloud`, `linechart`, `areachart`
  (2 each). Use `dplyr::filter(df, !is.na(.data[[col]]))` or a `vijDropMissing(data, names)` helper in
  `utilsData.R` (skipping `NULL` names). Not a bug today: `jmvcore::toNumeric()` runs before these
  filters, so the attribute loss described above doesn't bite, but moving it after them would. Leave
  `jmvcore::naOmit()` alone (`histogram`, `qqplot`, `lollipop`, `scatterplot`, `vijWeightedData()`): it
  filters on any column, on purpose. No visual snapshot may change.
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
  the variables' NA lines aren't merged into one. "(Missing)" is translated in `fr.po` ("(Manquant)"). Not yet done:
  copying `utils.R` to vijMulti.
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
in `lollipop.b.R` — note it's still installed transitively via `FactoMineR` (→ car → pbkrtest →
doBy → forecast) for as long as `FactoMineR` stays an Import.

### Avoid `do.call()`

Prefer a `switch()` (or direct `pkg::fun()` calls) over `do.call(getFromNamespace(x, "pkg"), args)`
or any other pattern that resolves a function to call by name at runtime. A `switch()` on a small,
explicit set of branches is just as flexible for dispatching on an option value, keeps every
callable function spelled out literally (grep-visible, no need for a defensive whitelist check —
see the "Dependency graph" note above for the `principal.b.R` rotation example this was fixed in),
and avoids `do.call`'s NSE/argument-matching quirks entirely.

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

### Ideas under study (not implemented; findings of 2026-10-29)

Possible global "appearance" options shared by all analyses (font family, overall text size, base
theme, alpha, line width, ...). Only the font and the background were investigated:

- **Font family: the plan, for vijPlots 1.5 (user's decision, 2026-11-03).** Nothing sets a `family`
  today; everything comes from jamovi's theme.
  Start with the generic `"sans"`/`"serif"`/`"mono"` only (safe on every platform; named fonts must
  exist on the machine running R). A `serif` test in `barplot` (a `family = "serif"` in `geomText`
  and a `theme(text = ...)` after `vijTitleAndLabelFormat()`) worked in jamovi's display; it was
  removed afterwards.
- **Font family: what the theme reaches.** `theme(text = element_text(family = ))` is enough for
  titles, axes, legend and facet labels (`vijTitleAndLabelFormat()` never sets `family`, so its
  `element_text`s inherit). It does **not** reach text layers (`geom_text`, `geom_label`,
  `ggrepel::geom_text_repel`/`geom_label_repel`, and probably `ggstats::gglikert`, not checked).
  Tested on ggplot2 4.0.3 / ggrepel 0.9.8: `geom_text`/`geom_label` follow only
  `theme(geom = element_geom(family = ))`, which exists in ggplot2 4.0 only, not in the 3.5 that
  jamovi 2.7 users run (`minApp: 2.7.1`); ggrepel has `family = ""` hard-coded in its
  `default_aes` and follows nothing. So every text layer needs an explicit `family =` (about 25
  calls in `barchart`, `barplot`, `boxplot`, `histogram`, `mosaic`, `piechart`, `scatterplot`,
  `mrfrequencies`, `mrcrosstabs`; `piechart`'s `baseGeomLab` covers four variants at once), computed
  once per `.plot()` and passed explicitly like the other options. Avoid
  `update_geom_defaults()`: global state in jamovi's long-lived R session, and it can't be reset in
  an `on.exit()` of `.plot()` since the plot is rendered after it returns. Add a check that no
  text layer is missing `family`.
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

### Hide axis (`barplot`, `mrfrequencies`, `mrcrosstabs`, `boxplot`, `raincloud`, 2026-11-07)

A "Hide axis" checkbox per axis (`xHideAxis`/`yHideAxis`, under "Grid lines" in the Axes panel), on
the displayed axis like the other Axes options (with `coord_flip()`, theme elements follow the
displayed axes). `vijHideAxis(x, y)` (`utils.R`), added right after `vijGridLines()` (so after
`ggtheme`), hides the axis line and the ticks but keeps the axis text. Jamovi's themes are not taken
into account (minimal/hadley already draw no axis line: the option then changes nothing).

- **Ticks are transparent, not `element_blank()`, on purpose:** a blank tick also drops its length,
  which brought the labels ~4 px closer to the bars. A transparent tick keeps the layout of the
  shown axis (same panel rectangle, checked on the snapshots). Don't "simplify" it back.
- **Expansion** (bar analyses only; `boxplot` and `raincloud` have the hiding alone, their
  distributions don't start at 0): when the *category* axis is hidden (`xHideAxis` vertical,
  `yHideAxis` horizontal), the value axis gets no lower expansion, so the bars start at the baseline:
  `expansion(mult = c(expandLow, expandHigh))`, `expandLow` 0 or 0.05, `expandHigh` 0.1 with labels
  at the top (never when stacked) else 0.05. **Automatic range only:** a manual range keeps
  `waiver()`, because `coord_cartesian(ylim = )` expands its limits with the scale's expansion (a
  2-8 range showed 1.7-8.3, then 2-8.6 with the axis hidden, before this was fixed).
- `mrfrequencies`/`mrcrosstabs` list both options in their plot's `clearWith:`.
- **Not yet extended** (survey of 2026-11-07): `barchart`, `histogram`, `lollipop`, `areachart` need a
  rule for the baseline (values can be negative in `barchart`/`lollipop`/`areachart`). Little use in
  `scatterplot`, `qqplot`, `linechart`. `mosaic` keeps its own global `hideAxes` (both axes, plus no
  expansion): replacing it by the per-axis options would drop the setting from old `.omv` files.

## Testing

Each test file (`tests/testthat/test-<name>.R`) exercises two levels:

1. **State checks** — call the exported `<name>(...)` wrapper and assert on
   `results$plot$state` (the data frame `.run()` staged before rendering).
2. **Visual snapshots** — call `<name>(...)$plot` (a jmvcore `Image` results object) and pass it
   to `expect_plot_snapshot(title, fig)`, defined once in `tests/testthat/helper-vdiffr.R`.
   Printing that `Image` object triggers its real `.render()` pipeline (resolving the actual
   global theme/palette), so this exercises the same code path jamovi itself uses rather than a
   hand-built fake theme.

`expect_plot_snapshot()` skips the real `vdiffr` pixel/SVG diff when `Sys.getenv("CI")` is truthy
(rendering is still exercised as a smoke test, just not compared), since SVG output isn't
reproducible across OS/font/graphics-device combinations; the full visual diff only runs locally.
Locally, `vdiffr`/snapshot-based checks also no-op unless `NOT_CRAN` is set truthy — call
`Sys.setenv(NOT_CRAN = "true")` before running tests interactively, or they'll silently skip.
Review new/changed snapshots with `testthat::snapshot_review("<name>")`, or accept blind with
`testthat::snapshot_accept("<name>")` (or by renaming the generated `*.new.svg` over the
reference file under `tests/testthat/_snaps/<name>/`).
