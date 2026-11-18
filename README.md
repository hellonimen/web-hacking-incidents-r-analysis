# Web hacking incidents: temporal and severity analysis in R

An R script that cleans a public dataset of web hacking incidents and analyses it over time: how many incidents per year, month and season, how severe they were, and what the next twelve months might look like. It is my individual coursework at Asia Pacific University.

## What the script does

1. **Data quality checks.** Inspects the structure, reports missing values per column, removes duplicate rows, standardises the date format and text fields (`UNKNOWN` becomes `NA`), sets impossible negative loss, downtime and ransom values to `NA`, and compares completeness before and after cleaning.
2. **Analysis dimensions.** Groups the columns into temporal, geographic, technical and economic dimensions, reports how complete each one is, and checks the relationship between downtime and loss.
3. **Time features.** Builds year, month, season, weekday, quarter and weekend flags, plus monthly attack counts and three-month rolling averages.
4. **Exploration.** Annual trend line, monthly distribution, seasonal box plots, a year-by-month heatmap and an overview area chart.
5. **Severity.** A monthly severity index (average loss multiplied by average downtime, divided by 1000), shown as a circular heatmap and as a smoothed trend over time.
6. **Three extra analyses.**
   - Time series decomposition into trend, seasonal and random components.
   - A 12 month ARIMA forecast of monthly attack counts (`auto.arima` with seasonality).
   - A correlation matrix of monthly attack count, average loss, average downtime, month and weekend ratio.

## Run it

You need R and these packages: `tidyverse`, `lubridate`, `dplyr`, `ggplot2`, `scales`, `forecast`, `corrplot` and `zoo` (the script installs `zoo` if it is missing).

```r
install.packages(c("tidyverse", "lubridate", "scales", "forecast", "corrplot"))
```

Put the dataset next to the script as `hackingData.csv`, set the working directory to that folder, then run it in RStudio section by section, or all at once with `Rscript analysis.R` (plots are then written to `Rplots.pdf`).

## The dataset

The script reads a CSV called `hackingData.csv`. It is a public web hacking incidents dataset and I did not include it here because it is not mine to redistribute. The script expects these columns:

| Column | Used as |
|---|---|
| `Date` | Incident date in `dd/mm/yyyy` format |
| `Country`, `IP` | Geographic dimension |
| `OS`, `WebServer`, `Encoding` | Technical dimension |
| `Loss`, `Ransom` | Economic dimension |
| `DownTime` | Temporal and economic dimension |

## Notes

- The only change from the version I handed in is that `install.packages("zoo")` now runs only when the package is missing.

## Licence

MIT, see [LICENSE](LICENSE).
