# pws

This package accompanies the book *[Playing With Statistics]*.

It provides datasets and helper functions used throughout the book.

---

## Installation

You can install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("stuart-coles1111/pws")
```

---

## Data

### `PL_goals`

A dataset containing English Premier League match results from the 1992–1993 season onward.

**Variables:**

* `season`: Season in "YYYY-YYYY" format
* `date`: Match date
* `home_team`: Home team
* `away_team`: Away team
* `home_goals`: Goals scored by home team
* `away_goals`: Goals scored by away team

---

## Example

```r
library(pws)

head(PL_goals)

# Compute goal difference
PL_goals$goal_diff <- PL_goals$home_goals - PL_goals$away_goals

# Average home advantage
mean(PL_goals$goal_diff)
```

---

## Data source

Data in this package are derived from the EngSoccerData R package.
Original data were compiled by Tony Ladson and contributors.

---

## License

This package is licensed under the GPL-3 license.
