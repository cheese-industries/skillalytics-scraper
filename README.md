# Skillalytics Scraper
R-based sports data scraping tool as well as repository of corresponding data output for use in sports analytics projects.

# 

<h3>Installation</h3>
<p>The package can be installed within R by leveraging the install_github command from the devtools package, as follows:</p>
```
devtools::install_github("https://github.com/skillalytics/skillalytics-scraper")
library("skillalytics")
```
#

<h3>Usage</h3>
<p>Current functions within the package provide data scraping for NHL league standings, game results, skater stats, and goalie stats.</p>

```
# Standings
standings_tbl <- href_standings(seas = c(1990:1999))

# Game Results
results_tbl <- href_results(seas = c(1990:1999), reg = TRUE, post = TRUE)

# Skater Stats
skater_stats <- href_skater_stats(seas = c(1990:1999), reg = TRUE, post = TRUE)

# Goalie Stats
goalie_stats <- href_goalie_stats(seas = c(1990:1999), reg = TRUE, post = TRUE)
```

<h4>Notes</h4>
<p>Each function has a parameter to select seasons to scrape which has no default set and must be numeric. Season year is based on when that season ends, therefore, setting the parameter to 2021 would gather 2020-2021 season data.  If scraping multiple seasons be sure to encapsulate the input within the combine <code>c()</code> function.</i></p>
<p>The game results, skater stats, and goalie stats functions also include parameters to select regular season and postseason data. Both of these are boolean and default to TRUE. If selecting both the final output returned will be one dataframe combined with a column titled Game_Type with values of 'REG' and 'POST' to differentiate the data.</p>

<h4>! Important</h4>
<p>The dataset contains both a team-by-team breakdown of player stats (for players who played on multiple teams within one season) as well as a season total for those players (marked as 'TOT' in the Team_Name variable). Depending on your purpose, the dataset will need to be filtered accordingly to ensure you are not double counting those statistics.</p>
