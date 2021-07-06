# Skillalytics Scraper
Sports data scraping tool and corresponding data output for use in sports analytics projects.

<b><i>Currently only includes function for scraping <a href="https://www.hockey-reference.com/">hockey-reference.com</a>, but more functions coming soon.</i></b>

# 

<h3>Hockey-Ref</h3>
<p>The "hockey-ref.R" file contains the <code>href_dat</code> function that allows you to scraper hockey-reference.com by season for: skater stats, goalie stats, league standings, and schedule and game results. (Currently this only pulls regular season data).</p>

<p>Example usage:</p>

```
#Package dependencies
library(tidyverse)
library(rvest)

#Select years to scrape
output_df <- href_dat(seasons = c(1918:2021))

#Get tables from returned list
skater_season_stats <- output_df$skater_stats
goalie_season_stats <- output_df$goalie_stats
league_standings <- output_df$league_standings
reg_sched_results <- output_df$reg_sched_results
```

<h4>Notes</h4>
<p>The function only has one argument (seasons) which requires a numerical input of the seasons you are looking to scrape.
<br><i>*If scraping multiple seasons be sure to encapsulate the input within the combine <code>c()</code> function.</i></p>
<p>The function outputs a four item lists of tables scraped. These can be extracted and added to their own R elements after the function has run as shown in the last part of the example above.</p>

<h4>Important</h4>
<p>The dataset contains both a team-by-team breakdown of player stats (for players who played on multiple teams within one season) as well as a season total for those players (marked as 'TOT' in the Team name variable). Depending on your purpose, the dataset will need to be filitered accordingly to ensure you are not double counting those statistics.</p>
