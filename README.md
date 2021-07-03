# tricordR <img src='man/figures/logo.png' align="right" height="139" />

  tricordR is a system for automating Twitter scraping and related data collection activities, particularly for studies that involve tracking the same sets of users over a long time period.  It is built upon rtweet, but provides key features not available from rtweet:
  
  - Token-hopping functions speed up data collection by utilizing all of a user's tokens, in sequence, to complete a data collection task
  - An architecture for tracking distinct sets of users longitudinally, which uses cron jobs to update datasets automatically once per day (or at a frequency of the user's choosing), making it easy to collect network data with high temporal resolution, while distributing computational resources more efficiently for collecting timeline data prospectively
  - A set of shiny dashboards visualize data collection at-a-glance, helping identify failures in data collection early, and providing peace of mind to the researcher
  
## Installation
  Since tricordR is still in development, itis only available by invitation to this private repository.  To install it, therefore, you must use Hadley's devtools package and pass your GitHub personal access token to the install_github function.
  
``` r
# install.packages("devtools")
devtools::install_github("willschulz/tricordR", auth_token = YOUR_GH_PERSONAL_ACCESS_TOKEN)
```

  Before collecting any data, run the following code to install tricordR's data folder in your home directory:
  
``` r
library(tricordR)
install_tricordR()
```

Finally, if you intend to use tricordR's automated scraping features, add the following line to your crontab:

``` bash
00 03 00 * * Rscript path/to/script.R
```
