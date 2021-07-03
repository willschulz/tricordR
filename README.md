# tricordR <img src='man/figures/logo.png' align="right" height="139" />

  tricordR is a system for automating Twitter scraping and related data collection activities, particularly for studies that involve tracking the same sets of users over a long time period.  It is built upon rtweet, but provides key features not available from rtweet:
  
  - High-level scraping functions that utilize all of a user's tokens, in sequence, to complete a data collection task quickly while respecting Twitter's rate limits
  - An architecture for tracking distinct sets of users longitudinally, which uses cron jobs to update datasets automatically once per day (or at a frequency of the user's choosing), making it easy to collect network data with high temporal resolution, while distributing computational resources more efficiently by collecting timeline data prospectively, rather than retrospectively
  - A set of ```shiny``` dashboards visualize data collection at-a-glance, helping identify failures in data collection early, and providing peace of mind to the researcher

These tools are designed to minimize the time and effort required to collect data, so you can focus on research design and analysis.
  
## Installation
  Since tricordR is still in development, it is only available by invitation to this private repository.  To install it, therefore, you must use Hadley's ```devtools``` package and pass your GitHub personal access token to the install_github function.
  
``` r
# install.packages("devtools")
devtools::install_github("willschulz/tricordR", auth_token = YOUR_GH_PERSONAL_ACCESS_TOKEN)
```

  Before collecting any data, run the following code to create tricordR's data folder in your home directory:
  
``` r
library(tricordR)
tricordR::initialize()
```

Then install a set of Twitter tokens by passing the relevant parameters to the ```saveToken``` function:

``` r
saveToken(set_name = "my_twitter_tokens", consumer_key, consumer_secret, access_token, access_secret)
```

Repeat this process, keeping ```set_name``` fixed, for all the tokens you have created on your account (up to 9).  This will save all your tokens in a convenient list object, so that high-level scraping functions can cycle through them to speed up data collection.


Finally, if you intend to use tricordR's automated scraping features, add the following line to your crontab:

``` bash
00 03 00 * * Rscript path/to/script.R
```

If you're new to cron jobs and use a mac, see <a href="https://ole.michelsen.dk/blog/schedule-jobs-with-crontab-on-mac-osx/">this helpful primer</a>: https://ole.michelsen.dk/blog/schedule-jobs-with-crontab-on-mac-osx/

## Collecting Data

Data collection in tricordR is organized with respect to user groups, which are nested within studies.  User groups serve to organize sets of users in a shared category (for example, study participants) within studies where other groups of users (for example, the accounts followed by the study participants) are also being tracked as part of the same research project.

So, to begin collecting data, first create a study:

``` r
addStudy("my_first_study")
```

This simply creates a new folder in the tricordR_data/studies directory, where user groups can be added.  Adding a user group is more involved, since it requires specifying the set of users to track, and the data you wish to collect about them.  For example:

