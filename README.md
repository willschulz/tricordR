# tricordR <img src='man/figures/logo.png' align="right" height="160" />

tricordR is an R package that helps automate and monitor Twitter scraping and related data collection activities, designed particularly for studies that involve tracking the same sets of users over a period of time.  It is built upon ```rtweet```, providing several key features that make large-scale data collection faster, simpler, and more manageable:
  
  - Robust scraping functions that utilize all of a user's tokens, in sequence, to complete a data collection task quickly while respecting Twitter's rate limits
  - An architecture for tracking distinct sets of users longitudinally, which uses cron jobs to update datasets automatically once per day (or at a frequency of the user's choosing), making it easy to collect network data with high temporal resolution, while distributing computational resources more efficiently by collecting timeline data prospectively, rather than retrospectively
  - A set of ```shiny``` dashboards visualize data collection at-a-glance, helping identify failures in data collection early, and providing peace of mind to the researcher

These tools are designed to minimize the time and effort required to collect data, so you can focus on research design and analysis.
  
## Getting Started

### Installation

Since tricordR is still in development, it is only available by invitation to this private repository.  To install it, therefore, you must use Hadley's ```devtools``` package and pass your GitHub personal access token to the install_github function.
  
``` r
# install.packages("devtools")
devtools::install_github("willschulz/tricordR", auth_token = YOUR_GH_PERSONAL_ACCESS_TOKEN)
```

### Initialization

Before collecting any data, run the following code to create tricordR's data folder (which is called "tricordings") in your home directory:
  
``` r
library(tricordR)
initialize()
```

The tricordings folder is where scraped data, scraping logs, and token sets will be stored.  Beware: re-initializing will result in deletion of any existing tricordR data that has been collected in tricordings!

### Saving Tokens

After initializing, install a set of Twitter tokens by passing the relevant parameters to the ```saveToken``` function:

``` r
saveToken(set_name = "my_twitter_tokens",
          consumer_key = YOUR_CONSUMER_KEY,
          consumer_secret = YOUR_CONSUMER_SECRET,
          access_token = YOUR_ACCESS_TOKEN,
          access_secret = YOUR_ACCESS_SECRET)
```

Repeat this process, keeping ```set_name``` fixed, for all the tokens you have created on your Twitter developer account (up to 9).  This will save all your tokens in a convenient list object, stored in tricordR_data/tokens/my_twitter_tokens.rds, so that high-level scraping functions can cycle through them to speed up data collection.

### Automated Scraping via Cron

If you intend to use tricordR's automated scraping features, add the following line to your crontab:

``` bash
00 12 00 * * * usr/local/bin/Rscript /Library/Frameworks/R.framework/Versions/4.0/Resources/library/tricordR/scripts/daily_twitter_scrape.R
```

This will prompt tricordR to update all tracked user panels (see below) at 12 noon every day, so long as your computer is not asleep.  If you're new to cron and use a mac, see <a href="https://ole.michelsen.dk/blog/schedule-jobs-with-crontab-on-mac-osx/" target="_blank">this helpful primer</a>.  Note: If the above path is not the correct path to your installation of tricordR, replace it with the path returned by the following R command:

``` r
system.file("scripts", "daily_twitter_scrape.R", package = "tricordR")
```

Similarly, you may need to specify a different path to your Rscript installation using the following bash command:

``` bash
which Rscript
```

Finally, to save logs from automated scrapes, add the following line to the end of your cron job, specifying the path to your tricordings folder (which should be in your home directory).

``` bash
%> /path/to/tricordings/logs/daily_twitter_scrape_log.txt
```

## Collecting Data

### Studies and Panels

Data collection in tricordR is organized with respect to user panels, which are nested within studies.  Panels serve to organize sets of users in a shared category (for example, study participants) within studies where other panels of users (for example, the users followed by the study participants) are also being tracked as part of the same research project.

So, to begin collecting data, first create a study:

``` r
addStudy("my_first_study")
```

This simply creates a new folder in the tricordR_data/studies directory, where user panels can be added.  Adding a user panel is more involved, since it requires specifying the set of users you wish to track, and the data you wish to collect about them.  For example:

``` r
library(dplyr)

my_tokens <- prepTokens("ws_botexp", 1:9) #prepare all nine of your tokens for usage

user_ids <- rtweet::stream_tweets(timeout = 5, #get some random user ids by streaming tweets for 10 seconds
                                  token = my_tokens[[1]], #you'll only need one of your tokens for this
                                  ) %>% pull(user_id) %>% unique()

addPanel(study_name = "my_first_study",
         panel_name = "my_first_panel",
         user_ids = user_ids,
         scrape_timelines = TRUE,
         scrape_friends = TRUE,
         scrape_followers = TRUE,
         scrape_favorites = FALSE,
         first_scrape = TRUE,
         tokens = my_tokens) #use all nine of your tokens for this
```

By calling addPanel, we create a new panel ("my_first_panel") within the study we just created ("my_first_study"), and specify the kinds of data we would like to collect: we want to scrape their timelines (AKA their tweets), and their friends (AKA the people the follow), but not their followers or their favorites (AKA their likes). By passing the value TRUE for ```initial_scrape```, we tell tricordR to go ahead and collect this data immediately, using the list of tokens we prepared above.  When scrape_timelines is TRUE, this initial scrape will include the last 3200 tweets available from each user.

Moreover, these settings are saved, and if you have added the daily_scrape_script.R to your crontab as instructed above, tricordR will automatically update these datasets daily: in this case, we would collect daily snapshots of the accounts these users follow (their friends), and download any new tweets from these users that have been tweeted since the previous timeline scrape.

If we subsequently want to edit the panel, we can call editPanel() and pass a set of additional users we'd like to add to the panel, or specify users to remove from the panel (data collected from removed users will not be deleted, but future scrapes will not collect further data on these users).  We can also change the kinds of data we collect about the users in this panel (however it is not recommended to initiate timeline or favorite scraping in panels where these data types were not initially being collected - if these data types are of interest, the panel should be initiated with these arguments set to TRUE).

``` r
editPanel(study_name = "my_first_study",
          panel_name = "my_first_panel",
          add_users = user_ids)
          
editPanel(study_name = "my_first_study",
          panel_name = "my_first_panel",
          remove_users = user_ids)
          
editPanel(study_name = "my_first_study",
          panel_name = "my_first_panel",
          scrape_followers = TRUE)
          
#calling editPanel with no optional arguments will print the panel's current settings
editPanel(study_name = "my_first_study",
          panel_name = "my_first_panel")
```

### Visualizing Data Collection

To check whether timeline scraping is up-to-date, launch the Timeline Dashboard using the following function:

``` r
runTimelineDash()
```


### A La Carte Scraping with Workhorse Functions

Sometimes we want to collect Twitter data in an isolated batch, without initiating these processes to track panels of users.  To do this, while still taking advantage of tricordR's token efficiencies, we use the mid-level workhorse functions shown below.

``` r
getTimelines()
getFollowers()
getFriends()
getFavorites()
```

## Reading Data for Analysis

When we want to read in the full datasets to R for analysis, we need convenient access to the data we have collected.




