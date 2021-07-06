README
================
Will Schulz
7/6/2021

<!-- README.md is generated from README.Rmd. Please edit that file -->

tricordR is an R package that helps automate and monitor Twitter
scraping and related data collection activities, designed particularly
for studies that involve tracking the same sets of users over a period
of time. It is built upon `rtweet`, providing several key features that
make large-scale data collection faster, simpler, and more manageable:

-   Robust scraping functions that utilize all of a researcher’s tokens,
    in sequence, to complete data collection quickly while respecting
    Twitter’s rate limits.
-   An architecture for tracking distinct sets of users longitudinally,
    which uses cron jobs to automatically update datasets daily (or more
    often), making it easy to collect network data with high temporal
    resolution, while distributing computational resources more
    efficiently by collecting timeline and favorite data prospectively,
    rather than retrospectively.
-   A set of `shiny` dashboards visualize data collection at-a-glance,
    helping identify missing data early, and providing peace of mind to
    the researcher.

These tools are designed to minimize the time and effort required to
collect data, so you can focus on research design and analysis.

<<<<<<< HEAD
=======
tricordR is an R package that helps automate and monitor Twitter scraping and related data collection activities, designed particularly for studies that involve tracking the same sets of users over a period of time.  It is built upon ```rtweet```, providing several key features that make large-scale data collection faster, simpler, and more manageable:
  
  - Robust scraping functions that utilize all of a researcher's tokens, in sequence, to complete data collection quickly while respecting Twitter's rate limits.
  - An architecture for tracking distinct sets of users longitudinally, which uses cron jobs to automatically update datasets daily (or more often), making it easy to collect network data with high temporal resolution, while distributing computational resources more efficiently by collecting timeline and favorite data prospectively, rather than retrospectively.
  - A set of ```shiny``` dashboards visualize data collection at-a-glance, helping identify missing data early, and providing peace of mind to the researcher.

These tools are designed to minimize the time and effort required to collect data, so you can focus on research design and analysis.
  
>>>>>>> 98ad7d3faac952e96dbd361b891b2b4313cd3c7f
## Getting Started

### Installation

Since tricordR is still in development, it is only available by
invitation to this private repository. To install it, therefore, you
must use Hadley’s `devtools` package and pass your GitHub personal
access token to the install\_github function.

``` r
# install.packages("devtools")
devtools::install_github("willschulz/tricordR", auth_token = YOUR_GH_PERSONAL_ACCESS_TOKEN)
```

### Initialization

Before collecting any data, run the following code to create tricordR’s
data folder (which is called “tricordings”) in your home directory:
