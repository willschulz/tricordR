# tricordR <img src='man/figures/logo.png' align="right" height="139" />

  tricordR is a system for automating Twitter scraping and related data collection activities, particularly for studies that involve tracking the same sets of users over a long time period.  It is built upon rtweet, but provides key features not available from rtweet:
  
  - Token-hopping functions speed up data collection by utilizing all of a user's tokens, in sequence, to complete a data collection task
  - An architecture for tracking distinct sets of users longitudinally, which uses cron jobs to update datasets automatically once per day (or at a frequency of the user's choosing) makes it easy to collect network data with high temporal resolution, while distributing computational resources more efficiently for collecting timeline data prospectively
  - A set of interactive dashboards visualize data collection at-a-glance, helping identify failures in data collection early, and providing peace of mind to the researcher
  
## Installation
  
  
  
  
