#########################################################################
# DAILY_TWITTER_SCRAPE
#########################################################################

# unified scraping script

options(tidyverse.quiet = TRUE)

library(tricordR)
library(dplyr)
library(stringr)

all_panels_contents <- dir("~/tricordings/studies", full.names = T) %>% dir(full.names = T) %>% dir(full.names = T)
scrape_settings_paths <- all_panels_contents[str_detect(all_panels_contents, "scrape_settings.rds")]
panel_directories <- str_remove_all(scrape_settings_paths,"scrape_settings.rds")

tokenset <- str_remove_all(dir("~/tricordings/tokens"), ".rds")[1]

for (i in 1:length(panel_directories)) {
  message("Scraping ", str_remove_all(panel_directories[i], ".*studies/"))
  scrapePanel(panel_directories[i], tokens = prepTokens(tokenset, 1:9), sentiment = "sentimentR", darmoc=FALSE)
}
