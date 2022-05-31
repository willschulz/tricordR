#########################################################################
# HOURLY_QUALTRICS_SCRAPE
#########################################################################

options(tidyverse.quiet = TRUE)

library(tricordR)

tokenset <- str_remove_all(dir("~/tricordings/tokens"), ".rds")[1]
tokens <- prepTokens(tokenset, 1:9)

all_panels_contents <- dir("~/tricordings/studies", full.names = T) %>% dir(full.names = T) %>% dir(full.names = T)
scrape_settings_paths <- all_panels_contents[str_detect(all_panels_contents, "scrape_settings.rds")]
wants_survey <- c()
for (i in 1:length(scrape_settings_paths)){
  wants_survey[i] <- (!is.null(readRDS(scrape_settings_paths[i])$qualtrics_survey_id)) & (is.null(readRDS(scrape_settings_paths[i])$pause_match_scrapes))
}
scrape_settings_paths_wantsurvey <- scrape_settings_paths[wants_survey]
panel_directories <- str_remove_all(scrape_settings_paths_wantsurvey,"/scrape_settings.rds")

for (i in 1:length(panel_directories)) {
  message("Scraping ", str_remove_all(panel_directories[i], ".*studies/"))
  names <- panel_directories[i] %>% str_remove_all(".*studies/") %>% str_split("/") %>% unlist
  this_study_name <- names[1]
  this_panel_name <- names[2]
  scrapeQualtrics(study_name = this_study_name, panel_name = this_panel_name, match_by = "follow3",
                  assignment_panel = NULL, max_treat_followers = 60000,
                  treatment_tokens = tokens, participant_tokens = tokens)
}

