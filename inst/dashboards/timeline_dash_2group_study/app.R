#########################################################################
# TWITTER_DASH_2GROUP_STUDY
#########################################################################

# R -e "shiny::runApp('~/Documents/GitRprojects/LaForge/shiny/twitter_dash_2panel_study/app.R', port = 4711)"
# /Library/Frameworks/R.framework/Resources/bin/Rscript -e "shiny::runApp('~/Documents/GitRprojects/LaForge/shiny/twitter_dash_2panel_study/app.R', port = 4711)"

#/Library/Frameworks/R.framework/Versions/4.0/Resources/library/tricordR/dashboards/timeline_dash_2group_study/app.R
options(warn=-1)

options(tidyverse.quiet = TRUE)
library(tricordR)
options(dplyr.summarise.inform = FALSE)
library(lubridate)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)

# SETTINGS

refresh_time=5*60*1000 #(milliseconds)
spinner_size <- .5

screen_name_label_cols <- c("white", "white", "red")
default_screen_name_fonts <- c(2, 2, 4)

point_cex <- .9
default_axis_cex <- 1
default_screen_name_cex <- .8
default_point_color <- rgb(1,1,1,.7)

# my_red <- rgb(1,.5,.5)
# my_blue <- rgb(.5,.5,1)
my_red <- rgb(1,1,1,.7)
my_blue <- rgb(1,1,1,.7)

my_sentiment_reference_scale = c(-1,1)
my_sentiment_reference_scale = NA
sentiment_left_color = c(1,.2,0,.7)
sentiment_right_color = c(0,1,0,.7)

#my_ideo_reference_scale = c(-.1,.1)
my_ideo_reference_scale = NA
ideo_left_color = c(.5,.5,1,.3)
ideo_right_color = c(.9,0,0,.3)

sure_left_color = c(.3,.5,.9,.7)
sure_right_color = c(.7,.7,0,.7)

survey_cumulation_colors <- c(rgb(.2,.8,.2),rgb(.8,0,0))

#survey_start_date <- "2021-04-21 12:00:00 EST"

tricordings_directory <- "~/tricordings/studies/"

study_names <- dir("~/tricordings/studies", full.names = T) %>%
  dir(full.names = T) %>%
  dir(full.names = T) %>%
  .[str_detect(., "scrape_settings.rds")] %>%
  file.info() %>%
  mutate(path = rownames(.)) %>%
  mutate(study_name = path %>% str_remove_all(".*/studies/") %>% str_remove_all("/.*")) %>%
  group_by(study_name) %>%
  summarise(most_recent = max(mtime), count = n()) %>%
  arrange(desc(count), desc(most_recent)) %>%
  pull(study_name)

#source("~/Documents/GitRprojects/LaForge/functions/dashboard_functions.R")
#source("~/Documents/GitRprojects/LaForge/functions/schulzFunctions.R")
#source("~/Documents/GitRprojects/LaForge/functions/twitter_scraping_functions.R")
#source("~/Documents/GitRprojects/LaForge/functions/qualtrics_functions.R")

#this takes the alphabetical first token set on the machine
tokenset <- str_remove_all(dir("~/tricordings/tokens"), ".rds")[1]
twitter_tokens <- prepTokens(tokenset, 1:9)

px_panel_1 <- 300
px_panel_2 <- 300

px_l <- 40

dash_theme = "grey_dark"
row_height = 200


# App title ----
header <- dashboardHeader(title = "Tweet Dashboard"
                          # title = shinyDashboardLogoDIY(
                          #   boldText = "LaForge",
                          #   mainText = "Twitter",
                          #   textSize = 16,
                          #   badgeText = " v1.0",
                          #   badgeTextColor = "white",
                          #   badgeTextSize = 2,
                          #   badgeBackColor = "#343E48",
                          #   badgeBorderRadius = 3
                          # )
)

sidebar <- dashboardSidebar(#width=12,
  #textInput("study_name", "Study Name", value = "test_study", width = "100%", placeholder = NULL),
  selectInput("study_name", "Study Name", choices = study_names), #target
  numericInput("days_back", "Days Back", value = 10, min = 1, max = 365, step = 1),
  #br(),#br(),br(),
  selectInput("color_variable", "Color:", #update this so it selects a user set, and add a feature for how color is used - sentiment, political content, lasso ideology, etc...
              c("Sentiment" = "sentiment",
                "Lasso Ideology" = "ideology",
                #"Lasso Sureness" = "sureness",
                "None" = "none")),
  numericInput("volume_smoothing", "Minute Smoothing", value = 15, min = 1, max = 60, step = 1),
  checkboxInput(inputId = "show_names", label = "Show Names", value = TRUE),
  checkboxInput(inputId = "load_all_since_first", label = "Load All", value = TRUE),
  checkboxInput(inputId = "include_historical", label = "Include Historical", value = TRUE),
  br(),br(),br(),
  actionButton("fetch_tweets","Fetch Tweets", icon("download"), style = "color:#CDCDCD; background-color: #44505A; border-color: #4C5A67"),
  actionButton("fetch_survey","Fetch Survey", icon("download"), style = "color:#CDCDCD; background-color: #44505A; border-color: #4C5A67")
)



# Main panel for displaying outputs ----
body <- dashboardBody(
  shinyDashboardThemes(theme = dash_theme),

  fluidRow(
    box(title=textOutput("panel_1_name"),
        width = 12,
        plotOutput("panel_1s_ts_l", height = px_l, width = "100%") %>%
          withSpinner(color="#777777", type=8, size = spinner_size),
        plotOutput("panel_1s_ts_d", height = px_panel_1, width = "100%") %>%
          withSpinner(color="#777777", type=8, size = spinner_size),
    ),

    box(title=textOutput("panel_2_name"),
        width = 12,
        plotOutput("panel_2s_ts_l", height = px_l, width = "100%") %>%
          withSpinner(color="#777777", type=8, size = spinner_size),
        plotOutput("panel_2s_ts_d", height = px_panel_2, width = "100%") %>%
          withSpinner(color="#777777", type=8, size = spinner_size),
    ),
  )
)


ui <- dashboardPage(header, sidebar, body)

# Data pre-processing ----



# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  observeEvent(input$fetch_tweets, {
    #source("/Users/wschulz/Documents/GitRprojects/LaForge/scripts/qualtrics_fetch.R")
    scrapeStudy(study_name = input$study_name,
                tokens = twitter_tokens,
                include_timelines = T,
                include_friends = F,
                include_followers = F,
                include_favorites = F,
                sentiment = T,
                darmoc = T)
    showModal(modalDialog(
      title = "Twitter scrape completed",
      paste0("X new tweets collected.")
    ))
  })

  experiment_directory <- reactive({paste0(tricordings_directory,"/", input$study_name, "/")})
  panels <- reactive({dir(experiment_directory())})

  output$panel_1_name <- renderText({
    panels()[1]
  })

  output$panel_2_name <- renderText({
    panels()[2]
  })

  participant_panel <- reactive({
    for (i in 1:2){
      if (!is.null(readRDS(paste0(experiment_directory(), panels()[i], "/scrape_settings.rds"))$qualtrics_survey_id)){
        participant_panel <- panels()[i]
      }
    }
    return(participant_panel)
  })

  observeEvent(input$fetch_survey, {
    scrape_qualtrics(participant_panel(), input$study_name, match_by = "follow3",
                     treatment_tokens = twitter_tokens, participant_tokens = twitter_tokens) #don't need to split anymore since automatically tries all
    #treatment_tokens = twitter_tokens[1:6], participant_tokens = twitter_tokens[7:9])
    showModal(modalDialog(
      title = "Fetch Completed",
      paste0(participant_panel(), " fetched!")
      #paste0(new_responses_count, " new responses collected.")
    ))
  })

  timeline_data_panel_1s <- reactive({
    invalidateLater(refresh_time)
    prep_timeline_data(panel_directory = paste0(experiment_directory(), panels()[1]),
                       sessions_back=(input$days_back),
                       include_historical = input$include_historical,
                       load_all_since_first = input$load_all_since_first)
  })

  timeline_data_panel_2s <- reactive({
    invalidateLater(refresh_time)
    prep_timeline_data(panel_directory = paste0(experiment_directory(), panels()[2]),
                       sessions_back=(input$days_back),
                       include_historical = input$include_historical,
                       load_all_since_first = input$load_all_since_first)
  })

  output$panel_1s_ts_l <- renderPlot({
    invalidateLater(refresh_time)
    timeline_data <- timeline_data_panel_1s()
    linePlot(timeline_data[[1]], input$days_back, input$volume_smoothing, axis_cex = default_axis_cex)
  })

  output$panel_1s_ts_d <- renderPlot({
    invalidateLater(refresh_time)
    timeline_data <- timeline_data_panel_1s()
    dotPlot(data_e = timeline_data[[1]],
            data_e_f = timeline_data[[2]],
            days = input$days_back,
            color_variable = input$color_variable,
            show_names = input$show_names,
            sentiment_left_color = sentiment_left_color,
            sentiment_right_color = sentiment_right_color,
            ideo_left_color = ideo_left_color,
            ideo_right_color = ideo_right_color,
            point_cex = point_cex,
            axis_cex = default_axis_cex,
            screen_name_cex = default_screen_name_cex,
            screen_name_cols = screen_name_label_cols,
            screen_name_fonts = default_screen_name_fonts,
            sentiment_reference_scale = my_sentiment_reference_scale,
            ideo_reference_scale = my_ideo_reference_scale)
  })

  output$panel_2s_ts_l <- renderPlot({
    invalidateLater(refresh_time)
    timeline_data <- timeline_data_panel_2s()
    linePlot(timeline_data[[1]], input$days_back, input$volume_smoothing, axis_cex = default_axis_cex)
  })

  output$panel_2s_ts_d <- renderPlot({
    invalidateLater(refresh_time)
    timeline_data <- timeline_data_panel_2s()
    dotPlot(data_e = timeline_data[[1]],
            data_e_f = timeline_data[[2]],
            days = input$days_back,
            color_variable = input$color_variable,
            show_names = input$show_names,
            sentiment_left_color = sentiment_left_color,
            sentiment_right_color = sentiment_right_color,
            ideo_left_color = ideo_left_color,
            ideo_right_color = ideo_right_color,
            point_cex = point_cex,
            axis_cex = default_axis_cex,
            screen_name_cex = default_screen_name_cex,
            screen_name_cols = screen_name_label_cols,
            screen_name_fonts = default_screen_name_fonts,
            sentiment_reference_scale = my_sentiment_reference_scale,
            ideo_reference_scale = my_ideo_reference_scale)
  })

}

shinyApp(ui, server)
