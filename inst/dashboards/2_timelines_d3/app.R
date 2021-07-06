#########################################################################
# TWITTER_DASH_2GROUP_STUDY_D3
#########################################################################

# R -e "shiny::runApp('~/Documents/GitRprojects/LaForge/shiny/twitter_dash_2panel_study_d3/app.R', port = 4710)"
# /Library/Frameworks/R.framework/Resources/bin/Rscript -e "shiny::runApp('~/Documents/GitRprojects/LaForge/shiny/twitter_dash_2panel_study_d3/app.R', port = 4710)"

options(warn=-1)

options(tidyverse.quiet = TRUE)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(lubridate)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(tricordR)

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

# source("~/Documents/GitRprojects/LaForge/functions/dashboard_functions.R")
# source("~/Documents/GitRprojects/LaForge/functions/schulzFunctions.R")
# source("~/Documents/GitRprojects/LaForge/functions/twitter_scraping_functions.R")
# source("~/Documents/GitRprojects/LaForge/functions/qualtrics_functions.R")

#this takes the alphabetical first token set on the machine
tokenset <- str_remove_all(dir("~/tricordings/tokens"), ".rds")[1]
twitter_tokens <- prepTokens(tokenset, 1:9)

px_panel_1 <- 300
px_panel_2 <- 300

px_l <- 40

dash_theme = "grey_dark"
row_height = 200


##### FUNCTIONS TEMP
require(r2d3)

dotPlot_simple <- function(data_e, data_e_f, days, point_cex){

  midnight_today <- as.POSIXct(paste0(as.character(Sys.Date()), " 00:00:00 EST"))
  time_range <- c((midnight_today-((days )*60*60*24)),midnight_today + 60*60*24)

  date_axis <- seq(time_range[1], time_range[2], by = 60*60*24)

  par(xpd=F, bg="#343E48")
  par(bty="n")

  par(mar=c(4.1, 4.1, 0.1, 4.1))

  plot(x=data_e$created_at, y=data_e$user_index,
       ylim = range(data_e_f$index),
       xlim = time_range,
       col = "grey",
       yaxt="n", ylab = "", pch=15, xaxt="n", cex=point_cex, xlab="")
  axis(side=1, at=date_axis, labels=format(date_axis, "%b %d"),
       cex.axis = axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
}


####



# App title ----
header <- dashboardHeader(title = "Timeline Dashboard"
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
  numericInput("days_back", "Days Back", value = 50, min = 1, max = 365, step = 1),
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

  br(),
  actionButton("fetch_tweets","Fetch Tweets", icon("download"), style = "color:#CDCDCD; background-color: #44505A; border-color: #4C5A67"),
  actionButton("fetch_survey","Fetch Survey", icon("download"), style = "color:#CDCDCD; background-color: #44505A; border-color: #4C5A67")
  #br(),br(),br(),

)



# Main panel for displaying outputs ----
body <- dashboardBody(
  shinyDashboardThemes(theme = dash_theme),

  fluidRow(column(width = 8,
                  box(title=textOutput("panel_1_name"),
                      width = 12,
                      plotOutput("panel_1s_ts_d", height = px_panel_1, width = "100%") %>%
                        withSpinner(color="#777777", type=8, size = spinner_size),
                  ),

                  box(title=textOutput("panel_2_name"),
                      width = 12,
                      div(id = "htmlwidget_container",
                          d3Output("panel_2s_ts_d", height = px_panel_2, width = "100%")
                      )
                  )),
           box(title="Tweet Details",
               width = 4,
               htmlOutput("tweet_viewer"))
  )


)


#test_text <- "test_text_yay"

ui <- dashboardPage(header, sidebar, body)

# Data pre-processing ----

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  output$tweet_viewer <- renderText(paste0("From: ",input$tweet_screen_name,"<br>",
                                           "At: ",input$tweet_created_at,"<br>",
                                           "Sentiment: ",round(as.numeric(input$tweet_sentiment),2),"<br>",
                                           input$tweet_text))

  observeEvent(input$fetch_tweets, {
    #source("/Users/wschulz/Documents/GitRprojects/LaForge/scripts/qualtrics_fetch.R")
    scrapeStudy(study_name = input$study_name,
                tokens = twitter_tokens,
                include_timelines = T,
                include_friends = F,
                include_followers = F,
                include_favorites = F)
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

  output$panel_1s_ts_d <- renderPlot({
    invalidateLater(refresh_time)
    timeline_data <- timeline_data_panel_1s()
    dotPlot(timeline_data[[1]], timeline_data[[2]], input$days_back, input$color_variable, input$show_names, sentiment_left_color, sentiment_right_color, ideo_left_color, ideo_right_color, point_cex, axis_cex,
            sentiment_reference_scale = my_sentiment_reference_scale,
            ideo_reference_scale = my_ideo_reference_scale)
  })

  output$panel_2s_ts_d <- renderD3({
    invalidateLater(refresh_time)
    timeline_data <- timeline_data_panel_2s()
    timeline_data[[1]] <- timeline_data[[1]] %>% filter(created_at > (Sys.time() - 60*60*24*input$days_back))
    data <- data.frame(x = (1000*as.numeric(timeline_data[[1]]$created_at)),
                       y = timeline_data[[1]]$user_index,
                       text = timeline_data[[1]]$text,
                       sentiment = timeline_data[[1]]$score,
                       screen_name = timeline_data[[1]]$screen_name,
                       created_at = timeline_data[[1]]$created_at,
                       size = 4)
    options(r2d3.theme = list(
      background = "#343E48",
      foreground = "#808080")
    )
    r2d3(data=data,
         script = "~/Documents/GitRprojects/LaForge/dev/d3_scripts/scatter5.js",
         options = list(margin_top = 6,
                        margin_bottom = 60,
                        margin_sides = 20,
                        colour = c("rgba(0,255,0,1)"),
                        hovercolour = "green",
                        xLabel = "",
                        yLabel = "",
                        xmin = min(data$x),
                        xmax = max(data$x),
                        ymin = min(data$y),
                        ymax = max(data$y),
                        chartTitle = ""
         )
    )
  })

}

shinyApp(ui, server)

