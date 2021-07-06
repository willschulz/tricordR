########################################
# Network DASH D3
#########################################

# R -e "shiny::runApp('~/Documents/GitRprojects/LaForge/shiny/survey_dash/app.R', port = 4712)"
# /Library/Frameworks/R.framework/Resources/bin/Rscript -e "shiny::runApp('~/Documents/GitRprojects/LaForge/shiny/survey_dash/app.R', port = 4712)"

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

survey_start_date <- "2021-06-01 12:00:00 EST"

refresh_time=5*60*1000 #(milliseconds)
spinner_size <- .5

screen_name_label_cols <- c("white", "white", "red")
default_screen_name_fonts <- c(2, 2, 4)

point_cex <- .9
default_axis_cex <- 1
default_screen_name_cex <- .8
default_point_color <- "rgba(200,200,200,.5)"
default_point_hover_color <- "rgba(108, 196, 20,.9)"

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

# source("~/Documents/GitRprojects/LaForge/functions/twitter_scraping_functions.R")
# source("~/Documents/GitRprojects/LaForge/functions/qualtrics_functions.R")
#
# #this takes the alphabetical first token set on the machine
# tokenset <- str_remove_all(dir("~/tricordings/tokens"), ".rds")[1]
# twitter_tokens <- prepTokens(tokenset, 1:9)

dash_theme = "grey_dark"


# App title ----
header <- dashboardHeader(title = "Network Dashboard"
)

sidebar <- dashboardSidebar(#width=12,
  selectInput("study_name", "Study Name", choices = study_names)
)

# Main panel for displaying outputs ----
body <- dashboardBody(
  shinyDashboardThemes(theme = dash_theme),

  fluidRow(width=12,
           box(title="Network",
               width = 12,
               networkD3::forceNetworkOutput("network_graph_d3", height = paste0(650, "px"), width = "100%") %>%
                 withSpinner(color="#777777", type=8)
               ,dataTableOutput("data_table") %>%
                 withSpinner(color="#777777", type=8)
           ),
  )
)#prop_followers_assigned

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {

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

  assignment_panel <- reactive({#this could be made more intelligent, but good enough for now
    for (i in 1:2){
      if (is.null(readRDS(paste0(experiment_directory(), panels()[i], "/scrape_settings.rds"))$qualtrics_survey_id)){
        assignment_panel <- panels()[i]
      }
    }
    return(assignment_panel)
  })

  survey_data_prepped <- reactive({invalidateLater(refresh_time)
    prep_survey_data(experiment_directory(),participant_panel())
  })


  output$survey_ts <- renderPlot({invalidateLater(refresh_time)

    surveys_df <- survey_data_prepped()[[1]]
    survey_data_joined_GOOD <- survey_data_prepped()[[2]]

    survey_start_date <- floor_date(as.POSIXct(survey_start_date), "day")
    message(survey_start_date)

    survey_ts <- make_survey_timeseries(surveys_df, 60, survey_data_joined_GOOD, survey_start_date)
    survey_cumulative <- ts_to_cumulative(survey_ts)

    data <- as.matrix(survey_cumulative[,c(3,4)])
    rownames(data) <- survey_cumulative$minute_span

    data <- t(data)
    cumulation_x_axis_indices <- seq(1, ncol(data), length.out = 12)
    cumulation_x_axis_marks <- colnames(data)[cumulation_x_axis_indices]
    cumulation_date_labels <- as.POSIXct(as.numeric(cumulation_x_axis_marks), origin = "1970-01-01")
    cumulation_date_labels <- c(min(cumulation_date_labels),rep("",length(cumulation_date_labels)-2),max(cumulation_date_labels))

    cumulation_y_axis <- c(0, max(survey_cumulative$count))

    par(bty="n",
        bg="#343E48",
        xpd=T,
        mar=c(4.1,4.1,2.5,4.1))

    barplot(data, col = survey_cumulation_colors, border=NA, space=0, xaxt="n", yaxt="n")
    axis(side=1, at=cumulation_x_axis_indices, labels=format(cumulation_date_labels, "%b %d"),
         cex.axis = axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
    axis(side=2, at=cumulation_y_axis, cex.axis = axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
    #legend("topleft", legend = c("Tweets Scraped", "Tweets Not Scraped"), fill = survey_cumulation_colors, border = NA, bty = "n", text.col = "gray")
    legend(x=min(cumulation_x_axis_indices), y=max(cumulation_y_axis)*1.4, legend = c("Tweets Scraped", "Tweets Not Scraped"), fill = survey_cumulation_colors, border = NA, bty = "n", text.col = "gray")
  })

  network_data_prepped <- reactive({invalidateLater(refresh_time)
    prep_network_data_igraph(experiment_directory(),participant_panel(),assignment_panel())
  })

  network_data_prepped_d3 <- reactive({invalidateLater(refresh_time)
    prep_network_data_d3(experiment_directory(),participant_panel(),assignment_panel())
  })


  output$network_graph_d3 <- networkD3::renderForceNetwork({invalidateLater(refresh_time)
    #par(bg="#343E48", fg="grey", mar = c(0,0,0,0))

    myNodes <- network_data_prepped_d3()$v %>% add_column(NodeID = 1:nrow(.)-1, .before = 0)

    myLinks <- network_data_prepped_d3()$e %>% mutate("source" = nodeIndexer(user, myNodes),
                                                      "target" = nodeIndexer(user_id, myNodes),
                                                      "value" = 2
    )

    #MyClickScript <- 'alert("You clicked " + d.name);'

    MyClickScript <- "Shiny.setInputValue('user_text', d.name);"

    fn <- forceNetwork(Links = myLinks, Nodes = myNodes, Value = "value", Source = "source", Target = "target", NodeID = "screen_name", Group = "group", opacity = 1, arrows = T, fontSize = 20, fontFamily = "helvetica", legend=T,
                       linkColour = myLinks$color, charge = -50, zoom = F, linkDistance = 80, clickAction = MyClickScript,
                       colourScale = paste0("d3.scaleOrdinal().domain(['assignment','placeboed','treated']).range([",
                                            paste0("\'",paste(gplots::col2hex(c(assignment_node_col,
                                                                                placeboed_node_col,
                                                                                treated_node_col)),
                                                              collapse = "\', \'"),"\'")
                                            ,"]);"))
    #make legend text white
    htmlwidgets::onRender(
      fn,
      'function(el, x) {
    d3.select("body").style("background-color", "#144370");
    d3.selectAll(".legend text").style("fill", "white");
  }'
    )
  })

  options_reactive <- reactive({list(pageLength = 5, editable = F,
                                     lengthChange = FALSE,
                                     search = list(search = input$user_text))})

  output$data_table <- renderDataTable(network_data_prepped_d3()[[2]] %>% select(ResponseId, user_id, screen_name, group),
                                       options = options_reactive)

  # text output
  output$text <- renderText({
    input$user_text
  })

}

shinyApp(ui, server)
