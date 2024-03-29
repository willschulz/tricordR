#launch dash net
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

survey_start_date <- "2022-05-13 12:00:00 EST"

#refresh_time=5*60*1000 #(milliseconds)
refresh_time=60*60*1000 #(milliseconds)

survey_cumulation_colors <- c(rgb(.2,.8,.2),rgb(.8,0,0))
default_axis_cex <- 1

assignment_node_col <- "gray40"
participant_node_col <- "plum"

placeboed_node_col <- "plum"
treated_node_col <- "dodgerblue2"

tricordings_directory <- "~/tricordings/studies/"

study_name <- "spirals_experiment"
participant_panel <- "participants"
assignment_panel <- "assignments"

# study_names <- dir("~/laforge_files/studies", full.names = T) %>%
#   dir(full.names = T) %>%
#   dir(full.names = T) %>%
#   .[str_detect(., "scrape_settings.rds")] %>%
#   file.info() %>%
#   mutate(path = rownames(.)) %>%
#   mutate(study_name = path %>% str_remove_all(".*/studies/") %>% str_remove_all("/.*")) %>%
#   group_by(study_name) %>%
#   summarise(most_recent = max(mtime), count = n()) %>%
#   arrange(desc(count), desc(most_recent)) %>%
#   pull(study_name)

#source("~/Documents/GitRprojects/LaForge/functions/dashboard_functions.R")
#source("~/Documents/GitRprojects/LaForge/functions/schulzFunctions.R")

# source("~/Documents/GitRprojects/LaForge/functions/twitter_scraping_functions.R")
# source("~/Documents/GitRprojects/LaForge/functions/qualtrics_functions.R")
#
# #this takes the alphabetical first token set on the machine
# tokenset <- str_remove_all(dir("~/laforge_files/tokens"), "_tokenslist.rds")[1]
# twitter_tokens <- prep_tokens(tokenset, 1:9)

dash_theme = "grey_dark"


# App title ----
header <- dashboardHeader(title = "Compliance Dashboard"
                          # title = shinyDashboardLogoDIY(
                          #   boldText = "LaForge",
                          #   mainText = "Survey",
                          #   textSize = 16,
                          #   badgeText = " v1.0",
                          #   badgeTextColor = "white",
                          #   badgeTextSize = 2,
                          #   badgeBackColor = "#343E48",
                          #   badgeBorderRadius = 3
                          # )
)

sidebar <- dashboardSidebar(#width=12,
  #selectInput("study_name", "Study Name", choices = study_names)
  checkboxInput(inputId = "include_protected", label = "Include Protected", value = TRUE),
  checkboxInput(inputId = "anonymize_participants", label = "Anonymize Participants", value = TRUE)
)

# Main panel for displaying outputs ----
body <- dashboardBody(
  shinyDashboardThemes(theme = dash_theme),

  fluidRow(
    column(width=4,
           box(title="Survey Data Collection (Cumulative)",
               width = 12,
               plotOutput("survey_ts", height = paste0(150, "px"), width = "100%") %>%
                 withSpinner(color="#777777", type=8)
           )
           # ,box(title="Lookup",
           #     width = 12, height = "600px",
           #     dataTableOutput("data_table") %>%
           #       withSpinner(color="#777777", type=8)
           # )
    ),
    column(width=8,
           box(title="Network",
               width = 12,
               networkD3::forceNetworkOutput("network_graph_d3", height = paste0(450, "px"), width = "100%") %>%
                 withSpinner(color="#777777", type=8)
               ,dataTableOutput("data_table") %>%
                 withSpinner(color="#777777", type=8)
           ),
    )
  )
)#prop_followers_assigned

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {

  experiment_directory <- reactive({paste0(tricordings_directory,"/", study_name, "/")})
  groups <- reactive({dir(experiment_directory())})

  output$group_1_name <- renderText({
    groups()[1]
  })

  output$group_2_name <- renderText({
    groups()[2]
  })


  participant_group <- "participants"
  assignment_group <- "assignments"
  # participant_group <- reactive({
  #   for (i in 1:2){
  #     if (!is.null(readRDS(paste0(experiment_directory(), groups()[i], "/scrape_settings.rds"))$qualtrics_survey_id)){
  #       participant_group <- groups()[i]
  #     }
  #   }
  #   return(participant_group)
  # })
  #
  # assignment_group <- reactive({#this could be made more intelligent, but good enough for now
  #   for (i in 1:2){
  #     if (is.null(readRDS(paste0(experiment_directory(), groups()[i], "/scrape_settings.rds"))$qualtrics_survey_id)){
  #       assignment_group <- groups()[i]
  #     }
  #   }
  #   return(assignment_group)
  # })

  survey_data_prepped <- reactive({invalidateLater(refresh_time)
    prep_survey_data(experiment_directory(),participant_group)
  })


  output$survey_ts <- renderPlot({invalidateLater(refresh_time)

    message("Loading survey data...")
    surveys_df <- survey_data_prepped()[[1]]
    survey_data_joined_GOOD <- survey_data_prepped()[[2]]

    survey_start_date <- floor_date(as.POSIXct(survey_start_date), "day")
    message(survey_start_date)

    message("Making survey time series...")
    survey_ts <- make_survey_timeseries(surveys_df, 60, survey_data_joined_GOOD, survey_start_date)
    survey_cumulative <- ts_to_cumulative(survey_ts)

    data <- as.matrix(survey_cumulative[,c(3,4)])
    rownames(data) <- survey_cumulative$minute_span

    message("Generating survey plot...")
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
         cex.axis = default_axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
    axis(side=2, at=cumulation_y_axis, cex.axis = default_axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
    #legend("topleft", legend = c("Tweets Scraped", "Tweets Not Scraped"), fill = survey_cumulation_colors, border = NA, bty = "n", text.col = "gray")
    legend(x=min(cumulation_x_axis_indices), y=max(cumulation_y_axis)*1.4, legend = c("Tweets Scraped", "Tweets Not Scraped"), fill = survey_cumulation_colors, border = NA, bty = "n", text.col = "gray")
    message("Survey plot generated!")
  })

  network_data_prepped_d3 <- reactive({invalidateLater(refresh_time)
    message("Prepping network data...")
    message("Study name: ", study_name)
    message("Participant group: ", participant_group)
    message("Assignment group: ", assignment_group)
    #prep_network_data_d3_spirals(study_name,participant_group,assignment_group)
    prep_network_data_d3_spirals(study_name,participant_group,assignment_group, include_protected = input$include_protected, compliance_threshold_daycount = 1, anonymize_participants = input$anonymize_participants)#note: including protected creates a huge data lift in the long term... figure out how to mitigate
  })


  output$network_graph_d3 <- networkD3::renderForceNetwork({invalidateLater(refresh_time)
    #par(bg="#343E48", fg="grey", mar = c(0,0,0,0))

    message("Organizing nodes...")
    myNodes <- network_data_prepped_d3()$v %>% add_column(NodeID = 1:nrow(.)-1, .before = 0)

    message("Organizing links...")
    myLinks <- network_data_prepped_d3()$e %>% mutate("source" = nodeIndexer(user, myNodes),
                                                      "target" = nodeIndexer(user_id, myNodes),
                                                      "value" = 2
    )

    #MyClickScript <- 'alert("You clicked " + d.name);'

    MyClickScript <- "Shiny.setInputValue('user_text', d.name);"

    message("Generating forceNetwork...")
    # fn <- networkD3::forceNetwork(Links = myLinks, Nodes = myNodes, Value = "value", Source = "source", Target = "target", NodeID = "screen_name", Group = "group", opacity = 1, arrows = T, fontSize = 20, fontFamily = "helvetica", legend=T,
    #                               linkColour = myLinks$color, charge = -10, zoom = F, linkDistance = 80,
    #                               clickAction = MyClickScript,  #commented out to fix "argument of length 0)"
    #                               bounded = T,
    #                               colourScale = paste0("d3.scaleOrdinal().domain(['assignment','participant','placeboed','treated']).range([",
    #                                                    paste0("\'",paste(gplots::col2hex(c(assignment_node_col,
    #                                                                                        participant_node_col,
    #                                                                                        placeboed_node_col,
    #                                                                                        treated_node_col)),
    #                                                                      collapse = "\', \'"),"\'")
    #                                                    ,"]);"))

    fn <- networkD3::forceNetwork(Links = myLinks, Nodes = myNodes, Value = "value", Source = "source", Target = "target", NodeID = "screen_name", Group = "group", opacity = 1, arrows = T, fontSize = 20, fontFamily = "helvetica", legend=T,
                                  linkColour = myLinks$color, charge = -10, zoom = F, linkDistance = 80,
                                  clickAction = MyClickScript,  #commented out to fix "argument of length 0)"
                                  bounded = T,
                                  colourScale = paste0("d3.scaleOrdinal().domain(['assignment','participant']).range([",
                                                       paste0("\'",paste(gplots::col2hex(c(assignment_node_col,
                                                                                           participant_node_col)),
                                                                         collapse = "\', \'"),"\'")
                                                       ,"]);"))
    fn$x$nodes$border <- myNodes$stroke_color

    fn <- htmlwidgets::onRender(fn,
                                'function(el, x) { d3.selectAll("circle").style("stroke", d => d.border); }')

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
                                     search = list(search = input$user_text) #commented out to fix "argument of length 0)"
  )})

  message("Prepping data table ...")
  output$data_table <- renderDataTable(network_data_prepped_d3()[[2]] %>% select(ResponseId, user_id, screen_name, group),
                                       options = options_reactive)

  # text output
  output$text <- renderText({
    message("Rendering text...")
    input$user_text
  })


}

shinyApp(ui, server)
