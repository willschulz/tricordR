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

#survey_start_date <- "2021-06-01 12:00:00 EST"

refresh_time=5*60*1000 #(milliseconds)
spinner_size <- .5

assignment_node_col <- "gray50"
participant_node_col <- "dodgerblue2"

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

study_name <- "spirals_bad_pilot"
participant_panel <- "participants"
assignment_panel <- "assignments"

dash_theme = "grey_dark"


# App title ----
header <- dashboardHeader(title = "Spirals Network Dashboard"
)

sidebar <- dashboardSidebar(#width=12,
#  selectInput("study_name", "Study Name", choices = )
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

  experiment_directory <- reactive({paste0(tricordings_directory,"/", study_name, "/")})
  panels <- reactive({dir(experiment_directory())})

  output$panel_1_name <- renderText({
    panels()[1]
  })

  output$panel_2_name <- renderText({
    panels()[2]
  })


  network_data_prepped_d3 <- reactive({invalidateLater(refresh_time)
    prep_network_data_d3(study_name,participant_panel,assignment_panel)
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

    fn <- networkD3::forceNetwork(Links = myLinks, Nodes = myNodes, Value = "value", Source = "source", Target = "target", NodeID = "screen_name", Group = "group", opacity = 1, arrows = T, fontSize = 20, fontFamily = "helvetica", legend=T,
                       linkColour = myLinks$color, charge = -10, zoom = F, linkDistance = 80, clickAction = MyClickScript,
                       colourScale = paste0("d3.scaleOrdinal().domain(['assignment','participant']).range([",
                                            paste0("\'",paste(gplots::col2hex(c(assignment_node_col,
                                                                                participant_node_col)),
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
