
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

refresh_time=5*60*1000 #(milliseconds)
spinner_size <- .5

dash_theme = "grey_dark"
row_height = 200

my_pch <- 16

legend_reference <- data.frame(scraping_functions = c("updateTimelines", "getFriendsBig", "getFollowersBig", "new_lookup_firstScrape", "rate_limit"),
                               color_code = c("blue", "green", "orange", "magenta", "brown"))

# screen_name_label_cols <- c("white", "white", "red")
# default_screen_name_fonts <- c(2, 2, 4)
#
# point_cex <- .9
# default_axis_cex <- 1
# default_screen_name_cex <- .8
# default_point_color <- "rgba(200,200,200,.5)"
# default_point_hover_color <- "rgba(108, 196, 20,.9)"
#
# # my_red <- rgb(1,.5,.5)
# # my_blue <- rgb(.5,.5,1)
# my_red <- rgb(1,1,1,.7)
# my_blue <- rgb(1,1,1,.7)
#
# my_sentiment_reference_scale = c(-1,1)
# my_sentiment_reference_scale = NA
# sentiment_left_color = c(1,.2,0,.7)
# sentiment_right_color = c(0,1,0,.7)
#
# #my_ideo_reference_scale = c(-.1,.1)
# my_ideo_reference_scale = NA
# ideo_left_color = c(.5,.5,1,.3)
# ideo_right_color = c(.9,0,0,.3)
#
# sure_left_color = c(.3,.5,.9,.7)
# sure_right_color = c(.7,.7,0,.7)
#
# survey_cumulation_colors <- c(rgb(.2,.8,.2),rgb(.8,0,0))



tokensets_dirs <- dir("~/tricordings/tokens", full.names = T)
tokenset_names <- str_remove_all(string = tokensets_dirs, pattern = ".*tricordings/tokens/") %>% str_remove_all(string = ., pattern = ".rds")

tokensets_infos_list <- list()
for(i in seq_along(tokensets_dirs)){
  rm(tokenset_info)

  tokenset <- readRDS(tokensets_dirs[[i]])
  tokenset_info <- list()
  tokenset_info$name <- tokenset_names[i]

  token_keys <- c()
  for (j in 1:length(tokenset)) {
    token_keys[j] <- tokenset[[j]]$app$key
  }
  tokenset_info$keys <- token_keys
  tokensets_infos_list[[i]] <- tokenset_info
}

prep_token_data <- function(tokensets_infos_list, tokenset_name, hours_back) {
  tokenset_info <- tokensets_infos_list[[which(tokenset_names==tokenset_name)]]

  token_log <- dir("~/tricordings/logs/token_usage/", full.names = T) %>% map_dfr(., readRDS)

  token_log_filtered <- token_log %>%
    filter(difftime(Sys.time(), time, units = "hours")<=hours_back) %>%
    filter(key %in% tokenset_info$keys)

  return(token_log_filtered)
}


#scatter_script_path <- system.file("d3", "scatter_grey.js", package = "tricordR")

##### FUNCTIONS TEMP
#require(r2d3)




####



# App title ----
header <- dashboardHeader(title = "Token Dashboard"
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
  selectInput("tokenset_name", "Token Set", choices = tokenset_names),
  numericInput("hours_back", "Hours Back", value = 1, min = 1, max = 480, step = 1)
)



# Main panel for displaying outputs ----
body <- dashboardBody(
  shinyDashboardThemes(theme = dash_theme),

  fluidRow(column(width = 12,

                  box(title="Token Usage",
                      width = 12,
                      # div(id = "htmlwidget_container",
                      #     r2d3::d3Output("panel_1s_ts_d", height = px_panel_1, width = "100%") %>% withSpinner(color="#777777", type=8, size = spinner_size)
                      # )
                      plotOutput(outputId = "token_usage_plot")#target
                  ),

                  )
  )


)


#test_text <- "test_text_yay"

ui <- dashboardPage(header, sidebar, body)

# Data pre-processing ----

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  token_data <- reactive({
    invalidateLater(refresh_time)
    prep_token_data(tokensets_infos_list = tokensets_infos_list, tokenset_name = input$tokenset_name, hours_back = input$hours_back)
  })

  output$token_usage_plot <- renderPlot({
    invalidateLater(refresh_time)
    token_log <- token_data()

    type_colors <- case_when(token_log$scraping_function == legend_reference$scraping_functions[1] ~ legend_reference$color_code[1],
                             token_log$scraping_function == legend_reference$scraping_functions[2] ~ legend_reference$color_code[2],
                             token_log$scraping_function == legend_reference$scraping_functions[3] ~ legend_reference$color_code[3],
                             token_log$scraping_function == legend_reference$scraping_functions[4] ~ legend_reference$color_code[4],
                             str_detect(token_log$scraping_function, legend_reference$scraping_functions[5]) ~ legend_reference$color_code[5])

    par(xpd=T, bty = "L", bg = "#343E48", fg = "gray", col.axis = "gray", col.lab = "gray", mar = c(5.1, 4.1, 4.1, 14.1))
    plot(token_log$time, token_log$object_bytes, col = type_colors, xlab = "Time", ylab = "Bytes Returned", pch = my_pch)
    #points(token_log$time[which(token_log$object_bytes==0)], token_log$object_bytes[which(token_log$object_bytes==0)], col="red", pch = 4)
    #legend(x = max(token_log$time), y = mean(range(token_log$object_bytes)), legend = legend_reference$scraping_functions, pch = my_pch, col = legend_reference$color_code, xjust = 0, yjust = .5)
    legend(x = max(token_log$time), y = mean(range(token_log$object_bytes)), legend = legend_reference$scraping_functions, pch = my_pch, col = legend_reference$color_code, xjust = 0, yjust = .5, bty = "n")
  })

  # output$panel_1s_ts_d <- r2d3::renderD3({
  #   invalidateLater(refresh_time)
  #   timeline_data <- timeline_data_panel_1s()
  #   timeline_data[[1]] <- timeline_data[[1]] %>% filter(created_at > (Sys.time() - 60*60*24*input$days_back))
  #   data <- data.frame(x = (1000*as.numeric(timeline_data[[1]]$created_at)),
  #                      y = timeline_data[[1]]$user_index,
  #                      text = timeline_data[[1]]$text,
  #                      sentiment = timeline_data[[1]]$score,
  #                      screen_name = timeline_data[[1]]$screen_name,
  #                      created_at = timeline_data[[1]]$created_at,
  #                      size = 4)
  #   options(r2d3.theme = list(
  #     background = "#343E48",
  #     foreground = "#808080")
  #   )
  #   r2d3::r2d3(data=data,
  #              script = scatter_script_path,
  #              options = list(margin_top = 6,
  #                             margin_bottom = 60,
  #                             margin_sides = 20,
  #                             colour = c(default_point_color),
  #                             hovercolour = c(default_point_hover_color),
  #                             xLabel = "",
  #                             yLabel = "",
  #                             xmin = min(data$x),
  #                             xmax = max(data$x),
  #                             ymin = min(data$y),
  #                             ymax = max(data$y),
  #                             chartTitle = ""
  #              )
  #   )
  # })

}

shinyApp(ui, server)

