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

survey_start_date <- "2021-07-01 12:00:00 EST"

#refresh_time=5*60*1000 #(milliseconds)
refresh_time=60*60*1000 #(milliseconds)

survey_cumulation_colors <- c(rgb(.2,.8,.2),rgb(.8,0,0))
default_axis_cex <- 1

assignment_node_col <- "gray40"
placeboed_node_col <- "plum"
treated_node_col <- "dodgerblue2"

tricordings_directory <- "~/tricordings/studies/"

study_name <- "spirals_bad_pilot"
participant_panel <- "participants"
assignment_panel <- "assignments"

#######
prep_network_data_d3_spirals <- function(study_name, panel_name, assignment_panel = "assignments", include_protected = TRUE, verbose = TRUE){

  if(verbose){message("Loading id_links ...")}
  id_links <- dir(paste0("~/tricordings/studies/", study_name, "/", panel_name, "/id_links_confirmed/"), full.names = T) %>% map_dfr(., readRDS)# %>% filter(ResponseId != "R_3fO7aQmR13LJ4zs") #target - remember to remove this filter and simply prevent duplicates in future
  id_links <- id_links[!duplicated(id_links$ResponseId, fromLast = T),]
  if(verbose){message("nrow id_links: ", nrow(id_links))}

  p_friends_all <- dir(paste0("~/tricordings/studies/", study_name, "/", panel_name, "/twitter_scrapes/friends/"), full.names = T) %>% map_dfr(., readRDS)

  if (nrow(p_friends_all)==0) {p_friends_all <- dir(paste0("~/tricordings/studies/", "/", study_name, "/", panel_name, "/twitter_scrapes/first_friends/"), full.names = T)[1] %>% map_dfr(., readRDS)}


  relevant_user_ids <- unique(id_links$user_id) %>% .[which(!is.na(.))]# %>% .[which(! . %in% unique(p_friends_all$user))]

  if (include_protected){
    a_followers_relevant <- dir(paste0("~/tricordings/studies/", study_name, "/", assignment_panel, "/twitter_scrapes/followers/"), full.names = T) %>% map_dfr(., readRelevantAssignmentFollowers, relevant_user_ids = relevant_user_ids)
    protected_relevant_friends <- a_followers_relevant %>% transmute(userx = user_id, user_idx = user, scraped_at) %>% rename("user" = userx, "user_id" = user_idx)
    p_friends_all <- rbind(p_friends_all, protected_relevant_friends)
  }

  par_info <- dir(paste0("~/tricordings/studies/", study_name, "/", panel_name, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "participant") %>% filter(user_id %in% id_links$user_id)
  ass_info <- dir(paste0("~/tricordings/studies/", study_name, "/", assignment_panel, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "assignment")
  all_info <- rbind(par_info, ass_info)

  survey_responses <- prep_survey_data(paste0("~/tricordings/studies/", study_name, "/"), panel_name)[[1]] %>% distinct(ResponseId, .keep_all = T) %>% filter(twitter_agreement=="Yes")# %>% filter(ResponseId != "R_3fO7aQmR13LJ4zs") #target - remember to remove this filter and simply prevent duplicates in future
  vertex_metadata <- id_links %>% select(ResponseId, start_date, shown, claimed, user_id) %>% left_join(., survey_responses %>% select(-c(shown, claimed, user_id)), by="ResponseId") %>% full_join(., all_info)
  #vertex_metadata <- all_info

  na_user_ids_indices <- which(is.na(vertex_metadata$user_id))
  for(i in 1:length(na_user_ids_indices)){
    vertex_metadata$user_id[na_user_ids_indices[i]] <- paste0("UNMATCHED_",i)
    vertex_metadata$screen_name[na_user_ids_indices[i]] <- paste0("UNMATCHED_",i)
  }

  vertex_metadata$stroke_group <- "none"
  vertex_metadata$stroke_group[na_user_ids_indices] <- "unmatched"
  vertex_metadata$stroke_group[which(vertex_metadata$protected)] <- "protected"

  vertex_metadata$stroke_color <- "white"
  vertex_metadata$stroke_color[na_user_ids_indices] <- "red"
  vertex_metadata$stroke_color[which(vertex_metadata$protected)] <- "orange"

  vertex_metadata$group[which(vertex_metadata$t==1)] <- "treated"
  vertex_metadata$group[which(vertex_metadata$t==0)] <- "placeboed"

  p_friends_all <- left_join(p_friends_all, (vertex_metadata %>% select(user_id, start_date)), by = c("user" = "user_id"))

  data <- p_friends_all %>%
    filter(user_id %in% vertex_metadata$user_id) %>%
    group_by(user, user_id) %>%
    summarise(first = min(scraped_at),
              last = max(scraped_at),
              start_date = start_date[1]
              # color = ifelse(difftime(Sys.time(), last, units = "day")>1,
              #                "orange",
              #                "green")
    ) %>%
    mutate(color = case_when((difftime(Sys.time(), last, units = "day")>1) ~ "orange",
                             ((difftime(Sys.time(), last, units = "day")<=1) & difftime(first, start_date, units = "day")>=0) ~ "green",
                             (difftime(first, start_date, units = "day")<0) ~ "blue")) %>% select(-start_date)


  #always-inactive claims should be red:
  data <- map_dfr(which((vertex_metadata$group!="assignment") & (sapply(vertex_metadata$claimed, length)>0)),
                  ~data.frame("user" = vertex_metadata$user_id[.x],
                              "user_id" = vertex_metadata$claimed[[.x]],
                              first = as.POSIXct(NA), last = as.POSIXct(NA), color = "red")) %>%
    anti_join(., data, by = c("user", "user_id")) %>%
    rbind(., data)

  #shown and not claimed should be gray
  data <- map_dfr(which(vertex_metadata$group!="assignment"),
                  ~data.frame("user" = vertex_metadata$user_id[.x],
                              "user_id" = vertex_metadata$shown[[.x]],
                              first = as.POSIXct(NA), last = as.POSIXct(NA), color = "gray")) %>%
    anti_join(., data, by = c("user", "user_id")) %>%
    rbind(., data)
  if(verbose){message("nrow e: ", nrow(data))}
  if(verbose){message("nrow v: ", nrow(vertex_metadata))}

  return(list("e" = data, "v" = vertex_metadata))
}
########

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
               networkD3::forceNetworkOutput("network_graph_d3", height = paste0(650, "px"), width = "100%") %>%
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

  participant_group <- reactive({
    for (i in 1:2){
      if (!is.null(readRDS(paste0(experiment_directory(), groups()[i], "/scrape_settings.rds"))$qualtrics_survey_id)){
        participant_group <- groups()[i]
      }
    }
    return(participant_group)
  })

  assignment_group <- reactive({#this could be made more intelligent, but good enough for now
    for (i in 1:2){
      if (is.null(readRDS(paste0(experiment_directory(), groups()[i], "/scrape_settings.rds"))$qualtrics_survey_id)){
        assignment_group <- groups()[i]
      }
    }
    return(assignment_group)
  })

  survey_data_prepped <- reactive({invalidateLater(refresh_time)
    prep_survey_data(experiment_directory(),participant_group())
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

  network_data_prepped_d3 <- reactive({#invalidateLater(refresh_time) #commented out in repair attempt
    message("Prepping network data...")
    #prep_network_data_d3_spirals(study_name,participant_group(),assignment_group())
    message("Study name: ",study_name)
    message("Participant group: ",participant_group())
    message("Assignment group: ",assignment_group())
    prep_network_data_d3_spirals(study_name = study_name,panel_name = participant_group(),assignment_panel = assignment_group(), include_protected = F)#note: including protected creates a huge data lift in the long term... figure out how to mitigate
    #prep_network_data_d3_spirals(study_name = "spirals_bad_pilot",panel_name = "participants", assignment_panel = "assignments", include_protected = F)
    #message("Network data prepped!")
  })

  output$network_graph_d3 <- networkD3::renderForceNetwork({invalidateLater(refresh_time)
    #par(bg="#343E48", fg="grey", mar = c(0,0,0,0))

    data <- network_data_prepped_d3()
    message(length(data))
    message(length(data$v))
    message(length(data$e))

    message("Organizing nodes...")
    myNodes <- data$v %>% add_column(NodeID = 1:nrow(.)-1, .before = 0)
    message("myNodes has nrow: ", nrow(myNodes))

    message("Organizing links...")
    myLinks <- data$e %>% mutate("source" = nodeIndexer(user, myNodes),
                                 "target" = nodeIndexer(user_id, myNodes),
                                 "value" = 2
    )
    message("myLinks has nrow: ", nrow(myLinks))

    #MyClickScript <- 'alert("You clicked " + d.name);'

    MyClickScript <- "Shiny.setInputValue('user_text', d.name);"

    message("Generating forceNetwork...")
    fn <- networkD3::forceNetwork(Links = myLinks, Nodes = myNodes, Value = "value", Source = "source", Target = "target", NodeID = "screen_name", Group = "group", opacity = 1, arrows = T, fontSize = 20, fontFamily = "helvetica", legend=T,
                 linkColour = myLinks$color, charge = -10, zoom = F, linkDistance = 80,
                 #clickAction = MyClickScript,  #commented out to fix "argument of length 0)"
                 bounded = T,
                 colourScale = paste0("d3.scaleOrdinal().domain(['assignment','placeboed','treated']).range([",
                                      paste0("\'",paste(gplots::col2hex(c(assignment_node_col,
                                                                          placeboed_node_col,
                                                                          treated_node_col)),
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
                                     lengthChange = FALSE#,
                                     #search = list(search = input$user_text) #commented out to fix "argument of length 0)"
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
