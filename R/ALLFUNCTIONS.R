# ALL FUNCTIONS

# General Functions

#' Initialization Function
#'
#' This function initializes tricordR by creating the tricordings directory to save data, tokens, and scraping logs.
#' @param location Location of tricordings directory, defaults to home directory (~/) and should not be changed at this time.
#' @keywords initialization
#' @export
#' @examples
#' initialize()

initialize <- function(location = "~/"){
  #create main directory
  tricordings_directory <- paste0(location, "tricordings/")
  dir.create(tricordings_directory)

  #create data directory
  studies_directory <- paste0(tricordings_directory, "studies")
  if (length(dir(studies_directory))>0){
    if (askYesNo(msg = "Data already exists here. Do you want to reset?\n")){
      unlink(x = paste0(studies_directory,"/"), recursive = T)
    }
  }
  dir.create(studies_directory)

  # create tokens directory
  dir.create(paste0(tricordings_directory, "tokens"))

  # create log directory
  dir.create(paste0(tricordings_directory, "logs"))
}

#' Save Tokens for Cycling Functions
#'
#' This function initializes tricordR by creating the tricordings directory to save data, tokens, and scraping logs.
#' @param set_name Name of token set (sets should correspond to Twitter developer accounts)
#' @param consumer_key Your app's consumer key string
#' @param consumer_secret Your app's consumer secret string
#' @param access_token Your app's access token string
#' @param access_secret Your app's access secret string
#' @keywords scraping
#' @export
#' @examples
#' saveToken()

saveToken <- function(set_name, consumer_key, consumer_secret, access_token, access_secret){
  require(rtweet)
  ###
  tricordings_directory <- "~/tricordings/"
  ###
  new_token <- create_token(consumer_key = consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_secret, set_renv = FALSE)
  if(file.exists(paste0(tricordings_directory, "tokens/", set_name, ".rds"))) {
    token_list <- readRDS(paste0(tricordings_directory, "tokens/", set_name, ".rds"))
    #if (list(new_token) %in% token_list) {stop("This token is already in this set.")} #for some reason this fires whether or not the token is already in the set
    message("Appending to existing token set.")
    token_list <- c(token_list, new_token)
    }
  else {
    message("Adding to new token set.")
    token_list <- list(new_token)
    }
  saveRDS(token_list, file = paste0(tricordings_directory, "tokens/", set_name, ".rds"))
}

#' Add a Study
#'
#' This function creates a new study in the studies folder of tricordings, to which panels can be added.
#' @param study_name Name of the study to be added.
#' @keywords management
#' @export
#' @examples
#' addStudy()

# create study
addStudy <- function(study_name){
  ###
  tricordings_directory <- "~/tricordings/"
  ###
  study_directory <- paste0(tricordings_directory, "studies/", study_name)
  if (dir.exists(study_directory)){
    if (askYesNo(msg = "This study already exists. Do you want to reset?\n")){
      unlink(x = paste0(study_directory,"/"), recursive = T)
    }
  }
  dir.create(study_directory)
}

#' Add a Panel to a Study
#'
#' This function creates a new panel in a given study folder of tricordings, specifies desired data types to scrape, and optionally conducts an initial scrape of these data.
#' @param study_name Name of the study to be added.
#' @param panel_name Name of the panel to be added.
#' @param user_ids Twitter user ids of users to be added to the new panel.
#' @param scrape_timelines Should timelines be scraped for this panel? Defaults to TRUE.
#' @param scrape_friends Should friends be scraped for this panel? Defaults to FALSE.
#' @param scrape_followers Should followers be scraped for this panel? Defaults to FALSE.
#' @param scrape_favorites Should favorites be scraped for this panel? Defaults to FALSE.
#' @param first_scrape Should tricordR conduct an initial scrape of the specified data for the specified users?
#' @param tokens List of tokens to use for initial scrape.  See prepTokens.
#' @param max_hours Maximum duration of initial scrape.  Defaults to 1 hour.
#' @param scrape_survey Should tricordR conduct hourly scrapes of survey responses associated with this panel? Defaults to FALSE.
#' @param qualtrics_survey_id If scrape_survey is TRUE, provide a Qualtrics survey id to identify the relevant survey data.  Note: to use this, you must also install your Qualtrics authentication credentials on your machine.
#' @keywords management
#' @export
#' @examples
#' addPanel()

addPanel <- function(study_name, panel_name, user_ids = c(),
                     scrape_timelines = T,
                     scrape_friends = F,
                     scrape_followers = F,
                     scrape_favorites = F,
                     first_scrape = T, tokens, max_hours = 1,
                     scrape_survey = F,
                     qualtrics_survey_id = NULL){
  ###
  tricordings_directory <- "~/tricordings/"
  ###
  #create panel directory
  panel_directory <- paste0(tricordings_directory, "studies/", study_name, "/", panel_name)
  if (dir.exists(panel_directory)){
    if (askYesNo(msg = "This panel already exists. Do you want to reset?\n")){#if you say no, it still proceeds with everything else... fix this!
      unlink(x = paste0(panel_directory,"/"), recursive = T)
    }
  }
  dir.create(panel_directory)

  if (scrape_survey) {
    dir.create(paste0(panel_directory,"/survey_scrapes"))
    dir.create(paste0(panel_directory,"/survey_scrape_logs"))
    dir.create(paste0(panel_directory,"/id_links"))
  }

  if (any(scrape_timelines,scrape_friends,scrape_followers,scrape_favorites)) {
    dir.create(paste0(panel_directory,"/twitter_scrapes"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/user_info"))
    saveRDS(user_ids, paste0(panel_directory, "/twitter_scrapes/user_ids.rds")) #save any user_ids that are available in the twitter_scrapes folder
  }

  if (scrape_timelines) {
    dir.create(paste0(panel_directory,"/twitter_scrapes/timelines"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/first_timelines"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/timeline_attempts"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/timeline_logs"))
  }

  if (scrape_friends) {
    dir.create(paste0(panel_directory,"/twitter_scrapes/friends"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/first_friends"))
  }
  if (scrape_followers) {
    dir.create(paste0(panel_directory,"/twitter_scrapes/followers"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/first_followers"))
  }

  if (scrape_favorites) {
    dir.create(paste0(panel_directory,"/twitter_scrapes/favorites"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/first_favorites"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/favorite_attempts"))
    dir.create(paste0(panel_directory,"/twitter_scrapes/favorite_logs"))
  }

  #save scraping settings
  scrape_settings <- list(qualtrics_survey_id = qualtrics_survey_id,
                          scrape_survey = scrape_survey,
                          scrape_timelines = scrape_timelines,
                          scrape_friends = scrape_friends,
                          scrape_followers = scrape_followers,
                          scrape_favorites = scrape_favorites)
  saveRDS(scrape_settings, paste0(panel_directory, "/scrape_settings.rds"))

  if (first_scrape & (length(user_ids)>0)){
    firstScrape(user_ids = user_ids,
                panel_directory = panel_directory,
                tokens = tokens,
                max_hours = max_hours)
  }
}

#' Edit a Panel's Membership or Scrape Settings
#'
#' This function creates a new panel in a given study folder of tricordings, specifies desired data types to scrape, and optionally conducts an initial scrape of these data.
#' @param study_name Name of the study to be edited.
#' @param panel_name Name of the panel to be edited.
#' @param add_users Twitter user ids of users to be added to the panel.
#' @param remove_users Twitter user ids of users to be removed from the panel.
#' @param scrape_timelines Should timelines be scraped for this panel?
#' @param scrape_friends Should friends be scraped for this panel?
#' @param scrape_followers Should followers be scraped for this panel?
#' @param scrape_favorites Should favorites be scraped for this panel?
#' @param first_scrape Should tricordR conduct an initial scrape of the specified data for the specified users?
#' @param tokens List of tokens to use for first scrape.  See prepTokens.
#' @param max_hours Maximum duration of first scrape.  Defaults to 1 hour.
#' @param scrape_survey Should tricordR conduct hourly scrapes of survey responses associated with this panel? Defaults to FALSE.
#' @param qualtrics_survey_id If scrape_survey is TRUE, provide a Qualtrics survey id to identify the relevant survey data.  Note: to use this, you must also install your Qualtrics authentication credentials on your machine.
#' @keywords management
#' @export
#' @examples
#' editPanel()

editPanel <- function(study_name, panel_name,
                      add_users = NULL,
                      remove_users = NULL,
                      scrape_timelines = NULL,
                      scrape_friends = NULL,
                      scrape_followers = NULL,
                      scrape_favorites = NULL,
                      first_scrape = FALSE,
                      tokens = NULL,
                      max_hours = 1,
                      scrape_survey = NULL,
                      qualtrics_survey_id = NULL){
  ###
  tricordings_directory <- "~/tricordings/"
  ###
  #construct panel directory path
  panel_directory <- paste0(tricordings_directory, "studies/", study_name, "/", panel_name)

  #load scraping settings
  scrape_settings <- readRDS(paste0(panel_directory, "/scrape_settings.rds"))
  #edit scraping settings
  if (!is.null(scrape_survey)){scrape_settings$scrape_survey <- scrape_survey}
  if (!is.null(scrape_timelines)){scrape_settings$scrape_timelines <- scrape_timelines}
  if (!is.null(scrape_friends)){scrape_settings$scrape_friends <- scrape_friends}
  if (!is.null(scrape_followers)){scrape_settings$scrape_followers <- scrape_followers}
  if (!is.null(scrape_favorites)){scrape_settings$scrape_favorites <- scrape_favorites}
  #save scraping settings
  saveRDS(scrape_settings, paste0(panel_directory, "/scrape_settings.rds"))

  #update userid list
  if ((!is.null(add_users)) & (length(add_users)>0)){
    current_user_ids <- readRDS(paste0(panel_directory, "/twitter_scrapes/user_ids.rds"))
    updated_user_ids <- unique(c(current_user_ids, add_users))
    saveRDS(updated_user_ids, paste0(panel_directory, "/twitter_scrapes/user_ids.rds"))
    message((length(updated_user_ids)-length(current_user_ids)), " users added to ", panel_name, " in ", study_name,".")
    message(sum(add_users %in% current_user_ids), " were already in this panel.")
    #target
    if (first_scrape){
      new_userids_toscrape <- updated_user_ids[which(! updated_user_ids %in% current_user_ids)]
      firstScrape(new_userids_toscrape, panel_directory, tokens, max_hours = max_hours)
    }
  }

  if (!is.null(remove_users)){
    current_user_ids <- readRDS(paste0(panel_directory, "/twitter_scrapes/user_ids.rds"))
    if(any(remove_users %in% current_user_ids)){
      updated_user_ids <- unique(current_user_ids[-which(current_user_ids %in% remove_users)])
      saveRDS(updated_user_ids, paste0(panel_directory, "/twitter_scrapes/user_ids.rds"))
      message((-length(updated_user_ids)+length(current_user_ids)), " users removed from user_id list for ", panel_name, " in ", study_name,".")
    }
    message(sum(!remove_users %in% current_user_ids), " were already not in this user_id list.")

    if (file.exists(paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))){
      current_user_lookup <- readRDS(paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))
      if(any(remove_users %in% current_user_lookup$user_id)){
        updated_user_lookup <- current_user_lookup %>% filter(!user_id %in% remove_users)
        saveRDS(updated_user_lookup, paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))
        message((-nrow(updated_user_lookup)+nrow(current_user_lookup)), " users removed from current_user_lookup file for ", panel_name, " in ", study_name,".")
      }
      message(sum(!remove_users %in% current_user_lookup$user_id), " were already not in this lookup file.")
    }
    #still need to add something to remove them from the log, but this is dangerous, so I'll do it carefully later
  }
  print(scrape_settings)
}


# # data management
#
# fixSentiment <- function(input, allowed_range = c(-1,1)){
#   input[which(input<allowed_range[1])] <- allowed_range[1]
#   input[which(input>allowed_range[2])] <- allowed_range[2]
#   return(input)
# }
#
#
# myIndexer <- function(input, reference){
#   myvec <- rep(NA, length(input))
#   for (i in 1:length(input)){
#     myvec[i] <- which(reference==input[i])
#   }
#   return(myvec)
# }
#
# read_and_session <- function(input){
#   return(readRDS(input) %>% mutate(scrape_session = str_remove_all(input, ".*timelines_|.rds")))
# }
#
#
# prep_timeline_data <- function(panel_directory, sessions_back, include_historical = FALSE, load_all_since_first = FALSE){
#   user_ids <- readRDS(file = paste0(panel_directory, "/twitter_scrapes/user_ids.rds")) #bind to available data
#   current_lookup <- readRDS(file = paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))
#
#   scrape_dir <- dir(paste0(panel_directory,"/twitter_scrapes/timelines/"), full.names = TRUE) %>% str_subset("timelines_")
#   if (!load_all_since_first){
#     scrape_dir <- scrape_dir %>% .[(length(.)-min((sessions_back-1), (length(.)-1))):length(.)]
#     scrape_dir <- scrape_dir[-which(is.na(scrape_dir))]
#   }
#
#   if (include_historical){
#     first_timelines_dir <- dir(paste0(panel_directory,"/twitter_scrapes/first_timelines/"), full.names = TRUE)
#     scrape_dir <- c(scrape_dir, first_timelines_dir)
#   }
#
#   timelines_bound <- scrape_dir %>% map_dfr(read_and_session)
#
#   keep_if_there <- c("user_id", "status_id", "screen_name", "created_at", "scrape_session", "is_retweet", "is_quote", "reply_to_user_id", "text", "sentiment_score", "ideology", "sureness")
#
#   e <- timelines_bound %>%
#     distinct(user_id, status_id, .keep_all = T) %>%
#     select(colnames(timelines_bound)[which(colnames(timelines_bound) %in% keep_if_there)]) %>%
#     mutate(scrape_session=as.numeric(scrape_session))
#
#   missing <- keep_if_there[which(! keep_if_there %in% colnames(e))]
#   missing_cols <- array(dim = c(nrow(e),length(missing)))
#   colnames(missing_cols) <- missing
#   e <- cbind(e, missing_cols)
#
#   unique_sessions <- unique(e$scrape_session)
#   unique_users <- unique(e$user_id)
#   e$session_index <- myIndexer(input=e$scrape_session, reference=unique_sessions)
#   e$user_index <- myIndexer(input=e$user_id, reference=unique_users)
#
#   e_f <- e %>% group_by(user_id) %>% summarise(screen_name = screen_name[1],
#                                                user_id = user_id[1],
#                                                last=max(created_at),
#                                                index=user_index[1])
#
#   subset_current_lookup <- current_lookup %>% filter(!(user_id %in% e_f$user_id)) %>% transmute(screen_name, user_id, index = NA)
#   for (i in 1:nrow(subset_current_lookup)){
#     subset_current_lookup$index[i] <- i + max(e_f$index)
#   }
#   e_f <- bind_rows(e_f,subset_current_lookup)
#
#   return(list(e, e_f))
# }
#
#
#
#
# #######################################
# ## TWITTER SCRAPING FUNCTIONS
# #######################################
#
#
# require(tidyverse)
# require(rtweet)
#

#' Prepare Tokens for Cycling
#'
#' This function loads tokens as a list, for use by cycling functions
#' @param tokenset Name of token set to load
#' @param which_tokens A numeric vector indicating which tokens to load from the set.  Defaults to 1:9, loading all tokens.
#' @keywords scraping
#' @export
#' @examples
#' prepTokens()

prepTokens <- function(tokenset, which_tokens = 1:9){
  list_tokens <- readRDS(file = paste0("~/tricordings/tokens/",tokenset,".rds"))
  list_tokens <- list_tokens[which_tokens]
  num_tokens <- length(which_tokens)
  assign(x = "num_tokens", value = num_tokens, envir = .GlobalEnv)
  message(paste("Loaded", num_tokens, "tokens from", tokenset, "\n"), which_tokens)
  return(list_tokens)
}

#' Return Nth Max
#'
#' Order an input by numeric value and return the Nth largest element
#' @param x Numeric vector
#' @param N An integer specifying which element to return. Defaults to 2, returning the second-greatest element.
#' @keywords utility
#' @export
#' @examples
#' maxN()

maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    #warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}

#' Return Nth Max of Character Vector
#'
#' Order a character input by alphanumeric value and return the Nth largest element as a character string
#' @param x Character vector
#' @param N An integer specifying which element to return. Defaults to 2, returning the second-greatest element.
#' @keywords utility
#' @export
#' @examples
#' maxNchar()

maxNchar <- function(x, N=2){
  x <- x[which(!is.na(x))]
  len <- length(x)
  if(N>len){
    #warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sorted_indices <- sort(as.numeric(x), index.return=T)$ix
  sorted_character_vector <- x[sorted_indices]
  return(sorted_character_vector[len-N+1])
}

#' Make Date Code
#'
#' Returns the current date as a string of numbers.
#' @keywords utility
#' @export
#' @examples
#' dateCode()

dateCode <- function(){ #add this function to schulzFunctions?
  require(tidyverse)
  return(as.character(Sys.Date()) %>% str_remove_all(pattern="-") %>% str_sub(., 3))
}

#' Make Time Code
#'
#' Returns the current time as a string of numbers.
#' @keywords utility
#' @export
#' @examples
#' timeCode()

timeCode <- function(){ #add this function to schulzFunctions?
  require(tidyverse)
  return(as.character(Sys.time()) %>% str_remove_all(pattern="-| |:"))
}

# ts_tid <- function(days_back = 1){
#   this_date <- Sys.Date()
#   return(system(paste0("/usr/local/bin/python3 ~/Documents/GitRprojects/LaForge/functions/ts_to_tid.py -y ", year(this_date), " -m ", month(this_date), " -d ", (day(this_date) - days_back)), intern = T))
# } #target - universalize
#
# # Network functions
#
# get_friends_rotate_maxToken <- function(users, n=5000, list_tokens, per_token_limit=15, max_hours=1){
#   require(tidyverse)
#   require(rtweet)
#   if (is.data.frame(users)){
#     users_df <- users
#   }
#   if (!is.data.frame(users)){
#     users_df <- data.frame(user_id=users, other=NA)
#   }
#   n_tokens <- length(list_tokens)
#   start_time <- Sys.time()
#   message("Started: ", start_time)
#   users_remaining <- users_df
#   friends_list <- list()
#   friends_megalist <- list()
#   already <- c()
#   attempted <- c()
#   batch <- 0
#   while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
#     batch <- batch + 1
#     message("Batch: ", batch)
#     for (i in 1:n_tokens) {
#       message("\nToken: ", i)
#       message("Users Remaining: ", nrow(users_remaining)) # This doesn't equal B + C, below
#       if (! length(already) < nrow(users_df)) {break}
#       rl <- rtweet::rate_limit(query = "get_friends", token = list_tokens[[i]])
#       if (rl$remaining < 2) {# used to be 15
#         wait <- rl$reset + 0.1
#         message(paste("Waiting for", round(wait,2),"minutes..."))
#         Sys.sleep(wait * 60)
#       }
#       slice_size <- min(per_token_limit,nrow(users_remaining))
#       users_remaining_subset <- users_remaining[1:slice_size,]
#       individual_friends_list <- list()
#       for (j in 1:slice_size) {
#         warned <- FALSE
#         warning_text <- ""
#
#         tryCatch({individual_friends_list[[j]] <- rtweet::get_friends(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]])},
#                  warning=function(w) {warning_text <<- (w$message); warned <<- TRUE})
#
#         if(str_detect(warning_text, "rate limit")){
#           if(j>1){j <- j-1}
#           message("Rate limit reached!  Moving on to next token...")
#           break
#         }
#       }
#       attempted_now <- users_remaining_subset$user_id[1:j]
#       attempted <- unique(c(attempted, attempted_now))
#       if (length(individual_friends_list)>0) {
#         friends_list[[i]] <- do.call(rbind, individual_friends_list)
#         already <- c(already, unique(friends_list[[i]]$user))
#       }
#       set.seed(as.POSIXct(Sys.time()))
#       users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
#     }
#     friends_df <- do.call(rbind, friends_list)
#     friends_megalist[[batch]] <- friends_df
#     if(all(users_df$user_id %in% attempted)){break}
#   }
#   friends_megadf <- do.call(rbind, friends_megalist)
#   friends_megadf <- friends_megadf %>% mutate(scraped_at = Sys.time())
#   #return(list(friends_megadf, attempted))
#   return(friends_megadf)
# }
#

#' Get Friends (>5k)
#'
#' Scrapes friend lists for users, rotating through tokens, and using pagination to collect all friends, even when a user has more than 5,000 friends.
#' @param users Either a character vector of user_ids, or a dataframe containing a user_id column.
#' @param n The maximum number of friends to scrape for each user.  Defaults to 20,000
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 1 hour.
#' @param randomize Should the order of the input users be randomized?  Defaults to TRUE.
#' @keywords scraping
#' @export
#' @examples
#' getFriendsBig()

getFriendsBig <- function(users, n=20000, list_tokens, max_hours=1, randomize = TRUE){
  require(tidyverse)
  require(rtweet)
  n_tokens <- length(list_tokens)
  start_time <- Sys.time()
  message("Started: ", start_time)
  if (is.data.frame(users)){
    users_df <- users
  }
  if (!is.data.frame(users)){
    users_df <- data.frame(user_id=users, other=NA)
  }
  users_remaining <- users_df
  friends_list <- list()
  friends_megalist <- list()
  already <- c()
  attempted <- c()
  batch <- 0
  i = 1
  #prior_divisible <- FALSE
  already_cycled <- FALSE
  while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
    batch <- batch + 1
    message("Batch: ", batch)
    message("Users Remaining: ", nrow(users_remaining))

    if (randomize) {
      users_remaining_subset <- users_remaining %>% sample_n(., 1)
    } else {
      users_remaining_subset <- users_remaining[1,]
    }

    prior_request_pagination_string <- "-1"
    individual_ids <- c()
    while (prior_request_pagination_string != "0") {
      message("\nToken: ", i)

      warned <- FALSE
      warning_text <- ""

      if(difftime(time1 = Sys.time(), time2 = start_time, units = "h") > max_hours){break}#safety line to prevent interminable scraping attempts


      message(paste0("Attempting to scrape friends from user ", users_remaining_subset$user_id))
      tryCatch({friends_unparsed <- rtweet::get_friends(user = users_remaining_subset$user_id,
                                                        n = n,
                                                        token = list_tokens[[i]],
                                                        page = prior_request_pagination_string,
                                                        parse = FALSE)},
               warning=function(w) {warning_text <<- (w$message); warned <<- TRUE})

      # if(str_detect(warning_text, "rate|Rate")){ #this version is bad, don't use it - it waits 15 minutes before every damn token
      #   message("Rate limit reached!  Moving on to next token...")
      #   ifelse(i==n_tokens, {i <- 1; already_cycled <- TRUE}, {i <- i+1})
      #   # rate limit waiting time code -- is this the best place to put it?
      #   # rl <- rtweet::rate_limit(query = "get_friends", token = list_tokens[[i]])
      #   # if (rl$remaining < 5) { #calibrate this
      #     if(already_cycled){
      #       #wait <- rl$reset + 0.1
      #       wait <- 15
      #       message(paste("Waiting for", round(wait,2),"minutes..."))
      #       Sys.sleep(wait * 60)
      #     }
      #   # }
      #   next
      # }
      if(warned){message(warning_text)}
      if(str_detect(warning_text, "rate|Rate")){
        message("Rate limit reached!  Moving on to next token...")
        ifelse(i==n_tokens, {i <- 1; already_cycled <- TRUE}, {i <- i+1})
        # rate limit waiting time code -- is this the best place to put it?
        rl <- rtweet::rate_limit(query = "get_friends", token = list_tokens[[i]])
        if (rl$remaining < 5) { #calibrate this
          if(already_cycled){
            wait <- rl$reset + 0.1
            message(paste("Waiting for", round(wait,2),"minutes..."))
            Sys.sleep(wait * 60)
          }
        }
        next
      }
      if(str_detect(warning_text, "Not authorized")){
        message("Not authorized to scrape this user.  Moving on and not reattempting in this scrape.")
        already <- c(already, users_remaining_subset$user_id)
        prior_request_pagination_string <- "0" #setting this to 0 will break us out of the while loop when we go to next
        next
      }
      if(!warned){
        if(length(friends_unparsed$ids)==0){
          message("Zero friends scraped!  Assuming zero friends and continuing...")
          prior_request_pagination_string <- "0" #setting this to 0 will break us out of the while loop when we go to next
          next
        }
        individual_ids <- c(individual_ids, friends_unparsed$ids) #save new ids gotten
        prior_request_pagination_string <- friends_unparsed$next_cursor_str #save cursor for next pass through while loop
      }
    }
    if(length(individual_ids)>0)
           {#if
             friends_list[[batch]] <- data.frame(user = users_remaining_subset$user_id, user_id = individual_ids)
             message(paste("Successfully scraped", nrow(friends_list[[batch]]), "friends from user", users_remaining_subset$user_id))
             already <- c(already, unique(friends_list[[batch]]$user))
           } else{#else
             message(paste("Zero friends scraped from user", users_remaining_subset$user_id))
           }

    attempted_now <- users_remaining_subset$user_id
    attempted <- unique(c(attempted, attempted_now))
    set.seed(as.POSIXct(Sys.time()))

    if (randomize) {
      users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
    } else {
      users_remaining <- users_df %>% filter(! user_id %in% already)
    }

    message("Unattempted: ", sum(!(users_df$user_id %in% attempted)))

    if(all(users_df$user_id %in% attempted)){break}
  }
  friends_megadf <- do.call(rbind, friends_list)
  if(!is.null(friends_megadf)){
    friends_megadf <- friends_megadf %>% mutate(scraped_at = Sys.time())
  }
  if(is.null(friends_megadf)){
    message("Returning NULL friends_megadf!  No friends scraped in this call...")
  }
  return(friends_megadf)
}


#' Scrape Friends of a Panel
#'
#' A high-level function to scrape friends of users in a given panel, and save the resulting data in that panel folder.
#' @param panel_directory The path to the panel folder corresponding to the set of users to be scraped.
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param n The maximum number of friends to scrape for each user.  Defaults to 20,000
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 1 hour.
#' @keywords scraping
#' @export
#' @examples
#' scrapeFriends()

scrapeFriends <- function(panel_directory, list_tokens, n=20000, per_token_limit=15, max_hours=1){
  message("Scraping friends...")
  today <- timeCode()
  users <- readRDS(paste0(panel_directory,"twitter_scrapes/user_ids.rds"))
  new_friends <- getFriendsBig(users=users, n=n, list_tokens=list_tokens, max_hours=max_hours)
  if (!is.null(new_friends)){
    message("Saving friends scrape to file.")
    saveRDS(new_friends, file = paste0(panel_directory,"twitter_scrapes/friends/friends_",today,".rds"))
  } else {message("No new friends scraped in this session.  Not saving any friends data file for this scrape.")}
}

# #generally speaking, better to use maxToken_BIG
# get_followers_rotate_maxToken <- function(users, n=5000, list_tokens, per_token_limit=15, max_hours=1){
#   require(tidyverse)
#   require(rtweet)
#   if (is.data.frame(users)){
#     users_df <- users
#   }
#   if (!is.data.frame(users)){
#     users_df <- data.frame(user_id=users, other=NA)
#   }
#   n_tokens <- length(list_tokens)
#   start_time <- Sys.time()
#   message("Started: ", start_time)
#   users_remaining <- users_df
#   followers_list <- list()
#   followers_megalist <- list()
#   already <- c()
#   attempted <- c()
#   batch <- 0
#   while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
#     batch <- batch + 1
#     message("Batch: ", batch)
#     for (i in 1:n_tokens) {
#       message("\nToken: ", i)
#       message("Users Remaining: ", nrow(users_remaining)) # This doesn't equal B + C, below
#       if (! length(already) < nrow(users_df)) {break}
#       rl <- rtweet::rate_limit(query = "get_followers", token = list_tokens[[i]])
#       if (rl$remaining < 15) {
#         wait <- rl$reset + 0.1
#         message(paste("Waiting for", round(wait,2),"minutes..."))
#         Sys.sleep(wait * 60)
#       }
#       slice_size <- min(per_token_limit,nrow(users_remaining))
#       users_remaining_subset <- users_remaining[1:slice_size,]
#       individual_followers_list <- list()
#       for (j in 1:slice_size) {
#         warned <- FALSE
#         warning_text <- ""
#
#         tryCatch({individual_followers_list[[j]] <- rtweet::get_followers(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]])},
#                  warning=function(w) {warning_text <<- (w$message); warned <<- TRUE})
#
#         if(str_detect(warning_text, "rate limit")){
#           if(j>1){j <- j-1}
#           message("Rate limit reached!  Moving on to next token...")
#           break
#         }
#         if(!warned){
#           try(individual_followers_list[[j]] <- individual_followers_list[[j]] %>% transmute(user = users_remaining_subset$user_id[j], user_id)) #try added to patch Error: Problem with `mutate()` input `..2`. ✖ object 'user_id' not found
#         }
#       }
#       attempted_now <- users_remaining_subset$user_id[1:j]
#       attempted <- unique(c(attempted, attempted_now))
#       if (length(individual_followers_list)>0) {
#         followers_list[[i]] <- do.call(rbind, individual_followers_list)
#         already <- c(already, unique(followers_list[[i]]$user))
#       }
#       set.seed(as.POSIXct(Sys.time()))
#       users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
#     }
#     followers_df <- do.call(rbind, followers_list)
#     followers_megalist[[batch]] <- followers_df
#     if(all(users_df$user_id %in% attempted)){break}
#   }
#   followers_megadf <- do.call(rbind, followers_megalist)
#   followers_megadf <- followers_megadf %>% mutate(scraped_at = Sys.time())
#   #return(list(followers_megadf, attempted))
#   return(followers_megadf)
# }

#' Get Followers (>5k)
#'
#' Scrapes follower lists for users, rotating through tokens, and detecting truncation due to rate limits, to collect all followers, even when a user has more than 5,000 followers.
#' @param users Either a character vector of user_ids, or a dataframe containing a user_id column.
#' @param n The maximum number of followers to scrape for each user.  Defaults to 20,000.
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param per_token_limit Batch size to reduce rate limit errors.  Defaults to 15 users per token.
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 1 hour.
#' @keywords scraping
#' @export
#' @examples
#' getFollowersBig()

getFollowersBig <- function(users, n=20000, list_tokens, per_token_limit=15, max_hours=1){
  require(tidyverse)
  require(rtweet)
  n_tokens <- length(list_tokens)
  start_time <- Sys.time()
  message("Started: ", start_time)
  if (is.data.frame(users)){
    users_df <- users
  }
  if (!is.data.frame(users)){
    users_df <- data.frame(user_id=users, other=NA)
  }
  users_remaining <- users_df
  followers_list <- list()
  followers_megalist <- list()
  already <- c()
  attempted <- c()
  batch <- 0
  prior_divisible <- FALSE
  already_cycled <- FALSE
  while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
    batch <- batch + 1
    message("Batch: ", batch)
    for (i in 1:n_tokens) {
      if (i == n_tokens){already_cycled <- TRUE}
      message("\nToken: ", i)
      message("Users Remaining: ", nrow(users_remaining)) # This doesn't equal B + C, below
      if (! length(already) < nrow(users_df)) {break}
      rl <- rtweet::rate_limit(query = "get_followers", token = list_tokens[[i]])
      if (rl$remaining < 5) { #calibrate this
        if(already_cycled){
          wait <- rl$reset + 0.1
          message(paste("Waiting for", round(wait,2),"minutes..."))
          Sys.sleep(wait * 60)
        }
        if(!already_cycled){
          next
        }
      }
      slice_size <- min(per_token_limit,nrow(users_remaining))
      users_remaining_subset <- users_remaining[1:slice_size,]
      individual_followers_list <- list()
      prior_followers_scraped_length <- 0
      for (j in 1:slice_size) {
        if (j>1) {
          #if(!is.null(individual_followers_list[[j-1]])){ #added null check to solve: Error in individual_followers_list[[j - 1]] : subscript out of bounds
          prior_followers_scraped_length <- 0
          try(prior_followers_scraped_length <- nrow(individual_followers_list[[j-1]]), silent = T) #wrapped in try() to solve: Error in individual_followers_list[[j - 1]] : subscript out of bounds
          #  } else {prior_followers_scraped_length <- NA}
          }
        warned <- FALSE
        warning_text <- ""

        message(paste0("Attempting to scrape followers from user ", users_remaining_subset$user_id[j]))
        tryCatch({individual_followers_list[[j]] <- rtweet::get_followers(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]])},
                 warning=function(w) {warning_text <<- (w$message); warned <<- TRUE})

        if(warned){message(warning_text)}

        if(str_detect(warning_text, "rate limit")){
          if(j>1){j <- j-1}
          message("Rate limit reached!  Moving on to next token...")
          break
        }

        if(str_detect(warning_text, "Sorry, that page does not exist.")){
          message("User does not exist.  Moving on and not reattempting in this scrape...")
          already <- c(already, users_remaining_subset$user_id[j])
          break
        }

        if(!warned){
          if(nrow(individual_followers_list[[j]])==0){
            if(j>1){
              if((prior_followers_scraped_length!=0) & ((prior_followers_scraped_length %% 5000)==0)){
                message("No followers scraped and prior attempt divisible by 5000!  Moving on to next token and reattempting both...")
                j <- j-2
                break
              }
            }
            if(j==1){
              message("Zero followers scraped with a fresh token!  Assuming zero followers and continuing...")
              #might need to add something here to handle true zeroes at the transmutation stage
              break
            }
          }
          if(((nrow(individual_followers_list[[j]]) %% 5000) == 0) & (!prior_divisible)){
            message("Number of followers scraped divisible by 5000!  Moving on to next token and reattempting once...")
            j <- j-1
            prior_divisible <- TRUE
            break
          }
          #if (nrow(individual_followers_list[[j]])>0){#wrapped in this to prevent error encountered in qualtrics matchign at spirals bad pilot launch
            message(paste("Successfully scraped", nrow(individual_followers_list[[j]]), "followers from user", users_remaining_subset$user_id[j]))
            individual_followers_list[[j]] <- individual_followers_list[[j]] %>% transmute(user = users_remaining_subset$user_id[j], user_id)
            prior_divisible <- FALSE #is this in the right place?
          #}
        }
      }
      attempted_now <- users_remaining_subset$user_id[1:j]
      attempted <- unique(c(attempted, attempted_now))
      if (length(individual_followers_list)>0) {
        #followers_list[[i]] <- do.call(rbind, individual_followers_list)
        #followers_list[[i]] <- do.call(rbind, individual_followers_list[1:j])
        followers_list[[i]] <- do.call(bind_rows, individual_followers_list[1:j])
        already <- c(already, unique(followers_list[[i]]$user))
      }
      set.seed(as.POSIXct(Sys.time()))
      users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
    }
    followers_df <- do.call(rbind, followers_list)
    followers_megalist[[batch]] <- followers_df
    if(all(users_df$user_id %in% attempted)){break}
  }
  followers_megadf <- do.call(rbind, followers_megalist)
  if (!is.null(followers_megadf)){
    followers_megadf <- followers_megadf %>% mutate(scraped_at = Sys.time())
  } else {
    message("Returning NULL followers_megadf!  No followers scraped in this call...")
  }
  #return(list(followers_megadf, attempted))
  return(followers_megadf)
}

#' Scrape Followers of a Panel
#'
#' A high-level function to scrape followers of users in a given panel, and save the resulting data in that panel folder.
#' @param panel_directory The path to the panel folder corresponding to the set of users to be scraped.
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param n The maximum number of followers to scrape for each user.  Defaults to 20,000.
#' @param per_token_limit Batch size to reduce rate limit errors.  Defaults to 15 users per token.
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 1 hour.
#' @keywords scraping
#' @export
#' @examples
#' scrapeFollowers()

scrapeFollowers <- function(panel_directory, list_tokens, n=20000, per_token_limit=15, max_hours=1){
  message("Scraping followers...")
  today <- timeCode()
  users <- readRDS(paste0(panel_directory,"twitter_scrapes/user_ids.rds"))
  # if (n>5000){ # unless you set n explicitly to be small, it will use the "BIG" function by default
    new_followers <- getFollowersBig(users=users, n=n, list_tokens=list_tokens, per_token_limit=per_token_limit, max_hours=max_hours)
  # }
  # if (n<=5000){ # is the non-big function still useful for anything
  #   new_followers <- get_followers_rotate_maxToken(users=users, n=n, list_tokens=list_tokens, per_token_limit=per_token_limit, max_hours=max_hours)
  # }
  if (!is.null(new_followers)){
    message("Saving followers scrape to file.")
    saveRDS(new_followers, file = paste0(panel_directory,"twitter_scrapes/followers/followers_",today,".rds"))
  } else {message("No new followers scraped in this session.  Not saving any followers data file for this scrape.")}
}

# # Favorites (likes) functions
#
# get_favorites_since <- function(users, n=3000, list_tokens, days_back = 1, per_token_limit=15, max_hours=1){
#   require(tidyverse)
#   require(rtweet)
#   if (is.data.frame(users)){
#     users_df <- users
#   }
#   if (!is.data.frame(users)){
#     users_df <- data.frame(user_id=users, other=NA)
#   }
#   n_tokens <- length(list_tokens)
#   start_time <- Sys.time()
#   message("Started: ", start_time)
#   users_remaining <- users_df
#   favorites_list <- list()
#   favorites_megalist <- list()
#   already <- c()
#   attempted <- c()
#   batch <- 0
#   while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
#     batch <- batch + 1
#     message("Batch: ", batch)
#     for (i in 1:n_tokens) {
#       message("\nToken: ", i)
#       message("Users Remaining: ", nrow(users_remaining)) # This doesn't equal B + C, below
#       if (! length(already) < nrow(users_df)) {break}
#       rl <- rtweet::rate_limit(query = "get_favorites", token = list_tokens[[i]])
#       if (rl$remaining < 30) {
#         wait <- rl$reset + 0.1
#         message(paste("Waiting for", round(wait,2),"minutes..."))
#         Sys.sleep(wait * 60)
#       }
#       slice_size <- min(per_token_limit,nrow(users_remaining))
#       users_remaining_subset <- users_remaining[1:slice_size,]
#       individual_favorites_list <- list()
#       for (j in 1:slice_size) {
#         warned <- FALSE
#         warning_text <- ""
#
#         if (!is.na(users_remaining_subset$penultimate_tweet[j])){
#           tryCatch({individual_favorites_list[[j]] <- rtweet::get_favorites(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]], since_id = ts_tid(days_back))}, #target
#                    error=function(e) {warning_text <<- (e$message); warned <<- TRUE})
#         }
#         if (is.na(users_remaining_subset$penultimate_tweet[j])){
#           tryCatch({individual_favorites_list[[j]] <- rtweet::get_favorites(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]])},
#                    error=function(e) {warning_text <<- (e$message); warned <<- TRUE})
#         }
#
#         if(str_detect(warning_text, "rate limit")){
#           if(j>1){j <- j-1}
#           message("Rate limit reached!  Moving on to next token...")
#           break
#         }
#         if(!warned){
#           individual_favorites_list[[j]] <- individual_favorites_list[[j]]
#         }
#       }
#       attempted_now <- users_remaining_subset$user_id[1:j]
#       attempted <- unique(c(attempted, attempted_now))
#       if (length(individual_favorites_list)>0) {
#         favorites_list[[i]] <- do.call(rbind, individual_favorites_list)
#         already <- c(already, unique(favorites_list[[i]]$favorited_by))
#       }
#       set.seed(as.POSIXct(Sys.time()))
#       users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
#     }
#     favorites_df <- do.call(rbind, favorites_list)
#     favorites_megalist[[batch]] <- favorites_df
#     if(all(users_df$user_id %in% attempted)){break}
#   }
#   favorites_megadf <- do.call(rbind, favorites_megalist)
#   #if (nrow(favorites_megadf)>0){ #in case no favorites are scraped
#   if (length(favorites_megadf)!=0){ #in case no favorites are scraped
#     favorites_megadf <- favorites_megadf %>% mutate(scraped_at = Sys.time())
#   }
#   #return(favorites_megadf)
#   return(list(favorites_megadf, attempted))
# }
#
# ws_scrape_favorites <- function(user_set, days_further_back = 0, n=3000, list_tokens, max_hours=12, sub_batch_size=30, sentiment=FALSE, darmoc=FALSE){
#   message("Scraping favorites...")
#   require(tidyverse)
#   require(rtweet)
#   message("Scraping users: ", user_set)
#   today <- dateCode()
#   message("Scraping date: ", today)
#   set.seed(as.numeric(today))
#   message("Reading prior scrape log...")
#   logs <- dir(paste0("data/",user_set,"/favorite_logs/")) %>% str_subset(., pattern="log_")
#   last_log_file <- max(logs)
#   previous_scrape_day <- last_log_file %>% str_sub(start = 5,end = 10)
#   message("Prior scraping date: ", previous_scrape_day)
#   days_ago <- as.Date.character(today, format = "%y%m%d") - as.Date.character(previous_scrape_day, format = "%y%m%d")
#   last_log <- readRDS(paste0("data/",user_set,"/favorite_logs/",last_log_file))
#   last_log <- sample_n(last_log, size = nrow(last_log))
#   message("Start scraping...")
#   #if (1==1){
#   data_list <- get_favorites_since(users_df = last_log, n=n, list_tokens = list_tokens, days_back=as.integer(days_ago + days_further_back), per_token_limit=sub_batch_size, max_hours=max_hours)
#   #}
#   data <- data_list[[1]]
#   attempted <- data_list[[2]]
#   message("Saving scraped data...")
#   #previous_scrape_day
#   if (file.exists(paste0("data/",user_set,"/favorites/favorites_", previous_scrape_day,".rds"))){
#     message("Merging with previous scrape...")
#     previous_data <- readRDS(file = paste0("data/",user_set,"/favorites/favorites_", previous_scrape_day,".rds"))
#     all_data <- bind_rows(previous_data, data) %>% distinct(status_id, favorited_by, .keep_all = T)
#     #saveRDS(all_data, file = paste0("data/",user_set,"/favorites/favorites_", today,".rds"))
#   }
#   if (file.exists(paste0("data/",user_set,"/favorites/favorites_", today,".rds"))){
#     message("Merging with earlier scrape from today...")
#     last_data <- readRDS(file = paste0("data/",user_set,"/favorites/favorites_", today,".rds"))
#     all_data <- bind_rows(last_data, all_data) %>% distinct(status_id, favorited_by, .keep_all = T)
#     #saveRDS(all_data, file = paste0("data/",user_set,"/favorites/favorites_", today,".rds"))
#   }
#   saveRDS(all_data, file = paste0("data/",user_set,"/favorites/favorites_", today,".rds"))
#   message("Saving new log...")
#   this_log <- data %>% distinct(status_id, favorited_by, .keep_all = T) %>% group_by(favorited_by) %>% summarise(penultimate_tweet = maxNchar(status_id, 2), ultimate_tweet = maxNchar(status_id, 1), count = n()) %>% rename(user_id=favorited_by)
#   new_log <- rbind((last_log %>% select(user_id, penultimate_tweet, ultimate_tweet)),(this_log %>% select(user_id, penultimate_tweet, ultimate_tweet))) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(penultimate_tweet, 1), ultimate_tweet = maxNchar(ultimate_tweet, 1)) #fixed parentheses order
#   saveRDS(new_log, file = paste0("data/",user_set,"/favorite_logs/log_", today,".rds"))
#   message(sum((this_log$count-1)), " new favorites scraped from ", sum(this_log$count>1)," users!\n", (nrow(last_log)-sum(this_log$count>1)), " users had no new favorites to scrape.")
#   message(sum(! last_log$ultimate_tweet %in% data$status_id), " users may have missing favorites")
#   saveRDS(attempted, file = paste0("data/",user_set,"/favorite_attempts/attempted_", today,".rds"))
#   message("Total attempted: ", sum(last_log$user_id %in% attempted))
#   message("Total unattempted: ", sum(! last_log$user_id %in% attempted))
# }
#
#
# ws_scrape_favorites2 <- function(panel_directory, days_further_back = 0, n=3000, list_tokens, max_hours=12, sub_batch_size=30, sentiment=FALSE, darmoc=FALSE){
#   message("Scraping favorites...")
#   require(tidyverse)
#   require(rtweet)
#   #message("Scraping users: ", user_set)
#   today <- dateCode()
#   message("Scraping date: ", today)
#   set.seed(as.numeric(today))
#   message("Reading prior scrape log...")
#   logs <- dir(paste0(panel_directory,"twitter_scrapes/favorite_logs/")) %>% str_subset(., pattern="log_")
#   last_log_file <- max(logs)
#   previous_scrape_day <- last_log_file %>% str_sub(start = 5,end = 10)
#   message("Prior scraping date: ", previous_scrape_day)
#   days_ago <- as.Date.character(today, format = "%y%m%d") - as.Date.character(previous_scrape_day, format = "%y%m%d")
#   last_log <- readRDS(paste0(panel_directory,"twitter_scrapes/favorite_logs/",last_log_file))
#   last_log <- sample_n(last_log, size = nrow(last_log))
#   message("Start scraping...")
#   #if (1==1){
#   data_list <- get_favorites_since(users = last_log, n=n, list_tokens = list_tokens, days_back=as.integer(days_ago + days_further_back), per_token_limit=sub_batch_size, max_hours=max_hours)
#   #}
#   data <- data_list[[1]]
#   attempted <- data_list[[2]]
#   if (nrow(data)>0){#often there are no new favorites, so need to make this robust
#     message("Saving scraped data...")
#     #previous_scrape_day -- not sure why I have this, it seems like it would balloon storage requirements for no reason
#     # if (file.exists(paste0(panel_directory,"twitter_scrapes/favorites/favorites_", previous_scrape_day,".rds"))){
#     #   message("Merging with previous scrape...")
#     #   previous_data <- readRDS(file = paste0(panel_directory,"twitter_scrapes/favorites/favorites_", previous_scrape_day,".rds"))
#     #   all_data <- bind_rows(previous_data, data) %>% distinct(status_id, favorited_by, .keep_all = T)
#     # }
#     if (file.exists(paste0(panel_directory,"twitter_scrapes/favorites/favorites_", today,".rds"))){
#       message("Merging with earlier scrape from today...")
#       last_data <- readRDS(file = paste0(panel_directory,"twitter_scrapes/favorites/favorites_", today,".rds"))
#       all_data <- bind_rows(last_data, all_data) %>% distinct(status_id, favorited_by, .keep_all = T)
#     }
#     if (!file.exists(paste0(panel_directory,"twitter_scrapes/favorites/favorites_", today,".rds"))){
#       all_data <- data
#     }
#     saveRDS(all_data, file = paste0(panel_directory,"twitter_scrapes/favorites/favorites_", today,".rds"))
#     message("Saving new log...")
#     this_log <- data %>% distinct(status_id, favorited_by, .keep_all = T) %>% group_by(favorited_by) %>% summarise(penultimate_tweet = maxNchar(status_id, 2), ultimate_tweet = maxNchar(status_id, 1), count = n()) %>% rename(user_id=favorited_by)
#     new_log <- rbind((last_log %>% select(user_id, penultimate_tweet, ultimate_tweet)),(this_log %>% select(user_id, penultimate_tweet, ultimate_tweet))) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(penultimate_tweet, 1), ultimate_tweet = maxNchar(ultimate_tweet, 1)) #fixed parentheses order
#     saveRDS(new_log, file = paste0(panel_directory,"twitter_scrapes/favorite_logs/log_", today,".rds"))
#     message(sum((this_log$count-1)), " new favorites scraped from ", sum(this_log$count>1)," users!\n", (nrow(last_log)-sum(this_log$count>1)), " users had no new favorites to scrape.")
#     message(sum(! last_log$ultimate_tweet %in% data$status_id), " users may have missing favorites")
#   }
#   if (nrow(data)==0){message("No new favorites scraped in this panel today.  Saving attempt...")}
#   saveRDS(attempted, file = paste0(panel_directory,"twitter_scrapes/favorite_attempts/attempted_", today,".rds"))
#   message("Total attempted: ", sum(last_log$user_id %in% attempted))
#   message("Total unattempted: ", sum(! last_log$user_id %in% attempted))
# }
#
# # Timeline functions
#

#' Update Timelines
#'
#' The workhorse function of prospective timeline collection, takes a dataframe of user ids and penultimate tweet ids, and scrapes all new tweets since the penultimate tweet.  Penultimate tweets are used to avoid any ambiguity about whether all tweets have been collected.  This function is not generally used on its own, but plays an important part in higher-level scraping functions.
#' @param users_df A dataframe containing a user_id column and a penultimate_id column.
#' @param n The maximum number of followers to scrape for each user.  Defaults to 3,200.
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param per_token_limit Batch size to reduce rate limit errors.  Defaults to 100 users per token.
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 4 hours.
#' @keywords scraping
#' @export
#' @examples
#' updateTimelines()

updateTimelines <- function(users_df, n=3200, list_tokens, per_token_limit=100, max_hours=4){
  require(tidyverse)
  require(rtweet)
  start_time <- Sys.time()
  message("Started: ", start_time)
  users_remaining <- users_df
  n_tokens=length(list_tokens)
  timelines_list <- list()
  timelines_megalist <- list()
  already <- c()
  attempted <- c()
  batch <- 0
  already_cycled <- FALSE
  while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
    batch <- batch + 1
    message("Batch: ", batch)
    for (i in 1:n_tokens) {
      if (i == n_tokens){already_cycled <- TRUE}
      message("\nToken: ", i)
      message("Users Remaining: ", nrow(users_remaining)) # This doesn't equal B + C, below
      if (! length(already) < nrow(users_df)) {break}
      rl <- rtweet::rate_limit(query = "get_timeline", token = list_tokens[[i]])
      if (rl$remaining < 5) { #used to be 100, but seems like a waste since I already check for rate limit errors in this function
        if(already_cycled){
          wait <- rl$reset + 0.1
          message(paste("Waiting for", round(wait,2),"minutes..."))
          Sys.sleep(wait * 60)
        }
        if(!already_cycled){
          next
        }
      }
      # if (rl$remaining < 5) { #used to be 100, but seems like a waste since I already check for rate limit errors in this function
      #   wait <- rl$reset + 0.1
      #   message(paste("Waiting for", round(wait,2),"minutes..."))
      #   Sys.sleep(wait * 60)
      # }
      slice_size <- min(per_token_limit,nrow(users_remaining))
      users_remaining_subset <- users_remaining[1:slice_size,]
      message("Attempting to scrape timelines from:", paste(users_remaining_subset$user_id, collapse = "\n"))
      individual_timelines_list <- list()
      for (j in 1:slice_size) {
        warned <- FALSE
        warning_text <- ""
        if (!is.na(users_remaining_subset$penultimate_tweet[j])){
          tryCatch({individual_timelines_list[[j]] <- rtweet::get_timeline(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]], check = FALSE, since_id = users_remaining_subset$penultimate_tweet[j])},
                   warning=function(w) {warning_text <<- (w$message); warned <<- TRUE})
        }
        if (is.na(users_remaining_subset$penultimate_tweet[j])){
          tryCatch({individual_timelines_list[[j]] <- rtweet::get_timeline(user = users_remaining_subset$user_id[j], n = n, token = list_tokens[[i]], check = FALSE)},
                   warning=function(w) {warning_text <<- (w$message); warned <<- TRUE})
        }
        if (!str_detect(warning_text, "rate limit") & nchar(warning_text)>0) {message(paste0(warning_text))}
        if (str_detect(warning_text, "rate limit")) {
          if(j>1){j <- j-1}
          message("Rate limit reached!  Moving on to next token...")
          break
        }
      }
      attempted_now <- users_remaining_subset$user_id[1:j]
      attempted <- unique(c(attempted, attempted_now))
      individual_timelines_list_bound <- do.call(rbind, individual_timelines_list)
      try(message("Rows:", nrow(individual_timelines_list_bound)))
      try(message("is.null: ", is.null(individual_timelines_list_bound)))
      #if (nrow(individual_timelines_list_bound)>0) {
      if (is.null(individual_timelines_list_bound)){message("Some kind of error in this batch.  individual_timelines_list_bound came back NULL.")}
      if (!is.null(individual_timelines_list_bound)) {  #I think this is better
        timelines_list[[i]] <- individual_timelines_list_bound
        already <- c(already, unique(timelines_list[[i]]$user_id))# this had problems when timelines_list[[i]] is NULL, hopefully fixed by wrapping in this if()
      }
      set.seed(as.POSIXct(Sys.time()))
      users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
    }
    timelines_df <- do.call(rbind, timelines_list)
    #if (nrow(timelines_df)>0) {
    if (!is.null(timelines_df)) { #is this better?
      timelines_megalist[[batch]] <- timelines_df
    }
    if (all(users_df$user_id %in% attempted)) {break}
  }
  timelines_megadf <- do.call(rbind, timelines_megalist)
  timelines_megadf <- timelines_megadf %>% mutate(scraped_at = Sys.time())
  return(list(timelines_megadf, attempted))
}

#' Get Historical Timelines (up to 3200 Tweets into the Past)
#'
#' The workhorse function of historical timeline collection, takes a dataframe of containing a user_id column, and scrapes as many tweets into the past as specified, up to 3200.  This function is only sometimes used on its own, but plays an important part in higher-level scraping functions.
#' @param users A dataframe containing a user_id column, or a character vector of user ids.
#' @param n The maximum number of followers to scrape for each user.  Defaults to 3,200.
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param per_token_limit Batch size to reduce rate limit errors.  Defaults to 100 users per token.
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 4 hours.
#' @keywords scraping
#' @export
#' @examples
#' getTimelinesHistorical()

getTimelinesHistorical <- function(users, n=3200, list_tokens, per_token_limit=100, max_hours=4){
  require(tidyverse)
  require(rtweet)
  start_time <- Sys.time()
  message("Started: ", start_time)
  if (is.data.frame(users)){
    users_df <- users
  }
  if (!is.data.frame(users)){
    users_df <- data.frame(user_id=users, other=NA)
  }
  users_remaining <- users_df
  n_tokens=length(list_tokens)
  timelines_list <- list()
  timelines_megalist <- list()
  already <- c()
  attempted <- c()
  batch <- 0
  already_cycled <- FALSE
  while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
    batch <- batch + 1
    message("Batch: ", batch)
    for (i in 1:n_tokens) {
      if (i == n_tokens){already_cycled <- TRUE}
      message("\nToken: ", i)
      message("Users Remaining: ", nrow(users_remaining))
      if (! length(already) < nrow(users_df)) {break}
      rl <- rtweet::rate_limit(query = "get_timeline", token = list_tokens[[i]])
      if (rl$remaining < 500) { #calibrate this
        if(already_cycled){
          wait <- rl$reset + 0.1
          message(paste("Waiting for", round(wait,2),"minutes..."))
          Sys.sleep(wait * 60)
        }
        if(!already_cycled){
          next
        }
      }
      # if (rl$remaining < 500) {
      #   wait <- rl$reset + 0.1
      #   message(paste("Waiting for", round(wait,2),"minutes..."))
      #   Sys.sleep(wait * 60)
      # }
      slice_size <- min(per_token_limit,nrow(users_remaining))
      users_remaining_subset <- users_remaining[1:slice_size,]
      {timelines_list[[i]] <- rtweet::get_timeline(user = users_remaining_subset$user_id, n = n, token = list_tokens[[i]], check = FALSE)}
      attempted_now <- users_remaining_subset$user_id
      attempted <- unique(c(attempted, attempted_now))
      already <- c(already, unique(timelines_list[[i]]$user_id))
      set.seed(as.POSIXct(Sys.time()))
      users_remaining <- users_df %>% filter(! user_id %in% already) %>% slice_sample(prop=1)
    }
    timelines_df <- do.call(rbind, timelines_list)
    timelines_megalist[[batch]] <- timelines_df
    if(all(users_df$user_id %in% attempted)){break}
  }
  timelines_megadf <- do.call(rbind, timelines_megalist)
  timelines_megadf <- timelines_megadf %>% mutate(scraped_at = Sys.time())
  return(list(timelines_megadf, attempted))
}

#' First Scrape of New Panel Members
#'
#' This workhorse function conducts an inaugural scrape when new users are added to a panel.  It reads the scrape_settings saved in the panel folder, and scrapes the data types specified for that panel.
#' @param user_ids A dataframe containing a user_id column and a penultimate_id column.
#' @param panel_directory The path to the panel folder to which users are being added.
#' @param tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 1 hour.
#' @param sentiment Should scraped tweets be analyzed for sentiment? Defaults to "none".
#' @param darmoc Should scrapted tweets be analyzed for ideology and sureness?  Defaults to FALSE
#' @keywords scraping
#' @export
#' @examples
#' firstScrape()

firstScrape <- function(user_ids, panel_directory, tokens, max_hours = 1, sentiment = "none", darmoc = FALSE){
  message("Running initial scrape for:\n", paste(user_ids, collapse = "\n"), "...")
  scrape_settings <- readRDS(paste0(panel_directory,"/scrape_settings.rds"))

  this_timecode <- timeCode()

  new_lookup <- try(rtweet::lookup_users(users = user_ids, token = tokens[[1]]))

  if (is.data.frame(new_lookup)){
    saveRDS(new_lookup, file = paste0(panel_directory, "/twitter_scrapes/user_info/new_lookup_",this_timecode,".rds"))

    if (file.exists(paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))){
      message("Binding to last current lookup.")
      last_current_lookup <- readRDS(paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))
      new_current_lookup <- bind_rows(last_current_lookup, new_lookup) %>% distinct(user_id, .keep_all = TRUE)
    } else {
      message("Creating new current lookup.")
      new_current_lookup <- new_lookup
    }

    saveRDS(new_current_lookup, paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))


    # code to immediately scrape all timeline history and add entries to the central log
    if (scrape_settings$scrape_timelines){
      message("Scraping first timelines...")
      init_log <- new_lookup %>% transmute(user_id = user_id, penultimate_tweet=NA, ultimate_tweet=NA)

      first_timelines_data <- updateTimelines(users = init_log, n = 3200, max_hours = max_hours, list_tokens = tokens) #used to use allHistory, but that can break because it doesn't check rate limits. This is slower but safer.
      first_timelines <- first_timelines_data[[1]]
      first_attempts <- first_timelines_data[[2]]

      #UNCOMMENT BELOW WHEN SENTIMENT AND DARMOC ARE READY FOR USAGE BY tricordR
      # if (sentiment==TRUE){
      #   message("Analyzing sentiment...")
      #   source("~/Documents/GitRprojects/LaForge/functions/sentiment_analysis_functions.R")
      #   first_timelines <- addSentiment(first_timelines)
      # }
      # if (darmoc==TRUE){
      #   message("Analyzing ideology and sureness...")
      #   library(tidyverse)
      #   library(glmnet)
      #   library(quanteda)
      #   source("~/Documents/GitRprojects/LaForge/functions/v1_darmoc/featurization.R") #featurization scripts for the classifiers used here
      #   #load classifiers and feature names
      #   load(file = "~/Documents/GitRprojects/LaForge/functions/v1_darmoc/bin_liborcon_Nint.rda")
      #   load(file = "~/Documents/GitRprojects/LaForge/functions/v1_darmoc/bin_most_not_NIAA.rda")
      #   load(file = "~/Documents/GitRprojects/LaForge/functions/v1_darmoc/training_featnames.rda")
      #   preds <- myTokMatchClass(input = first_timelines$text, i_mod = bin_liborcon_Nint, s_mod = bin_most_not_NIAA, match_to = training_featnames, type = "response")
      #   first_timelines <- cbind(first_timelines,preds)
      # }

      saveRDS(first_timelines, file = paste0(panel_directory,"/twitter_scrapes/first_timelines/timelines_", this_timecode, ".rds"))
      saveRDS(first_attempts, file = paste0(panel_directory,"/twitter_scrapes/timeline_attempts/attempted_", this_timecode, ".rds"))

      this_log <- first_timelines %>% distinct(user_id, status_id, .keep_all = T) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(status_id, 2), ultimate_tweet = maxNchar(status_id, 1))
      new_log <- bind_rows(init_log, this_log) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(penultimate_tweet, 1), ultimate_tweet = maxNchar(ultimate_tweet, 1))

      logs <- dir(paste0(panel_directory,"/twitter_scrapes/timeline_logs/")) %>% str_subset(., pattern="log_")

      last_log_file <- max(logs)
      if(!is.na(last_log_file)){
        message("Prior scraping date: ", last_log_file %>% str_sub(start = 5,end = 10))
        last_log <- readRDS(paste0(panel_directory,"/twitter_scrapes/timeline_logs/",last_log_file))
        new_log <- bind_rows(last_log,new_log) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(penultimate_tweet, 1), ultimate_tweet = maxNchar(ultimate_tweet, 1))
      }
      if(is.na(last_log_file)){
        message("First scrape")
      }
      saveRDS(new_log, file = paste0(panel_directory,"/twitter_scrapes/timeline_logs/log_", str_sub(this_timecode, 3, 8),".rds"))
    }


    # code to immediately scrape friend lists
    if (scrape_settings$scrape_friends){
      message("Scraping first friends...")
      new_friends <- getFriendsBig(users = new_lookup, list_tokens=tokens, max_hours = max_hours)
      #colnames(new_friends) <- c("user_id", "friends", "scraped_at")
      saveRDS(new_friends, paste0(panel_directory,"/twitter_scrapes/first_friends/friends_",this_timecode,".rds"))
    }


    # and immediately scrape followers
    if (scrape_settings$scrape_followers){
      message("Scraping first followers...")
      new_followers <- getFollowersBig(users = new_lookup, list_tokens=tokens, max_hours = max_hours)
      #colnames(new_followers) <- c("user_id", "followers", "scraped_at") #this line fucks everything up, don't use it
      saveRDS(new_followers, paste0(panel_directory,"/twitter_scrapes/first_followers/followers_",this_timecode,".rds"))
    }

    # and scrape favorites and add rows to the favorite log/create novel favorites log
    if (scrape_settings$scrape_favorites){
      message("Scraping first favorites...")
      new_favorites <- get_favorites_since(users = new_lookup %>% mutate(penultimate_tweet=NA), list_tokens=tokens, max_hours = max_hours)
      new_favorites_data <- new_favorites[[1]]
      new_favorites_attempted <- new_favorites[[2]]
      saveRDS(new_favorites_data, paste0(panel_directory,"/twitter_scrapes/first_favorites/favorites_",this_timecode,".rds"))
      saveRDS(new_favorites_attempted, paste0(panel_directory,"/twitter_scrapes/favorite_attempts/attempt_",this_timecode,".rds"))

      message("Saving new log...")
      this_log <- new_favorites_data %>% distinct(status_id, favorited_by, .keep_all = T) %>% group_by(favorited_by) %>% summarise(penultimate_tweet = maxNchar(status_id, 2), ultimate_tweet = maxNchar(status_id, 1), count = n()) %>% rename(user_id=favorited_by)

      logs <- dir(paste0(panel_directory,"/twitter_scrapes/favorite_logs/")) %>% str_subset(., pattern="log_")

      last_log_file <- max(logs)
      if(!is.na(last_log_file)){
        message("Prior scraping date: ", last_log_file %>% str_sub(start = 5,end = 10))
        last_log <- readRDS(paste0(panel_directory,"/twitter_scrapes/favorite_logs/",last_log_file))
        new_log <- bind_rows(last_log,new_log) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(penultimate_tweet, 1), ultimate_tweet = maxNchar(ultimate_tweet, 1))
      }

      if(is.na(last_log_file)){
        message("First favorites scrape, saving new log.")
      }

      saveRDS(new_log, file = paste0(panel_directory,"/twitter_scrapes/favorite_logs/log_", str_sub(this_timecode, 3, 8),".rds"))
    }
  }

  if (!is.data.frame(new_lookup)){
    message("Lookup error!  Initial scrape aborted.")
  }
}

#' Scrape Timelines of a Panel
#'
#' A high-level function to scrape timelines of users in a given panel, and save the resulting data in that panel folder.
#' @param panel_directory The path to the panel folder corresponding to the set of users to be scraped.
#' @param N The number of tweets to be scraped.  Defaults to 3200, the maximum.
#' @param list_tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param max_hours The maximum number of hours to continue scraping.  Defaults to 12 hours.
#' @param sub_batch_size Batch size to reduce rate limit errors.  Defaults to 100 users per token.
#' @param sentiment Should tweets be analyzed for sentiment?  Defaults to FALSE.
#' @param darmoc Should tweets be analyzed for ideology and sureness? Defaults to FALSE.
#' @keywords scraping
#' @export
#' @examples
#' scrapeTimelines()

scrapeTimelines <- function(panel_directory, N=3200, list_tokens, max_hours=12, allHistory=FALSE, sub_batch_size=100, sentiment=FALSE, darmoc=FALSE){
  message("Scraping timelines...")
  require(tidyverse)
  require(rtweet)
  message("Scraping users: ", panel_directory)
  today <- dateCode()
  message("Scraping date: ", today)
  set.seed(as.numeric(today))
  message("Reading prior scrape log...")
  logs <- dir(paste0(panel_directory,"twitter_scrapes/timeline_logs/")) %>% str_subset(., pattern="log_")
  last_log_file <- max(logs)
  message("Prior scraping date: ", last_log_file %>% str_sub(start = 5,end = 10))
  last_log <- readRDS(paste0(panel_directory,"twitter_scrapes/timeline_logs/",last_log_file))
  # this adds back any user_ids in the main folder and attempts to scrape them again if they're not in the log file for whatever reason
  raw_userids <- readRDS(paste0(panel_directory,"twitter_scrapes/user_ids.rds"))
  raw_userids <- raw_userids[which(!is.na(raw_userids))] #this fixes a bug where any NA user_ids who get added (shouldn't happen anymore, but might) don't break this function
  previously_attempted <- dir(paste0(panel_directory,"twitter_scrapes/timeline_attempts"), full.names = T) %>% map(readRDS) %>% unlist %>% unique
  if (any(!raw_userids %in% previously_attempted)){ #needs road testing
    new_userids <- raw_userids[which(!raw_userids %in% previously_attempted)]
    raw_userids <- raw_userids[which(!raw_userids %in% new_userids)]
    firstScrape(new_userids, panel_directory = panel_directory, tokens = list_tokens)
  }
  if (any(!raw_userids %in% last_log)){
    message("Pruning removed users from log file...")
    last_log <- last_log %>% filter(user_id %in% raw_userids)
  }
  # if (any(!raw_userids %in% last_log$user_id)) { #old version I am updating to do a firstScrape when appropriate, see above
  #   to_append <- cbind(raw_userids[which(!raw_userids %in% last_log$user_id)],
  #                      rep(NA, sum(!raw_userids %in% last_log$user_id)),
  #                      rep(NA, sum(!raw_userids %in% last_log$user_id)))
  #   colnames(to_append) <- colnames(last_log)
  #   last_log <- rbind(last_log, to_append)
  # }
  last_log <- sample_n(last_log, size = nrow(last_log))
  message("Start scraping...")
  if (allHistory==FALSE){
    data_list <- updateTimelines(users_df=last_log, n=N, list_tokens = list_tokens, per_token_limit=sub_batch_size, max_hours=max_hours)
  }
  if (allHistory==TRUE){
    data_list <- getTimelinesHistorical(users=last_log, n=N, list_tokens = list_tokens, per_token_limit=sub_batch_size, max_hours=max_hours)
  }
  data <- data_list[[1]]
  attempted <- data_list[[2]]
  if (nrow(data)>0){
    # UNCOMMENT WHEN SENTIMENT  is available to tricordR
    if (sentiment==TRUE){
      message("Analyzing sentiment...")
      source("~/Documents/GitRprojects/LaForge/functions/sentiment_analysis_functions.R")
      data <- addSentiment(data)
      # message("Saving data...")
      # saveRDS(data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
    }
    # if (sentiment=="google"){
    #   message("Analyzing sentiment...")
    #   source("~/Documents/GitRprojects/LaForge/functions/sentiment_analysis_functions.R")
    #   data <- addSentiment(data) #uses google, which costs money
    #   # message("Saving data...")
    #   # saveRDS(data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
    # }
    if (darmoc==TRUE){
      message("Analyzing ideology and sureness...")
      #load classifiers and feature names
      require(darmoc)
      preds <- darmoc::darmocClassify(input = data$text, type = "response")
      data <- cbind(data,preds)
      # message("Saving data...")
      # saveRDS(data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
    }
    if (darmoc==TRUE | sentiment == TRUE){
      message("Saving classified data...")
      if (file.exists(paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))){
        message("Binding to earlier scrape from today...")
        last_data <- readRDS(file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
        all_data <- bind_rows(last_data, data) %>% distinct(user_id, status_id, .keep_all = T)
        saveRDS(all_data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
      }
      if (!file.exists(paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))){
        saveRDS(data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
      }
    }
    if (!(darmoc==TRUE | sentiment == TRUE)){
      message("Saving scraped data...")
      if (file.exists(paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))){
        message("Binding to earlier scrape from today...")
        last_data <- readRDS(file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
        all_data <- bind_rows(last_data, data) %>% distinct(user_id, status_id, .keep_all = T)
        saveRDS(all_data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
      }
      if (!file.exists(paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))){
        saveRDS(data, file = paste0(panel_directory,"twitter_scrapes/timelines/timelines_", today,".rds"))
      }
    }
    message("Saving new log...")
    this_log <- data %>% distinct(user_id, status_id, .keep_all = T) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(status_id, 2), ultimate_tweet = maxNchar(status_id, 1), count = n())
    new_log <- rbind((last_log %>% select(user_id, penultimate_tweet, ultimate_tweet)),(this_log %>% select(user_id, penultimate_tweet, ultimate_tweet))) %>% group_by(user_id) %>% summarise(penultimate_tweet = maxNchar(penultimate_tweet, 1), ultimate_tweet = maxNchar(ultimate_tweet, 1)) #fixed parentheses order
    saveRDS(new_log, file = paste0(panel_directory,"twitter_scrapes/timeline_logs/log_", today,".rds"))
    message(sum((this_log$count-1)), " new tweets scraped from ", sum(this_log$count>1)," users!\n", (nrow(last_log)-sum(this_log$count>1)), " users had no new tweets to scrape.")
    message(sum(! last_log$ultimate_tweet %in% data$status_id), " users may have missing tweets.")
    saveRDS(attempted, file = paste0(panel_directory,"twitter_scrapes/timeline_attempts/attempted_", timeCode(),".rds"))
    message("Total attempted: ", sum(last_log$user_id %in% attempted)) # B
    message("Total unattempted: ", sum(! last_log$user_id %in% attempted)) # C ## I think I've fixed the numbers-not-adding-up problem, which I think was due to using this_log instead of last_log as the first argument to the logical
  }
  if (!(nrow(data)>0)){message("Zero new tweets scraped from this panel, skipping tweet analysis/saving...")}
}


#' Scrape All Specified Data for a Panel
#'
#' A high-level function to scrape all data types specified in scrape_settings for users in a given panel, and save the resulting data in that panel folder.
#' @param panel_directory The path to the panel folder corresponding to the set of users to be scraped.
#' @param tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param include_timelines Should timelines be scraped?
#' @param include_friends Should timelines be scraped?
#' @param include_followers Should timelines be scraped?
#' @param include_favorites Should favorites be scraped?
#' @param sentiment Should tweets be analyzed for sentiment?  Defaults to FALSE
#' @param darmoc Should tweets be analyzed for ideology and sureness? Defaults to FALSE
#' @keywords scraping
#' @export
#' @examples
#' scrapePanel()

scrapePanel <- function(panel_directory, tokens,
                        include_timelines = TRUE,
                        include_friends = TRUE,
                        include_followers = TRUE,
                        include_favorites = TRUE,
                        sentiment = FALSE,
                        darmoc = FALSE){
  scrape_settings <- readRDS(paste0(panel_directory, "scrape_settings.rds"))

  if (scrape_settings$scrape_timelines & include_timelines){
    scrapeTimelines(panel_directory = panel_directory,
                         list_tokens = tokens,
                         max_hours = 2, sentiment = sentiment, darmoc = darmoc)
  }
  if (scrape_settings$scrape_friends & include_friends){
    scrapeFriends(panel_directory = panel_directory,
                      list_tokens = tokens, n = 5000) #can't go higher than 5k yet
  }
  if (scrape_settings$scrape_followers & include_followers){
    scrapeFollowers(panel_directory = panel_directory,
                        list_tokens = tokens, n = 5000) #this can go bigger than 5k
  }
  if (scrape_settings$scrape_favorites & include_favorites){
    ws_scrape_favorites2(panel_directory = panel_directory,
                         days_further_back = 0, n=3000, list_tokens = tokens, max_hours=1) #this needs some road testing
  }
}

#' Scrape All Specified Data for All Panels in a Study
#'
#' A high-level function to scrape all data types specified in scrape_settings for users in all panel in a given study, and save the resulting data in the panel folders.
#' @param study_name The path to the panel folder corresponding to the set of users to be scraped.
#' @param tokens The list of tokens to be used for scraping.  See prepTokens().
#' @param include_timelines Should timelines be scraped?
#' @param include_friends Should timelines be scraped?
#' @param include_followers Should timelines be scraped?
#' @param include_favorites Should favorites be scraped?
#' @param sentiment Should tweets be analyzed for sentiment?  Defaults to none
#' @param darmoc Should tweets be analyzed for ideology and sureness? Defaults to FALSE
#' @keywords scraping
#' @export
#' @examples
#' scrapeStudy()

scrapeStudy <- function(study_name, tokens,
                        include_timelines = TRUE,
                        include_friends = TRUE,
                        include_followers = TRUE,
                        include_favorites = TRUE,
                        sentiment = "none",
                        darmoc = FALSE){
  all_panels_contents <- dir(paste0("~/tricordings/studies/",study_name), full.names = T) %>% dir(full.names = T)
  scrape_settings_paths <- all_panels_contents[str_detect(all_panels_contents, "scrape_settings.rds")]
  panel_directories <- str_remove_all(scrape_settings_paths,"scrape_settings.rds")

  for (i in 1:length(panel_directories)) {
    message("Scraping ", str_remove_all(panel_directories[i], ".*studies/"))
    scrapePanel(panel_directories[i], tokens = tokens,
                include_timelines,
                include_friends,
                include_followers,
                include_favorites,
                sentiment,
                darmoc)
  }
}

# implement this in all token-hopping functions:
# already_cycled <- FALSE
# while ((difftime(time1 = Sys.time(), time2 = start_time, units = "h") < max_hours) & (length(already) < nrow(users_df))){
#   batch <- batch + 1
#   message("Batch: ", batch)
#   for (i in 1:n_tokens) {
#     if (i == n_tokens){already_cycled <- TRUE}
#     message("\nToken: ", i)
#     message("Users Remaining: ", nrow(users_remaining)) # This doesn't equal B + C, below
#     if (! length(already) < nrow(users_df)) {break}
#     rl <- rtweet::rate_limit(query = "get_followers", token = list_tokens[[i]])
#     if (rl$remaining < 5) { #calibrate this
#       if(already_cycled){
#         wait <- rl$reset + 0.1
#         message(paste("Waiting for", round(wait,2),"minutes..."))
#         Sys.sleep(wait * 60)
#       }
#       if(!already_cycled){
#         next
#       }
#     }
#
#
# ##################################
# # SENTIMENT ANALYSIS FUNCTIONS
# ##################################
#
# # script to get Google sentiment analysis for a dataframe of tweets
#
# require(tidyverse, warn.conflicts = FALSE)
# # require(googleLanguageR)
# # gl_auth("~/.keys/gl-auth.json")
# #
# # addSentiment <- function(timelines_df){
# #   sentiments <- gl_nlp(string=timelines_df$text,
# #                          nlp_type = "analyzeSentiment")
# #   timelines_df_c <- cbind(timelines_df,sentiments$documentSentiment)
# #   return(timelines_df_c)
# # }
#


#' Make Sentiment
#'
#' A function to classify tweets for sentiment.  Usually used via higher-level functions.
#' @param input Text to classify for sentiment.
#' @keywords sentiment
#' @export
#' @examples
#' makeSentiment()

makeSentiment <- function(input){
  sentences <- sentimentr::get_sentences(input)
  sentiments <- sentimentr::sentiment_by(sentences, polarity_dt = lexicon::hash_sentiment_jockers_rinker)$ave_sentiment
  return(sentiments)
}

#' Batched Sentiment
#'
#' A function to classify tweets for sentiment in batches.  Usually used via higher-level functions.
#' @param input Text to classify for sentiment.
#' @param batch_size Size of batches to classify.  Defaults to 1000.
#' @keywords sentiment
#' @export
#' @examples
#' batchedSentiment()

batchedSentiment <- function(input, batch_size=1000){
  nBatches <- length(input)/batch_size
  nWholeBatches <- floor(nBatches)
  p_list <- list()
  for (i in 1:nWholeBatches) {
    #pb <- txtProgressBar(min=0, max=nWholeBatches, style=3) #when min=1, this breaks 1-iteration loops.  Commenting out for safety (not needed for scripts anyway)
    #setTxtProgressBar(pb, i)
    p_list[[i]] <- makeSentiment(input[((i-1)*batch_size +1):((i)*batch_size)])
  }
  message("Analyzing final batch...")
  if (nBatches!=nWholeBatches){p_list[[i+1]] <- makeSentiment(input[((i)*batch_size +1):length(input)])}
  message("Binding batches together...")
  return(do.call(c,p_list))
}

#' Add Sentiment
#'
#' A function to add sentiment scores to timeline data.
#' @param timelines_df Timeline dataframe to add sentiment scores to.
#' @keywords sentiment
#' @export
#' @examples
#' addSentiment()

addSentiment <- function(timelines_df){
  if (nrow(timelines_df)>1000){
    sentiments <- batchedSentiment(timelines_df$text)
  }
  if (nrow(timelines_df)<=1000){
    sentiments <- makeSentiment(timelines_df$text)
  }
  timelines_df_c <- cbind(timelines_df, "sentiment_score" = sentiments)
  return(timelines_df_c)
}

#
# ########################################
# # DASHBOARD FUNCTIONS
# ###################################
#

#' Run Dashboard
#'
#' A convenient function to launch any dashboard.
#' @param dashboard Which Dashboard to run.
#' @param port Specify the port to run the dashboard on.  Defaults to 4201.
#' @keywords dashboard
#' @export
#' @examples
#' runTimelineDash()

runTimelineDash <- function(dashboard, port = 4201) {
  appDir <- system.file(paste0("dashboards/",dashboard), "app.R", package = "tricordR")
  if (appDir == "") {
    stop("Could not find dashboard directory. Try re-installing `tricordR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", port = port)
}


#' Run Timeline Dashboard
#'
#' A convenient function to launch the interactive timeline data collection dashboard.
#' @param port Specify the port to run the dashboard on.  Defaults to 4201.
#' @keywords dashboard
#' @export
#' @examples
#' runTimelineDash()

runTimelineDash <- function(port = 4201) {
  appDir <- system.file("dashboards/timeline_dash_2group_study", "app.R", package = "tricordR")
  if (appDir == "") {
    stop("Could not find dashboard directory. Try re-installing `tricordR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", port = port)
}


#' Run Network Dashboard
#'
#' A convenient function to launch the interactive network graph dashboard.
#' @param port Specify the port to run the dashboard on.  Defaults to 4202.
#' @keywords dashboard
#' @export
#' @examples
#' runNetworkDash()

runNetworkDash <- function(port=4202){
  appDir <- system.file("dashboards/2_networks_d3",
                        "app.R", package = "tricordR")
  if (appDir == "") {
    stop("Could not find dashboard directory. Try re-installing `tricordR`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", port = port)
}


#' Fix Sentiment
#'
#' A function to fix sentiment values.
#' @param input Sentiment values to fix
#' @keywords dashboard
#' @export
#' @examples
#' fixSentiment()

fixSentiment <- function(input, allowed_range = c(-1,1)){
  input[which(input<allowed_range[1])] <- allowed_range[1]
  input[which(input>allowed_range[2])] <- allowed_range[2]
  return(input)
}

#' Run Timeline Dashboard
#'
#' A convenient function to launch the interactive timeline data collection dashboard.
#' @param port Specify the port to run the dashboard on.  Defaults to 4201.
#' @keywords dashboard
#' @export
#' @examples
#' runTimelineDashD3()

runTimelineDashD3 <- function(port = 4202) {
  appDir <- system.file("dashboards/2_timelines_d3", "app.R", package = "tricordR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tricordR`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", port = port)
}

#' Fix Sentiment
#'
#' A function to fix sentiment values.
#' @param input Sentiment values to fix
#' @keywords dashboard
#' @export
#' @examples
#' fixSentiment()

fixSentiment <- function(input, allowed_range = c(-1,1)){
  input[which(input<allowed_range[1])] <- allowed_range[1]
  input[which(input>allowed_range[2])] <- allowed_range[2]
  return(input)
}

#' Indexer
#'
#' A function to make index values.
#' @param input Input values to index
#' @param reference Reference values against which to index
#' @keywords dashboard
#' @export
#' @examples
#' myIndexer()

myIndexer <- function(input, reference){
  myvec <- rep(NA, length(input))
  for (i in 1:length(input)){
    myvec[i] <- which(reference==input[i])
  }
  return(myvec)
}

#' Read and Session
#'
#' A function to read timeline scrapes and append their session ids.
#' @param input Directory from which to read scrapes
#' @keywords dashboard
#' @export
#' @examples
#' read_and_session()

read_and_session <- function(input){
  return(readRDS(input) %>% mutate(scrape_session = str_remove_all(input, ".*timelines_|.rds")))
}

#' Prep Timeline Data
#'
#' A workhorse function to read timeline data into R for dashboard visualization.
#' @param panel_directory The directory in which panel data is stored.
#' @param sessions_back How many sessions back do you want to read in? Irrelevant if load_all_since_first is TRUE.
#' @param include_historical Should the historical timeline scrapes collected in the "first_timelines" folder be included?  Defaults to FALSE.
#' @param load_all_since_first Should all sessions be loaded?  Defaults to FALSE.
#' @param all_columns Should all timeline variables be included, or only those needed for dashboard visualizations?  Defaults to FALSE.
#' @keywords dashboard
#' @export
#' @examples
#' prep_timeline_data()

prep_timeline_data <- function(panel_directory, sessions_back, include_historical = FALSE, load_all_since_first = FALSE, all_columns = FALSE){
  user_ids <- readRDS(file = paste0(panel_directory, "/twitter_scrapes/user_ids.rds")) #bind to available data
  current_lookup <- readRDS(file = paste0(panel_directory, "/twitter_scrapes/user_info/current_lookup.rds"))

  scrape_dir <- dir(paste0(panel_directory,"/twitter_scrapes/timelines/"), full.names = TRUE) %>% str_subset("timelines_")
  if (!load_all_since_first){
    scrape_dir <- scrape_dir %>% .[(length(.)-min((sessions_back-1), (length(.)-1))):length(.)]
    scrape_dir <- scrape_dir[-which(is.na(scrape_dir))]
  }

  if (include_historical){
    first_timelines_dir <- dir(paste0(panel_directory,"/twitter_scrapes/first_timelines/"), full.names = TRUE)
    scrape_dir <- c(scrape_dir, first_timelines_dir)
  }

  timelines_bound <- scrape_dir %>% map_dfr(read_and_session)

  if(all_columns){
    keep_if_there <- colnames(timelines_bound)
  } else {
    keep_if_there <- c("user_id", "status_id", "screen_name", "created_at", "scrape_session", "is_retweet", "is_quote", "reply_to_user_id", "text", "sentiment_score", "ideology", "sureness")
    }

  e <- timelines_bound %>%
    distinct(user_id, status_id, .keep_all = T) %>%
    select(colnames(timelines_bound)[which(colnames(timelines_bound) %in% keep_if_there)]) %>%
    mutate(scrape_session=as.numeric(scrape_session))

  missing <- keep_if_there[which(! keep_if_there %in% colnames(e))]
  missing_cols <- array(dim = c(nrow(e),length(missing)))
  colnames(missing_cols) <- missing
  e <- cbind(e, missing_cols)

  unique_sessions <- unique(e$scrape_session)
  unique_users <- unique(e$user_id)
  e$session_index <- myIndexer(input=e$scrape_session, reference=unique_sessions)
  e$user_index <- myIndexer(input=e$user_id, reference=unique_users)

  e_f <- e %>% group_by(user_id) %>% summarise(screen_name = screen_name[1],
                                               user_id = user_id[1],
                                               last=max(created_at),
                                               index=user_index[1])

  subset_current_lookup <- current_lookup %>% filter(!(user_id %in% e_f$user_id)) %>% transmute(screen_name, user_id, index = NA)
  for (i in 1:nrow(subset_current_lookup)){
    subset_current_lookup$index[i] <- i + max(e_f$index)
  }
  e_f <- bind_rows(e_f,subset_current_lookup)

  if (all_columns) {return(e)} else {return(list(e, e_f))}
}


#' Load Timelines
#'
#' A user-friendly function to load all timeline data collected for a given panel.
#' @param study_name The name of the study in which the panel to load is located.
#' @param panel_name The name of the panel to load.
#' @keywords dashboard
#' @export
#' @examples
#' loadTimelines()

loadTimelines <- function(study_name, panel_name){
  return(prep_timeline_data(panel_directory = paste0("~/tricordings/studies/",study_name,"/",panel_name), sessions_back = 2, include_historical = TRUE, load_all_since_first = TRUE, all_columns = TRUE))
}


#' Make Time Series
#'
#' A function to make time series data.
#' @param input Input data to reformat as time series.
#' @keywords dashboard
#' @export
#' @examples
#' make_timeseries()

make_timeseries <- function(input, volume_smoothing, midnight_today, time_range){
  output <- input %>%
    mutate(minute_span = round_date(created_at, unit = paste(volume_smoothing,"minutes"))) %>%
    group_by(minute_span) %>%
    summarise(count = n())
  all_minutes <- seq.POSIXt(min(output$minute_span, na.rm=T), midnight_today, by = paste(volume_smoothing,"min"))
  other_minutes <- all_minutes[which(!all_minutes %in% output$minute_span)]
  output <- bind_rows(output, data.frame("minute_span"=other_minutes,"count"=rep(0, length(other_minutes)))) %>% arrange(minute_span) %>% filter(minute_span>time_range[1])
}


#' Dot Plot
#'
#' A function to visualize timeline scrapes as dot plots.
#' @param data_e Timeline data to plot.
#' @param data_e_f Timeline metadata to plot.
#' @param days days back
#' @param color_variable which variable to color by
#' @param show_names show names?
#' @param sentiment_left_color left end of sentiment color spectrum
#' @param sentiment_right_color right end of sentiment color spectrum
#' @param ideo_left_color left end of ideo color spectrum
#' @param ideo_right_color right end of ideo color spectrum
#' @param point_cex point size
#' @param axis_cex axis size
#' @param screen_name_cex screen name size
#' @param screen_name_cols screen name colors
#' @param sentiment_reference_scale sentiment reference scale
#' @param ideo_reference_scale ideo reference scale
#' @keywords dashboard
#' @export
#' @examples
#' dotPlot()

dotPlot <- function(data_e, data_e_f, days, color_variable, show_names, sentiment_left_color, sentiment_right_color, ideo_left_color, ideo_right_color, point_cex, axis_cex, screen_name_cex, screen_name_cols, screen_name_fonts, sentiment_reference_scale = NA, ideo_reference_scale = NA){

  midnight_today <- as.POSIXct(paste0(as.character(Sys.Date()), " 00:00:00 EST"))
  time_range <- c((midnight_today-((days )*60*60*24)),midnight_today + 60*60*24)

  date_axis <- seq(time_range[1], time_range[2], by = 60*60*24)

  #par(xpd=T, bg="#222222", mfrow = c(2,1))
  #layout(matrix(matrix(c(1,2,2,2)), nrow=4, ncol=1, byrow = TRUE))
  #par(xpd=T, bg="#222222")
  par(xpd=F, bg="#343E48")
  par(bty="n")

  par(mar=c(4.1, 4.1, 0.1, 4.1))
  if (color_variable=="none") {plot(x=data_e$created_at, y=data_e$user_index,
                                    ylim = range(data_e_f$index),
                                    col=default_point_color,
                                    xlim = time_range,
                                    yaxt="n", ylab = "", pch=15, xaxt="n", cex=point_cex, xlab="")}
  if (color_variable=="sentiment_score") {plot(x=data_e$created_at, y=data_e$user_index,
                                         ylim = range(data_e_f$index),
                                         col=plotGradient(input = fixSentiment(data_e$sentiment_score), left_color = sentiment_left_color, right_color = sentiment_right_color, reference_scale = sentiment_reference_scale),
                                         xlim = time_range,
                                         yaxt="n", ylab = "", pch=15, xaxt="n", cex=point_cex, xlab="")}
  if (color_variable=="ideology") {plot(x=data_e$created_at, y=data_e$user_index,
                                        ylim = range(data_e_f$index),
                                        col=plotGradient(input = data_e$ideology, left_color = ideo_left_color, right_color = ideo_right_color, reference_scale = ideo_reference_scale),
                                        xlim = time_range,
                                        yaxt="n", ylab = "", pch=15, xaxt="n", cex=point_cex, xlab="")}
  axis(side=1, at=date_axis, labels=format(date_axis, "%b %d"), cex.axis = axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
  if(show_names){
    par(xpd=T)
    data_e_f$status <- 3
    data_e_f$status[which(data_e_f$last<min(date_axis))] <- 2
    data_e_f$status[which(data_e_f$last>=min(date_axis))] <- 1
    data_e_f$tpos <- 4
    data_e_f$tpos[which(data_e_f$status>1)] <- 2
    data_e_f$xpos <- data_e_f$last
    data_e_f$xpos[which(data_e_f$status>1)] <- min(date_axis)
    text(x=data_e_f$xpos, y=data_e_f$index,
         labels = data_e_f$screen_name,
         cex = screen_name_cex,
         pos = data_e_f$tpos,
         col=screen_name_cols[data_e_f$status],
         font = screen_name_fonts[data_e_f$status])
  }
  #if(show_now){
  par(xpd=F)
  time_now <- Sys.time()
  abline(v = time_now, col = "gray")
  par(xpd=T)
  text(x = time_now, y=0, labels = format(time_now, format = "%H:%M"), col = "gray", pos = 1)
  #}
}

#' Line Plot
#'
#' A function to visualize timeline scrapes as line plots.
#' @param data_e timeline data to plot
#' @keywords dashboard
#' @export
#' @examples
#' linePlot()

linePlot <- function(data_e, days, volume_smoothing, axis_cex){

  midnight_today <- as.POSIXct(paste0(as.character(Sys.Date()), " 00:00:00 EST"))
  time_range <- c((midnight_today-((days )*60*60*24)),midnight_today + 60*60*24)

  date_axis <- seq(time_range[1], time_range[2], by = 60*60*24)

  panel_timeseries <- make_timeseries(data_e, volume_smoothing, midnight_today, time_range)

  #par(xpd=T, bg="#222222", mfrow = c(2,1))

  #par(xpd=T, bg="#222222")
  par(xpd=T, bg="#343E48")
  par(bty="n")

  par(mar=c(2.1, 4.1, 0.1, 4.1))
  plot(panel_timeseries$minute_span, panel_timeseries$count,
       type="l",
       col="gray", xlab="",
       xaxt="n",
       ylab = "",
       yaxt = "n",
       col.axis = "gray",
       fg = "gray",
       ylim = range(c(panel_timeseries$count, panel_timeseries$count), na.rm = T),
       xlim = time_range)
  axis(side=1, at=date_axis, labels=format(date_axis, "%b %d"), cex.axis = axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
  #axis(side=2, at=range(panel_timeseries$count), cex.axis = axis_cex, col = "grey", col.ticks="grey", col.axis="grey")
}


# # Survey stuff
#
# prep_survey_data <- function(experiment_directory, panel_name){
#   survey_dir <- dir(paste0(experiment_directory, panel_name, "/survey_scrapes/"), full.names = T)
#   surveys_bound <- readRDS(max(survey_dir))
#
#   link_dir <- dir(paste0(experiment_directory, panel_name, "/id_links/"), full.names = T)
#   links_bound <- map_dfr(link_dir, readRDS)
#
#   survey_data_joined <- left_join(surveys_bound, links_bound)
#   survey_data_joined_GOOD <- survey_data_joined %>% filter(!is.na(user_id))
#
#   return(list(survey_data_joined, survey_data_joined_GOOD))
# }
#
# make_survey_timeseries <- function(input, volume_smoothing, survey_data_joined_GOOD, survey_start_date){
#   output <- input %>%
#     mutate(minute_span = round_date(RecordedDate, unit = paste(volume_smoothing,"minutes"))) %>%
#     group_by(minute_span) %>%
#     summarise(count = n(),
#               count_good = sum(ResponseId %in% survey_data_joined_GOOD$ResponseId, na.rm = T),
#               count_bad = sum(! ResponseId %in% survey_data_joined_GOOD$ResponseId, na.rm = T))
#   all_minutes <- seq.POSIXt(as.POSIXct(survey_start_date), ceiling_date(Sys.time(), "day"), by = paste(volume_smoothing,"min")) #changed noon_today to sys.time()
#   other_minutes <- all_minutes[which(!all_minutes %in% output$minute_span)]
#   output <- bind_rows(output, data.frame("minute_span"=other_minutes,
#                                          "count"=rep(0, length(other_minutes)),
#                                          "count_good"=rep(0, length(other_minutes)),
#                                          "count_bad"=rep(0, length(other_minutes)))) %>% arrange(minute_span)
#   return(output)
# }
#
# ts_to_cumulative <- function(survey_ts){
#   survey_cumulative <- survey_ts %>% transmute(minute_span,
#                                                count=NA,
#                                                count_good=NA,
#                                                count_bad=NA)
#   for (i in 1:nrow(survey_cumulative)) {
#     survey_cumulative$count[i] <- sum(survey_ts$count[1:i])
#     survey_cumulative$count_good[i] <- sum(survey_ts$count_good[1:i])
#     survey_cumulative$count_bad[i] <- sum(survey_ts$count_bad[1:i])
#   }
#   return(survey_cumulative)
# }
#
# # NETWORK STUFF
# require(igraph)
#

#
# id_to_sn <- function(id, lookup){
#   require(dplyr)
#   if(length(id)==1){out <- lookup %>% filter(user_id == id) %>% pull(screen_name)}
#   if(length(id)>1){
#     out <- c()
#     for (i in 1:length(id)){
#       out[i] <- lookup %>% filter(user_id == id[i]) %>% pull(screen_name)
#     }
#   }
#   return(out)
# }
#
# # prep_network_data <- function(experiment_directory, panel_name, assignment_panel){
# #   p_friends_all <- dir(paste0(experiment_directory, panel_name, "/twitter_scrapes/friends/"), full.names = T) %>% map_dfr(., readRDS)
# #
# #   par_info <- dir(paste0(experiment_directory, panel_name, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "participant")
# #   ass_info <- dir(paste0(experiment_directory, assignment_panel, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "assignment")
# #   all_info <- rbind(par_info, ass_info)
# #
# #   id_links <- dir(paste0(experiment_directory, panel_name, "/id_links/"), full.names = T) %>% map_dfr(., readRDS)
# #   survey_responses_GOOD <- prep_survey_data(experiment_directory, panel_name)[[2]]
# #
# #   vertex_metadata <- all_info %>% left_join(., id_links) %>% left_join(., survey_responses_GOOD)
# #   vertex_metadata$group[which(vertex_metadata$t==1)] <- "treated"
# #   vertex_metadata$group[which(vertex_metadata$t==0)] <- "placeboed"
# #
# #   data <- p_friends_all %>% filter(user_id %in% vertex_metadata$user_id) %>% group_by(user, user_id) %>% summarise(first = min(scraped_at), last = max(scraped_at), orange = difftime(Sys.time(), last, units = "day")>1)
# #
# #   graph <- graph_from_data_frame(data, directed = TRUE, vertices = vertex_metadata)
# #
# #   V(graph)$label <- V(graph)$screen_name
# #
# #   #V(graph)$frame.color <- "red"
# #
# #   V(graph)$color[which(V(graph)$group=="assignment")] <- "gray40"
# #   V(graph)$color[which(V(graph)$group=="treated")] <- "dodgerblue2"
# #   V(graph)$color[which(V(graph)$group=="placeboed")] <- "deeppink3"
# #   E(graph)$color[which(E(graph)$orange)] <- "orange"
# #   E(graph)$color[which(!E(graph)$orange)] <- "green"
# #
# #   return(graph)
# # }
#
# prep_network_data_igraph <- function(experiment_directory, panel_name, assignment_panel){
#   p_friends_all <- dir(paste0(experiment_directory, panel_name, "/twitter_scrapes/friends/"), full.names = T) %>% map_dfr(., readRDS)
#
#   par_info <- dir(paste0(experiment_directory, panel_name, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "participant")
#   ass_info <- dir(paste0(experiment_directory, assignment_panel, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "assignment")
#   all_info <- rbind(par_info, ass_info)
#
#   id_links <- dir(paste0(experiment_directory, panel_name, "/id_links/"), full.names = T) %>% map_dfr(., readRDS)
#   survey_responses_GOOD <- prep_survey_data(experiment_directory, panel_name)[[2]]
#
#   vertex_metadata <- all_info %>% left_join(., id_links) %>% left_join(., survey_responses_GOOD)
#   vertex_metadata$group[which(vertex_metadata$t==1)] <- "treated"
#   vertex_metadata$group[which(vertex_metadata$t==0)] <- "placeboed"
#
#   data <- p_friends_all %>% filter(user_id %in% vertex_metadata$user_id) %>% group_by(user, user_id) %>% summarise(first = min(scraped_at), last = max(scraped_at), color = ifelse(difftime(Sys.time(), last, units = "day")>1, "orange", "green"))
#
#   data <- map_dfr(which(vertex_metadata$group!="assignment"),
#                   ~data.frame("user" = vertex_metadata$user_id[.x],
#                               "user_id" = vertex_metadata$shown[[.x]],
#                               first = NA, last = NA, color = "red")) %>%
#     anti_join(., data, by = c("user", "user_id")) %>%
#     rbind(., data)
#
#   graph <- graph_from_data_frame(data, directed = TRUE, vertices = vertex_metadata)
#
#   V(graph)$label <- V(graph)$screen_name
#
#   #V(graph)$frame.color <- "red"
#   V(graph)$color[which(V(graph)$group=="assignment")] <- assignment_node_col
#   V(graph)$color[which(V(graph)$group=="treated")] <- treated_node_col
#   V(graph)$color[which(V(graph)$group=="placeboed")] <- placeboed_node_col
#   #E(graph)$color[which(E(graph)$orange)] <- "orange"
#   #E(graph)$color[which(!E(graph)$orange)] <- "green"
#
#   return(graph)
# }

#require(networkD3)

# prep_network_data_d3_SPIRALS200706 <- function(experiment_directory, panel_name, assignment_panel){
#   p_friends_all <- dir(paste0(experiment_directory, panel_name, "/twitter_scrapes/friends/"), full.names = T) %>% map_dfr(., readRDS)
#
#   par_info <- dir(paste0(experiment_directory, panel_name, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "participant")
#   ass_info <- dir(paste0(experiment_directory, assignment_panel, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "assignment")
#   all_info <- rbind(par_info, ass_info)
#
#   id_links <- dir(paste0(experiment_directory, panel_name, "/id_links/"), full.names = T) %>% map_dfr(., readRDS)
#   id_links <- id_links[!duplicated(id_links$ResponseId, fromLast = T),]
#
#   #anyDuplicated(id_links$ResponseId, fromLast = T)
#
#   # survey_responses_GOOD <- prep_survey_data(experiment_directory, panel_name)[[2]]
#   # vertex_metadata <- all_info %>% left_join(., id_links) %>% left_join(., survey_responses_GOOD)
#
#   survey_responses <- prep_survey_data(experiment_directory, panel_name)[[1]] %>% distinct(ResponseId, .keep_all = T)
#   #vertex_metadata <- all_info %>% left_join(., id_links) %>% left_join(., survey_responses)
#   #vertex_metadata <- survey_responses %>% left_join(., id_links) %>% full_join(., all_info)
#
#   vertex_metadata <- id_links %>% select(ResponseId, shown, claimed, user_id) %>% left_join(., survey_responses %>% select(-c(shown, claimed, user_id)), by="ResponseId") %>% full_join(., all_info)
#   #vertex_metadata <- all_info %>% full_join(., id_links) %>% left_join(., survey_responses)
#
#   vertex_metadata$group[which(vertex_metadata$t==1)] <- "treated"
#   vertex_metadata$group[which(vertex_metadata$t==0)] <- "placeboed"
#
#   na_user_ids_indices <- which(is.na(vertex_metadata$user_id))
#   for(i in 1:length(na_user_ids_indices)){
#     vertex_metadata$user_id[na_user_ids_indices[i]] <- paste0("UNKNOWN_",i)
#     vertex_metadata$screen_name[na_user_ids_indices[i]] <- paste0("UNKNOWN_",i)
#   }
#
#   data <- p_friends_all %>%
#     filter(user_id %in% vertex_metadata$user_id) %>%
#     group_by(user, user_id) %>%
#     summarise(first = min(scraped_at),
#               last = max(scraped_at),
#               color = ifelse(difftime(Sys.time(), last, units = "day")>1,
#                              "orange",
#                              "green"))
#
#   data <- map_dfr(which(vertex_metadata$group!="assignment"),
#                   ~data.frame("user" = vertex_metadata$user_id[.x],
#                               "user_id" = vertex_metadata$shown[[.x]],
#                               first = NA, last = NA, color = "red")) %>%
#     anti_join(., data, by = c("user", "user_id")) %>%
#     rbind(., data)
#   return(list("e" = data, "v" = vertex_metadata))
# }

#' Prepare Network Data for D3 Visualization
#'
#' A function to prepare network data for D3 visualization in dashboards.
#' @param study_name Directory to visualize
#' @param panel_name Name of the panel representing "participants".
#' @param assignment_panel Name of the panel representing "assignments".  Defaults to "assignments".
#' @keywords dashboard
#' @export
#' @examples
#' prep_network_data_d3()

prep_network_data_d3 <- function(study_name, panel_name, assignment_panel = "assignments"){
  p_friends_all <- dir(paste0("~/tricordings/studies/", study_name, "/", panel_name, "/twitter_scrapes/friends/"), full.names = T) %>% map_dfr(., readRDS)

  if (nrow(p_friends_all)==0) {p_friends_all <- dir(paste0("~/tricordings/studies/", "/", study_name, "/", panel_name, "/twitter_scrapes/first_friends/"), full.names = T)[1] %>% map_dfr(., readRDS)}

  par_info <- dir(paste0("~/tricordings/studies/", study_name, "/", panel_name, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "participant")
  ass_info <- dir(paste0("~/tricordings/studies/", study_name, "/", assignment_panel, "/twitter_scrapes/user_info/"), full.names = T) %>% map_dfr(readRDS) %>% arrange(desc(created_at)) %>% distinct(user_id, .keep_all = T) %>% mutate(group = "assignment")
  all_info <- rbind(par_info, ass_info)

  #id_links <- dir(paste0("~/tricordings/studies/", study_name, "/", panel_name, "/id_links/"), full.names = T) %>% map_dfr(., readRDS)
  #id_links <- id_links[!duplicated(id_links$ResponseId, fromLast = T),]

  #survey_responses <- prep_survey_data(experiment_directory, panel_name)[[1]] %>% distinct(ResponseId, .keep_all = T)
  #vertex_metadata <- id_links %>% select(ResponseId, shown, claimed, user_id) %>% left_join(., survey_responses %>% select(-c(shown, claimed, user_id)), by="ResponseId") %>% full_join(., all_info)
  vertex_metadata <- all_info


  #vertex_metadata$group[which(vertex_metadata$t==1)] <- "treated"
  #vertex_metadata$group[which(vertex_metadata$t==0)] <- "placeboed"

  # na_user_ids_indices <- which(is.na(vertex_metadata$user_id))
  # for(i in 1:length(na_user_ids_indices)){
  #   vertex_metadata$user_id[na_user_ids_indices[i]] <- paste0("UNKNOWN_",i)
  #   vertex_metadata$screen_name[na_user_ids_indices[i]] <- paste0("UNKNOWN_",i)
  # }

  data <- p_friends_all %>%
    filter(user_id %in% vertex_metadata$user_id) %>%
    group_by(user, user_id) %>%
    summarise(first = min(scraped_at),
              last = max(scraped_at),
              color = ifelse(difftime(Sys.time(), last, units = "day")>1,
                             "orange",
                             "green"))

  # data <- map_dfr(which(vertex_metadata$group!="assignment"),
  #                 ~data.frame("user" = vertex_metadata$user_id[.x],
  #                             "user_id" = vertex_metadata$shown[[.x]],
  #                             first = NA, last = NA, color = "red")) %>%
  #   anti_join(., data, by = c("user", "user_id")) %>%
  #   rbind(., data)
  return(list("e" = data, "v" = vertex_metadata))
}

#' Index Single Network Nodes
#'
#' A workhorse function for nodeIndexer
#' @param twitter_user_id The twitter user_id to look up.
#' @param thismetadata The metadata in which to look up the twitter user_id.
#' @keywords dashboard
#' @export
#' @examples
#' singleNodeIndexer()

singleNodeIndexer <- function(twitter_user_id, thismetadata)
  {return(thismetadata$NodeID[which(thismetadata$user_id==twitter_user_id)])}

#' Index Network Nodes
#'
#' A function to index network nodes for visualization
#' @param twitter_user_id The twitter user_id to look up.
#' @param metadata The metadata in which to look up the twitter user_id.
#' @keywords dashboard
#' @export
#' @examples
#' nodeIndexer()

nodeIndexer <- function(twitter_user_id, metadata){
  if (length(twitter_user_id)==1){
    return(singleNodeIndexer(twitter_user_id,thismetadata=metadata))
  }
  if (length(twitter_user_id)>1){
    return(sapply(X = twitter_user_id, FUN=singleNodeIndexer, thismetadata=metadata))
  }
}

################################
# QUALTRICS FUNCTIONS
###############################

# qualtrics-related functions


#' Match Screenname to User ID
#'
#' A function to match screen names to user ids.
#' @param sn Screen name to match
#' @param treatment_acct_info Reference info set to consult.
#' @keywords utility
#' @export
#' @examples
#' sn_to_userid()

sn_to_userid <- function(sn, treatment_acct_info){
  if(! sn %in% treatment_acct_info$screen_name){
    message("This screen name is not in the reference set!")
    return(NA)
  }
  if(length(sn)==1){
    return(treatment_acct_info %>% filter(screen_name == sn) %>% pull(user_id))
  }
  if(length(sn)>1){
    output <- c()
    for (i in 1:length(sn)){
      output[i] <- treatment_acct_info %>% filter(screen_name == sn[i]) %>% pull(user_id)
    }
    return(output)
  }
}


#' Return index of maximum value
#'
#' A function to return the index of the maximum value in the input.
#' @param x Input vector to maximize.
#' @keywords utility
#' @export
#' @examples
#' which_maxes()

which_maxes <- function(x){which(x == max(x, na.rm = TRUE))}

#' Link IDs
#'
#' A function to link ids.
#' @param claims Accounts claimed by the participant.
#' @param new_friends_by_follower New friends by follower.
#' @keywords matching
#' @export
#' @examples
#' link_ids()

link_ids <- function(claims, new_friends_by_follower){
  id_links <- claims %>% mutate(user_id = NA, perfect_match = NA, unique_match = NA, no_match = NA, candidates = list(NA))
  for (i in 1:nrow(id_links)) {
    this_claim <- id_links$claimed[i] %>% unlist #who did respondent i claim to follow
    list_match_logical <- list()
    for (j in 1:nrow(new_friends_by_follower)) {
      list_match_logical[[j]] <- this_claim %in% new_friends_by_follower$new_friends[[j]]
    }
    id_links$candidates[[i]] <- new_friends_by_follower %>% transmute(follower, all_match = sapply(list_match_logical, all), match_count = sapply(list_match_logical, sum)) %>% filter(match_count>1) # made this match_count>1 (used to be match_count>0), since all matches must have at least 1
    if (sum(id_links$candidates[[i]]$all_match)==1){ #if there is one perfect match
      id_links$user_id[i] <- id_links$candidates[[i]] %>% filter(all_match) %>% pull(follower)
      id_links$perfect_match[i] <- TRUE
      id_links$unique_match[i] <- TRUE
      id_links$no_match[i] <- FALSE
    }
    if (sum(id_links$candidates[[i]]$all_match)>1){ #if there are several perfect matches
      id_links$perfect_match[i] <- TRUE
      id_links$unique_match[i] <- FALSE
      id_links$no_match[i] <- FALSE
    }
    if (sum(id_links$candidates[[i]]$all_match)<1){ #if there are no perfect matches
      id_links$perfect_match[i] <- FALSE
      these_maxes <- which_maxes(id_links$candidates[[i]]$match_count)
      if (length(these_maxes)==1){ #if there is one best match (changed so don't try to guess if imperfect; leave to remediation)
        #id_links$user_id[i] <- id_links$candidates[[i]]$follower[these_maxes]
        id_links$unique_match[i] <- TRUE
        id_links$no_match[i] <- FALSE
      }
      if (length(these_maxes)>1){ #if there are several best matches
        id_links$unique_match[i] <- FALSE
        id_links$no_match[i] <- FALSE
      }
      if (length(these_maxes)<1){ #if there are no matches
        id_links$unique_match[i] <- FALSE
        id_links$no_match[i] <- TRUE
      }
    }
  }
  return(id_links)
}

# match_by_following_5 <- function(responses_new, study_name, panel_name, assignment_dir, participant_tokens, this_timecode){
#   prior_treatment_followers_path <- maxN(dir(paste0(assignment_dir,"/twitter_scrapes/followers"), full.names = T), N = 3) #get 3rd most recent
#
#   prior_treatment_followers <- readRDS(prior_treatment_followers_path)
#   treatment_acct_info <- readRDS(file = paste0(assignment_dir,"/twitter_scrapes/user_info/current_lookup.rds"))
#
#   message("Identifying claims...")
#   claims <- responses_new %>% select(ResponseId, starts_with("follow"), f1, f2, f3, f4, f5) %>%
#     transmute(ResponseId,
#               claim1 = !is.na(follow1),
#               claim2 = !is.na(follow2),
#               claim3 = !is.na(follow3),
#               claim4 = !is.na(follow4),
#               claim5 = !is.na(follow5),
#               f1=sn_to_userid(f1, treatment_acct_info),
#               f2=sn_to_userid(f2, treatment_acct_info),
#               f3=sn_to_userid(f3, treatment_acct_info),
#               f4=sn_to_userid(f4, treatment_acct_info),
#               f5=sn_to_userid(f5, treatment_acct_info))
#
#   c_mat <- claims %>% select(starts_with("claim")) %>% as.matrix
#   f_mat <- claims %>% select(starts_with("f")) %>% as.matrix
#
#   all_list <- list()
#   claim_list <- list()
#   for(i in 1:nrow(claims)){
#     all_list[[i]] <- f_mat[i,]
#     claim_list[[i]] <- f_mat[i,c_mat[i,]]
#   }
#
#   claims <- claims %>% transmute(ResponseId, shown = all_list, claimed = claim_list)
#
#   message("Reading most recent treatment followers scrape...")
#   treatment_followers_current_scrape_path <- max(dir(paste0(assignment_dir,"/twitter_scrapes/followers"), full.names = T)) #get first most recent
#   treatment_followers_current_scrape <- readRDS(treatment_followers_current_scrape_path)
#
#   #new followers of treatments
#   new_followers <- treatment_followers_current_scrape %>% filter(! user_id %in% prior_treatment_followers$user_id) #this might lose anyone who happened to follow any treatments before
#   if(nrow(new_followers)==0){stop("No new followers!")}
#
#   unique_new_followers <- unique(new_followers$user_id)
#   new_friends_list <- list()
#   message("Compiling unique new followers... (this may take some time)")
#   for (i in 1:length(unique_new_followers)){
#     new_friends_list[[i]] <- new_followers %>% filter(user_id == unique_new_followers[i]) %>% pull(user)
#   }
#
#   new_friends_by_follower <- data.frame(follower = unique_new_followers) %>% mutate(new_friends = new_friends_list)
#   id_links <- link_ids(claims, new_friends_by_follower)
#   message(sum(!is.na(id_links$user_id)), " of ", nrow(id_links), " users successfully identified!")
#
#   if(nrow(id_links)>0){
#     saveRDS(id_links, file = paste0("~/tricordings/studies/",study_name,"/",panel_name, "/id_links/id_links_",this_timecode,".rds"))
#     editPanel(study_name, panel_name, add_users = id_links$user_id, first_scrape = T, tokens = participant_tokens, max_hours = 1)
#   }
# }


#' Match by Following 3
#'
#' A function to match survey responses to twitter IDs based on following assigned accounts
#' @param responses_new New survey responses
#' @param study_name Name of study
#' @param panel_name Name of participant panel, passed from higher-level function.
#' @param assignment_dir Directory of the assignment accounts, passed from higher-level function.
#' @param participant_tokens Tokens to scrape participant twitter data, passed from higher-level function.
#' @param this_timecode Timecode passed from higher-level function
#' @keywords matching
#' @export
#' @examples
#' match_by_following_3()

match_by_following_3 <- function(responses_new, study_name, panel_name, assignment_dir, participant_tokens, this_timecode){
  prior_treatment_followers_path <- maxN(dir(paste0(assignment_dir,"/twitter_scrapes/followers"), full.names = T), N = 3) #get 3rd most recent

  prior_treatment_followers <- readRDS(prior_treatment_followers_path)
  treatment_acct_info <- readRDS(file = paste0(assignment_dir,"/twitter_scrapes/user_info/current_lookup.rds"))

  message("Identifying claims...")
  claims <- responses_new %>% select(ResponseId, starts_with("follow"), f1, f2, f3) %>% filter(!is.na(f1)) %>%
    transmute(ResponseId,
              #claim1 = !is.na(follow1),
              #claim2 = !is.na(follow2),
              #claim3 = !is.na(follow3),
              f1=sn_to_userid(f1, treatment_acct_info),
              f2=sn_to_userid(f2, treatment_acct_info),
              f3=sn_to_userid(f3, treatment_acct_info)
    )

  #c_mat <- claims %>% select(starts_with("claim")) %>% as.matrix
  f_mat <- claims %>% select(starts_with("f")) %>% as.matrix

  all_list <- list()
  claim_list <- list()
  for(i in 1:nrow(claims)){
    all_list[[i]] <- f_mat[i,]
    #claim_list[[i]] <- f_mat[i,c_mat[i,]]
  }

  #claims <- claims %>% transmute(ResponseId, shown = all_list, claimed = claim_list)
  claims <- claims %>% transmute(ResponseId, shown = all_list, claimed = all_list) #this just assumes all shown are claimed; can be made more parsimonious if we stick with this assumption


  message("Reading most recent treatment followers scrape...")
  treatment_followers_current_scrape_path <- max(dir(paste0(assignment_dir,"/twitter_scrapes/followers"), full.names = T)) #get first most recent
  treatment_followers_current_scrape <- readRDS(treatment_followers_current_scrape_path)

  #new followers of treatments
  #new_followers <- treatment_followers_current_scrape %>% filter(! user_id %in% prior_treatment_followers$user_id) #this might lose anyone who happened to follow any treatments before
  new_followers_ids <- anti_join(treatment_followers_current_scrape[,c(1,2)], prior_treatment_followers[,c(1,2)]) %>% pull(user_id) %>% unique
  new_followers <- treatment_followers_current_scrape %>% filter(user_id %in% new_followers_ids)

  if(nrow(new_followers)==0){
    warning("No new followers!")
    for (i in 1:nrow(responses_new)) {
      match_by_following_3_BYHAND(responses_new = responses_new[i,], user_id = NA,
                                  study_name = study_name,
                                  panel_name = panel_name,
                                  assignment_panel = assignment_panel,
                                  add = F)
    }
  }

  if(nrow(new_followers)>0){
    unique_new_followers <- unique(new_followers$user_id)
    new_friends_list <- list()
    message("Compiling unique new followers... (this may take some time)")
    for (i in 1:length(unique_new_followers)){
      new_friends_list[[i]] <- new_followers %>% filter(user_id == unique_new_followers[i]) %>% pull(user)
    }

    new_friends_by_follower <- data.frame(follower = unique_new_followers) %>% mutate(new_friends = new_friends_list)
    id_links <- link_ids(claims, new_friends_by_follower)
    message(sum(!is.na(id_links$user_id)), " of ", nrow(id_links), " users successfully identified!")

    if(nrow(id_links)>0){
      saveRDS(id_links, file = paste0("~/tricordings/studies/",study_name,"/",panel_name, "/id_links/id_links_",this_timecode,".rds"))
      editPanel(study_name, panel_name, add_users = id_links$user_id[which(!is.na(id_links$user_id))], first_scrape = T, tokens = participant_tokens, max_hours = 1)
    }
  }
}

#' Match by Following 3 (Investigation Function)
#'
#' A function to match survey responses to twitter IDs based on following assigned accounts, for investigation.
#' @param responses_new New survey responses
#' @param before Follower dataframe selected to represent the "before survey response" period.
#' @param after Follower dataframe selected to represent the "after survey response" period.
#' @param study_name Name of study
#' @param panel_name Name of participant panel, passed from higher-level function.  Defaults to "participants".
#' @param assignment_panel Name of participant panel, passed from higher-level function.  Defaults to "assignments".
#' @param add Go ahead and add these users to the panel?  Defaults to FALSE.
#' @param tokens Tokens to scrape participant twitter data, passed from higher-level function.
#' @param use_claims Condition matching on respondent claims?  Defaults to FALSE.
#' @keywords matching
#' @export
#' @examples
#' match_by_following_3_INVESTIGATE()

match_by_following_3_INVESTIGATE <- function(responses_new, before, after, study_name, panel_name = "participants", assignment_panel = "assignments", add = FALSE, tokens = NULL, use_claims = FALSE){

  prior_treatment_followers <- before
  treatment_acct_info <- readRDS(file = paste0("~/tricordings/studies/",study_name,"/",assignment_panel,"/twitter_scrapes/user_info/current_lookup.rds"))

  message("Identifying claims...")
  claims <- responses_new %>% select(ResponseId, starts_with("follow"), f1, f2, f3) %>%
    transmute(ResponseId,
              claim1 = str_detect(follow1, "confirm"),
              claim2 = str_detect(follow2, "confirm"),
              claim3 = str_detect(follow3, "confirm"),
              f1=sn_to_userid(f1, treatment_acct_info),
              f2=sn_to_userid(f2, treatment_acct_info),
              f3=sn_to_userid(f3, treatment_acct_info)
    )

  c_mat <- claims %>% select(starts_with("claim")) %>% as.matrix
  f_mat <- claims %>% select(starts_with("f")) %>% as.matrix

  all_list <- list()
  claim_list <- list()
  for(i in 1:nrow(claims)){
    claim_list[[i]] <- f_mat[i,c_mat[i,]]
    all_list[[i]] <- f_mat[i,]
    }

  if(use_claims) {claims <- claims %>% transmute(ResponseId, shown = all_list, claimed = claim_list)}
  if(!use_claims) {claims <- claims %>% transmute(ResponseId, shown = all_list, claimed = all_list)}

  treatment_followers_current_scrape <- after

  new_followers_ids <- anti_join(treatment_followers_current_scrape[,c(1,2)], prior_treatment_followers[,c(1,2)]) %>% pull(user_id) %>% unique
  new_followers <- treatment_followers_current_scrape %>% filter(user_id %in% new_followers_ids)

  if(nrow(new_followers)==0){stop("No new followers!")}

  unique_new_followers <- unique(new_followers$user_id)
  new_friends_list <- list()
  message("Compiling unique new followers... (this may take some time)")
  for (i in 1:length(unique_new_followers)){
    new_friends_list[[i]] <- new_followers %>% filter(user_id == unique_new_followers[i]) %>% pull(user)
  }

  new_friends_by_follower <- data.frame(follower = unique_new_followers) %>% mutate(new_friends = new_friends_list)
  id_links <- link_ids(claims, new_friends_by_follower)
  message(sum(!is.na(id_links$user_id)), " of ", nrow(id_links), " users successfully identified!")

  if(nrow(id_links)>0){
    if(!add){return(id_links)}
    if(add & !is.null(tokens)){
      saveRDS(id_links, file = paste0("~/tricordings/studies/",study_name,"/",panel_name, "/id_links/id_links_",timeCode(),".rds"))
      editPanel(study_name, panel_name, add_users = id_links$user_id[which(!is.na(id_links$user_id))], first_scrape = T, tokens = tokens, max_hours = 1)
    }
  }
}

#' Match by Following 3 (By Hand)
#'
#' A function to match survey responses to twitter IDs based on following assigned accounts, for investigation.
#' @param responses_new New survey responses
#' @param user_id User ID, manually entered, to associated with the new response.
#' @param study_name Name of study
#' @param panel_name Name of participant panel, passed from higher-level function.  Defaults to "participants".
#' @param assignment_panel Name of participant panel, passed from higher-level function.  Defaults to "assignments".
#' @param add Go ahead and add these users to the panel?  Defaults to FALSE.
#' @param tokens Tokens to scrape participant twitter data, passed from higher-level function.
#' @keywords matching
#' @export
#' @examples
#' match_by_following_3_BYHAND()

match_by_following_3_BYHAND <- function(responses_new, user_id, study_name, panel_name = "participants", assignment_panel = "assignments", add = FALSE, tokens = NULL){

  if (nrow(responses_new)!=1){stop("responses_new must be a 1-row dataframe!")}

  treatment_acct_info <- readRDS(file = paste0("~/tricordings/studies/",study_name,"/",assignment_panel,"/twitter_scrapes/user_info/current_lookup.rds"))

  message("Identifying claims...")
  claims <- responses_new %>% select(ResponseId, starts_with("follow"), f1, f2, f3) %>%
    transmute(ResponseId,
              f1=sn_to_userid(f1, treatment_acct_info),
              f2=sn_to_userid(f2, treatment_acct_info),
              f3=sn_to_userid(f3, treatment_acct_info)
    )

  f_mat <- claims %>% select(starts_with("f")) %>% as.matrix

  all_list <- list()
  for(i in 1:nrow(claims)){all_list[[i]] <- f_mat[i,]}

  claims <- claims %>% transmute(ResponseId, shown = all_list, claimed = all_list)
  id_links <- claims %>% mutate(user_id = user_id, perfect_match = NA, unique_match = NA, no_match = NA, candidates = NA)

  print(id_links)
  #return(id_links)
  if(add & !is.null(tokens)){
    saveRDS(id_links, file = paste0("~/tricordings/studies/",study_name,"/",panel_name, "/id_links/id_links_",timeCode(),".rds"))
    editPanel(study_name, panel_name, add_users = id_links$user_id[which(!is.na(id_links$user_id))], first_scrape = T, tokens = tokens, max_hours = 1)
  }
}

#

#' Scrape Qualtrics
#'
#' A high-level qualtrics scraping function.
#' @param study_name Name of study.
#' @param panel_name Name of participant panel to scrape survey data from.
#' @param match_by Matching algorithm to use, if applicable.
#' @param assignment_dir Directory of assignment panel.
#' @param max_treat_followers Maximum number of followers to attempt to scrape from treatments. Defaults to 60,000.
#' @param treatment_tokens Tokens to scrape treatment followers.
#' @param participant_tokens Tokens to scrape participant data.
#' @keywords matching
#' @export
#' @examples
#' scrapeQualtrics()

scrapeQualtrics <- function(study_name, panel_name, match_by = NULL, assignment_dir = NULL, max_treat_followers = 60000, treatment_tokens, participant_tokens){
  panel_directory <- paste0("~/tricordings/studies/",study_name,"/",panel_name,"/")

  if(is.null(assignment_dir)){
    study_panels <- dir(paste0("~/tricordings/studies/",study_name,"/"))
    other_panel <- study_panels[which(study_panels != panel_name)]
    assignment_dir <- paste0("~/tricordings/studies/",study_name,"/",other_panel,"/")
  }

  #fetch survey
  this_timecode <- timeCode()
  # qualtrics credentials now permanently installed, no need to read anything in
  responses_fetched <- qualtRics::fetch_survey(surveyID = readRDS(paste0(panel_directory,"scrape_settings.rds"))$qualtrics_survey_id,
                                    verbose = FALSE, force_request = TRUE)
  # NOT NEEDED IF CLAIMS IGNORED
  # responses_fetched <- responses_fetched %>%
  #   mutate(follow1 = as.character(follow1),
  #          follow2 = as.character(follow2),
  #          follow3 = as.character(follow3)#,
  #          #follow4 = as.character(follow4),
  #          #follow5 = as.character(follow5)
  #          )

  #fetch treatment followers
  message("Scraping all treatment followers...")
  treatment_acct_info <- readRDS(file = paste0(assignment_dir,"/twitter_scrapes/user_info/current_lookup.rds"))
  treatment_followers_current_scrape <- getFollowersBig(treatment_acct_info, list_tokens = treatment_tokens, n = max_treat_followers)
  saveRDS(treatment_followers_current_scrape, file = paste0(assignment_dir,"/twitter_scrapes/followers/followers_",this_timecode,".rds"))

  prior_responses_path <- max(dir(paste0(panel_directory,"/survey_scrapes"), full.names = T))

  if (is.na(prior_responses_path)) {
    message("First survey scrape!")
    responses_new <- responses_fetched
  }

  if (!is.na(prior_responses_path)){
    prior_responses_fetched <- readRDS(prior_responses_path)
    responses_new <- responses_fetched %>% filter(! ResponseId %in% prior_responses_fetched$ResponseId)
  }

  message(nrow(responses_new), " new responses collected...")

  if(nrow(responses_new)>0){
    message("Saving...")
    saveRDS(responses_fetched, file = paste0(panel_directory, "/survey_scrapes/responses_fetched_",this_timecode,".rds")); message("Saved.")
    if ((match_by=="follow5")){ #generalize to other systems, like direct capture
      message("Matching by follow-5 system...")
      match_by_following_5(responses_new = responses_new, study_name = study_name, panel_name = panel_name, assignment_dir = assignment_dir, participant_tokens = participant_tokens, this_timecode = this_timecode)
    }
    if ((match_by=="follow3")){
      message("Matching by follow-3 system...")
      match_by_following_3(responses_new = responses_new, study_name = study_name, panel_name = panel_name, assignment_dir = assignment_dir, participant_tokens = participant_tokens, this_timecode = this_timecode)
    }
  }
}


# #####################################
# # FIX HANDLE FROM SURVEY
# ####################################
#
# fixHandle <- function(RID_to_fix, fixed_handle){
#   fixed_handle_list_old <- readRDS("~/scrapeTweets/qualtrics_data/twitterhandles_fixed.rds")
#   fixed_handle_list_old <- fixed_handle_list_old %>% filter(fixed_handle_list_old$RID != RID_to_fix)
#   fixed_handle_list_new <- rbind(fixed_handle_list_old, c(RID_to_fix, tolower(fixed_handle)))
#   saveRDS(fixed_handle_list_new, "~/scrapeTweets/qualtrics_data/twitterhandles_fixed.rds")
#
#   prids <- readRDS("~/scrapeTweets/qualtrics_data/prior_response_ids.rds")
#   if(RID_to_fix %in% prids){prids <- prids[-which(prids==RID_to_fix)]}
#   saveRDS(prids, "~/scrapeTweets/qualtrics_data/prior_response_ids.rds")
#   message(paste0("Handle changed to ", fixed_handle, ". Fetch survey to reattempt scraping."))
# }
#
# ####################################
# # SCHULZFUNCTIONS
# #####################################
#
#
# #wills functions

#' Rescale Variable
#'
#' A function to rescale a numeric input to the 0-1 interval.
#' @param input Numeric input to rescale
#' @keywords utility
#' @export
#' @examples
#' myRescale()

myRescale <- function(input){
  newinput <- (input-min(input, na.rm = T))
  return((newinput)/max(newinput, na.rm = T))
}

#' Centered Sequence
#'
#' Make a centered sequence for histogram breaks
#' @param l Lower bound
#' @param u Upper bound
#' @param b Bandwidth
#' @keywords utility
#' @export
#' @examples
#' cSeq()


cSeq <- function(l=-2, u=2, b=.1){
  r <- c(l,u)
  return(seq(r[1]-b/2, r[2]+b/2, by=b))
}

#' Centered Histogram
#'
#' Make a centered histogram breaks.
#' @param x Data to plot.
#' @param b Bandwidth, defaults to 1.
#' @param main Plot title, defaults to NULL.
#' @keywords utility
#' @export
#' @examples
#' chist()

# chist <- function(x, b=1, main=NULL){
#   hist(x,
#        breaks = cSeq(min(x, na.rm = T), max(x, na.rm = T), b),
#        main = ifelse(test = is.null(main), yes = deparse1(substitute(x)), no = main))
# }

chist <- function(x, b=1, main=NULL){
  hist(x,
       breaks = cSeq(floor(min(x, na.rm = T)+b/2), ceiling(max(x, na.rm = T)-b/2), b),
       main = ifelse(test = is.null(main), yes = deparse1(substitute(x)), no = main))
}


#
# #explore extreme values
# extremes <- function(input, n=10){
#   my_length <- length(input)
#   my_order <- order(input, decreasing = T)
#   return(rbind(cbind(rownames(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(rownames(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))
# }
#
# extremes <- function(input, n=10){
#   my_length <- length(input)
#   my_order <- order(input, decreasing = T)
#   if(is.array(input)){return(rbind(cbind(rownames(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(rownames(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))}
#   if(class(input)=="dgCMatrix"){return(rbind(cbind(rownames(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(rownames(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))}
#   if(is.vector(input)){return(rbind(cbind(names(input)[my_order[1:n]],input[my_order[1:n]]), c("...","..."), cbind(names(input)[my_order[(my_length-n):my_length]],input[my_order[(my_length-n):my_length]])))}
# }

#
# #####
#
# #generate colors for a base plot by a categorical variable
# plotColors <- function(data, colors=c("red","blue","orange","green","cyan","brown", "black", "magenta", "purple"), seed=4711){
#   set.seed(4711)
#   color_list <- sample(colors)
#   data_levels <- unique(data)
#   out_vector <- rep(NA, length(data))
#   for (i in 1:length(out_vector)) {
#     out_vector[i] <- colors[which(data_levels==data[i])]
#   }
#   return(out_vector)
# }
#
# #cosine similarity
# cosineSimilarity <- function(a, b){
#   return(t(a)%*%b/(sqrt(sum(diag(a %*% t(a))))*sqrt(sum(diag(b %*% t(b))))))
# }
#
# #generate random colors
# randomColors <- function(my_count=10, my_seed=7, max_lightness=2, opacity=.8){
#   set.seed(my_seed)
#   output <- rep(NA, my_count)
#   for(i in 1:my_count){
#     for(j in 1:20){
#       attempt <- c(runif(1),runif(1),runif(1))
#       if (sum(attempt)<max_lightness){break}
#     }
#     output[i] <- rgb(attempt[1],attempt[2],attempt[3],opacity)
#   }
#   return(output)
# }
#
#
# randomColors <- function(my_count=2, my_seed=7, max_lightness=5, min_lightness=0, opacity=.75, max_similarity=.95){
#   attempt <- rep(NA,3)
#   set.seed(my_seed)
#   stored <- matrix(NA, my_count, 3)
#   output <- rep(NA, my_count)
#   for(j in 1:1000){
#     attempt <- runif(3)
#     if ((sum(attempt)<max_lightness) & (sum(attempt)>min_lightness)){break}
#   }
#   stored[1,] <- attempt
#   output[1] <- rgb(stored[1,1],stored[1,2],stored[1,3],opacity)
#   for(j in 1:1000){
#     attempt <- runif(3)
#     if ((sum(attempt)<max_lightness) & (sum(attempt)>min_lightness) & (cosineSimilarity(attempt, stored[1,])<max_similarity)){break}
#   }
#   stored[2,] <- attempt
#   output[2] <- rgb(stored[2,1],stored[2,2],stored[2,3],opacity)
#   if (my_count>2){
#     for(i in 3:my_count){
#       for(j in 1:10000){
#         attempt <- runif(3)
#         if ((sum(attempt)<max_lightness) & (sum(attempt)>min_lightness) & (!any(apply(stored[which(!is.na(stored[,1])),], MARGIN = 1, cosineSimilarity, attempt)>max_similarity))){break}
#       }
#       stored[i,] <- attempt
#       output[i] <- rgb(stored[i,1],stored[i,2],stored[i,3],opacity)
#     }
#   }
#   return(output)
#   #return(stored)
# }
#
#
# plotColors <- function(data, colors=c("red","blue","orange","green","cyan","brown", "black", "magenta", "purple"), seed=4711){
#   set.seed(4711)
#   #color_list <- sample(colors)
#   data_levels <- unique(data)
#   color_list <- randomColors(length(data_levels))
#   out_vector <- rep(NA, length(data))
#   for (i in 1:length(out_vector)) {
#     out_vector[i] <- color_list[which(data_levels==data[i])]
#   }
#   return(out_vector)
# }
#



#' Gradients for Plots
#'
#' A function to make color gradients for plots.
#' @param input Input values to map to a color spectrum
#' @keywords graphics
#' @export
#' @examples
#' plotGradient()


# version below should be robust to NAs in the input, allow custom selection of backup color for NAs
plotGradient <- function(input, left_color=c(0,0,1,.5), right_color=c(1,0,0,.5), NA_color=c(.5,.5,.5,.5), transparency = NULL, reference_scale = NA){
  input <- myRescale(input)
  input_length <- length(input)
  input_std <- (input-min(input, na.rm=TRUE))/max(input, na.rm=TRUE) #Why isn't this line redundant with myRescale in the first line...?
  if (!is.na(reference_scale)){
    input <- myRescale(c(input,reference_scale))
    input_std <- (input-min(input, na.rm=TRUE))/max(input, na.rm=TRUE)
    input_std <- input_std[1:input_length]
  }
  lr_diff <- right_color-left_color
  l_values <- matrix(rep(left_color, input_length), nrow = input_length, ncol = 4, byrow = T)
  c_values <- input_std %*% t(lr_diff) + l_values
  out <- rep(NA, input_length)
  if (!is.null(transparency)){
    transparency_rescaled <- myRescale(transparency)
  }
  for(i in 1:input_length){
    if(any(is.na(c_values[i,]))){
      out[i] <- rgb(NA_color[1], NA_color[2], NA_color[3], NA_color[4])
    } else {
      if (is.null(transparency)) {out[i] <- rgb(c_values[i,1], c_values[i,2], c_values[i,3], c_values[i,4])
      } else {
        out[i] <- rgb(c_values[i,1], c_values[i,2], c_values[i,3], transparency_rescaled[i])
      }
    }
  }
  return(out)
}





