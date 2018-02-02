library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(googlesheets)
library(dplyr)
library(haven)
library(shinycssloaders)
library(leaflet)
library(Hmisc)
library(nnet)
library(broom)
library(sandwich)
library(reshape2)
library(lmtest)

options(scipen = '999')
if('prepared_data.RData' %in% dir()){
  load('prepared_data.RData')
} else {
  # Read in haiti data
  haiti <- read_dta('data/Haiti/Haiti_forApp_June2016.dta')
  n <- ncol(haiti)
  # Make sure to fix the _ at beginning of variable name
  names(haiti) <- ifelse(substr(names(haiti),1,1) == '_',
                         substr(names(haiti),2,nchar(names(haiti))),
                         names(haiti))
  
  # Read in haiti dictionary
  dictionary <- gs_url('https://docs.google.com/spreadsheets/d/17-Kd9-a-X2JvVXql679LxI9QG3is1IHEFS-Fe9PlO9Y/edit#gid=0')
  dictionary <- gs_read_csv(dictionary)
  # Remove any incomplete parts of the dictionary
  dictionary <-
    dictionary %>%
    filter(!is.na(variable_translation_short))
  
  # Translate the haiti data
  haiti <- haiti[,names(haiti) %in% dictionary$variable]
  
  translate_column <- function(data = haiti,
                               column = 'tuftsid'){
    
    # Get the original data
    this_data <- data %>% dplyr::select_(column)
    names(this_data) <- 'old_vals'
    
    # Get the translation
    this_dictionary <- dictionary %>% filter(variable == column)
    
    # Define if numeric or not
    is_numeric <- any(grepl('<NUMERIC>', this_dictionary$response, fixed = TRUE))
    
    # If numeric, only change the column (and ensure the values are numeric)
    if(is_numeric){
      new_vals <- as.numeric(this_data$old_vals)
    } else {
      this_data <- left_join(this_data %>%
                               mutate(old_vals = as.character(old_vals)),
                             this_dictionary %>%
                               dplyr::select(response,
                                             response_translation),
                             by = c('old_vals' = 'response'))
      new_vals <- this_data$response_translation
    }
    
    # Get the new column name
    new_name <- this_dictionary$variable_translation_short[1]
    
    # Return the translated data
    out <- data_frame(x = new_vals)
    names(out) <- new_name
    return(out)
  }
  cn <- names(haiti)
  out_list <- list()
  for(j in 1:length(cn)){
    message(j)
    this_column <- cn[j]
    this_translation <- translate_column(data = haiti,
                                         column = this_column)
    out_list[[j]] <- this_translation
  }
  # Overwrite with the translation
  haiti <- bind_cols(out_list)
  
  # Read in key indicators
  read_key_indicators()

  
  save(haiti, key_indicators, file = 'prepared_data.RData')
}

# create an object that is a list of all variables with two levels to be used in the app
two_level_factor_index <- apply(haiti, 2, function(x) length(unique(x[!is.na(x)]))  == 2)
two_level_factor_names <- colnames(haiti)[two_level_factor_index]

# get string that has the actualy number of levles (besides NA to paste with names)
variable_level_length <- as.data.frame(apply(haiti, 2, function(x) length(unique(x[!is.na(x)]))))
variable_level_length$var_name <- row.names(variable_level_length)
colnames(variable_level_length)[1] <- c('count')
variable_level_length$var_name_count <- paste0(variable_level_length$var_name, ' ', '(',variable_level_length$count, ')')

# remove levels below 2 and above 10
variable_level_length <- variable_level_length[variable_level_length > 1 & variable_level_length < 9,]

# Get a list of documents available for download
download_list <- dir('Documentation/', recursive = TRUE)
