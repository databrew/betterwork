library(dplyr)
library(purrr)
library(haven)
# Read haiti data
haiti <- read_dta('../data/Haiti/Haiti_forApp_June2016.dta')
n <- ncol(haiti)
labels_list <- map(1:n, function(x) attr(haiti[[x]], "label") )
# if a vector of character strings is preferable
labels_vector <- map_chr(1:n, function(x){
  x <- attr(haiti[[x]], "label")[1] 
  if(is.null(x)){
    x <- NA
  }
  x
})
# Make sure to fix the _ at beginning of variable name
names(haiti) <- ifelse(substr(names(haiti),1,1) == '_',
                       substr(names(haiti),2,nchar(names(haiti))),
                       names(haiti))


# Create dictionary
variable_dictionary <- data_frame(variable = names(haiti),
                         stata_label = labels_vector)


# Create dictionary
dictionary_list <- list()
for (i in 1:nrow(variable_dictionary)){
  message(i)
  dict <- variable_dictionary[i,]
  var <- dict$variable
  data <- haiti %>%
    group_by_(response = var) %>%
    tally %>%
    filter(!is.na(response)) %>%
    mutate(variable_translation = '',
           response_translation = '')
  if(all(data$response < 0 | data$response %in% 0:1)){
    data$response_translation <- ifelse(data$response < 0, NA,
                            ifelse(data$response == 0, 'No',
                                   ifelse(data$response == 1, 'Yes',
                                          data$response)))
  }
  data$response_translation <-
    ifelse(as.numeric(data$response) < 0,
           NA,
           data$response_translation)
  if(nrow(data) >= 20){
    data <- data[1,]
    data$response <- '<NUMERIC>'
  }
  data <- data %>%
    mutate(variable = dict$variable,
           stata_label = dict$stata_label) %>%
    dplyr::select(variable, variable_translation, 
                  stata_label, response, response_translation) %>%
    mutate(variable = as.character(variable),
           stata_label = as.character(stata_label),
           response = as.character(response))
  dictionary_list[[i]] <- data
  # print(head(data))
}
dictionary <- bind_rows(dictionary_list)
readr::write_csv(dictionary, '~/Desktop/dict.csv')  
# Convert negatives to NAs
