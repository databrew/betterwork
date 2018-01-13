# This file only needs to be run once, 
# in order to produce the "key_indicators.csv"
# which is used for a table on the main page

library(readr)
library(tidyr)
countries <- c('haiti',
               'indonesia',
               'jordan',
               'nicaragua',
               'vietnam')
results_list <- list()
for (i in 1:length(countries)){
  print(i)
  the_country <- countries[i]
  the_files <- dir(the_country)
  the_file <- the_files[which(substr(the_files, 1, 3) == 'API')]
  ki <- read_csv(paste0(countries[i],
                        '/',
                        the_file),
                 skip = 4)
  ki <- ki[,1:60]
  names(ki)[5:60] <- paste0('year_', names(ki)[5:60] )
  ki <- gather(ki, year, value, year_1960:year_2015)
  ki$year <- as.numeric(gsub('year_', '', ki$year))
  results_list[[i]] <- ki
}
key_indicators <- do.call('rbind', results_list)
write_csv(key_indicators, 'key_indicators.csv')
