
render_map <- 
  function(the_country,
           suppress_missing = FALSE,
           leaflet = TRUE){
    require(maptools)
    require(raster)
    require(RColorBrewer)
    if(length(the_country) == 0){
      return(NULL)
    } else {
      shp_list <- list()
      colors <- c()
      
      # Generate a shapefile for all countries
      for (country in the_country){
        iso <- 
          ifelse(country == 'haiti', 'HTI',
                 ifelse(country == 'indonesia', 'IDN',
                        ifelse(country == 'vietnam', 'VNM',
                               ifelse(country == 'jordan', 'JOR',
                                      ifelse(country == 'nicaragua', 'NIC',
                                             NA)))))
        # Define different colors for each country
        color <-
          ifelse(country == 'haiti', 'red',#  'YlOrRd', 
                 ifelse(country == 'indonesia', 'purple', # 'Purples', 
                        ifelse(country == 'vietnam','darkgreen',# 'Greens', 
                               ifelse(country == 'jordan', 'darkorange', # 'Oranges', 
                                      ifelse(country == 'nicaragua', 'blue', # 'Blues',
                                             NA)))))
        
        # Get a shapefile of country
        shp <- getData('GADM', 
                       country = toupper(substr(iso, 1, 3)), 
                       level = 1)
        # Pop into results_list
        shp_list[[country]] <- shp
        colors <- c(colors, rep(color, nrow(shp)))
      }
      n1 <- nrow(shp_list[[1]])
      row.names(shp_list[[1]]) <- as.character(1:n1)
      if(length(the_country) > 1){
        for (i in 2:length(shp_list)){
          assign(paste0('n', i),
                 nrow(shp_list[[i]]))
          row.names(shp_list[[i]]) <- 
            as.character((get(paste0('n', i-1))+1): 
                           (get(paste0('n', i-1))+
                              get(paste0('n', i))))
        }
      }
      
      shp <- shp_list[[1]]
      if(length(the_country) > 1){
        for(i in 2:length(shp_list)){
          shp <- rbind(shp,
                       shp_list[[i]],
                       makeUniqueIDs = TRUE)
        }
      }
      
      # Simplify the shape
      # shp <- thinnedSpatialPoly(SP = shp,
      #                           minarea = 0,
      #                           tolerance = 0.2,
      #                           topologyPreserve = TRUE)
      
      if(leaflet){
        m <- leaflet(shp) %>%
          addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
          addPolygons(
            stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
            # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
            color = colors)
        m
      } else {
        map('world', fill = TRUE)
        plot(shp, add = TRUE, col = colors, lwd = 0.1)
      }
    }
  }


render_key_indicators_plot <- 
  function(the_country = NULL,
           var = NULL){
    require(scales)
    # if(length(the_country) != 1){
    if(is.null(the_country) | is.null(var)){
      return(NULL)
    } else {
      ki <- key_indicators %>%
        filter(`Country Name` %in% Hmisc::capitalize(the_country))
      # Keep only a few of the key indicators
      keepers <- 
        c('GDP at market prices (current US$)',
          'Population, total',
          'Gross enrollment ratio, primary, both sexes (%)', 
          'CO2 emissions (metric tons per capita)',
          'Rural poverty headcount ratio at national poverty lines (% of rural population)',
          'Urban poverty headcount ratio at national poverty lines (% of rural population)',
          'Life expectancy at birth, total (years)',
          'GNI per capita, Atlas method (current US$)',
          'Coverage (%) - All Labor Market',
          'Labor force, total',
          'Unemployment, total (% of total labor force)',
          'GDP growth (annual %)')
      ki <-
        ki %>%
        filter(`Indicator Name` %in% keepers)
      
      bs <- 8
      if(!is.null(var)){
        ki <- ki %>%
          filter(`Indicator Name` %in% var)
        bs <- ifelse(length(var) < 3, 17,
                     ifelse(length(var) < 5, 14,
                            ifelse(length(var < 9), 10, bs)))
      }
      cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(ki$`Country Name`)))
      the_plot <-
        ggplot(data = ki,
               aes(x = factor(year), 
                   y = value / 1000000)) + 
        geom_line(aes(color = `Country Name`), alpha = 0.6,
                  group = 1) +
        geom_point(alpha = 0.6) +
        xlab('Year') +
        ylab('Value') +
        theme_world_bank(base_size = bs) +
        facet_wrap(~`Indicator Name`, scales = 'free_y') +
        theme(axis.text.x = element_text(size = 10)) +
        scale_y_continuous(name="Value (millions)", labels = comma) +
        scale_color_manual(name = '',
                           values = cols)
      
      # ggplotly(the_plot)
      print(the_plot) 
    }
  }


#' Read key indicators
read_key_indicators <- function(){
  x <- readr::read_csv('key_indicators/key_indicators.csv')
  # Keep only recent
  x <- x %>%
    filter(year >= 2005)
  # Remove coverage
  x <- x %>%
    filter(!grepl('coverage', tolower(`Indicator Name`)))
  # Make year a factor
  x$year <- factor(x$year)
  assign('key_indicators',
         x,
         envir = .GlobalEnv)
}


# Define function for plotting between 1 and 2 variables
plotter <- function(df, variable = NULL){
  
  if(length(variable) < 1 |
     length(variable) > 2){
    message('Too many or too few variables')
    return(NULL)
  }
  
  # Keep only the columns in questoin
  df <- df[,variable]
  
  # Get the types
  classes <- unlist(lapply(df, class))
  
  # Ensure that the characters come before the numerics
  df <- df[,order(classes)]
  classes <- classes[order(classes)]
  
  # Get number of variables to be plotted
  n <- length(variable)
  
  # Rename the variables
  names(df)[1] <- 'x'
  if(n > 1){
    names(df)[2] <- 'y'
  }
  if(n == 3){
    names(df)[3] <- 'z'
  }
  
  # Create list for return
  out_list <- list()
  # One variable
  if(n == 1){
    # Categorical
    if(classes[1] == 'character'){
      data <- df %>%
        group_by(x) %>%
        tally
      names(data)[1] <- variable
      plot <- ggplot(data = df,
                     aes(x = x)) +
        geom_bar(alpha = 0.6,
                 fill = 'darkblue') +
        labs(x = variable,
             y = 'Count')
    } else {
      # Numeric
      data <- df %>%
        summarise(Variable = variable,
                  min = min(x, na.rm = TRUE),
                  IQR = paste0(quantile(x, c(0.25, 0.75), na.rm = TRUE), collapse = '-'),
                  median = median(x, na.rm = TRUE),
                  mean = mean(x, na.rm = TRUE),
                  max = max(x, na.rm = TRUE),
                  NAs = length(which(is.na(x))),
                  n = n())
      plot <- ggplot(data = df,
                     aes(x = x)) +
        geom_density(alpha = 0.6,
                     fill = 'darkblue') +
        labs(x = variable,
             y = 'Density')
    }
    # Two variable
  } else if(n == 2){
    # Both are numeric if the first one is (due to ordering)
    if(classes[1] == 'numeric'){
      data <- df %>%
        summarise(variable = 'x',
                  min = min(x, na.rm = TRUE),
                  IQR = paste0(quantile(x, c(0.25, 0.75), na.rm = TRUE), collapse = '-'),
                  median = median(x, na.rm = TRUE),
                  mean = mean(x, na.rm = TRUE),
                  max = max(x, na.rm = TRUE),
                  NAs = length(which(is.na(x))),
                  n = n()) %>%
        bind_rows(
          df %>%
            summarise(variable = 'y',
                      min = min(y, na.rm = TRUE),
                      IQR = paste0(quantile(y, c(0.25, 0.75), na.rm = TRUE), collapse = '-'),
                      median = median(y, na.rm = TRUE),
                      mean = mean(y, na.rm = TRUE),
                      max = max(y, na.rm = TRUE),
                      NAs = length(which(is.na(y))),
                      n = n())
        )
      data$variable <- variable
      plot <- ggplot(data = df,
                     aes(x = x,
                         y = y)) +
        geom_point(alpha = 0.6) +
        geom_smooth(alpha = 0.3) +
        labs(x = variable[1],
             y = variable[2])
      # One categorical, one numeric
    } else if(classes[1] == 'character' & classes[2] == 'numeric'){
      data <- df %>%
        group_by(x) %>%
        summarise(variable = variable[2],
                  min = min(y, na.rm = TRUE),
                  IQR = paste0(quantile(y, c(0.25, 0.75), na.rm = TRUE), collapse = '-'),
                  median = median(y, na.rm = TRUE),
                  mean = mean(y, na.rm = TRUE),
                  max = max(y, na.rm = TRUE),
                  NAs = length(which(is.na(y))),
                  n = n())
      names(data)[1] <- variable[1]
      
      cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$x)))
      plot <- ggplot(data = df, aes(x = y)) + 
        geom_density(aes(fill = factor(x), 
                         group = factor(x)),
                     alpha = 0.6) +
        scale_fill_manual(name = variable[1],
                          values = cols) +
        labs(x = variable[2],
             y = 'Density')
      
      # Both categorical
    } else if(classes[1] == 'character' & classes[2] == 'character'){
      data_simple <- 
        df %>%
        group_by(x,y) %>%
        tally %>%
        ungroup
      data <- data_simple
      names(data)[1:2] <- variable
      cols <- colorRampPalette(brewer.pal(n = 9, 'Set1'))(length(unique(data_simple$y)))
      plot <- 
        ggplot(data = data_simple,
               aes(x = x,
                   group = y,
                   fill = y,
                   y = n)) +
        geom_bar(stat = 'identity',
                 position = 'dodge',
                 alpha = 0.6) +
        scale_fill_manual(name = variable[2],
                          values = cols) +
        labs(x = variable[1],
             y = 'Count')
      
    }
    
  }
  out_list$plot <- plot +
    theme_world_bank() +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = 'bottom')
  out_list$data <- data
  return(out_list)
}



# Define function for printing nice html tables
prettify <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                      cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                      round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                      data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             # scrollY = '300px', paging = FALSE,
                                                             dom = "Bfrtip", buttons = list("copy", "print",
                                                                                            list(extend = "collection", buttons = "csv",
                                                                                                 text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          columnDefs = list(list(className = "dt-right",
                                 targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}
