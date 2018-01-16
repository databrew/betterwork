
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
      
      # simplify the shape # (no longer doing for rgeos reasons)
      shp <- shp
      # thinnedSpatialPoly(SP = shp,
      #                  minarea = 0,
      #                  tolerance = 1,
      #                  topologyPreserve = TRUE)
      
      if(leaflet){
        m <- leaflet(shp) %>%
          addProviderTiles("Stamen.TonerLite") %>%
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
           dataframe = NULL,
           var = NULL){
    
    if(length(the_country) != 1){
      return(NULL)
    } else {
      x <- dataframe
      ki <- key_indicators %>%
        filter(`Country Name` == Hmisc::capitalize(the_country))
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
      
      # # Remove any which have fewer than 2 NAs
      # ki <-
      #   ki %>%
      #   group_by(`Indicator Name`) %>%
      #   mutate(flag = length(which(!is.na(value))) <2) %>%
      #   filter(!flag) %>%
      #   dplyr::select(-flag)
      bs <- 8
      if(!is.null(var)){
        ki <- ki %>%
          filter(`Indicator Name` %in% var)
        bs <- ifelse(length(var) < 3, 17,
                     ifelse(length(var) < 5, 14,
                            ifelse(length(var < 9), 10, bs)))
      }
      the_plot <-
        ggplot(data = ki,
               aes(x = factor(year), 
                   y = value)) + 
        geom_line(color = 'darkblue', alpha = 0.6,
                  group = 1) +
        xlab('Year') +
        ylab('Value') +
        theme_world_bank(base_size = bs) +
        facet_wrap(~`Indicator Name`, scales = 'free_y') +
        theme(axis.text.x = element_text(angle = 90))
      # ggplotly(the_plot)
      print(the_plot) 
    }
  }


#' Read key indicators
read_key_indicators <- function(){
  x <- read_csv('key_indicators/key_indicators.csv')
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
