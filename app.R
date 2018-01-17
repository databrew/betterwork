source('functions.R')
source('global.R')
source('theme.R')

header <- dashboardHeader(title="Better Work Research Portal")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Home",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text="Key indicators",
      tabName="ki",
      icon=icon("key")),
    menuItem(
      text="Basic analysis",
      tabName="basic",
      icon=icon("pencil-square")),
    menuItem(
      text="Advanced analysis",
      tabName="advanced",
      icon=icon("keyboard-o")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))))

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        
        h3('Welcome!'),
        fluidRow(
          column(6,
                 
                 p(paste0("Welcome to the 'Better Work Research Portal', a collaboration between Better Work, Tufts University, and the World Bank Group.
                          This app is intended to help researchers to explore the results of the 5 country 'Better Work' survey.
                          To get started, select a country (right), then choose visit the 'Advanced'' analysis (for users familiar with R) or the 'Basic' analysis tab (for all users).")),
                 
                 p(paste0('Welcome to the "Better Work Research Portal", a collaboration ',
                          'between Better Work, Tufts University, and the World Bank. ')),
                 p(paste0('This app is intended to help researchers to explore ',
                          'the results of the five-country Better Work survey.')),
                 p(a("Betterwork homepage",     href="https://betterwork.org/")),
                 p(a('Interwoven report', href = 'https://openknowledge.worldbank.org/bitstream/handle/10986/22699/99729.pdf?sequence=1&isAllowed=y')),
                 p(a('Betterwork compliance data', href = 'https://portal.betterwork.org/transparency/compliance'))),
          column(6,
                 selectInput('country', 'Country: ',
                             choices = c('Haiti' = 'haiti'), 
                             multiple = TRUE),
                 # plotOutput('country_map', height = 300),
                 h1(textOutput('ready_text'), align = 'center'),
                 withSpinner(leafletOutput('leaf', height = 300)))
          # plotlyOutput('key_indicators_plot'))
                 )
        
          )
),
tabItem(
  tabName = 'ki',
  fluidPage(
    fluidRow(column(12,
                    h2(textOutput('ki_ki')),
                    h3(textOutput('key_indicators_header'), align = 'center'),
                    uiOutput('key_indicators_ui'),
                    plotOutput('key_indicators_plot')))
  )
),
tabItem(
  tabName = 'basic',
  fluidPage(
    fluidRow(column(6,
                    uiOutput('variable_basic_in')),
             column(6)),
    fluidRow(
      column(12,
             plotOutput('basic_plot')),
      column(12,
             DT::dataTableOutput('basic_table'))
    )
  )
),
tabItem(
  tabName = 'advanced',
  fluidPage(
    fluidRow(column(4,
                    uiOutput('outcome_var')),
             column(4,
                    uiOutput('predictors')),
             column(4,
                    uiOutput('model_type'))), 
    fluidRow(
      column(12,
             plotOutput('model_plot')),
      column(12,
             DT::dataTableOutput('model_table')),
      column(12,
             h1('Methodology'),
             helpText('For outcome variables with two levels, we estimate a binomial logistic regression,
                       or a linear probability model (with robust stand errors). 
                       For outcome variables with more than two levels, 
                       we estimate a multinomial logistic model. This functionality is still under construction.'))
    )
    # THIS IS WHERE WE NEED TO BUILD MODELING INPUTS AND OUTPUTS
  )
),
tabItem(
  tabName = 'about',
  fluidPage(
    fluidRow(
      column(6,
             includeMarkdown('includes/about.md')),
      column(6,
             h1('Survey documentation'),
             helpText('For full details on column names and',
                      'meanings, download the survey documentation below.'),
             
             # uiOutput('file_downloader'),
             selectInput('filenames',
                         'Choose a document to download:',
                         choices = download_list),
             downloadLink('downloadData', 'Download'),
             h1('Survey "data dictionaries"'),
             helpText('The column names and levels can be confusing',
                      'to those not familiar with the survey.',
                      'See the below',
                      '"dictionaries" to decipher headers and responses.',
                      'As with the survey documentation,',
                      'if you have multiple countries selected,',
                      'data will be restricted only to the country',
                      ' which appears first alphabetically.'),
             h2('Headers dictionary'),
             dataTableOutput('simple_dictionary_table'),
             h2('Responses dictionary'),
             dataTableOutput('complete_dictionary_table'))
    )))
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  # Reactive country
  country <- reactive({
    input$country
  })
  
  # Reactive dataframe
  df <- reactive({
    haiti
  })
  
  # Okay if a country is selected and we have data
  ok <- reactive({
    ic <- input$country
    d <- df()
    !is.null(ic) & !is.null(d)
  })
  
  output$leaf <- renderLeaflet({
    if(ok()){
      render_map(the_country = input$country)
    }
  })
  
  output$ready_text <-
    renderText({
      x <- input$country
      if(is.null(df())){
        Sys.sleep(0.2)
      } 
      if(is.null(x) | length(x) < 1){
        the_label <- 'Choose a country (or countries)'
      } else {
        the_label <- 'Data ready!'
      }
      return(the_label)
    })
  
  # Download of documentation
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("output", "zip", sep=".")
    },
    content = function(file) {
      zip("out.zip",
          files =paste0('Documentation/', input$filenames))
      file.copy("out.zip", file)
    },
    contentType = 'application/zip')
  
  # Plot of key indicators
  output$key_indicators_plot <-
    # renderPlotly({
    renderPlot({
      render_key_indicators_plot(the_country = country(),
                                 dataframe = df(),
                                 var = input$key_indicators_input)
    })
  
  # Key indicators header
  output$key_indicators_header <-
    renderText({
      the_country <- country() 
      if(length(the_country) != 1){
        'Select 1 country on the home tab and return to examine the key indicators table.'
      } else {
        'Key indicators'
      }
    })
  
  # Key indicators ui
  output$key_indicators_ui <-
    renderUI({
      the_country <- country() 
      if(length(the_country) != 1){
        NULL
      } else {
        selectInput('key_indicators_input',
                    'Select indicators',
                    choices = c('GDP at market prices (current US$)',
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
                                'GDP growth (annual %)'),
                    multiple = TRUE,
                    selected = c('GDP at market prices (current US$)',
                                 'Population, total'),
                    width = '200%')
      }
    })
  
  output$variable_basic_in <- renderUI({
    x <- df()
    if(!is.null(x)){
      selectInput('basic_variable',
                  'Select variable(s) for analysis',
                  choices = names(x),
                  multiple = TRUE,
                  selected = c('Sex'))
    } else {
      NULL
    }
  })
  
  output$basic_plot <- renderPlot({
    x <- input$basic_variable
    d <- df()
    if(is.null(x) | is.null(d)){
      return(NULL)
    } else {
      if(!length(x) %in% 1:2){
        if(length(x) > 2){
          ggplot() +
            theme_world_bank() +
            labs(title = 'Too many variables selected')
        } else {
          ggplot() +
            theme_world_bank() +
            labs(title = 'Select 1 or 2 variables at left')
        }
      } else {
        g <- plotter(df = d,
                     variable = x)
        g$plot
      }
    }
  })
  
  output$basic_table <- DT::renderDataTable({
    x <- input$basic_variable
    d <- df()
    if(is.null(x) | is.null(d)){
      return(NULL)
    } else {
      if(!length(x) %in% 1:2){
        DT::datatable(data_frame(' ' = 'Select one or two variables for analysis'), rownames = FALSE, options = list(dom = 't'))
      } else {
        g <- plotter(df = d,
                     variable = x)
        prettify(g$data,
                 download_options = TRUE)
      }
      
    }
  })
  
  output$outcome_var <- renderUI({
    x <- df()
    if(!is.null(x)){
      selectInput('outcome_var',
                  'Select variable interest',
                  choices = names(x),
                  multiple = FALSE,
                  selected = c('Injured at factory'))
    } else {
      NULL
    }
  })
  
  output$predictors <-renderUI({
    x <- df()
    x_sub <- x[, sapply(x, class) == 'character']
    
    bad_var_flag <- apply(x_sub, 2, function(x) length(unique(x)) < 3)
    x_sub <- x_sub[ , !bad_var_flag]
    x_names <- colnames(x_sub) 
    if(!is.null(x_names)){
      selectInput('predictors',
                  'Select predictor variable(s)',
                  choices = x_names,
                  multiple = TRUE,
                  selected = c('Sex'))
    } else {
      NULL
    }
  })
  
  # MLR , OLR
  output$model_type <-renderUI({
    model_type <- c('Logistic', 'Linear probability model')
    if(!is.null(model_type)){
      selectInput('model_type',
                  'Select the model',
                  choices = model_type,
                  multiple = FALSE,
                  selected = c('Logistic'))
    } else {
      NULL
    }
  })
  
  
  output$model_table <- DT::renderDataTable({
    # get specificaitons 
    y_side <- input$outcome_var
    x_side <- input$predictors
    model_type <- input$model_type
    d <- df()
    
    if(is.null(y_side) | is.null(x_side) | is.null(model_type) | is.null(d)) {
      return(NULL)
    } else {
      d_sub <- d[, sapply(d, class) == 'character']
      pred_sub <- as.data.frame(d_sub[, colnames(d_sub) %in% x_side])
      pred_sub$outcome_y <- unlist(d[, y_side])
      pred_sub <- pred_sub[complete.cases(pred_sub),]
      
      if(apply(pred_sub, 2, function(x) length(unique(x)) <2) & ncol(pred_sub) >1) {
        DT::datatable(data_frame(' ' = 'The combination yields a factor with one level'), rownames = FALSE, options = list(dom = 't'))
        
      } else {
        if (nrow(pred_sub) < 20){
          DT::datatable(data_frame(' ' = 'Too many NAs for this variable combination'), rownames = FALSE, options = list(dom = 't'))
        } else {
          if(model_type == 'Linear probability model') {
            
            if(length(unique(pred_sub$outcome_y)) == 2) {
              unique_levles <-  unique(pred_sub$outcome_y)
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[1]] <- 0 
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[2]] <- 1
              
              # change to factor
              # pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
              mod_results <- as.data.frame(broom::tidy(lm(outcome_y~ ., data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              
            } else {
              DT::datatable(data_frame(' ' = 'The linear probability model requires an outcome with 2 categories'), rownames = FALSE, options = list(dom = 't'))
              
            }
            
          } else if(model_type == 'Logistic') {
            # change to factor
            pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
            # get x_side length
            if(length(unique(pred_sub$outcome_y)) > 2) {
              mod_summary<- multinom(outcome_y ~., data = pred_sub)
              var_coef <- round(summary(mod_summary)$coefficients, 3)
              var_std <- round(summary(mod_summary)$standard.errors, 3)
              z <- var_coef[, 2:(length(x_side) +1)]/var_std[, 2:(length(x_side) +1)]
              # wald test to obtain pvalue
              p <- round((as.data.frame((1 - pnorm(abs(z), 0 , 1))*2)), 2)
              colnames(p)[1:length(x_side)] <- paste0(x_side, '_',rep.int('p_value', length(x_side)))
              odds_ratio <- round(exp(var_coef),2)
              mod_results <- as.data.frame(cbind(odds_ratio, p_value = p))
              
              # 
              # if(is.null(mod_results)){
              #   return(NULL)
              # } else {
              #   mod_results
              # }
              # 
            } else if(length(unique(pred_sub$outcome_y)) == 2) {
              mod_results <- as.data.frame(broom::tidy(glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              if(is.null(mod_results)){
                return(NULL)
              } else {
                mod_results
              }
            } else {
              DT::datatable(data_frame(' ' = 'Pick an outcome variable with 2 or more levels'), rownames = FALSE, options = list(dom = 't'))
            }
          }
        }
      }
    }   
  })
  
  
  # 
  output$model_plot <- renderPlot({
    # get specificaitons 
    y_side <- input$outcome_var
    x_side <- input$predictors
    model_type <- input$model_type
    d <- df()
  
    
    if(is.null(y_side) | is.null(x_side) | is.null(model_type) | is.null(d)) {
      return(NULL)
    } else {
      d_sub <- d[, sapply(d, class) == 'character']
      pred_sub <- as.data.frame(d_sub[, colnames(d_sub) %in% x_side])
      pred_sub$outcome_y <- unlist(d[, y_side])
      pred_sub <- pred_sub[complete.cases(pred_sub),]
      
      if(apply(pred_sub, 2, function(x) length(unique(x)) <2) & ncol(pred_sub) >1) {
        DT::datatable(data_frame(' ' = 'The combination yields a factor with one level'), rownames = FALSE, options = list(dom = 't'))
        
      } else {
        if(nrow(pred_sub) < 20){
          DT::datatable(data_frame(' ' = 'Too many NAs for this variable combination'), rownames = FALSE, options = list(dom = 't'))
        } else {
          if(model_type == 'Linear probability model') {
            
            if(length(unique(pred_sub$outcome_y)) == 2) {
              unique_levles <-  unique(pred_sub$outcome_y)
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[1]] <- 0 
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levles[2]] <- 1
              
              # change to factor
              # pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
              mod_results <- as.data.frame(broom::tidy(lm(outcome_y~ ., data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(mod_results$term)))
              p <- ggplot(mod_results, aes(term, estimate)) + geom_bar(stat = 'identity',  alpha = 0.7) +
                xlab('') + ylab('') +
                scale_fill_discrete(name = '') +
                geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.1) +
                theme_world_bank()
              
              return(p)
              
            } else {
              DT::datatable(data_frame(' ' = 'The linear probability model requires an outcome with 2 categories'), rownames = FALSE, options = list(dom = 't'))
              
            }
            
          } else if(model_type == 'Logistic') {
            # change to factor
            pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
            # get x_side length
            if(length(unique(pred_sub$outcome_y)) > 2) {
              mod_summary<- multinom(outcome_y ~., data = pred_sub)
              var_coef <- round(summary(mod_summary)$coefficients, 3)
              var_std <- round(summary(mod_summary)$standard.errors, 3)
              z <- var_coef[, 2:(length(x_side) +1)]/var_std[, 2:(length(x_side) +1)]
              # wald test to obtain pvalue
              p <- round((as.data.frame((1 - pnorm(abs(z), 0 , 1))*2)), 2)
              colnames(p)[1:length(x_side)] <- paste0(x_side, '_',rep.int('p_value', length(x_side)))
              odds_ratio <- round(exp(var_coef),2)
              mod_results <- as.data.frame(cbind(odds_ratio, p_value = p))
              mod_results$outcome <- rownames(mod_results)
              
              mod_results <- melt(mod_results, id.vars = 'outcome')
              
              p <- ggplot(mod_results, aes(outcome, value, fill = variable)) + 
                geom_bar(stat = 'identity', position = 'dodge',  alpha = 0.7) + theme_light() + 
                xlab('') + ylab('') +
                scale_fill_discrete(name = '') +
                theme_world_bank()
              
              return(p)
              
              # 
              # if(is.null(mod_results)){
              #   return(NULL)
              # } else {
              #   mod_results
              # }
              # 
            } else if(length(unique(pred_sub$outcome_y)) == 2) {
              mod_results <- as.data.frame(broom::tidy(glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)))
              
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              
              p <- ggplot(mod_results, aes(term, estimate)) + geom_bar(stat = 'identity', alpha = 0.7) +
                geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.1)  +
                xlab('') + ylab('') + 
                scale_fill_discrete(name = '') +
                theme_world_bank()
              
              if(is.null(p)){
                return(NULL)
              } else {
                p
              }
            } else {
              DT::datatable(data_frame(' ' = 'Pick an outcome variable with 2 or more levels'), rownames = FALSE, options = list(dom = 't'))
            }
            
          }
          
        }
        
      }
      
      }
      
  })
  
}


shinyApp(ui, server)