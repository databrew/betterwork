source('functions.R')
source('global.R')
source('theme.R')

header <- dashboardHeader(title="Better Work Research Portal")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Home",
      tabName="main",
      icon=icon("bank")),
    menuItem(
      text="Key indicators",
      tabName="ki",
      icon=icon("key")),
    menuItem(
      text="Explore variables",
      tabName="basic",
      icon=icon("binoculars")),
    menuItem(
      text="Model associations",
      tabName="advanced",
      icon=icon("keyboard-o")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon")),
    br(),br(),br(),br(),
    fluidRow(
      br(),
      div(img(src='logo_world_bank.png', align = "center", width = '150px'), style="text-align: center;"),
      br(),
      div(img(src='logo_tufts.png', align = "center", width = '100px'), style="text-align: center;"),
             br(),
             div(img(src='logo_better_work.png', align = "center", width = '100px'), style="text-align: center;"),
             br()
    )
    
    ))

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
                          This app is intended to help researchers to explore the results of the 5 country Better Work survey.
                          Addtional research information, data dictionaries, and downloadable copies of the original surveys are available in the 'About' section.
                          To get started, select a country (right), then visit the 'Advanced' analysis tabe (for users familiar with R) or the 'Basic' analysis tab (for all users).")),
      
                 p(a("Betterwork homepage",     href="https://betterwork.org/")),
                 p(a("Betterwork homepage",     href="bw_logo.png")),
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
    fluidRow(column(4,
                    p('Levels from variable chosen (the last one is the "reference level"'),
                    textOutput('outcome_text_levels')),
             column(4,
                    uiOutput('outcome_text')),
             column(4,
                    uiOutput('outcome_type'))),
    fluidRow(
      column(12,
             h1('Methodology'),
             helpText('In each case, choose between a binomial logistic regression or a linear probability model. These two models
                       will estimate the likelihood or probaility of the outcome variable, given the covariates. In the data,
                       each variable (survey question) has two or more possible responses. If the question has more than 
                       two responses, you will be prompted to choose which response to estimate in the model - the refernce 
                       case will be all other possible responses).')),
      column(12,
             DT::dataTableOutput('model_table')),
      column(12,
             plotOutput('model_plot'))
      
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
                                 var = input$key_indicators_input)
    })
  
  # Key indicators header
  output$key_indicators_header <-
    renderText({
      the_country <- country() 
      if(length(the_country) != 1){
        'Select 1 country on the home tab and return to examine the key indicators table.'
      } else {
        NULL
      }
    })
  
  # Key indicators ui
  output$key_indicators_ui <-
    renderUI({
      the_country <- country() 
      if(length(the_country) != 1){
        NULL
      } else {
        
        ki <- key_indicators %>%
          filter(`Country Name` %in% Hmisc::capitalize(the_country))
        
        choices <- 
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
        
        # Keep only the choices which appear in the data
        choices <- choices[choices %in% ki$`Indicator Name`]
        
        fluidPage(
          fluidRow(h1('Key indicators', align = 'center')),
          fluidRow('Explore some of the World Bank\'s "World Development Indicators".'),
          fluidRow(helpText('Full details and data available on ', a(href = 'https://data.worldbank.org/data-catalog/world-development-indicators', 'the World Bank\'s DataBank page.'))),
          fluidRow(
            selectInput('key_indicators_input',
                        'Select indicators',
                        choices = choices,
                        multiple = TRUE,
                        selected = c('GDP at market prices (current US$)',
                                     'Population, total'),
                        width = '200%')
          )
        )
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
    # get choices from variable_level_length
    choices <- variable_level_length$var_name
    if(!is.null(x)){
      selectInput('outcome_var',
                  'Select variable interest',
                  choices = choices,
                  multiple = FALSE,
                  selected = c('Daughters in school'))
    } else {
      NULL
    }
  })
  
  
  output$outcome_type <- renderUI({
    x <- df()
    if(!is.null(x) & !is.null(input$outcome_var)){
      if(!input$outcome_var %in% two_level_factor_names) {
        var <- x %>% select(input$outcome_var)
        choices <- as.data.frame((unique(var)))
        choices <- choices[!is.na(choices)]
        y_type <- x %>% filter()
        selectInput('outcome_type',
                    'Select variable interest',
                    choices = choices,
                    multiple = FALSE)
      }
      
    } else {
      NULL
    }
  })
  
  
  output$outcome_text <- renderText({
    x <- df()
    if(!is.null(x) & !is.null(input$outcome_var)){
      if(!input$outcome_var %in% two_level_factor_names) {
        paste0("You've selected a variable with more than two levels. Pleae specify which level you would like to examine")
      }
      
    } else {
      NULL
    }
  })
  
  output$outcome_text_levels <- renderText({
    x <- df()
    outcome_var <- input$outcome_var
   
    if(!is.null(x) & !is.null(input$outcome_var)){
      y_data <- as.data.frame(x[, outcome_var])
      y_data <- y_data[!is.na(y_data)]
      y_data_levels <- sort(unique(y_data))
      y_data_levles <- sort(y_data_levels)
      paste0(y_data_levels, collapse = ', ')

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
  
  # make a reactive data object for both model and plot
  
  output$model_table <- DT::renderDataTable({
    # get specificaitons 
    y_side <- input$outcome_var
    y_side_type <- input$outcome_type
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
      
      if(!is.null(y_side_type)){
        #overwrite previous outcome with 2 level chosen outcome
        pred_sub$outcome_y <- ifelse(pred_sub$outcome_y == y_side_type, y_side_type, 'reference_data')
      }
      
      if(any(apply(pred_sub, 2, function(x) length(unique(x)) <2) & ncol(pred_sub) >1)) {
        DT::datatable(data_frame(' ' = 'The combination yields a factor with one level'), rownames = FALSE, options = list(dom = 't'))
        
      } else {
        if (nrow(pred_sub) < 20){
          DT::datatable(data_frame(' ' = 'Too many NAs for this variable combination'), rownames = FALSE, options = list(dom = 't'))
        } else {
          if(model_type == 'Linear probability model') {
            
            if(length(unique(pred_sub$outcome_y)) == 2) {
              unique_levels <-  unique(pred_sub$outcome_y)
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levels[1]] <- 0 
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levels[2]] <- 1
              
              lm1 <- lm(outcome_y~., pred_sub)
              conf_int <- confint(lm1)
              vv <- vcovHC(lm1, type="HC1")
              mod_results <- cbind(broom::tidy(coeftest(lm1, vcov = vv)), conf_int)
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              mod_results$std.error <- mod_results$statistic <- mod_results$p.value <- NULL  
              names(mod_results) <- c('Variable level','Estimate', 'lower', 'upper')
              mod_results <- mod_results[-1,]
              mod_results <- mod_results[, c('Estimate', 'lower', 'upper', 'Variable level')]
              
            } else {
              DT::datatable(data_frame(' ' = 'The linear probability model requires an outcome with 2 categories'), rownames = FALSE, options = list(dom = 't'))
              
            }
            
          } else if(model_type == 'Logistic') {
            # change to factor
            pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
            
            x <- glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)
            mod_results <- data.frame(cbind(exp(coef(x)), exp(confint(x))))
            if(any(apply(mod_results, 2, function(x) any(is.infinite(x))))){
              DT::datatable(data_frame(' ' = 'The model did not converge with those parameters - try the LPM'), rownames = FALSE, options = list(dom = 't'))
              
            } else{
              mod_results<-mod_results[-1,]
              names(mod_results)<-c('OR', 'lower', 'upper')
              mod_results$`Variable level`<- row.names(mod_results)
              
              mod_results[, 1:3] <- apply(mod_results[, 1:3], 2, function(x) round(x, 3))
              
              
              # # get x_side length
              # mod_results <- as.data.frame(broom::tidy(glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)))
              # 
              
              if(is.null(mod_results)){
                return(NULL)
              } else {
                mod_results
              }
              
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
    y_side_type <- input$outcome_type
    model_type <- input$model_type
    d <- df()
    
    if(is.null(y_side) | is.null(x_side) | is.null(model_type) | is.null(d)) {
      return(NULL)
    } else {
      d_sub <- d[, sapply(d, class) == 'character']
      pred_sub <- as.data.frame(d_sub[, colnames(d_sub) %in% x_side])
      pred_sub$outcome_y <- unlist(d[, y_side])
      pred_sub <- pred_sub[complete.cases(pred_sub),]
    
      if(!is.null(y_side_type)){
        #overwrite previous outcome with 2 level chosen outcome
        pred_sub$outcome_y <- ifelse(pred_sub$outcome_y == y_side_type, y_side_type, 'reference_data')
      }
      
      
      if(any(apply(pred_sub, 2, function(x) length(unique(x)) <2) & ncol(pred_sub) >1)) {
        DT::datatable(data_frame(' ' = 'The combination yields a factor with one level'), rownames = FALSE, options = list(dom = 't'))
        
      } else {
        if(nrow(pred_sub) < 20){
          DT::datatable(data_frame(' ' = 'Too many NAs for this variable combination'), rownames = FALSE, options = list(dom = 't'))
        } else {
          if(model_type == 'Linear probability model') {
            
            if(length(unique(pred_sub$outcome_y)) == 2) {
              unique_levels <-  unique(pred_sub$outcome_y)
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levels[1]] <- 0 
              pred_sub$outcome_y[pred_sub$outcome_y == unique_levels[2]] <- 1
              
              # change to factor
              # pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
              lm1 <- lm(outcome_y~., pred_sub)
              conf_int <- confint(lm1)
              vv <- vcovHC(lm1, type="HC1")
              mod_results <- cbind(broom::tidy(coeftest(lm1, vcov = vv)), conf_int)
              mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
              mod_results$std.error <- mod_results$statistic <- mod_results$p.value <- NULL  
              names(mod_results) <- c('Variable','Estimate', 'lower', 'upper')
              mod_results <- mod_results[-1,]
              # get confidence inteval 
              
              plot_odds_lpm <-function(x, title = y_side){
                
                
                p <- ggplot(x, aes(y= Estimate, x = reorder(Variable, Estimate))) +
                  geom_point() +
                  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
                  geom_hline(yintercept =0.5, linetype=2) +
                  coord_flip() +
                  labs(title = title, x = 'Variables', y = 'Estimate (probability)') +
                  theme_bw()
              }
              
              p <- plot_odds_lpm(mod_results, title = y_side)
              # plot y against the first x
              if(is.null(p)){
                return(NULL)
              } else {
                p
              }
            
              
            } else {
              DT::datatable(data_frame(' ' = 'The linear probability model requires an outcome with 2 categories'), rownames = FALSE, options = list(dom = 't'))
              
            }
            
          } else if(model_type == 'Logistic') {
            # change to factor
            pred_sub <-  as.data.frame(apply(pred_sub, 2, function(x) as.factor(x)), stringsAsFactors = T)
            
            mod_results <- glm(outcome_y~.,  family = binomial(link = 'logit'), data = pred_sub)
            # mod_results <- as.data.frame(broom::tidy(glm(outcome_y~ ., family = binomial(link = 'logit'), data = pred_sub)))
            # 
            # mod_results[, 2:ncol(mod_results)] <- apply(mod_results[, 2:ncol(mod_results)], 2, function(x) round(x, 3))
            # 
            mod_results<-data.frame(cbind(exp(coef(mod_results)), exp(confint(mod_results))))
            
            if(any(apply(mod_results, 2, function(x) any(is.infinite(x))))){
              return(NULL)
            } else {
              
              plot_odds<-function(x, title = y_side){
                odds<-x[-1,]
                names(odds)<-c('OR', 'lower', 'upper')
                odds$vars<-row.names(odds)
                # ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
                
                p <- ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
                  geom_point() +
                  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
                  # scale_y_log10(breaks=ticks, labels = ticks) +
                  geom_hline(yintercept = 1, linetype=2) +
                  coord_flip() +
                  labs(title = title, x = 'Variables', y = 'OR') +
                  theme_bw()
              }
              
              p <- plot_odds(mod_results, title = y_side)
              
              if(is.null(p)){
                return(NULL)
              } else {
                p
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
  })
  
}


shinyApp(ui, server)