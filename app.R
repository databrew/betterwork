source('global.R')
source('functions.R')
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
                          To get started, select a country (below), then choose visit the 'Advanced'' analysis (for users familiar with R) or the 'Basic' analysis tab (for all users).")),
                 
                 p(paste0('Welcome to the "Better Work Research Portal", a collaboration ',
                          'between Better Work, Tufts University, and the World Bank. ')),
                 p(paste0('This app is intended to help researchers to explore ',
                          'the results of the five-country Better Work survey.')),
                 p(paste0('To get started, select one or more countries (below). Then, from the main menu (top of the page), ',
                          'go to the "Basic" analysis tab (for all users), or ',
                          'the "Advanced" tab (for users familiar with R).')),
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
    fluidRow(column(6),
             column(6)) # THIS IS WHERE WE NEED TO BUILD MODELING INPUTS AND OUTPUTS
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
    )),
fluidPage(
  fluidRow(
    div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
    h4('Built in partnership with ',
       a(href = 'http://databrew.cc',
         target='_blank', 'Databrew'),
       align = 'center'),
    p('Empowering research and analysis through collaborative data science.', align = 'center'),
    div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                       icon = icon("envelope", lib = "font-awesome")),
          href="mailto:info@databrew.cc",
          align = 'center')), 
    style = 'text-align:center;'
  )
)
)
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
        the_label <- 'Choose a country'
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
  
  }


shinyApp(ui, server)