
setwd("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis")
source("Analysis.R")

library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(mapdeck)
library(highcharter)
library(waiter)
library(rsconnect)
library(leaflet)
library(leaflet.extras2)
library(DT)



# Key for mapdeck (create an account at https://www.mapbox.com/ and receive a key in order for maps to work properly)





ui <- bs4DashPage(
  dark = T,
  #--------------------------------------------------------------------------------------------------------------------------------- Header
  header = bs4DashNavbar(
    title = "Capstone Project",
    compact = T,
    fixed = T
  ),
  #--------------------------------------------------------------------------------------------------------------------------------- End Header
  
  
  #--------------------------------------------------------------------------------------------------------------------------------- Sidebar
  sidebar = bs4DashSidebar(
    id = "sidebar",
    minified = F,
    width = 350,
    bs4SidebarMenu( # >>>> Sidebar Menu
      bs4SidebarMenuItem("Data Visualization", tabName = "tabDataVisualization", icon = icon("binoculars")),
      bs4SidebarMenuItem("Data Exploration", tabName = "tabDataExploration", icon = icon("clipboard"),
                         bs4SidebarMenuSubItem("Point Pattern: Introduction and Data Selection", tabName = "tabDataExploratoryPPIntro", icon = icon("earth-americas")),
                         bs4SidebarMenuSubItem("Point Pattern: Intensity", tabName = "tabDataExploratoryPPIntensity", icon = icon("fire")),
                         bs4SidebarMenuSubItem("Point Pattern: Correlation", tabName = "tabDataExploratoryPPCorrelation", icon = icon("link")),
                         bs4SidebarMenuSubItem("Point Pattern: Spacing", tabName = "tabDataExploratoryPPSpacing", icon = icon("expand")),
                         startExpanded = T),
      bs4SidebarMenuItem("Exploratory Analysis: Example Discussion", tabName = "tabDiscussion", icon = icon("pen"))
    ) # End Sidebar Menu <<<<
  ),
  #--------------------------------------------------------------------------------------------------------------------------------- End SideBar
  
  #--------------------------------------------------------------------------------------------------------------------------------- Body 
  body = bs4DashBody(
    useWaiter(),
    waiterPreloader(),
    autoWaiter(html = spin_loader()),
    bs4TabItems(
      #------------------------------------------------------------------------------------------------- Tab Information
      
      #------------------------------------------------------------------------------------------------- End Tab Information
      
      
      #------------------------------------------------------------------------------------------------- Tab Data Visualization
      bs4TabItem(
        tabName = "tabDataVisualization",
        fluidRow(
          
          
          bs4Dash::column(width = 5,
                          bs4Card(
                            title = "Multiple Bar Chart",
                            width = NULL,
                            collapsible = T,
                            headerBorder = T, 
                            height = "440px",
                            maximizable = T,
                            background = "navy",
                            id = "multiplebarCard",
                            highchartOutput("barPlots")
                          )
          ),
          bs4Dash::column(width = 7,
                          bs4Card(
                            sidebar = bs4CardSidebar(
                              startOpen = F,
                              background = "#222d32",
                              id = "timePlotssidebar",
                              airDatepickerInput(
                                inputId = "timePlotDateRange",
                                label = "Date Range",
                                placeholder = "Placeholder",
                                multiple = 2, 
                                clearButton = TRUE,
                                minDate = min(data$event_date),
                                maxDate = max(data$event_date),
                                value = c(base::as.Date("2022-02-24"), max(data$event_date)),
                                update_on = "close",
                                range = T
                              ),
                              pickerInput(inputId = "timePlotAdmin1",
                                          label = "Location:",
                                          choices = unique(data$admin1),
                                          multiple = T,
                                          options = list(`actions-box` = T),
                                          selected = c("Luhansk", "Donetsk")),
                              pickerInput(inputId = "timePlotEventType",
                                          label = "Event Type:",
                                          choices = as.character(unique(data$sub_event_type)),
                                          multiple = T,
                                          options = list(`actions-box` = T),
                                          selected = c("Shelling/artillery/missile attack", "Armed clash", "Attack", 
                                                       "Remote explosive/landmine/IED", "Air/drone strike")),
                              pickerInput(inputId = "timePlotData",
                                          label = "Type:",
                                          choices = c("Daily", "Monthly"),
                                          multiple = F,
                                          options = list(`actions-box` = T),
                                          selected = "Daily"
                              ),
                              actionBttn(inputId = "refreshtimebarPlot", label = "Refresh")
                            ),
                            title = "Time Series Visualization",
                            width = NULL,
                            height = "440px",
                            collapsible = T,
                            headerBorder = T,
                            maximizable = T,
                            background = "navy",
                            id = "timeseriesCard",
                            highchartOutput("timePlots")
                          )
          ),
          bs4Dash::column(width = 5,
                          bs4Card(
                            highchartOutput("piePlots"),
                            collapsible = T,
                            headerBorder = T,
                            background = "navy",
                            width = NULL,
                            boxToolSize = "md",
                            height = "440px",
                            id = "pieCard",
                            title = "Event Distributions"
                          )
          ),
          bs4Dash::column(width = 7,
                          bs4Card( 
                            sidebar = bs4CardSidebar(
                              startOpen = F,
                              background = "#222d32",
                              id = "MapdeckSidebar",
                              airDatepickerInput(
                                inputId = "MapdeckDateSlider",
                                label = "Date Range",
                                placeholder = "Placeholder",
                                multiple = 2, 
                                clearButton = TRUE,
                                minDate = min(data$event_date),
                                maxDate = max(data$event_date),
                                value = c(base::as.Date("2022-02-24"), max(data$event_date)),
                                update_on = "close",
                                range = T
                              ),
                              pickerInput(inputId = "MapdeckEventType",
                                          label = "Event Type:",
                                          choices = as.character(unique(data$sub_event_type)),
                                          multiple = T,
                                          options = list(`actions-box` = T),
                                          selected = as.character(unique(data$sub_event_type))),
                              pickerInput(inputId = "MapdeckType",
                                          label = "Select map type:", choices = c("Basemap", "3-D Hexagonal", "Heatmap", "3-D Chloropleth"),
                                          selected = "3-D Hexagonal", multiple = F,
                                          options = list())
                            ),
                            title = "Maps",
                            width = NULL,
                            height = "440px",
                            collapsible = T,
                            maximizable = T,
                            boxToolSize = "md",
                            headerBorder = T,
                            id = "mapdeckCard",
                            background = "navy", 
                            mapdeckOutput("outputMapdeck")
                          )
          ),
          bs4Dash::column(width = 12,
                          tabBox(
                            id = "introduction",
                            width = 12, 
                            type = "tabs",
                            title = "Getting Started",
                            solidHeader = T,
                            background = "navy",
                            collapsible = T,
                            boxToolSize = "md",
                            headerBorder = T,
                            tabPanel(
                              title = "Navigation and Features",
                              div(style = 'overflow-y: scroll', htmlOutput("htmlGettingStarted"))
                            ),
                            tabPanel(
                              title = "Data Selection and Options Bars",
                              div(style = 'overflow-y: scroll', htmlOutput("dataselection"))
                            ),
                            tabPanel(
                              title = "Information Bar",
                              div(style = 'overflow-y: scroll', htmlOutput("informationbar"))
                            ),
                            tabPanel(
                              title = "Data Visualization: Graphical Interpretations",
                              div(style = 'overflow-y: scroll', htmlOutput("DVinterpretation"))
                            )
                          )
                          
          )
        )
        
        
      ),
      #------------------------------------------------------------------------------------------------- End Tab Data Visualization
      
      #------------------------------------------------------------------------------------------------- Tab Point Pattern: Introduction and Data Selection
      bs4TabItem(
        tabName = "tabDataExploratoryPPIntro",
        fluidRow(
          column(width = 4,
                 bs4Card(
                   sidebar = bs4CardSidebar(
                     id = "leafletCardSidebar",
                     startOpen = F,
                     background = "#222d32",
                     pickerInput(inputId = "selected_locations",
                                 label = "Selected:",
                                 choices = ukr.admin.level.1.boundaries.selected$admin1,
                                 options = list(`actions-box` = T),
                                 selected = NULL,
                                 multiple = TRUE)
                   ),
                   title = "Selection Map",
                   width = NULL,
                   collapsible = T,
                   headerBorder = T,
                   maximizable = T,
                   background = "navy",
                   id = "leafletCard",
                   leafletOutput("map")
                 )
          ),
          column(width = 8,
                 bs4Card(
                   sidebar = bs4CardSidebar(
                     startOpen = F,
                     width = 50,
                     background = "#222d32",
                     id = "sidebarDataExplorationPointPattern",
                     tags$style(type = "text/css", 
                                "#pppMapDate {
              margin-left: 45px;
              margin-right: 45px;
              margin-bottom: 15px;
              }"),
                     airDatepickerInput(
                       inputId = "pppMapDate",
                       label = "Date Range",
                       placeholder = "Placeholder",
                       multiple = 2, 
                       clearButton = TRUE,
                       minDate = min(data$event_date),
                       maxDate = max(data$event_date),
                       value = c(base::as.Date("2022-02-24"), max(data$event_date)),
                       update_on = "close",
                       range = T
                     ),
                     pickerInput(inputId = "selected_events",
                                 label = "Event Type:",
                                 choices = as.character(unique(data$sub_event_type)),
                                 multiple = T,
                                 options = list(`actions-box` = T),
                                 selected = c("Shelling/artillery/missile attack", "Armed clash", "Attack", 
                                              "Remote explosive/landmine/IED", "Air/drone strike"))
                   ),
                   div(style = "topright"),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   title = "Point Pattern",
                   background = "navy",
                   headerBorder = T,
                   mapdeckOutput("mapdeckPP")
                 )
          ),
          column(width = 12,
                 tabBox(
                   id = "tDEPPInfo",
                   width = 12,
                   type = "tabs",
                   title = "What is a point pattern?",
                   solidHeader = T,
                   background = "navy",
                   collapsible = T,
                   boxToolSize = "md",
                   headerBorder = T,
                   tabPanel(
                     title = "Introduction",
                     div(style = "overflow-y: scroll", htmlOutput("mathdefppintro"))
                   ),
                   tabPanel(
                     title = "Assumptions",
                     div(style = "overflow-y: scroll", htmlOutput("assumptionsppintro"))
                   ),
                   tabPanel(
                     title = "Stationarity/Isotropism",
                     div(style = "overflow-y: scroll", htmlOutput("statisoppintro"))
                   )
                 )
          )
        )
      ),
      #------------------------------------------------------------------------------------------------- End Tab Point Pattern: Introduction and Data Selection
      
      #-------------------------------------------------------------------------------------------------  Tab Point Pattern: Intensity
      bs4TabItem(
        tabName = "tabDataExploratoryPPIntensity",
        fluidRow(
          column(width = 6,
                 bs4Card(
                   title = "Quadrat Counts",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     id = "sidebarQuadratPlot",
                     numericInput(inputId = "inputNX", label = "# of quadrats in x direction", value = 5, min = 3, max = 30),
                     numericInput(inputId = "inputNY", label = "# of quadrats in y direction", value = 5, min = 3, max = 30)
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("quadratPlots")
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Kernel Estimation",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     id = "sidebarKernelPlot",
                     selectizeInput(inputId = "kernelPlotsBandwidthSelection", label = "Smoothing bandwidth:",
                                    choices = c("Mean Square Error Cross-Validation", "Likelihood Cross-Validation", 
                                                "Scott's Rule", "Default (heuristic based on window size)"),
                                    multiple = F, selected = "Default (heuristic based on window size)"),
                     radioButtons(inputId = "kernelPlotsKernel",
                                  label = "Kernel:",
                                  choiceNames = c("Gaussian","Epanechnikov","Quartic", "Disc"),
                                  choiceValues = c("gaussian", "epanechnikov", "quartic", "disc"),
                                  selected = "gaussian"),
                     materialSwitch(inputId = "kernelPlotsDiggle", label = "Jones-Diggle Improved Edge Correction")
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("kernelPlots")
                 )
          ),
          column(width = 12, 
                 bs4Card(
                   title = "Methodology",
                   selectInput(inputId = "selectIntensityMethod", label = "Section: ",
                               choices = c("Introduction - Intensity", "Quadrat Counting", "Kernel Estimation - Intro")),
                   div(style = "overflow-y: scroll", htmlOutput("methodIntensity")),
                   solidHeader = T,
                   collapsible = T,
                   width = 12, 
                   background = "navy"
                 ))
        )
      ),
      #-------------------------------------------------------------------------------------------------  End Tab Point Pattern: Intensity
      
      #-------------------------------------------------------------------------------------------------  Tab Point Pattern: Correlation
      bs4TabItem(
        tabName = "tabDataExploratoryPPCorrelation",
        fluidRow(
          column(width = 6,
                 bs4Card(
                   title = "Fry Plot",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     id = "sidebarFryPlot",
                     pickerInput(inputId = "fryPlotMark", label = "Choose mark", 
                                 choices = c("sub_event_type", "n"), selected = "sub_event_type"),
                     numericInput(inputId = "fryWidth", label = "Adjust plot:", value = 6, 
                                  max = 40, min = 0)
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("fryPlots")
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Function Estimate",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     width = 75,
                     id = "sidebarKFunction",
                     pickerInput(inputId = "KplotFunction", label = "Choose function:", 
                                 choices = c("K-Function","Inhomogeneous K-Function","L-Function", "Inhomogeneous Pair Correlation Function",
                                             "Sector K-Function", "Pair Correlation Function"), selected = "K-function", multiple = F),
                     selectInput(inputId = "KplotOptionscorrection", label = "Correction:", 
                                 selected = NULL, multiple = F, choices = c("None", "Border", "Modified Border", "Isotropic", "Ripley", "Translate", "Translation",
                                                                            "Rigid", "Periodic", "Good", "Best", "All")),
                     numericRangeInput(inputId = "KplotOptionsSectorBE", label = "Range of angles (for Sector K)", min = 0, max = 360,
                                       value = c(0, 360))
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("kernelFunctionPlots")
                 )
          ),
          column(width = 12,
                 bs4Card(
                   title = "Methodology",
                   selectInput(inputId = "selectCorrelationMethod", label = "Section: ",
                               choices = c("Correlation - Intro", "Fry Plot", "K-Function - Intro")),
                   div(style = "overflow-y: scroll", htmlOutput("methodCorrelation")),
                   width = NULL,
                   collapsible = T,
                   solidHeader = T,
                   background = "navy",
                   headerBorder = T
                 ))
        )
      ),
      #-------------------------------------------------------------------------------------------------  End Tab Point Pattern: Correlation
      
      #-------------------------------------------------------------------------------------------------  Tab Point Pattern: Spacing
      bs4TabItem(
        tabName = "tabDataExploratoryPPSpacing",
        fluidRow(
          column(width = 6,
                 bs4Card(
                   title = "Stienen Diagram",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     id = "sidebarstienenPlot"
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("stienenPlots")
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Dirichlet Tesselation/Voronoi Diagram",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     id = "sidebarnnmPlot",
                     pickerInput(inputId = "deldirOptions", label = "Lines:",
                                 choices = c("triang", "tess", "both"))
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("deldirPlots")
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Nearest Neighbour Distance Distribution Function",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     width = 90,
                     id = "sidebarNNDPlot",
                     pickerInput(inputId = "nnddFunction", label = "Choose function:", 
                                 choices = c("Nearest Neighbour Distance Distribution Function",
                                             "Inhomogeneous Nearest Neighbour Distance Distribution Function"), 
                                 selected = "Nearest Neighbour Distance Distribution Function", multiple = F),
                     pickerInput(inputId = "nnddFunctionCorrection", label = "Correction:",
                                 choices = c("rs", "km", "han"), multiple = F)
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("nnddPlots")
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Empty-space Function",
                   sidebar = bs4CardSidebar(
                     startOpen = T,
                     background = "#222d32",
                     width = 90,
                     id = "sidebarESF",
                     pickerInput(inputId = "esfFunction", label = "Choose function:", 
                                 choices = c("Empty Space Function", "Inhomogeneous Empty Space Function"), 
                                 selected = "Empty Space Function", multiple = F),
                     pickerInput(inputId = "esfFunctionCorrection", label = "Correction:",
                                 choices = c("rs", "km", "cs"))
                   ),
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   footer = descriptionBlock(),
                   plotOutput("esfPlots")
                 )
          ),
          column(width = 12,
                 bs4Card(
                   title = "Methodology",
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   selectInput(inputId = "selectSpacingMethod", label = "Section:",
                               choices = c("Introduction - Spacing", "Voronoi Diagram", "Stienen Diagram", 
                                           "Nearest Neighbour Distance Distribution Function - Intro",
                                           "Empty-space Function - Intro")),
                   div(style = "overflow-y: scroll", htmlOutput("methodSpacing"))
                 )
                 )
        )
      ),
      #-------------------------------------------------------------------------------------------------  End Tab Point Pattern: Spacing
      
      #------------------------------------------------------------------------------------------------- Tab Exploratory Analysis: Discussion
      bs4TabItem(
        tabName = "tabDiscussion", 
        fluidRow(
          column(width = 12,
                 bs4Card(
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   headerBorder = T,
                   div(style = "overflow-y: scroll", htmlOutput("discussionIntro")),
                   sliderTextInput(inputId = "discussionDate", label = "Select Month:",
                                   choices = c("January 2022", "February 2022", "March 2022", "April 2022", "May 2022", "June 2022", 
                                               "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022"))
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Kernel Estimated Intensity: Armed Clashes",
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   imageOutput("imageIntensityClashes", fill = T, inline = T)
                 )),
          column(width = 6,
                 bs4Card(
                   title = "Voronoi Diagram: Armed Clashes",
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   imageOutput("imageVoronoiClashes", fill = T, inline = T))),
          column(width = 6,
                 bs4Card(
                   title = "Kernel Estimated Intensity: Shelling/Artillery/Missile Attacks",
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   imageOutput("imageIntensityShelling", fill = T, inline = T))),
          column(width = 6,
                 bs4Card(
                   title = "Voronoi Diagram: Shelling/Artillery/Missile Attacks",
                   solidHeader = T,
                   collapsible = T,
                   width = NULL,
                   background = "navy",
                   imageOutput("imageVoronoiShelling", fill = T, inline = T)))
        )
        
      )
      
      #------------------------------------------------------------------------------------------------- End Tab Exploratory Analysis: Discussion
      
      
      
    ) # End body
    
  ),
  #--------------------------------------------------------------------------------------------------------------------------------- End Body
  
  
  #--------------------------------------------------------------------------------------------------------------------------------- Controlbar
  bs4DashControlbar(
    width = 1350,
    overlay = T,
    bs4Card(
      title = "ACLED Codebook",
      solidHeader = T, 
      background = "navy",
      collapsible = T,
      collapsed = T,
      closable = F,
      maximizable = F,
      width = 12,
      selectInput(inputId = "inputCodebook", label = "Codebook Section",
                  choices = c("Definitions of ACLED Event and Sub-Event Types", "Introduction and Brief Description",
                              "Violent Events-Battles", "Violent Events-Explosions and remote violence", 
                              "Violent Events-Violence against civilians", "Demonstrations-Protests",
                              "Demonstrations-Riots","Non-violent actions-Strategic developments", 
                              "Important Notes Regarding Event Type Codes", "Event Geography and Spatial Precision",
                              "Temporal Precision", "Information Sources", "Relationships to Other Datasets")),
      div(style = 'overflow-y: scroll', htmlOutput("htmlinputCodebook"))
    ),
    bs4Card(
      tags$head(tags$style(HTML("#dataTables tr.selected {background-color:white}"))),
      title = "Data Tables",
      solidHeader = T,
      background = "navy",
      collapsible = T,
      collapsed = T,
      closable = F,
      maximizable = F,
      width = 12,
      selectInput(inputId = "inputDataTable", label = "Data:",
                  choices = c("Processed Data", "Time Series Visualization", "Multiple Bar Chart", "Maps", "Point Pattern")),
      DT::dataTableOutput("dataTables", height = 500)
    )
  ),
  #--------------------------------------------------------------------------------------------------------------------------------- End Controlbar
  
  
  #--------------------------------------------------------------------------------------------------------------------------------- Footer
  footer = bs4DashFooter(
    
  )
  #--------------------------------------------------------------------------------------------------------------------------------- End Footer
  
)




server <- function(input, output, session) {
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------------------------------------Data
  
  # Used: Mapdeck output
  data.react.map <- reactive({
    data %>% filter(event_date >= input$MapdeckDateSlider[1] & event_date <= input$MapdeckDateSlider[2], 
                    sub_event_type %in% input$MapdeckEventType) %>% droplevels()
  }) 
  
  # Used: Mapdeck output
  data.react.map.count <- reactive({
    st_sf(data.react.map() %>% 
            group_by(event_date, admin1, sub_event_type) %>%
            dplyr::count(event_date, admin1, sub_event_type) %>%
            plyr::ddply(.variables = c("admin1"), .fun = plyr::summarise, 
                        Count = sum(n)) %>% 
            mutate(Elevation = Count/sum(Count)) %>% 
            inner_join(ukr.admin.level.1.boundaries.selected, by = c("admin1" = "admin1")))
  }) 
  
  # Used: Highchart Time Series Plot and Multiple Barplot
  data.react.count <- reactive({
    if(input$timePlotData == "Daily") {
      data.count.day %>% filter(admin1 %in% input$timePlotAdmin1, event_date >= input$timePlotDateRange[1], 
                                event_date <= input$timePlotDateRange[2], sub_event_type %in% input$timePlotEventType) %>% droplevels()
    } else if (input$timePlotData == "Monthly") {
      data.count.month %>% filter(admin1 %in% input$timePlotAdmin1, event_date >= input$timePlotDateRange[1], 
                                  event_date <= input$timePlotDateRange[2], sub_event_type %in% input$timePlotEventType) %>% droplevels()
    }
  }) %>% 
    bindEvent(input$timePlotDateRange, input$timePlotAdmin1, input$timePlotEventType, input$timePlotData, ignoreInit = F, ignoreNULL = F)
  
  # ------------------------------------------ Uses scaled and projected data from object 'data.projected'
  # Used: Point Pattern Exploratory (anything to do with spatstat)
  data.react.map.selection <- reactive({
    data.projected %>% 
      filter(event_date >= input$pppMapDate[1] & event_date <= input$pppMapDate[2],
             admin1 %in% input$selected_locations, sub_event_type %in% input$selected_events) %>%
      droplevels()
  }) 
  
  # Used: Since mapdeck only allows WGS84 projection, I can't use the scaled and reprojected data,
  # so I'll have to create another object specifically for this UI part, which sucks 
  data.react.map.selection.mapdeck <- reactive({
    data %>% filter(event_date >= input$pppMapDate[1] & event_date <= input$pppMapDate[2],
                    admin1 %in% input$selected_locations, sub_event_type %in% input$selected_events)
  }) 
  
  
  # Used: data.react.ppp(), this observation window has also been reprojected
  data.react.ppp.owin <- reactive({
    k = which(names(owin.ukr.admin.level.1.boundaries.list) %in% input$selected_locations)
    spatstat.geom::union.owin(owin.ukr.admin.level.1.boundaries.list[k])
  }) %>% bindCache(input$selected_locations) %>% bindEvent(input$selected_locations)
  
  # Used: Ok, this is the projected data
  data.react.points <- reactive({
    data.react.map.selection() %>%
      dplyr::group_by(sub_event_type, X, Y, admin1) %>% 
      dplyr::count(sub_event_type, X, Y)
  }) %>% bindEvent(data.react.map.selection())
  
  # Used: data.react.ppp.unstacked(), data.react.ppp.quadrat(), data.react.ppp.density(), data.react.ppp.relRisk()
  # This is the scaled and project data, from the projected data
  data.react.ppp <- reactive({
    rescale.ppp((ppp(x = data.react.points()$X, y = data.react.points()$Y, 
                               window = data.react.ppp.owin(),
                               marks = data.react.points()[,c("sub_event_type", "n")])), s = 1609.344, 
                unitname = "miles") 
  }) %>% bindCache(input$selected_events, input$selected_locations, input$pppMapDate) %>% 
    bindEvent(data.react.points())
  
  data.react.ppp.unstacked <- reactive({
    if (input$fryPlotMark == "sub_event_type") {
      unstack.ppp(data.react.ppp())$sub_event_type
    } else if (input$fryPlotMark == "n") {
      unstack.ppp(data.react.ppp())$n
    }
  }) %>% bindEvent(input$refreshPPP, input$pppMapDate, data.react.ppp(), input$fryPlotMark)
  
  
  
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------- End Data
  
  
  #-------------------------------------------------------------------------------------------------------------------------------------------------------Data Visualization Page 
  #--------------------------------------------------------------------------------------------- Tab Data Visualization: Mapdeck
  
  # Mapdeck: Initialization
  output$outputMapdeck <- renderMapdeck({
    mapdeck(token = "enter key here", max_pitch = 60,
            min_pitch = 0, pitch = 60, show_view_state = F,
            location = c("34.5", "49"), zoom = 5,
            height = "600", bearing = -80)  
  }) 
  
  # Mapdeck: Display Data Visualization
  observeEvent(
    eventExpr = {
      input$MapdeckType
      input$MapdeckDateSlider
      input$MapdeckEventType
    },
    handlerExpr = {
      req(input$MapdeckType, input$MapdeckDateSlider, input$MapdeckEventType)
      
      if (input$MapdeckType == "3-D Hexagonal") {
        mapdeck_update(map_id = "outputMapdeck", map_type = "mapdeck_update",
                       deferUntilFlush = T) %>%
          clear_heatmap(layer_id = "heat_layer") %>% clear_polygon("chloro_layer") %>%
          add_hexagon(
            data = data.react.map(),
            lon = "longitude",
            lat = "latitude",
            update_view = F,
            focus_layer = F,
            elevation_scale = 150,
            radius = 1500,
            layer_id = "hex_layer",
            legend = T,
            legend_options = list(title = "# of Events")
          )
      } else if (input$MapdeckType == "Heatmap") {
        mapdeck_update(map_id = "outputMapdeck", map_type = "mapdeck_update",
                       deferUntilFlush = T) %>%
          clear_hexagon(layer_id = "hex_layer") %>% clear_polygon("chloro_layer") %>%
          mapdeck::add_heatmap(
            data = data.react.map(),
            update_view = F,
            weight = NULL,
            focus_layer = F,
            lon = "longitude",
            lat = "latitude",
            radius_pixels = 40,
            threshold = .07,
            layer_id = "heat_layer"
          ) 
      } else if (input$MapdeckType == "3-D Chloropleth") {
        mapdeck_update(map_id = "outputMapdeck", map_type = "mapdeck_update",
                       deferUntilFlush = T) %>% 
          clear_heatmap(layer_id = "heat_layer") %>% clear_hexagon(layer_id = "hex_layer") %>%
          add_polygon(
            data = data.react.map.count(),
            elevation = "Elevation",
            fill_colour = "Count",
            elevation_scale = 30000,
            legend = T,
            legend_format = list(fill_colour = as.integer),
            legend_options = list(title = "Number of events",
                                  css = "background-color: #46555b;
                            color: white;"),
            update_view = F,
            focus_layer = F,
            layer_id = "chloro_layer"
          )
      } else if (input$MapdeckType == "Basemap") {
        mapdeck_update(map_id = "outputMapdeck", map_type = "mapdeck_update", deferUntilFlush = T) %>%
          clear_heatmap(layer_id = "heat_layer") %>%
          clear_hexagon(layer_id = "hex_layer")  %>%
          clear_polygon(layer_id = "chloro_layer") 
      }
      
    }
  ) 
  #--------------------------------------------------------------------------------------------- End Tab Data Visualization: Mapdeck
  
  #--------------------------------------------------------------------------------------------- Tab Data Visualization: Highchart Time Series Plot
  output$timePlots <- renderHighchart({
    data.react.count() %>% hchart("column", hcaes(x = event_date, y = n, group = sub_event_type)) %>%
      hc_navigator(enabled = T) %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "# of Events"))
  }) %>% bindCache(data.react.count()) %>% bindEvent(input$refreshtimebarPlot, ignoreInit = F, ignoreNULL = F)
  #--------------------------------------------------------------------------------------------- End Tab Data Visualization: End Highchart Time Series Plot
  
  #--------------------------------------------------------------------------------------------- Tab Data Visualization: Highchart Pie Chart
  output$piePlots <- renderHighchart({
    shiny::validate(
      need(input$MapdeckEventType, "")
    )
    data.react.map()$sub_event_type %>% hchart("pie") %>% hc_tooltip(formatter = JS("function(){
                                return  '<b>' + this.point.name + ' ~ </b> Frequency:' +this.y+', Percentage: '+Highcharts.numberFormat(this.percentage)+'%'
  }"), useHTML = FALSE) %>% hc_plotOptions(pie =list(dataLabels = list(enabled = TRUE,format="{point.label} {point.name}"))) %>% hc_add_theme(hc_theme_economist())
  }) %>% bindCache(data.react.map()) %>% bindEvent(data.react.map())
  #--------------------------------------------------------------------------------------------- End Tab Data Visualization: Highchart Pie Chart
  
  #--------------------------------------------------------------------------------------------- Tab Data Visualization: Multiple Bar Chart
  output$barPlots <- renderHighchart({
    data.react.count() %>% ddply(.variables = c("admin1", "sub_event_type"), .fun = summarise, Count = sum(n)) %>%
      select(admin1, sub_event_type, Count) %>%
      hchart("bar", hcaes(x = admin1, y = Count, group = sub_event_type)) %>% 
      hc_add_theme(hc_theme_economist()) %>%
      hc_xAxis(title = list(text = "Administrative Region")) %>%
      hc_yAxis(title = list(text = "# of Events"))
  }) %>% bindCache(data.react.count()) %>% bindEvent(input$refreshtimebarPlot, ignoreInit = F, ignoreNULL = F)
  
  #--------------------------------------------------------------------------------------------- End Tab Data Visualization: Multiple Bar Chart
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------- End Data Visualization Page 
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------- Data Exploration Page
  
  #--------------------------------------------------------------------------------------------- Tab Point Pattern: Introduction and Data Selection
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap) %>%
      addPolygons(data = ukr.admin.level.1.boundaries.selected,
                  fillColor = "white",
                  fillOpacity = 0.5,
                  color = "black",
                  stroke = TRUE,
                  weight = 1,
                  layerId = ~admin1,
                  group = "regions",
                  label = ~admin1) %>%
      addPolygons(data = ukr.admin.level.1.boundaries.selected,
                  fillColor = "navy",
                  fillOpacity = 0.5,
                  weight = 1,
                  color = "black",
                  stroke = TRUE,
                  layerId = ~ID,
                  group = ~admin1) %>%
      hideGroup(group = ukr.admin.level.1.boundaries.selected$admin1) # nc$CNTY_ID
  }) #END RENDER LEAFLET
  
  #define leaflet proxy for second regional level map
  proxy <- leafletProxy("map")
  
  #create empty vector to hold all click ids
  selected <- reactiveValues(groups = vector())
  
  observeEvent(input$map_shape_click, {
    if(input$map_shape_click$group == "regions"){
      selected$groups <- c(selected$groups, input$map_shape_click$id)
      proxy %>% showGroup(group = input$map_shape_click$id)
    } else {
      selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
      proxy %>% hideGroup(group = input$map_shape_click$group)
    }
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = "selected_locations",
                         choices = ukr.admin.level.1.boundaries.selected$admin1,
                         selected = selected$groups)
  })
  
  observeEvent(input$selected_locations, {
    removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
    added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
    
    if(length(removed_via_selectInput) > 0){
      selected$groups <- input$selected_locations
      proxy %>% hideGroup(group = removed_via_selectInput)
    }
    
    if(length(added_via_selectInput) > 0){
      selected$groups <- input$selected_locations
      proxy %>% showGroup(group = added_via_selectInput)
    }
  }, ignoreNULL = FALSE)
  
  
  
  # Mapdeck: Display Data Exploration
  output$mapdeckPP <- renderMapdeck({
    shiny::validate(
      need(input$selected_locations, "")
    )
    mapdeck(token = "enter key here", max_pitch = 60,
            min_pitch = 0, pitch = 60, show_view_state = F,
            location = c("34.5", "49"), zoom = 5,
            height = "600")  
  }) 
  
  observeEvent(
    eventExpr = {
      input$selected_locations
      input$selected_events
      input$pppMapDate
    }, 
    handlerExpr = {
      shiny::validate(
        need(input$selected_locations, ""),
        need(input$selected_events, ""),
        need(input$pppMapDate, "")
      )
      
      
      mapdeck_update(map_id = "mapdeckPP", deferUntilFlush = T, map_type = "mapdeck_update") %>% 
        clear_legend(layer_id = "point") %>% clear_pointcloud(layer_id = "point") %>%
        add_pointcloud(data = data.react.map.selection.mapdeck(), 
                       lon = "longitude",
                       lat = "latitude",
                       layer_id = "point",
                       fill_colour = "sub_event_type",
                       tooltip = "sub_event_type",
                       update_view = F,
                       radius = 5,
                       legend = F,
                       auto_highlight = T)
    }
  )
  
  
  
  #--------------------------------------------------------------------------------------------- End Tab Point Pattern: Introduction and Data Selection
  
  
  #--------------------------------------------------------------------------------------------- Tab Point Pattern: Intensity
  
  sigma <- reactive({
    if (input$kernelPlotsBandwidthSelection == "Mean Square Error Cross-Validation") {
      bw.diggle(data.react.ppp())
    } else if (input$kernelPlotsBandwidthSelection == "Likelihood Cross-Validation") {
      bw.ppl(data.react.ppp())
    } else if (input$kernelPlotsBandwidthSelection == "Scott's Rule") {
      bw.scott(data.react.ppp())
    } else if (input$kernelPlotsBandwidthSelection == "Default (heuristic based on window size)") {
      return(NULL)
    } 
  })
  
  observeEvent(
    eventExpr = {
      input$selected_locations
      input$selected_events
      input$pppMapDate
    },
    handlerExpr = {
      shiny::validate(
        need(input$selected_locations, ""),
        need(input$selected_events, ""),
        need(input$pppMapDate, "")
      )
      
      output$kernelPlots <- renderPlot({
        plot.im(
          density.ppp(
            data.react.ppp(), weights = data.react.ppp()$marks$n, sigma = sigma(), 
            kernel = input$kernelPlotsKernel, diggle = input$kernelPlotsDiggle,
            at = "pixels"
          ),
          clipwin = owin.ukr.admin.level.0.boundary.scaled, 
          main = "", ribside = "right", ribsep = .025, ribwid = .02,
          addcontour = T, axes = T)
      })
      
      output$quadratPlots <- renderPlot({
        plot.quadratcount(
          quadratcount.ppp(data.react.ppp(), nx = input$inputNX, ny = input$inputNY),
          clipwin = owin.ukr.admin.level.0.boundary.scaled,
          main = "", ribside = "right", ribsep = .025, ribwid = .02,
          axes = T, show.tiles = T)
      })
      
      
      
      
    }
  )
  #--------------------------------------------------------------------------------------------- End Tab Point Pattern: Intensity
  
  #--------------------------------------------------------------------------------------------- Tab Point Pattern: Correlation
  observeEvent(
    eventExpr = {
      input$selected_locations
      input$selected_events
      input$pppMapDate
      input$fryPlotMark
      input$fryWidth
    },
    handlerExpr = {
      
      
      output$fryPlots <-renderPlot({
        shiny::validate(
          need(input$selected_locations, "")
        )
        fryplot(data.react.ppp.unstacked(), width = input$fryWidth, main = "")
      }) %>% 
        bindEvent(input$refreshLocations, input$refreshPPP, input$fryPlotMark, input$fryWidth)
      
      
    }
  )
  
  observeEvent(
    eventExpr = {
      input$KplotFunction
    },
    handlerExpr = {
      
      if (input$KplotFunction == "K-Function") {
        updateSelectInput(inputId = "KplotOptionscorrection", label = "Correction:", 
                          selected = NULL, choices = c("none", "border", "bord.modif", "translation", "rigid", "good", "best"))
      } 
      else if (input$KplotFunction == "Inhomogeneous K-Function") {
        updateSelectInput(inputId = "KplotOptionscorrection", label = "Correction:", 
                          selected = NULL, choices = c("none", "border", "bord.modif", "translation", "good", "best"))
      }
      else if (input$KplotFunction == "L-Function") {
        updateSelectInput(inputId = "KplotOptionscorrection", label = "Correction:", 
                          selected = NULL, choices = c("none", "border", "bord.modif", "translation", "rigid", "good", "best"))
      } else if (input$KplotFunction == "Pair Correlation Function") {
        updateSelectInput(inputId = "KplotOptionscorrection", label = "Correction:",
                          selected = "translation", choices = c("translation"))
      } else if (input$KplotFunction == "Sector K-Function") {
        updateSelectInput(inputId = "KplotOptionscorrection", label = "Correction:",
                          selected = NULL, choices = c("none", "border", "bord.modif", "translation", "good", "best"))
      } else if (input$KplotFunction == "Inhomogeneous Pair Correlation Function") {
        updateSelectInput(inputId = "KplotOptionscorrection", label = "Correction:",
                          selected = "translation", choices = c("translation"))
      }
      
      
    }
  )
  
  observeEvent(
    eventExpr = {
      input$selected_locations
      input$selected_events
      input$pppMapDate
      input$KplotFunction
      input$KplotOptionscorrection
      input$KplotOptionsSectorBE
      input$KplotOptionsrenormalise
      input$KplotOptionsKernel
      input$kernelPlotsDiggle
      input$kernelPlotsKernel
    },
    handlerExpr = {
      
      output$kernelFunctionPlots <- renderPlot({
        shiny::validate(
          need(input$selected_locations, ""),
          need(input$KplotFunction, "Please Select Function"),
          need(input$KplotOptionscorrection, "")
        )
        if (input$KplotFunction == "K-Function") {
          plot.fv(Kest(data.react.ppp(), weights = data.react.ppp()$marks$n, correction = input$KplotOptionscorrection), main = "")
        } else if (input$KplotFunction == "Inhomogeneous K-Function") {
          plot.fv(Kinhom(data.react.ppp(), correction = input$KplotOptionscorrection,
                         lambda = density.ppp(
            data.react.ppp(), weights = data.react.ppp()$marks$n, sigma = sigma(), 
            kernel = input$kernelPlotsKernel, diggle = input$kernelPlotsDiggle,
            at = "pixels"
          )), main = "")
        } else if (input$KplotFunction == "L-Function") {
          plot.fv(Lest(data.react.ppp(), correction = input$KplotOptionscorrection), main = "")
        } else if(input$KplotFunction == "Pair Correlation Function") {
          plot.fv(pcf.ppp(data.react.ppp(), correction = "translation", stoyan = .15, kernel = "epanechnikov"))
        } else if (input$KplotFunction == "Sector K-Function") {
          plot.fv(Ksector(data.react.ppp(), begin = input$KplotOptionsSectorBE[1], end = input$KplotOptionsSectorBE[2],
                          units = "degrees", correction = input$KplotOptionscorrection), main = "")
        } else if (input$KplotFunction == "Inhomogeneous Pair Correlation Function") {
          plot.fv(pcfinhom(data.react.ppp(), 
                           lambda = density.ppp(
                             data.react.ppp(), weights = data.react.ppp()$marks$n, sigma = sigma(), 
                             kernel = input$kernelPlotsKernel, diggle = input$kernelPlotsDiggle, at = "pixels"), 
                           correction = "translation", kernel = "epanechnikov"), main = "")
        }
      })
      
      
    }
  )
  #--------------------------------------------------------------------------------------------- End Tab Point Pattern: Correlation
  
  
  #--------------------------------------------------------------------------------------------- Tab Point Pattern: Spacing
  
  output$stienenPlots <- renderPlot({
    stienen(data.react.ppp(), clipwin = owin.ukr.admin.level.0.boundary.scaled, main = "")
  })
  
  output$deldirPlots <- renderPlot({
    plot.tile.list(tile.list(deldir(data.react.ppp())), showpoints = F)
  })
  
  
  
  
  observeEvent(
    eventExpr = {
      input$selected_locations
      input$selected_events
      input$pppMapDate
      input$nnddFunctionCorrection
      input$nnddFunction
    }, 
    handlerExpr = {
      
      output$nnddPlots <- renderPlot({
        shiny::validate(
          need(input$selected_locations, ""),
          need(input$nnddFunction, "Please Select Function"),
          need(input$nnddFunctionCorrection, "")
        )
        if (input$nnddFunction == "Nearest Neighbour Distance Distribution Function") {
          plot.fv(Gest(data.react.ppp(), correction = input$nnddFunctionCorrection), main = "")
        } else if (input$nnddFunction == "Inhomogeneous Nearest Neighbour Distance Distribution Function") {
          plot.fv(Ginhom(data.react.ppp(), 
                         lambda = density.ppp(
                           data.react.ppp(), weights = data.react.ppp()$marks$n, sigma = sigma(), 
                           kernel = input$kernelPlotsKernel, diggle = input$kernelPlotsDiggle, at = "pixels")), main = "")
        }
        
      })
      
    }
  )
  
  observeEvent(
    eventExpr = {
      input$selected_locations
      input$selected_events
      input$pppMapDate
    }, 
    handlerExpr = {
      
      output$esfPlots <- renderPlot({
        shiny::validate(
          need(input$selected_locations, ""),
          need(input$esfFunction, "Please Select Function"),
          need(input$esfFunctionCorrection, "")
        )
        if (input$esfFunction == "Empty Space Function") {
          plot.fv(Fest(data.react.ppp(), correction = input$esfFunctionCorrection), main = "")
        } else if (input$esfFunction == "Inhomogeneous Empty Space Function") {
          plot.fv(Finhom(data.react.ppp(),
                         lambda = density.ppp(
                           data.react.ppp(), weights = data.react.ppp()$marks$n, sigma = sigma(),
                           kernel = input$kernelPlotsKernel, diggle = input$kernelPlotsDiggle, at = "pixels")), main = "")
        }
        
      })
      
    }
  )
  
  #--------------------------------------------------------------------------------------------- End Tab Point Pattern: Spacing
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------- End Data Exploration Page
  
  
 
  # Render html file selection 
  output$mathdefppintro <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/mathdefppintro.html")
  )
  
  output$assumptionsppintro <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/AssumptionsForAnalysis.html")
  )
  
  output$statisoppintro <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/stationarity.html")
  )
  
  
  html.codebookInformation <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/definitions/",
          as.character(input$inputCodebook), ".html", sep = "")
  })
  
  output$htmlinputCodebook <- renderUI(
    includeHTML(html.codebookInformation())
  )
  
  output$htmlGettingStarted <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/htmlGettingStarted.html")
  )
  
  output$dataselection <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/dataselection.html")
  )
  
  output$informationbar <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/informationbar.html")
  )
  
  output$DVinterpretation <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/dvgraphinterp.html")
  )
  
  html.methodIntensity <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/", as.character(input$selectIntensityMethod),
          ".html", sep = "")
  })
  
  output$methodIntensity <- renderUI(
    includeHTML(html.methodIntensity())
  )
  
  html.methodCorrelation <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/", as.character(input$selectCorrelationMethod),
          ".html", sep = "")
  })
  
  output$methodCorrelation <- renderUI(
    includeHTML(html.methodCorrelation())
  )
  
  html.methodSpacing <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/", as.character(input$selectSpacingMethod),
          ".html", sep = "")
  })
  
   output$methodSpacing <- renderUI(
    includeHTML(html.methodSpacing())
  )
  
 
  
  
  # Render Data Tables
  output$dataTables <- DT::renderDataTable(
    expr = {
      if (input$inputDataTable == "Processed Data") {
        datatable(data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)], rownames = F, fillContainer = T, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#ffffff'});",
            "}"))) %>% 
          formatStyle(columns = colnames(data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]), background = "white", backgroundColor = "white")
      } else if (input$inputDataTable == "Time Series Visualization") {
        datatable(data.react.count(), rownames = F, fillContainer = T, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#ffffff'});",
            "}"))) %>% 
          formatStyle(columns = colnames(data.react.count()), background = "white", backgroundColor = "white")
      } else if (input$inputDataTable == "Multiple Bar Chart") {
        datatable(data.react.count() %>% ddply(.variables = c("admin1", "sub_event_type"), .fun = summarise, Count = sum(n)) %>%
          select(admin1, sub_event_type, Count), rownames = F, fillContainer = T, options = list(
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'color': '#ffffff'});",
              "}"))) %>% 
          formatStyle(columns = colnames(data.react.count() %>% ddply(.variables = c("admin1", "sub_event_type"), .fun = summarise, Count = sum(n)) %>%
                                           select(admin1, sub_event_type, Count)), background = "white", backgroundColor = "white")
      } else if (input$inputDataTable == "Maps") {
        datatable(data.react.map(), rownames = F, fillContainer = T, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#ffffff'});",
            "}"))) %>% 
          formatStyle(columns = colnames(data.react.map()), background = "white", backgroundColor = "white")
      } else if (input$inputDataTable == "Point Pattern") {
        shiny::validate(
          need(input$selected_locations, "")
        )
        datatable(data.react.points()[,c("sub_event_type", "X", "Y", "admin1", "n")], rownames = F, fillContainer = T, options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#ffffff'});",
            "}"))) %>% 
          formatStyle(columns = colnames(data.react.points()), background = "white", backgroundColor = "white")
      }
    }
  )
  
  output$discussionIntro <- renderUI(
    includeHTML("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/html/Notes/discussionIntro.html")
  )
  
  
  html.imageIntensityClashes <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/Image/Clashes/Intensity/", as.character(input$discussionDate),
          ".png", sep = "")
  })
  
  output$imageIntensityClashes <- renderImage({
    list(src = html.imageIntensityClashes())
  }, deleteFile = F)

  html.imageVoronoiClashes <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/Image/Clashes/Voronoi/", as.character(input$discussionDate),
          ".png", sep = "")
  })
  
  output$imageVoronoiClashes <- renderImage({
    list(src = html.imageVoronoiClashes())
  }, deleteFile = F)

  html.imageIntensityShelling <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/Image/Shelling/Intensity/", input$discussionDate,
          ".png", sep = "")
  })

  output$imageIntensityShelling <- renderImage({
    list(src = html.imageIntensityShelling())
  }, deleteFile = F)

  html.imageVoronoiShelling <- reactive({
    paste("D:/R-4.0.3/RStudio/RProjects/Projects/Projects/Personal/Ukraine Analysis/Image/Shelling/Voronoi/", as.character(input$discussionDate),
          ".png", sep = "")
  })

  output$imageVoronoiShelling <- renderImage({
    list(src = html.imageVoronoiShelling())
  }, deleteFile = F)

  
  
  
  
}

shinyApp(ui,server)





