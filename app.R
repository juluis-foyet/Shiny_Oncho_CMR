# library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
library(plotly)
library(DescTools)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)

oncho_data <- read.csv("data-CM-Oncho-sitelevel-2013-2022.csv", stringsAsFactors = FALSE )
oncho_data <- data.frame(oncho_data) %>% 
  mutate(SurveyYear = ifelse(is.na(SurveyYear), "Missing", SurveyYear), ADMIN1_NAME = str_to_title(ADMIN1_NAME), ADMIN2_NAME = str_to_title(ADMIN2_NAME))

ui <- dashboardPage(
  
  dashboardHeader(title = "Oncho Transmission in Cameroon"),
  dashboardSidebar(width = 150,shinyDashboardThemes(theme = "blue_gradient"),
    sidebarMenu(
      menuItem("Interactive Map", tabName = "Interactive_Map", icon = icon("map")),
      menuItem("Data Exploration", tabName = "Data_Exploration", icon = icon("th")),
      menuItem("Read Me", tabName = "ReadMe", icon = icon("info"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Interactive_Map",
              sidebarLayout(sidebarPanel(checkboxGroupInput("year", "Survey Year", choices = sort(unique(oncho_data$SurveyYear)), 
                                                            selected = sort(unique(oncho_data$SurveyYear)), inline = T),
                                         selectizeInput("region", "Choose your Regions", choices = sort(unique(oncho_data$ADMIN1_NAME)), selected = oncho_data$ADMIN1_NAME, multiple = T),
                                         selectizeInput("division", "Choose your Divisions", choices = sort(unique(oncho_data$ADMIN2_NAME)), selected = oncho_data$ADMIN2_NAME, multiple = T),
                                         box(plotlyOutput("preval"), width = 12, collapsible = T, title = "Graph",
                                             collapsed = T, background = "teal", solidHeader = T, status = "primary"),
                                         valueBoxOutput("prev.points")
              ),
              mainPanel(leafletOutput("Map", height=920)))
              ),
      tabItem(tabName = "Data_Exploration",
              DT::dataTableOutput("data")
      ),
      tabItem(tabName = "ReadMe",
              h1("General Comments")
      )
    )
  )
  )


#=======================================================================================================================================
server <- function(input, output) {
  
  oncho_data_year <- reactive({
      oncho_data %>% filter(SurveyYear %in% input$year)
    })
  
  observeEvent(oncho_data_year(), {
    choices <- sort(unique(oncho_data_year()$ADMIN1_NAME))
    updateSelectizeInput(inputId = "region", choices = choices)
  })

  oncho_data_region <- reactive({
    oncho_data_year() %>% filter(ADMIN1_NAME %in% input$region)
  })
  
  observeEvent(oncho_data_region(), {
    choices2 <- sort(unique(oncho_data_region()$ADMIN2_NAME))
    updateSelectizeInput(inputId = "division", choices = choices2)
  })
  
  oncho_data2 <- reactive({
    oncho_data_region() %>% filter(ADMIN2_NAME %in% input$division)
  })
    
  
#--------------------------------------------------------------------------------------------------------

  output$data <-DT::renderDataTable(datatable(
    oncho_data ,filter = 'top'
  ))
  
# -------------------------------------------------------------------------------------------------------
  
  output$Map <- renderLeaflet({
    
    pal <- colorBin("viridis", bins = seq(min(oncho_data2()$Prevalence), max(oncho_data2()$Prevalence), length.out = 5))
    leaflet(oncho_data2()) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, color = ~pal(Prevalence), label = ~paste0(LocationName, "; ", Prevalence)) %>%
      addLegend("bottomright", pal = pal, values = ~Prevalence, title = "Prevalence") %>%
      addScaleBar(position = c("bottomleft")) %>% 
      addMiniMap()
  })
  
  #---------------------------------------------------------------------------------------------------
  output$prev.points <- renderValueBox(
    valueBox(nrow(oncho_data2()), "Prevalence points", color = "light-blue")
  )
  
  #------------------------------------------------------------------------------------------
  
  output$preval <- renderPlotly({
    Plo <- oncho_data2() %>% 
      group_by(SurveyYear) %>% 
      summarize(Mean = mean(Prevalence), se = MeanSE(Prevalence)) %>% 
      ggplot(aes(x = SurveyYear, y = Mean, ymin = Mean - se, ymax = Mean + se, color = SurveyYear))+
      geom_point()+
      geom_linerange()+
      # scale_color_brewer(palette = "Dark2")+
      ylim(0,1)+
      theme_bw()+
      labs(y = "Mean Oncho Prevalence ± se", x = "Survery Year", title = "Onchocerciasis Prevalence")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "#222d32"),
            plot.background = element_rect(fill = "#3c8dbc"),
            axis.title = element_text(face = "bold", size = 14),
            plot.title = element_text(face = "bold", size = 14, hjust = .5))
    
    ggplotly(Plo)
    
  })
}


shinyApp(ui = ui, server = server)


