

# load_libraries ------------------------------------------------------

library(shiny)
library(flexdashboard)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(data.table)
library(purrr)
library(lubridate)
library(DT)
library(lubridate)
library(xts)
library(dygraphs)

# load_data ----------------------------------------------------------

data <- fread('www/data/PDSData.csv', check.names = TRUE)

colnames <- c(
    'date',
    'time',
    'shot',
    'cycle_time_s',
    'fill_time_s',
    'injection_start_position_mm',
    'actual_cushion_mm',
    'minimum_cushion_mm',
    'charge_time_s',
    'charge_stop_mm',
    'shift_position_mm',
    'shift_pressure_bar',
    'injection_peak_pressure_bar',
    'mold_close_time_s',
    'mold_open_time_s',
    'mold_open_position_mm',
    'oil_temperature_ac',
    'throat_temperature_ac',
    'zone1_temperature_ac',
    'zone2_temperature_ac',
    'zone3_temperature_ac',
    'zone4_temperature_ac',
    'nozzle1_temperature_ac',
    'sprue_temperature_ac',
    'tonnage_ton'
)


setnames(data, colnames)

# head --------------------------------------------------------------------

head <- dashboardHeader(title = "Lumax Production")


# sidebar -----------------------------------------------------------------

side <- dashboardSidebar(sidebarMenu(# Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
    )))


# body --------------------------------------------------------------------

body <- dashboardBody(
    fluidRow(
        valueBoxOutput('cycle')
        ,
        valueBoxOutput('fill')
        ,
        valueBoxOutput("injection")
    )
    ,
    fluidRow(
        column(width = 6
               , dygraphOutput(
                   'ts'
                   , width = "100%"
                   , height = "500px"
               ))
        
        ,
        column(width = 6
               , dygraphOutput(
                   'ts2'
                   , width = "100%"
                   , height = "500px"
               ))
    )
)


# ui_function -------------------------------------------------------------

ui <- dashboardPage(head,
                    side,
                    body)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
    num <- reactive({
        invalidateLater(5000)
        floor(((minute(Sys.time(
        )) * 60) + second(Sys.time())) * 7)
    })
    
    
    df <- reactive({
        data[1:num(), ]
    })
    
    
    # valueBoxes --------------------------------------------------------------
    
    fill_time <- reactive(as.double(df()[.N, fill_time_s]))
    
    output$fill <- renderValueBox(valueBox(
        value = fill_time()
        ,
        color = ifelse(fill_time() < 1.90, "red"
                       , ifelse((fill_time() <= 2.04 &
                                     fill_time() >= 1.90),
                                "green"
                                ,
                                ifelse(fill_time() > 2.04, "yellow", "aqua")
                       ))
        ,
        subtitle = "Fill Time"
        
    ))
    
    
    # ValueBox2 ---------------------------------------------------------------
    
    cycle_time <- reactive(as.double(df()[.N, cycle_time_s]))
    
    output$cycle <- renderValueBox(valueBox(
        value = cycle_time()
        ,
        color = ifelse(cycle_time() < 47, "red"
                       , ifelse((cycle_time() <= 47.5 &
                                     cycle_time() >= 47.00),
                                "green"
                                ,
                                ifelse(cycle_time() > 47.5, "yellow", "aqua")
                       ))
        ,
        subtitle = "Cycle Time"
        
    ))
    
    # ValueBox3 ---------------------------------------------------------------
    
    injection_start <-
        reactive(as.double(df()[.N, injection_start_position_mm]))
    
    output$injection <- renderValueBox(valueBox(
        value = injection_start()
        ,
        color = ifelse(injection_start() < 76, "red"
                       , ifelse((injection_start() <= 76 &
                                     injection_start() >= 77.00),
                                "green"
                                ,
                                ifelse(injection_start() > 77, "yellow", "aqua")
                       ))
        ,
        
        subtitle = "Injection Time"
        
    ))
    
    
    # timeseries --------------------------------------------------------------
    
    time_series <- reactive({
        xts(x = df()[, .(
            zone1_temperature_ac ,
            zone2_temperature_ac
            ,
            zone3_temperature_ac,
            zone4_temperature_ac
            ,
            nozzle1_temperature_ac
        )]
        , order.by = df()[, dmy_hms(paste(date, time))])
    })
    
    output$ts <- renderDygraph(
        dygraph(time_series(), main = "Zone Temprature") %>%
            dyLegend(show = "onmouseover", hideOnMouseOut = TRUE)
    )
    
    
    # Timeseries2 -------------------------------------------------------------
    
    time_series2 <- reactive({
        xts(x = df()[, .(oil_temperature_ac,
                         throat_temperature_ac
                         ,
                         sprue_temperature_ac)]
            , order.by = df()[, dmy_hms(paste(date, time))])
    })
    
    output$ts2 <- renderDygraph(
        dygraph(time_series2(), main = "Oil & Throat Temprature") %>%
            dyLegend(show = "onmouseover", hideOnMouseOut = TRUE)
    )
    
    
    # end ---------------------------------------------------------------------
    
    
    
    
}


# runapp ------------------------------------------------------------------

shinyApp(ui, server)
