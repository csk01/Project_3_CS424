setwd("C:/Users/Krishnan CS/424_Project3")
print(getwd())

# LIBRARIES=======================================================================================================
library(lubridate)
# library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(stringr)
library(shinyjs)
library(data.table)
library(purrr)
library(rgdal)

library(DT)


jsCode <- 'shinyjs.markerClick = function(id) {
              try{map.eachLayer(function (layer) {
                if (layer.options.layerId == id) {
                  layer.fire("click");
                }
              })}
              catch(err){
              
              }
           };'

options(scipen=999)


# READ DATA AND CONVERT TO USABLE FORMAT=======================================================================================================

#Reading from the split csv files
print("reading data")
taxi <- do.call(rbind, lapply(list.files(pattern = "*.csv"), fread)) 
print("read data")

#Reading community boundaries from a shape file 
# Source: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
community_shp <- rgdal::readOGR("shp_files/geo_export_fca70ba1-774b-4562-b299-3cbfe3855c4d.shp",
  layer = "geo_export_fca70ba1-774b-4562-b299-3cbfe3855c4d", GDAL1_integer64_policy = TRUE)

print("read shp file")

#labels for each community 
labels <- sprintf(
  "<strong>%s</strong><br/>",
  community_shp$community
) %>% lapply(htmltools::HTML)
#PREPROCESSING=================================================================================

#init hash
hash <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)

#Vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

#Edited company names to a more readable format 
values <- c("24 Seven Taxi","312 Medallion","5 Star Taxi","Adwar H. Nikola","Ahzmi Inc","American United","American United Taxi Aff.",
            "Arrington Enterprises","Babylon Express Inc.","Benny Jona","Blue Diamond","Blue Ribbon Taxi","Checker Taxi","Checker Taxi Aff.","Chicago Carriage Cab",
            "Chicago Independents","Chicago Medallion","Chicago Star Taxicab","Chicago Taxicab","Choice Taxi","Chuks Cab","City Service","David K. Cab",
            "Flash Cab","G.L.B. Cab","Globe Taxi","Gold Coast Taxi","Jay Kim","JBL Cab Inc.","KOAM Taxi","Leonard Cab","Luhak Corp","Medallion Leasin",
            "Metro Jet Taxi A","N and W Cab","Nova Taxi Aff.","Omar Jada","Patriot Taxi","Petani Cab","RC Andrews Cab","Reny Cab","Salifu Bawa","Sam Mestas",
            "Santamaria Express","Sbeih company","Sergey Cab","Setare Inc","Star North","Suburban Dispatch","Sun Taxi","T.A.S.","Tasha ride inc",
            "Taxi Aff. Service Yellow","Taxi Aff. Services","Taxicab Insurance","Top Cab Aff.","U Taxicab","Yellow Cab")

#Keys are string values of numbers 
keys <- sprintf("%s",seq(1:58))

#Assigning hash
assign_hash(keys, values, hash)

#To extract value 
#hash[["1"]]





#Community areas list to access Later
community_areas <- c("Rogers Park", "West Ridge","Uptown","Lincoln Square","North Center","Lake View","Lincoln Park", "Near North Side", "Edison Park",
                     "Norwood Park","Jefferson Park","Forest Glen","North Park","Albany Park","Portage Park","Irving Park","Dunning","Montclare","Belmont Cragin",
                     "Hermosa","Avondale","Logan Square","Humboldt Park","West Town","Austin","West Garfield Park","East Garfield Park","Near West Side",
                     "North Lawndale","South Lawndale","Lower West Side","The Loop","Near South Side","Armour Square","Douglas","Oakland","Fuller Park",
                     "Grand Boulevard","Kenwood","Washington Park","Hyde Park","Woodlawn","South Shore","Chatham","Avalon Park","South Chicago","Burnside",
                     "Calumet Heights","Roseland","Pullman","South Deering","East Side","West Pullman","Riverdale","Hegewisch","Garfield Ridge","Archer Heights",
                     "Brighton Park","McKinley Park","Bridgeport","New City","West Elsdon","Gage Park","Clearing","West Lawn","Chicago Lawn","West Englewood",
                     "Englewood","Greater Grand Crossing","Ashburn","Auburn Gresham","Beverly","Washington Heights","Mount Greenwood","Morgan Park","O'Hare","Edgewater","Outside Chicago")

years<-c(2001:2021)

time_in_24 <-c( '0000',  '0100', '0200', '0300', '0400', '0500', '0600', '0070', '0080', '0900', '1000', '1100', '1200', '1300', '1400',
                '1500', '1600', '1700', '1800', '1900', '2000', '2100', '2200', '2300' )

time_in_12 <- c('00:00 am','01:00 am','02:00 am','03:00 am','04:00 am','05:00 am','06:00 am','07:00 am','08:00 am','09:00 am','10:00 am','11:00 am','12:00 pm','13:00 pm','14:00 pm','15:00 pm','16:00 pm','17:00 pm','18:00 pm','19:00 pm','20:00 pm','21:00 pm','22:00 pm','23:00 pm')

months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'Novermber', 'December')


# Summarize the number of taxi rides by day
daily_rides <- taxi %>%
  group_by(Date) %>%
  summarise(n_rides = n())

print("daily summarized")

# Summarize the number of taxi rides by hour
hourly_rides <- taxi %>%
  group_by(Hour) %>%
  summarise(n_rides = n())

print("hourly summarized")

# Summarize the number of taxi rides by day of the week
weekdays_rides <- taxi %>%
  group_by(wday(Date)) %>%
  summarise(n_rides = n())

print("wday summarized")

# Summarize the number of taxi rides by month
month_rides <- taxi %>%
  group_by(month(Date)) %>%
  summarise(n_rides = n())

print("monthly summarized")

month_rides <- rename(month_rides, "newMonth" = "month(Date)" )

print("renaming")

#defining basic leaflet map to add on to later
map_plot <- leaflet() %>%
      addTiles() 

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Sleepy Subway"),
  
  # Application title
  dashboardSidebar(
    #comment
    collapsed = TRUE, 
    sidebarMenu(
      br(),br(),br(),br(), br(),br(),br(),br(), br(),br(),br(),br(), br(),br(),br(),br(), 
      menuItem("Home", tabName = "dashboard", icon = NULL),
      
      menuItem("About", tabName = "about", icon = NULL),
      
      menuItem("Yearly Plots", tabName = "yearlyPlots", icon = NULL)
      
    )
    
  ),
   
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "dashboard",
              
              
              fluidRow(
                
                column(1,
                       
                       fluidRow(style="height:40vh"),
                       p("Input controls"),
                       # dateInput("date1", "Date:", value = "2021-08-23"),
                       # dateInput("date2", "Date 2:", value = "2020-08-23"),
                       # actionButton("prevDay", "Previous Day"),
                       # actionButton("nextDay", "Next  Day"),
                       
                       selectInput(inputId = "station", label = "Select community", choices = sort(community_areas))
                       # radioButtons(
                       #   inputId = "radio",
                       #   label = "Mode",
                       #   choices = c("To", "From"),
                       #   selected = "To",
                       #   inline = FALSE,
                       #   width = NULL
                       # )
                ),
                
                column(11, 
                       
                       
                       #### CONTROLS
                       
                       fluidRow(style='height:40vh',
                                column(4,
                                       box( title = textOutput("text"), solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("hist1", height="34vh"), height="40vh")
                                ),
                                column(2,
                                       box(solidHeader = TRUE, status = "primary", width = 180,
                                           DT::dataTableOutput("dailyTable")
                                       )
                                ),
                                column(4,
                                       box( title = "something", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("histHourly", height="34vh"), height="40vh")
                                ),
                                column(2,
                                       box(solidHeader = TRUE, status = "primary", width = 180, title="table for hourly",
                                           dataTableOutput("hourlyTable")
                                       )
                                )
                                
                       ),
                       
                       fluidRow(style='height:50vh; margin-top: 50px',
                                fluidRow(
                                  column(4,
                                         box( title = "Days of the  Week", solidHeader = TRUE, status = "primary", width = 12,
                                              plotOutput("histDay", height="34vh"), height="40vh")
                                  ),
                                  column(2,
                                         box(solidHeader = TRUE, status = "primary", width = 180,
                                             dataTableOutput("dayTable")
                                         )
                                  ),
                                  column(4,
                                         box( title = "Month", solidHeader = TRUE, status = "primary", width = 12,
                                              plotOutput("histMonthly", height="34vh"), height="40vh")
                                  ),
                                  column(2,
                                         box(solidHeader = TRUE, status = "primary", width = 180,
                                             dataTableOutput("monthlyTable")
                                         )
                                  )
                                ),
                                
                                fluidRow(
                                  
                                  column(4,
                                         box( title = "Binned Mileage", solidHeader = TRUE, status = "primary", width = 12,
                                              plotOutput("histBinMile", height="34vh"), height="40vh")
                                         
                                  ),
                                  
                                  column(4,
                                         box( title = "Binned Trip Time", solidHeader = TRUE, status = "primary", width = 12,
                                              plotOutput("histTripTime", height="34vh"), height="40vh")
                                  )
                                  
                                )
                                
                                
                                
                                
                       ),
                       fluidRow(
                         leafletOutput("main_map")
                       )
                       
                       
                       
                       
                       
                )
                
                
                
                
                
                
              )
              
              
              
              
      ),
      
      tabItem(tabName="yearlyPlots",
              
              fluidRow(
                
                column(2,
                       
                       fluidRow(style="height:40vh"),
                       p("Input controls"),
                       selectInput("years", "Select the year", years, selected = "2021"),
                       selectInput(inputId = "yearly_station", label = "Select station", choices = NULL),
                       selectInput("order_for_single", "Select chart Type", c("BarPlot", "Table"), selected = "BarPlot")
                       
                ),
                column(10,
                       
                       column(6, 
                              
                              fluidRow(style="height:40vh", 
                                       box( title = "Daily entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("daily"), dataTableOutput("tableDaily")
                                       )
                                       
                                       
                              ),
                              
                              fluidRow(style="height: 40vh", 
                                       
                                      #  leafletOutput("mymap2")
                              )
                              
                       ),
                       column(4,
                              
                              fluidRow(
                                column(12, 
                                       box( title = "Yearly entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("yearly"), dataTableOutput("tableYearly")
                                       )
                                       
                                )
                              ),
                              fluidRow(
                                column(6,
                                       box( title = "Monthly entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("monthly"), dataTableOutput("tableMonthly")
                                       )
                                ),
                                column(6, 
                                       box( title = "Day of Week entries", solidHeader = TRUE, status = "primary", width = 12,
                                            plotOutput("weekly"), dataTableOutput("tableWeekly")
                                       )
                                )
                                
                              )
                              
                       )
                       
                )
                
              )
              
      ),
      
      tabItem(tabName= "about",
              h2("About"), 
              p("Application written by Gautam Kushwah & Add your name here for CS424 spring 2022 taught by Dr. Andrew Johnson"),
              p("Data taken from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              p("The app helps visualize CTA L data over the last 20 years and helps uncover trends in data"),
              p("Reference for R functions through https://shiny.rstudio.com/reference/shin")
      )
      
    ),
    
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode, functions = c('markerClick'))
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # date1 <- reactive({input$date1})
  # date2 <- reactive({input$date2})
  # orders <- reactive({input$orders})   #order for the bar plot
  # values <- reactiveValues(selected=NULL)
  # yearly_values <- reactiveValues(selected="UIC-Halsted")
  # mode <- reactive({input$radio}) ###checking for which mode the user is in
  # single_year <- reactive({input$years})
  # yearly_station <- reactive({input$yearly_station})
  
  
  
  # #   code for dynamic header
  # # output$text <-renderText({ paste("Total entries for", date( input$date1), ", ", weekdays(input$date1) ) })
  # # tmpdata <- reactive({ subset(mergedData, newDate==input$date1)})
  # # tmpdata2 <-reactive ({subset(mergedData, newDate==input$date2) })
  # #   dfnew3 <- reactive({data.frame(tmpdata()$stationname, tmpdata()$rides)})
  # #   names(dfnew3) <- c("Station", "Rides")
  
  # choices_stations <- reactive({
  #   choices_stations <- tmpdata() %>% distinct(stationname) %>% arrange()
  
  # })
  
  # choices_stations_new <- reactive({
  #   choices_stations_new <- single_df() %>% distinct(stationname) %>% arrange()
  
  # })
  
  
  # observe({
  #   updateSelectInput(session = session, inputId = "station", choices = choices_stations())
  # })
  
  # ####updating station list on the yearly plot page, single df is the data frame
  # observe({
  #   updateSelectInput(session = session, inputId = "yearly_station", choices = choices_stations_new(), selected="UIC-Halsted")
  # })
  
  
  # observeEvent(input$prevDay, {
  #   curDate <- date1()
  #   curDate <- curDate - 1
  #   updateDateInput(session, "date1", value = curDate)
  # })
  
  # observeEvent(input$nextDay, {
  #   curDate <- date1()
  #   curDate <- curDate + 1
  #   updateDateInput(session, "date1", value = curDate)
  # })
  
  
  output$hist1 <- renderPlot({
    print("Daily plot")
    g<- ggplot(data = daily_rides, aes(x = Date, y = n_rides)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Taxi Rides per Day for the year of 2019",
           #subtitle = sub,
           x = "Date", 
           y = "Rides") +
      scale_x_date(date_labels = "%d-%b", breaks = date_breaks("months"),date_minor_breaks="days" ) +
      scale_y_continuous(labels = scales::comma)   
      print("plotted daily plot")
    return(g)
  })
  
  output$main_map <- renderLeaflet({
    print("map")
      map_plot %>%
      addPolygons( data = community_shp,
                        color = "#444444",
                        weight = 1, 
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillOpacity = 0.65,
                        dashArray = "3",
                        highlightOptions = highlightOptions(color = "white", 
                                                            weight = 2,
                                                            dashArray = "",
                                                            bringToFront = TRUE),
                        popup=labels,
                        label = labels,
                  
                  
                )
      print("plotted map")
      return(map_plot)
  })
  
  output$histHourly <- renderPlot({
    
    g <- ggplot(data = hourly_rides, aes(x = factor(Hour), y = n_rides)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Taxi Rides per Day for the year of 2019",
           #subtitle = sub,
           x = "Hour", 
           y = "Rides") +
          scale_y_continuous(labels = scales::comma) + scale_x_discrete(labels = time_in_12, guide=guide_axis( angle = 45))
          print("plotted hourly")
        return(g)
  })
  
  
  output$histDay <- renderPlot({
    dft <- taxi
    f <- factor(weekdays(taxi$Date), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    g<- ggplot(dft, aes(x= f)) +labs(x="Days of the week", y="Total number of entries") + geom_bar(stat="count", position="dodge", fill="deepskyblue4")  
    print("plotting day of the week")
        return(g)
  })
  
  
  
  
  output$histMonthly <- renderPlot({
    g <- ggplot(month_rides, aes(x= factor(newMonth), y= n_rides)) +labs(x="Month of the week", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  + scale_x_discrete(labels = months, guide=guide_axis( angle = 45))
    print("plotting monthly")
    return(g)
  })
  
  
  
  output$histBinMile <- renderPlot({
    taxi %>% mutate(pop_cut = cut_number(Miles, n = 6)) %>% ggplot(aes(x = pop_cut)) + geom_bar(stat="count", fill="steelblue") + labs(title = "Taxi Rides Binned")
    
  })
  output$histTripTime <- renderPlot({
    taxi %>% mutate(pop_cut = cut_number(Duration, n = 6)) %>% ggplot(aes(x = pop_cut)) + geom_bar(stat="count", fill="steelblue") + labs(title = "Taxi Rides Binned")
    
  })
  
  
  
  output$dailyTable <- DT::renderDataTable({
    
    datatable(daily_rides, 
              options = list(
                searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15)
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")
    
    
    
  })
  
  output$hourlyTable <- renderDataTable({
    
    datatable(hourly_rides, 
              options = list(
                searching = FALSE,pageLength = 5, lengthMenu = c(5, 10, 15)
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")
    
    
    
  })
  
  
  output$dayTable <- renderDataTable({
    
    datatable(weekdays_rides, 
              options = list(
                searching = FALSE,pageLength = 7, lengthMenu = c(5, 10, 15),
                order = list(list(1, 'asc'))
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")
    
    
    
  })
  
  
  output$monthlyTable <- renderDataTable({
    
    datatable(taxi, 
              options = list(
                searching = FALSE,pageLength = 12,
                order = list(list(1, 'asc'))
              )) %>% 
      formatCurrency(2, currency = "", interval = 3, mark = ",")
    
    
    
  })
  
  # output$barplottable <- renderDataTable({
  #   # df <- tmpdata()
  #   # df2 <- tmpdata2()
  
  #   # if(mode()=="Single Date"){
  #   #   dfl <- df[, c("stationname", "rides", "lines")]
  #   #   fm <- "rides"
  #   # }else{
  #   #   dfl <- df %>% full_join(df2, by="station_id")
  #   #   dfl <- dfl[!is.na(dfl$stationname.x),]
  #   #   dfl$rides.y <- replace_na(dfl$rides.y, 0)
  #   #   dfl$diff = dfl$rides.x - dfl$rides.y 
  #   #   dfl <- dfl[, c("stationname.x", "diff", "lines.x")]
  #   #   colnames(dfl) <- c("Station Name", "Difference", "Lines")
  #   #   fm <- "Difference"
  #   # }
  
  
  
  
  #   # if(orders() == "Descending"){
  #   #   tab_order <- list(list(2, 'dsc'))
  #   # } else if(orders()=="Ascending"){
  #   #   tab_order <- list(list(2, 'asc'))
  #   # } else{
  #   #   tab_order <- list(list(1, 'asc'))
  #   # }
  #   # datatable(dfl, 
  #   #           options = list(
  #   #             searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
  #   #             order = tab_order
  #   #           )) %>% 
  #   #   formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
  #   #   formatRound(fm, digits = 0)
  # })
  
  
  
  
  # ### observer for map
  # observe({
  #   proxy <- leafletProxy("mymap")
  #   proxy %>% clearPopups() 
  #   event <- input$mymap_marker_click
  #   values$selected <- event$id
  #   if (is.null(event))
  #     return()
  #   # print(length(input$mymap_click))
  #   if(length(input$mymap_click) > 0) {
  #     updateSelectInput(session = session, inputId = "station", selected =  values$selected)
  #   }  
  
  
  # })
  
  # ### anotherObserver
  # observeEvent(input$station, {
  #   proxy <- leafletProxy("mymap")
  #   proxy %>% clearPopups() 
  #   shinyjs::js$markerClick(input$station)
  #   # df <- tmpdata()
  #   # values$selected <- input$station
  #   # df <- subset(df, stationname == values$selected)
  #   # print("it comes here")
  
  #   # content <-  paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
  #   #                   df$lines, "<br>",
  #   #                   "Rides: ", df$rides, "<br> </center>")
  #   # if(length(input$mymap_click) == 0){
  #   #     leafletProxy("mymap") %>% addPopups(df$long, df$lat, content)
  #   # }
  
  
  # })
  
  # ##############for second map
  
  # ### observer for map
  # observe({
  #   proxy <- leafletProxy("mymap2")
  #   proxy %>% clearPopups() 
  #   event <- input$mymap2_marker_click
  #   yearly_values$selected <- event$id
  #   if (is.null(event))
  #     return()
  #   # print(length(input$mymap_click))
  #   if(length(input$mymap2_click) > 0) {
  #     updateSelectInput(session = session, inputId = "yearly_station", selected =  yearly_values$selected)
  #   }  
  
  
  # })
  
  
  # observeEvent(input$yearly_station, {
  #   proxy <- leafletProxy("mymap2")
  #   proxy %>% clearPopups() 
  #   shinyjs::js$markerClick(input$yearly_station)
  
  # })
  
  # observeEvent(input$radio, {
  #   if(input$radio == "Single Date"){
  #     shinyjs::hide(id="date2")
  #   }else{
  #     shinyjs::show(id="date2")
  #   }
  # })
  
  
  # observeEvent(input$order_for_single, {
  #   if(input$order_for_single == "BarPlot"){
  #     shinyjs::hide(id="tableYearly")
  #     shinyjs::hide(id="tableMonthly")
  #     shinyjs::hide(id="tableWeekly")
  #     shinyjs::hide(id="tableDaily")
  #     shinyjs::show(id="yearly")
  #     shinyjs::show(id="monthly")
  #     shinyjs::show(id="weekly")
  #     shinyjs::show(id="daily")
  
  #   }else{
  #     shinyjs::hide(id="yearly")
  #     shinyjs::hide(id="monthly")
  #     shinyjs::hide(id="weekly")
  #     shinyjs::hide(id="daily")
  #     shinyjs::show(id="tableYearly")
  #     shinyjs::show(id="tableMonthly")
  #     shinyjs::show(id="tableWeekly")
  #     shinyjs::show(id="tableDaily")
  #   }
  # })
  
  
  
  
  
  
  
  
  # ########### yearly page
  
  
  # output$mymap2 <- renderLeaflet({
  #   # df <- subset(mergedData, newDate==date1())
  #   # df$line_color <-  str_extract(df$line, "\\w+")
  #   # map <- leaflet(options= leafletOptions()) %>%
  #   #   addTiles(group="Base") %>% 
  #   #   addCircleMarkers(data = df, lat = ~lat, lng = ~long, 
  
  #   #                    radius = ~log(rides+10)*1.25,
  #   #                    layerId = ~stationname,
  #   #                    color = ~line_color,
  #   #                    fillOpacity = 0.6,
  #   #                    popup = paste("<center><strong>" ,df$stationname, "</strong>", "<br>",
  #   #                                  df$lines, "<br>",
  #   #                                  "Rides: ", df$rides, "<br> </center>")
  #   #   )%>%
  #   #   setView( lat = 41.8781, lng = -87.6298, zoom = 10) %>%
  #   #   addResetMapButton() %>%
  #   #   addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%
  #   #   addProviderTiles("CartoDB.Positron", group="Positron") %>%
  #   #   addLayersControl(
  #   #     baseGroups = c("Base", "Satellite", "Positron"),
  #   #     options = layersControlOptions(collapsed = FALSE)
  #   #   )
  #   # map <- map %>% 
  #   #   htmlwidgets::onRender("
  #   #       function(el, x) {
  #   #         map = this;
  #   #       }"
  #   #   )     
  #   # return(map)
  
  # })
  
  
  
  # output$daily <- renderPlot({
  
  #   # dft <- single_df()
  #   # dft <- subset(dft, stationname==yearly_station())
  #   # ggplot(dft, aes(x= newDate, y=rides)) +labs(x="Date ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") 
  
  # })
  
  # output$yearly <- renderPlot({
  
  #   # dft <- mergedData
  #   # dft <- subset(dft, stationname==yearly_station())
  #   # ggplot(dft, aes(x= year(newDate), y=rides)) +labs(x="Year ", y = "Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") 
  
  # })
  
  
  # output$monthly <- renderPlot({
  
  #   # dft <- single_df()
  #   # dft <- subset(dft, stationname==yearly_station())
  #   # ggplot(dft, aes(x= month(newDate), y=rides)) +labs(x="Months (1 = Jan, 12=Dec)", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  + scale_x_continuous(breaks = seq(1, 12, by = 1)) + ggtitle(paste("Station Name: ", input$yearly_station), subtitle=paste("Year", input$years))
  
  # })
  
  
  # output$weekly <- renderPlot({
  
  #   # dft <- single_df()
  #   # dft <- subset(dft, stationname==yearly_station())
  #   # f <- factor(weekdays(dft$newDate), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
  #   # ggplot(dft, aes(x= f, y=rides)) +labs(x="Days of the week", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  + ggtitle(paste("Station Name: ", input$yearly_station), subtitle=paste("Year", input$years))
  
  # })
  
  # output$tableYearly <- renderDataTable({
  
  #   # df<- subset(mergedData, stationname==yearly_station()) %>%
  #   #   mutate(year = format(newDate, "%Y")) %>%
  #   #   group_by(year) %>%
  #   #   summarise(rides = sum(rides))
  
  
  
  #   # datatable(df, 
  #   #           options = list(
  #   #             searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
  #   #             order = list(list(1, 'asc'))
  #   #           )) %>% 
  #   #   formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
  #   #   formatRound('rides', digits = 0)
  # })
  
  
  # output$tableMonthly <- renderDataTable({
  
  #   # df<- subset(mergedData, stationname==yearly_station()) %>%
  #   #   mutate(month = month(newDate)) %>%
  #   #   group_by(month) %>%
  #   #   summarise(rides = sum(rides))
  
  
  
  #   # datatable(df, 
  #   #           options = list(
  #   #             searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
  #   #             order = list(list(1, 'asc'))
  #   #           )) %>% 
  #   #   formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
  #   #   formatRound('rides', digits = 0)
  # })
  
  
  # output$tableWeekly <- renderDataTable({
  
  #   # df<- subset(mergedData, stationname==yearly_station()) %>%
  #   #   mutate(weekdays = weekdays(newDate)) %>%
  #   #   group_by(weekdays) %>%
  #   #   summarise(rides = sum(rides))
  
  
  
  #   # datatable(df, 
  #   #           options = list(
  #   #             searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
  #   #             order = list(list(1, 'asc'))
  #   #           )) %>% 
  #   #   formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
  #   #   formatRound('rides', digits = 0)
  # })
  
  # output$tableDaily <- renderDataTable({
  #   # df<- single_df()
  #   # df<- subset(mergedData, stationname==yearly_station()) 
  
  #   # df <- df[, c("newDate", "rides")]
  
  #   # datatable(df, 
  #   #           options = list(
  #   #             searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
  #   #             order = list(list(1, 'asc'))
  #   #           )) %>% 
  #   #   formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
  #   #   formatRound('rides', digits = 0)
  # })
  
  
  
  ##############last bracket
  
}

# Run the application 
shinyApp(ui = ui, server = server)
