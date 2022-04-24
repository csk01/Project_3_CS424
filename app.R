# LIBRARIES=======================================================================================================
library(lubridate)
# library(DT)
library(ggplot2)
# library(plotly)
library(leaflet)
library(leaflet.extras)
library(dplyr)
# library(tidyr)
library(scales)
library(shiny)
library(shinyjs)
library(shinydashboard)
# library(stringr)
# library(shinyjs)
library(data.table)
library(purrr)
library(rgdal)
library(RColorBrewer)

library(DT)


jsCode <- 'shinyjs.shapeClick = function(id) {
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

taxi_original <- do.call(rbind, lapply(list.files(pattern = "*.csv"), fread)) 
taxi_outside <- taxi_original
taxi_inside <- taxi_original[Pickup>=1 & Dropoff >=1]


#Reading community boundaries from a shape file 
# Source: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
community_shp <- rgdal::readOGR("shp_files/geo_export_fca70ba1-774b-4562-b299-3cbfe3855c4d.shp",
                                layer = "geo_export_fca70ba1-774b-4562-b299-3cbfe3855c4d", GDAL1_integer64_policy = TRUE)

# new_row<-c(0, 79, 79, 0 , 0, 'Outside Chicago', 0, 0, 0)
# community_shp@data[nrow(community_shp) + 1,] <- new_row

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
values <- c('24 Seven Taxi','312 Medallion Management Corp','5 Star Taxi','Adwar H. Nikola',
            'Ahzmi Inc','American United','American United Taxi Affiliation',
            'Arrington Enterprises','Babylon Express Inc.','Benny Jona','Blue Diamond',
            'Blue Ribbon Taxi Association Inc.','Checker Taxi','Checker Taxi Affiliation',
            'Chicago Carriage Cab Corp','Chicago Independents','Chicago Medallion Management',
            'Chicago Star Taxicab','Chicago Taxicab','Choice Taxi Association','Chuks Cab','City Service',
            'David K. Cab Corp.','Flash Cab','G.L.B. Cab Co','Globe Taxi','Gold Coast Taxi','Jay Kim',
            'JBL Cab Inc.','KOAM Taxi Association','Leonard Cab Co','Luhak Corp','Medallion Leasin',
            'Metro Jet Taxi A','N and W Cab Co','Nova Taxi Affiliation Llc','Omar Jada',
            'Patriot Taxi Dba Peace Taxi Associat','Petani Cab Corp','RC Andrews Cab','Reny Cab Co','Salifu Bawa',
            'Sam Mestas','Santamaria Express, Alvaro Santamaria','Sbeih company','Sergey Cab Corp.','Setare Inc',
            'Star North Management LLC','Sun Taxi','Tasha ride inc','Taxi Affiliation Service Yellow',
            'Taxi Affiliation Services','Taxicab Insurance Agency, LLC','Top Cab Affiliation','U Taxicab','Yellow Cab', "All")

#Keys are string values of numbers 
keys <- sprintf("%s",seq(1:57))

#Assigning hash
assign_hash(keys, values, hash)

#To extract value 
#hash[["1"]]

#Credits for below code snippet: https://stackoverflow.com/questions/70288989/programatically-trigger-marker-mouse-click-event-in-r-leaflet-for-shiny


#Community areas list to access Later
community_areas <- c("Rogers Park", "West Ridge","Uptown","Lincoln Square","North Center","Lake View","Lincoln Park", "Near North Side", "Edison Park",
                     "Norwood Park","Jefferson Park","Forest Glen","North Park","Albany Park","Portage Park","Irving Park","Dunning","Montclare","Belmont Cragin",
                     "Hermosa","Avondale","Logan Square","Humboldt Park","West Town","Austin","West Garfield Park","East Garfield Park","Near West Side",
                     "North Lawndale","South Lawndale","Lower West Side","The Loop","Near South Side","Armour Square","Douglas","Oakland","Fuller Park",
                     "Grand Boulevard","Kenwood","Washington Park","Hyde Park","Woodlawn","South Shore","Chatham","Avalon Park","South Chicago","Burnside",
                     "Calumet Heights","Roseland","Pullman","South Deering","East Side","West Pullman","Riverdale","Hegewisch","Garfield Ridge","Archer Heights",
                     "Brighton Park","McKinley Park","Bridgeport","New City","West Elsdon","Gage Park","Clearing","West Lawn","Chicago Lawn","West Englewood",
                     "Englewood","Greater Grand Crossing","Ashburn","Auburn Gresham","Beverly","Washington Heights","Mount Greenwood","Morgan Park","O'Hare","Edgewater", "All of Chicago")

community_areas_outside <- append(community_areas, "Outside Chicago")
#func to convert NAs in Pickup and Dropoff to 78)
f_dowle3 = function(DT) {

  for (j in 3:5)
    set(DT,which(is.na(DT[[j]])),j,79)
}
f_dowle3(taxi_outside)


years<-c(2001:2021)

time_in_24 <-c( '0000',  '0100', '0200', '0300', '0400', '0500', '0600', '0070', '0080', '0900', '1000', '1100', '1200', '1300', '1400',
                '1500', '1600', '1700', '1800', '1900', '2000', '2100', '2200', '2300' )

time_in_12 <- c('12:00 am','01:00 am','02:00 am','03:00 am','04:00 am','05:00 am','06:00 am','07:00 am','08:00 am','09:00 am','10:00 am','11:00 am','12:00 pm','01:00 pm','02:00 pm','03:00 pm','04:00 pm','05:00 pm','06:00 pm','07:00 pm','08:00 pm','09:00 pm','10:00 pm','11:00 pm')

months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'Novermber', 'December')

days_in_week <- c('Sunday','Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')

#defining basic leaflet map to add on to later
map_plot <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  #addResetMapButton() %>%
  addResetMapButton() %>%
  setMaxBounds(lng1 = -87.999, lat1=41.50, lng2=-87.00412 , lat2=42.380379 ) %>%
  #-87.94011, lat1=41.619478, lng2=-87.00412 , lat2=42.080379 ) %>%
  addPolygons( data = community_shp,
    color = "#444444",
    weight = 3, 
    smoothFactor = 0.5,
     opacity = 1.0,
     fillOpacity = 0.75,
     dashArray = "2",
     highlightOptions = highlightOptions(color = "white",
     weight = 2,
     dashArray = "",
     bringToFront = TRUE),
    #popup=labels,
     label = labels,
     layerId = ~community_shp$area_numbe)




###### use the same data frames for tables

# Define UI for application that draws a histogram
ui <- dashboardPage(
                    dashboardHeader(title = "Big Yellow Taxi"),
  
                    # Application title
                    dashboardSidebar(
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
                            tabItem(
                                tabName = "dashboard",
                                
                                fluidRow(
                                    shinyjs::useShinyjs(),
                                    shinyjs::extendShinyjs(text = jsCode, functions = c('shapeClick')),
                                    column(1,
                                        fluidRow(style="height:40vh"),
                                        p("Input controls"),
                                        checkboxInput(inputId="outsideChicago", label="Outside Chicago", value = FALSE, width = NULL),
                                        radioButtons(
                                            inputId = "radioMode",
                                            label = "Mode",
                                            choices = c("To", "From"),
                                            selected = "To",
                                            inline = FALSE,
                                            width = NULL
                                        ),
                                        selectInput(inputId = "community", label = "Select community", choices = sort(community_areas), selected="All of Chicago"),
                                        selectInput(inputId = "taxiCompany", label = "Select Taxi Company", choices = sort(values), selected = "All"),
                                        
                                        radioButtons(
                                            inputId = "radioTime",
                                            label = "Time Format",
                                            choices = c("12HR", "24HR"),
                                            selected = "12HR",
                                            inline = FALSE,
                                            width = NULL
                                        ),
                                        radioButtons(
                                            inputId = "radioDistance",
                                            label = "Show Distance in",
                                            choices = c("KM", "Mi"),
                                            selected = "KM",
                                            inline = FALSE,
                                            width = NULL
                                        )
                                    ),
                                    column(1,
                                        id="histcontainer",
                                        column(12,
                                            box(title = "% rides", 
                                                solidHeader = TRUE,
                                                status = "primary", 
                                                #width = 12,
                                                plotOutput("histCommunity", height="90vh") 
                                            )
                                        )
                                    ),
                                    column(10, 
                                        fluidRow(style='height:40vh',
                                            column(6,
                                                   box(
                                                    title = "Daily Entries", 
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 12,
                                                    plotOutput(
                                                        "hist1", 
                                                        height="34vh"
                                                    ), 
                                                    height="40vh")
                                            ),
                                            column(1,
                                                box(
                                                    title= "Daily Entries Table",
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 180,
                                                    DT::dataTableOutput("dailyTable"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(2,
                                                box( 
                                                    title = "Hourly Entries", 
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 12,
                                                    plotOutput("histHourly", height="34vh"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(1,
                                                box(
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 180, 
                                                    title="table for hourly",
                                                    dataTableOutput("hourlyTable"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(1,
                                                box( 
                                                    title = "Entries: Day of the Week", 
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 12,
                                                    plotOutput("histDay", height="34vh"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(1,
                                                box(
                                                    title="DOW Table",
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 180,
                                                    dataTableOutput("dayTable"), 
                                                    height="40vh"
                                                )
                                            )
                                        ),
                                        fluidRow(
                                            style='height:50vh; margin-top: 100px',
                                            column(3,
                                                box(
                                                    status= "primary",
                                                    solidHeader = FALSE,
                                                    title="Chicago Community Map",
                                                    width = 12,
                                                    leafletOutput("main_map", height="50vh")
                                                )
                                            ),
                                            column(2,
                                                box( 
                                                    title = "Entries by Month", 
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 12,
                                                    plotOutput("histMonthly", height="34vh"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(1,
                                                box(
                                                    title="Monthly Table",
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 180,
                                                    dataTableOutput("monthlyTable"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(2,
                                                box( 
                                                    title = "Binned Mileage", 
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 12,
                                                    plotOutput("histBinMile", height="34vh"), 
                                                    height="40vh"
                                                ), 
                                                height="40vh"
                                            ),
                                            column(1,
                                                box(
                                                    title = "Mileage Table",
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 180,
                                                    dataTableOutput("binTable"), 
                                                    height="40vh"
                                                )
                                            ),
                                            column(2,
                                                box( 
                                                    title = "Binned Trip Time", 
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 12,
                                                    plotOutput("histTripTime", height="34vh"), 
                                                    height="40vh"
                                                ), 
                                                height="40vh"
                                            ),
                                            column(1,
                                                box(
                                                    title = "Trip Time Table",
                                                    solidHeader = TRUE, 
                                                    status = "primary", 
                                                    width = 180,
                                                    dataTableOutput("tripTable"), 
                                                    height="40vh"
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            tabItem(
                                tabName= "about",
                                h2("About"), 
                                p("Application written by Gautam Kushwah & Krishnan C. S. for CS424 spring 2022 taught by Dr. Andrew Johnson"),
                                p("Data taken from the following sources"),
                                p("1. Community Shape Files - Chicago data portal - https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6"),
                                p("2. Chicago Taxi Trips data - https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy"),
                                p("The app helps visualize Chicago Taxi Data data for the year 2019 years based on community names and taxi companies and helps uncover trends in data"),
                                p("Reference for R functions through https://shiny.rstudio.com/reference/shiny")
                            )
                        )
                    )
      )

# Define server logic required to draw a histogram

####jump to reactive
server <- function(input, output, session) {


    mode <- reactive({
        if(input$radioMode == "To") return("Dropoff") else return("Pickup")
    })
    time_format <- reactive({
        input$radioTime
    })
    community <- reactive({
        if(outside_chicago()){
                which(community_areas_outside == input$community)
        }else{
               which(community_areas == input$community)
        }
    })   
    distance_format <- reactive({
        input$radioDistance
    })
    outside_chicago <- reactive({
        input$outsideChicago
    })
    #Val to hold outside chicago percent
    outside <- reactiveValues()

    taxi_company <- reactive({
        return(which(values == input$taxiCompany))
    })


##########################
choices_community <- reactive({
    if(outside_chicago()){
        return(sort(community_areas_outside))
    }else{
        return(sort(community_areas))
    }
    
})

choices_community_selected <- reactive({
    return(input$community)
})
observe({

    updateSelectInput(session = session, inputId = "community", choices = choices_community(), selected=choices_community_selected())
  })
#########################

    comm_reactive <- reactive({
        if(outside_chicago()){
            taxi <- (taxi_outside)
        }else{
            taxi <- taxi_inside
        }
        if(input$community == "All of Chicago" & input$taxiCompany == "All"){
                shinyjs::hide(id = "histcontainer")
        }else{
                shinyjs::show(id = "histcontainer")
        }
        if(community() != 78 && taxi_company() != 57){

            if(mode() == "Dropoff"){
                common_DT <- taxi[ Company == taxi_company() & Dropoff== community() ]
            }
            else{
                common_DT <- taxi[ Company == taxi_company() & Pickup== community() ]
            }
            return(common_DT)

        }else if (community() == 78 && taxi_company() != 57 ){

            if(mode() == "Dropoff"){
                common_DT <- taxi[ Company == taxi_company()  ]
            }
            else{
                common_DT <- taxi[ Company == taxi_company()  ]
            }
            return(common_DT)

        }else if(community()!= 78 && taxi_company() == 57 ){

            if(mode() == "Dropoff"){
                common_DT <- taxi[ Dropoff== community()  ]
            }
            else{
                common_DT <- taxi[ Pickup== community()  ]
            }
            return(common_DT)

        }else{
            common_DT <- taxi
            return(common_DT)

        }

    })

    daily_reactive <- reactive({
        daily_rides_local <- comm_reactive()
        daily_rides_local <- daily_rides_local[, .N, by=Date]
        return(daily_rides_local)
    })



    hourly_reactive <- reactive({
        hourly_rides_local <- comm_reactive()
        hourly_rides_local <- hourly_rides_local[, .N, by=Hour]
        return(hourly_rides_local)
    })



    weekday_reactive <- reactive({

        weekdays_rides_local <- comm_reactive()
        weekdays_rides_local <- weekdays_rides_local[, .N, by=wday(Date)]
        weekdays_rides_local <- rename(weekdays_rides_local, "weekday" = "wday" )
        # names(weekdays_rides_local)[names(weekdays_rides_local) == 'wday'] <- 'weekday'
        return(weekdays_rides_local)

    })



    month_reactive <- reactive({

        month_rides_local <- comm_reactive()
        month_rides_local <- month_rides_local[, .N, by=month(Date)]
        month_rides_local <- rename(month_rides_local, "newMonth" = "month" )
        return(month_rides_local)

    })

    bin_reactive_distance <- reactive({
        binned_mile <- comm_reactive()
        if(distance_format() == "KM"){
            binned_mile <- binned_mile[, KM:=(Miles*1.609)]
            binned_mile <- binned_mile[, distance_bin:=cut(
                                                            KM,
                                                            breaks = c(0.8, 5, 10, 20, 40, 60, 80, 100, Inf ),
                                                            labels=c("<5 KM", "5-10 KM", "10-20 KM", "20-40 KM", "40-60 KM", "60-80 KM", "80-100 KM", ">100 KM"),
                                                            include.lowest = TRUE
                                                        )
            ]
        }else{
            binned_mile <- binned_mile[, distance_bin:=cut(
                                                            Miles,
                                                            breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100, Inf ),
                                                            labels=c("<1 Mi", "1-2 Mi", "2-5 Mi", "5-10 Mi", "10-20 mi", "20-50 Mi", "50-100 Mi", ">100 Mi"),
                                                            include.lowest = TRUE
                                                        )
            ]
        }
        
                                    
        binned_mile <- binned_mile[, .N, by=distance_bin]
        return(binned_mile)        

    })

    shape_reactive <-reactive({
        comm_df <-comm_reactive()
        
        comm_name <- community()
        if(mode() == "Pickup"){
                

                
                

                ride_percent <- comm_df
                total_rides <- count(ride_percent)
                ride_percent <- ride_percent[, .N, by=Dropoff]    
                ride_percent <- ride_percent[, percentage:=((N/as.integer(total_rides) )*100)]
                


                # merged spatial df  file to plot heatmap
                mynewspdf <- merge(community_shp, ride_percent, by.x = "area_numbe", by.y = "Dropoff", all = TRUE)
                
                temp <- ride_percent %>% filter(Dropoff==79)
                outside$percentage <- temp$percentage
                
            }
        else{
            ride_percent <- comm_df %>% 
            group_by(Pickup) %>% 
            summarise(n_rides=n())
            
            ride_percent$percentage <- 100*(ride_percent$n_rides/sum(ride_percent$n_rides))
            #merged spatial df  file to plot heatmap
            mynewspdf <- merge(community_shp, ride_percent, by.x="area_numbe", by.y="Pickup" , all=TRUE)
            
            temp <- ride_percent %>% filter(Pickup==79)
            outside$percentage <- temp$percentage
        }
        
        
        return(mynewspdf)

    })

    bin_reactive_time <- reactive({
        binned_time <- comm_reactive()
        binned_time <- binned_time[, Mins:=(Duration/60)]
        binned_time <- binned_time[, time_bin:=cut(
                            Mins, 
                            breaks=c(1, 5,10, 15, 30, 45, 60, 120, Inf), 
                            labels = c("< 5 Mins", "5 - 10 Mins", "10 - 15 Mins", "15 - 30 Mins", "30 - 45 Mins", "45Mins - 1hr", "1 - 2Hr", "> 2Hr"), 
                            include.lowest = TRUE )]
        binned_time <- binned_time[, .N, by=time_bin]
        return(binned_time)
    })



 ##################### Histograms #################### 
    output$selectedVar <- renderText({
        paste("mode() taxi[Company=",taxi_company()," & ", mode(), "==",  community(), "]")
    })
  

    output$hist1 <- renderPlot({
        
    
        g<- ggplot(
            data = daily_reactive(), 
            aes(x = Date, y = N)) +
            geom_bar(
                stat="identity", 
                fill="deepskyblue4", 
                width=0.9) +
            labs(
                title = paste("Taxi Rides per Day, 2019 for ", input$community, " community & " ,input$taxiCompany, " service provider" ),
                x = "Date", 
                y = "Total number of entries") +
                scale_x_date(date_labels = "%d-%b", breaks = date_breaks("months"),date_minor_breaks="days" ) +
                scale_y_continuous(labels = scales::comma)   
    
        
        return(g)
    })
  










     #leaflet map ================================================================================
    output$main_map <- renderLeaflet({
        print("inside leaflet map")

        spdf <- shape_reactive()
        
        # print()
        #Bins and pal for map
        bins <- c(0,0.025,0.1,0.5,1,2.5,5,10,Inf) 
        # mypalette <- colorBin( palette="RdYlGn", domain=spdf$percentage ,bins=bins, pretty=FALSE)
        # NAcol = mypalette(0)
        # mypalette <- colorBin( palette="RdYlGn", domain=spdf$percentage ,bins=bins, pretty=FALSE, na.color = NAcol)

        mypalette <- colorBin( palette="RdYlGn", domain=spdf$percentage ,bins=bins, pretty=FALSE)
        NAcol = mypalette(0)
        mypalette <- colorBin( palette="RdYlGn", domain=spdf$percentage ,bins=bins, pretty=FALSE, na.color = NAcol)

        if(community() == 78 && taxi_company() == 57)
        {
          print("all commty, all company selected")
          map<- map_plot 
          if(outside_chicago()){
            map <- map %>% addRectangles(
                lat1 =41.970111, lat2=41.889261,
                lng1=-87.459141, lng2=-87.553412,
                fillColor = '#444444',
                label = "Outside Chicago",
                weight = 1, 
                layerId=79,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.65,
                dashArray = "3",
                highlightOptions = highlightOptions(color = "white",
                weight = 2,
                dashArray = "",
                bringToFront = TRUE)) 
          }
          return(map)
        }
        
       

         print(spdf@data$Pickup)

        map_plot <- map_plot %>% 
        setMaxBounds(lng1 = -87.999, lat1=41.50, lng2=-87.00412 , lat2=42.380379 ) %>%
        addPolygons(data = spdf,
            color = ~mypalette(percentage),
            weight = 1, 
            smoothFactor = 0.5,
            opacity = 1.0,
            fillOpacity = 0.65,
            dashArray = "3",
            stroke = 1,
            highlightOptions = highlightOptions(color = "white",
                    weight = 2,
                    dashArray = "",
                    bringToFront = TRUE),
        #popup=labels,
        label = labels,
        layerId = ~spdf@data$area_numbe)%>%
        addLegend(pal=mypalette,values= bins,
        position="bottomright", title = "Percentage of Rides(%)",
        opacity = 0.8)
        
        if(outside_chicago()){
            print("outside percent val")
            print(outside$percentage)
            map_plot <- map_plot %>% 
            addRectangles(
                lat1 =41.970111, lat2=41.889261,
                lng1=-87.459141, lng2=-87.553412,
                fillColor = mypalette(outside$percentage),
                label = "Outside Chicago",
                weight = 1, 
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.65,
                dashArray = "3",
                highlightOptions = highlightOptions(color = "white",
                weight = 2,
                dashArray = "",
                bringToFront = TRUE)) 
        }
    return(map_plot)
    })

    #Change value of Selectize input on map click
    observeEvent(input$main_map_shape_click,{
        #print("Community cicked on map")
        
        #updating select-input based on map
        click <- input$main_map_shape_click
        community_id <- click$id
        print(community_id)
        print(community_areas[as.numeric(community_id)])
        isolate(
            updateSelectInput(
                session, 
                "community", 
                selected = community_areas[as.numeric(community_id)])
        )
    })
  
    
  
    
    #to highlight on select community option
    observeEvent(input$community,{
        #print("select community chosen")
        comm_name <- input$community
        comm_id <- which(community_areas == comm_name)
        shinyjs::js$shapeClick(comm_id)
        to_highlight <- subset(community_shp, area_numbe == comm_id)
        leafletProxy("main_map", session) %>% 
            removeShape(layerId = "selected") %>%
            addPolygons(
                layerId = 'selection',
                data = to_highlight, 
                fill =  "#D24618", 
                color = "blue", 
                #popup = comm_name
            )
    })
  

    output$histHourly <- renderPlot({
   

        g <-ggplot(data = hourly_reactive(), aes(x = factor(Hour), y = N)) +
            geom_bar(stat="identity", fill="deepskyblue4") +
            labs(
                title = paste("", input$community, " community & " ,input$taxiCompany, " service provider" ),
                x = "Hour", 
                y = "Total number of entries"
            ) 
          

        if( dim(hourly_reactive())[1] != 0 ){
            if(time_format() == "12HR"){
                g <- g + scale_x_discrete(labels = time_in_12, guide=guide_axis( angle = 45)) + scale_y_continuous(labels = scales::comma) 
            }else{
                g <- g + scale_x_discrete(labels = time_in_24, guide=guide_axis( angle = 45)) + scale_y_continuous(labels = scales::comma) 
            }
        }
        

        return(g)

    })
  
  
    output$histDay <- renderPlot({
    
        if(dim(weekday_reactive())[1] != 0){
           g <- ggplot(weekday_reactive(), aes(x= factor(weekday), y=N)) +labs(x="Days of the week", y="Total number of entries", title=paste("", input$community, " community & " ,input$taxiCompany, " service provider" )) + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  + scale_x_discrete(labels = days_in_week, guide=guide_axis( angle = 45)) 
        }
        else{
            g <- ggplot(weekday_reactive(), aes(x= factor(weekday), y=N)) +labs(x="Days of the week", y="Total number of entries", title=paste("", input$community, " community & " ,input$taxiCompany, " service provider" )) + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")  
        }
        
        
        return(g)
    
    
     })




    output$histCommunity <- renderPlot({
            if(outside_chicago()){
                look_up <- community_areas_outside
                taxi <- taxi_outside
            }else{
                look_up <- community_areas
                taxi <- taxi_inside
            }

            if(input$radioMode=="To"){
                communityDF <- taxi[Dropoff == community()]
                total_rides <- count(communityDF)
                communityDF <- communityDF[, .N, by=Pickup]    
                communityDF <- communityDF[, Percentage:=((N/as.integer(total_rides) )*100)]
                communityDF <- communityDF %>% mutate(Pickup = look_up[Pickup] )
                g<- ggplot(communityDF, aes(x= factor(Pickup), y=Percentage)) +labs(x="Community", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")   + coord_flip()
            }else{
                communityDF <- taxi[Pickup == community()]
                total_rides <- count(communityDF)
                communityDF <- communityDF[, .N, by=Dropoff]
                communityDF <- communityDF[, Percentage:=((N/as.integer(total_rides))*100)]
                communityDF <- communityDF %>% mutate(Dropoff = look_up[Dropoff] )
                g<- ggplot(communityDF, aes(x= factor(Dropoff), y=Percentage)) +labs(x="Community", y="Total number of entries") + geom_bar(stat="identity", position="dodge", fill="deepskyblue4")   + coord_flip()
            }
           
                 
            if(dim(communityDF)[1]>0){
                g <- g +scale_x_discrete(guide=guide_axis(angle =0))
            }
            
        
            
        
        # f <- factor(weekdays(taxi$Date), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
        
        
        return(g)
    })
  
  
  
  
    output$histMonthly <- renderPlot({
        
        if( dim(month_reactive())[1] != 0 ){
            g <- ggplot(month_reactive(), aes(x= factor(newMonth), y= N)) +labs(x="Month ", y="Total number of entries", title=paste("", input$community, " community & " ,input$taxiCompany, " service provider" )) + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") +   scale_x_discrete(labels = months, guide=guide_axis( angle = 45))
        }else{
            g <- ggplot(month_reactive(), aes(x= factor(newMonth), y= N)) +labs(x="Month ", y="Total number of entries", title=paste("", input$community, " community & " ,input$taxiCompany, " service provider" )) + geom_bar(stat="identity", position="dodge", fill="deepskyblue4") 
        }
        
        
        return(g)
    })
  
  
  # *1.609
    output$histBinMile <- renderPlot({
        
        ggplot(bin_reactive_distance(), aes(x = distance_bin, y=N)) + geom_bar(stat="identity", fill="deepskyblue4") + labs(x= "Distance", y="Total number of entries", title = paste("Binned by distance for", input$community, " community & " ,input$taxiCompany, " service provider" )) + scale_y_continuous(labels = scales::comma)
        
    })


    output$histTripTime <- renderPlot({
        
        binned_time_local <- bin_reactive_time()
        if(dim(binned_time_local)[1] == 0){
            ggplot(binned_time_local, aes(x = time_bin, y=N)) + geom_bar(stat="identity", fill="deepskyblue4") + labs(x= "Time", y="Total number of entries", title = paste("Binned by time for", input$community, " community & " ,input$taxiCompany, " service provider" )) + scale_y_continuous(labels = scales::comma) 
        }else{
            ggplot(binned_time_local, aes(x = time_bin, y=N)) + geom_bar(stat="identity", fill="deepskyblue4") + labs(x= "Time", y="Total number of entries", title = paste("Binned by time for", input$community, " community & " ,input$taxiCompany, " service provider" )) + scale_y_continuous(labels = scales::comma) + scale_x_discrete(guide=guide_axis(angle =45))
        }
      })
  

  ##################### END Histograms ####################











   # Commenting out all the tables
  
    output$dailyTable <- DT::renderDataTable({
        DT <-as.data.frame(daily_reactive())
        datatable(DT, 
                  options = list(
                    searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),

                    columnDefs = list(list(width = '200px', targets = "_all"))
                  )) %>% 
        formatCurrency(2, currency = "", interval = 3, mark = ",")
      })
  
    output$hourlyTable <- renderDataTable({
        hourly_table <- hourly_reactive()
        if(time_format()=="12HR"){
            hourly_table <- hourly_table %>% mutate(Hour = time_in_12[Hour+1] )    
        }else{
            hourly_table <- hourly_table %>% mutate(Hour = time_in_24[Hour+1] )    
        }
        
        datatable(hourly_table, 
                  colnames=c("Time", "Total Entries"),
                  options = list(
                    searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15),
                    columnDefs = list(list(width = '200px', targets = "_all"))
                  ))  %>% 
        formatCurrency(2, currency = "", interval = 3, mark = ",")
    })
  
  
    output$dayTable <- renderDataTable({
        weekday_table <- weekday_reactive()
        weekday_table <- weekday_table[order(weekday_table$weekday),]
        weekday_table <- weekday_table %>% mutate(weekday = days_in_week[weekday] )    
        datatable(weekday_table, 
                  options = list(
                    searching = FALSE,pageLength = 7, lengthMenu = c(5, 10, 15),
                    # order = list(list(2, 'asc')),
                    columnDefs = list(list(width = '200px', targets = "_all"))
                  )) %>% 
        formatCurrency(2, currency = "", interval = 3, mark = ",")
    })

  
    output$monthlyTable <- renderDataTable({
        monthly_table <- month_reactive()
        monthly_table <- monthly_table[order(monthly_table$newMonth),]
        monthly_table <- monthly_table %>% mutate(newMonth = factor(months[newMonth]) )
        datatable(monthly_table, 
                  colnames=c("Month", "Total Entries"),
                  options = list(
                    searching = FALSE,pageLength = 12,
                    # order = list(list(2, 'asc')),
                    columnDefs = list(list(width = '200px', targets = "_all"))
                  )) %>% 
        formatCurrency(2, currency = "", interval = 3, mark = ",")
    })

    output$binTable <- renderDataTable({
         datatable(bin_reactive_distance(), 
                   options = list(
                     searching = FALSE,pageLength = 12,
                     order = list(list(1, 'asc')),
                     columnDefs = list(list(width = '200px', targets = "_all"))
                   )) %>% 
         formatCurrency(2, currency = "", interval = 3, mark = ",")
     })

    output$tripTable <- renderDataTable({
        
        datatable(bin_reactive_time(), 
                  options = list(
                    searching = FALSE,pageLength = 12,
                    order = list(list(1, 'asc')),
                    columnDefs = list(list(width = '200px', targets = "_all"))
                  )) %>% 
        formatCurrency(2, currency = "", interval = 3, mark = ",")
    })
  
  
  
  
  ##############last bracket
  
}

# Run the application 
shinyApp(ui = ui, server = server)
