#setwd("C:/Users/Krishnan CS/424_Project2")
#print(getwd())

# LIBRARIES=======================================================================================================
library(lubridate)
library(DT)
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


options(scipen=999)


# READ DATA AND CONVERT TO USABLE FORMAT=======================================================================================================

#Reading from the split csv files
taxi <- do.call(rbind, lapply(list.files(pattern = "*.csv"), fread)) 
taxi <- taxi[,-1]

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





#Community areas list 
community_areas <- c("Rogers Park", "West Ridge","Uptown","Lincoln Square","North Center","Lake View","Lincoln Park", "Near North Side", "Edison Park",
"Norwood Park","Jefferson Park","Forest Glen","North Park","Albany Park","Portage Park","Irving Park","Dunning","Montclare","Belmont Cragin",
"Hermosa","Avondale","Logan Square","Humboldt Park","West Town","Austin","West Garfield Park","East Garfield Park","Near West Side",
"North Lawndale","South Lawndale","Lower West Side","The Loop","Near South Side","Armour Square","Douglas","Oakland","Fuller Park",
"Grand Boulevard","Kenwood","Washington Park","Hyde Park","Woodlawn","South Shore","Chatham","Avalon Park","South Chicago","Burnside",
"Calumet Heights","Roseland","Pullman","South Deering","East Side","West Pullman","Riverdale","Hegewisch","Garfield Ridge","Archer Heights",
"Brighton Park","McKinley Park","Bridgeport","New City","West Elsdon","Gage Park","Clearing","West Lawn","Chicago Lawn","West Englewood",
"Englewood","Greater Grand Crossing","Ashburn","Auburn Gresham","Beverly","Washington Heights","Mount Greenwood","Morgan Park","O'Hare","Edgewater")


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
