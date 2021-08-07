library(shiny)
library(ggplot2)
library(mapproj)
library(readxl)
load("~/R/labdata20SU21.RData")
nst_est2019_01 <- read_excel("~/R/nst-est2019-01.xlsx")

ui <- fluidPage(

    titlePanel("Car Accident Data by State (continental US)"),

    sidebarLayout(
        
        # let user enter date range and variable
        sidebarPanel(dateRangeInput("daterange", "Date range:", start = "2016-01-01", 
                           end = "2019-12-31", min = "2016-01-01", 
                           max = "2019-12-31"),
                     selectInput("var", "Metric:", c("Deaths per capita", "Deaths per accident", "Accidents per capita", 
                                                            "Average people per accident", "People in accidents per capita")),
                     h5("This app summarizes car accident data by state in any given time frame
                        between 2016 and 2019. Enter a date range and metric, and a heat map of data for each state will be displayed.")
                     ), 
        mainPanel(plotOutput("ggplot"))
    )
)

server <- function(input, output) {
    output$ggplot <- renderPlot({
        
        # convert start and end dates to numbers
        start_year <- as.numeric(format((input$daterange)[1], "%Y"))
        start_month <- as.numeric(format((input$daterange)[1], "%m"))
        start_day <- as.numeric(format((input$daterange)[1], "%d"))
        end_year <- as.numeric(format((input$daterange)[2], "%Y"))
        end_month <- as.numeric(format((input$daterange)[2], "%m"))
        end_day <- as.numeric(format((input$daterange)[2], "%d"))
        start_num = (start_year - 2016) * 10000 + start_month * 100 + start_day
        end_num = (end_year - 2016) * 10000 + end_month * 100 + end_day
        
        #if user inputs "Deaths per capita", calculate deaths
        if (input$var == "Deaths per capita") {
            for (x in 1:56) {
                temp[x, 1] = tolower(((labdata20SU21$STATENAME)[labdata20SU21$STATE == x])[1])
                v <- (labdata20SU21$DEATHS)[labdata20SU21$STATE == x & labdata20SU21$time_num >= start_num & labdata20SU21$time_num <= end_num]
                temp[x, 2] = sum(v)
            }
        }
        
        # if user inputs "Deaths per accident", calculate deaths per accident
        if (input$var == "Deaths per accident") {
            for (x in 1:56) {
                temp[x, 1] = tolower(((labdata20SU21$STATENAME)[labdata20SU21$STATE == x])[1])
                v <- (labdata20SU21$DEATHS)[labdata20SU21$STATE == x & labdata20SU21$time_num >= start_num & labdata20SU21$time_num <= end_num]
                temp[x, 2] = sum(v) / length(v)
            }
        }
        
        # if user inputs "Accidents per capita", calculate accidents
        if (input$var == "Accidents per capita") {
            for (x in 1:56) {
                temp[x, 1] = tolower(((labdata20SU21$STATENAME)[labdata20SU21$STATE == x])[1])
                v <- labdata20SU21[labdata20SU21$STATE == x & labdata20SU21$time_num >= start_num & labdata20SU21$time_num <= end_num,]
                temp[x, 2] = nrow(v)
            }
        }
        
        # if user inputs "Average people per accident", calculate people in each accident
        if (input$var == "Average people per accident") {
            for (x in 1:56) {
                temp[x, 1] = tolower(((labdata20SU21$STATENAME)[labdata20SU21$STATE == x])[1])
                v <- (labdata20SU21$PERSONS)[labdata20SU21$STATE == x & labdata20SU21$time_num >= start_num & labdata20SU21$time_num <= end_num]
                temp[x, 2] = sum(v) / length(v)
            }
        }
        
        # if user inputs "People in accidents per capita", calculate total people in accidents
        if (input$var == "People in accidents per capita") {
            for (x in 1:56) {
                temp[x, 1] = tolower(((labdata20SU21$STATENAME)[labdata20SU21$STATE == x])[1])
                v <- (labdata20SU21$PERSONS)[labdata20SU21$STATE == x & labdata20SU21$time_num >= start_num & labdata20SU21$time_num <= end_num]
                temp[x, 2] = sum(v)
            }
        }
        
        # take out data from DC, Virgin Islands, etc.
        temp <- temp[-c(3,7,11,14,43,52),]
        
        #calculate per capita
        if(input$var == "Deaths per capita" || input$var == "Accidents per capita" || input$var == "People in accidents per capita") {
            temp$Quantity <- temp$Quantity / populations
        }
        
        #plot data
        choro <- merge(states, temp, sort = FALSE, by = "region")
        choro <- choro[order(choro$order), ]
        ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill=Quantity)) + coord_map("albers",  lat0 = 45.5, lat1 = 29.5)
    })
}

populations <- ((nst_est2019_01$...11 + nst_est2019_01$...12) / 2)[c(9:16, 18:59)]
states <- map_data("state")
labdata20SU21$time_num <- (labdata20SU21$YEAR - 2016) * 10000 + labdata20SU21$MONTH * 100 + labdata20SU21$DAY
temp <- data.frame(matrix(ncol = 2, nrow = 56))
colnames(temp) = c("region", "Quantity")
shinyApp(ui = ui, server = server)
