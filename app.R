library(shiny)
library(mlbench)
library(plotly)
library(shinythemes)
library(dplyr)
library(png)
library(jpeg)
library(ggplot2)
library(shinyjs)
library(leaflet)
library(sp)
library(rgdal)
library(maptools)
library(tigris)
library(acs)
library(stringr) # to pad fips codes

# Datasets - ASSETS 
#Load Datasets (Assets)
Dat.AssetCount <- read.csv("https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/Grouped_Count.csv")
Dat.AssetPercent <- read.csv("https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/Grouped_Percent.csv")
Dat.NonRes <- read.csv("https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/NonResAssets.csv")

#Colors for Dat.NonRes
Dat.NonRes$Color <- ifelse(Dat.NonRes$Status == "Vacant", "red", "black")
Dat.NonRes$Color <- ifelse(Dat.NonRes$Status == "Occupied", "blue", Dat.NonRes$Color)
Dat.NonRes$Color <- ifelse(Dat.NonRes$Status == "No Information", "gray", Dat.NonRes$Color)

#Round up Percents
Dat.AssetPercent[,c(2:28,31:34)] <- round(100*Dat.AssetPercent[,c(2:28,31:34)], 2)
Dat.AssetCount[,c(2:28,31:34)] <- round(100*Dat.AssetCount[,c(2:28,31:34)], 2)

#as numeric
Dat.AssetCount$Row.Labels <- as.numeric(as.character(Dat.AssetCount$Row.Labels))
Dat.AssetPercent$Row.Labels <- as.numeric(as.character(Dat.AssetPercent$Row.Labels))

#Cut off Totals
Dat.AssetCount <- Dat.AssetCount[c(1:55),]
Dat.AssetPercent <- Dat.AssetPercent[c(1:55),]

# DATAFRAME - CENSUS INFORMATION
#Download ACS 2014 Data
ACS14 <- read.csv("https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Raw/ACS14.csv")
ACS14$CensusTract3 <- as.character(ACS14$CensusTract3)


#clean up colnames
colnames(ACS14) <- c("CensusTract", "CensusTract2", "CensusTract3", "Population (16 Plus)", "MOE_Population_16Plus",
                     "Percent of Population in the Labor Force", "MOE_Population_LaborForce", "Percent of Population Employed", 
                     "MOE_Population_Employed", "Unemployment Rate", "MOE_UnemploymentRate", "Total Number of Households", 
                     "Median Income (in dollars)", "MOE_MedianIncome_dollars", "Mean Income (in dollars)", "MOE_MeanIncome_dollars",
                     "# of Owner Occupied Households", "# of Owner Occupants with NO Vehicle", "% of Owner Occupants with NO Vehicle", 
                     "# Rental Occupied Households", "RONoVehicle", "PercentRONoVehicle", "Total # of Households", "Percent of Households with No Vehicle") 


#ACS INFO Merged
Dat.AssetCount <- merge(Dat.AssetCount, ACS14, by.x = "Row.Labels",by.y = "CensusTract")
Dat.AssetCount <- Dat.AssetCount[, c(1:41, 57:64)]

Dat.AssetPercent <- merge(Dat.AssetPercent, ACS14, by.x = "Row.Labels",by.y = "CensusTract")
Dat.AssetPercent <- Dat.AssetPercent[, c(1:41, 57:64)]

#clean up colnames
colnames(Dat.AssetPercent) <- c("Row.Labels", "Nontraditional Housing", "Alcohol Commercial", "Auto Commercial", "Banks and Lending", 
                                "Care Commercial", "Community Safety", "Convenience Commercial", "Education","Entertainment",
                                "Food Commercial","Gasoline Commercial","Health and Wellness", "Hotel and Motel", "Industrial",
                                "Infrastructure", "Legal", "Manufacturing", "Mixed Use", "Office Space", 
                                "Public Space and Services", "Religious", "Residential", "Retail Commercial", "Shopping Center",
                                "Storage Commercial", "Vacant Building", "Grand Total", "Population (16 Plus)", "MOE_Population_16Plus",
                                "Percent of Population in the Labor Force", "MOE_Population_LaborForce", "Percent of Population Employed", "MOE_Population_Employed", "Unemployment Rate", 
                                "MOE_UnemploymentRate", "Total Number of Households", "Median Income (in dollars)", "MOE_MedianIncome_dollars", "Mean Income (in dollars)",
                                "MOE_MeanIncome_dollars", "# of Owner Occupied Households", "# of Owner Occupants with NO Vehicle", "% of Owner Occupants with NO Vehicle", 
                                "# Rental Occupied Households", "RONoVehicle", "PercentRONoVehicle", "Total # of Households", "Percent of Households with No Vehicle") 

colnames(Dat.AssetCount) <- c("Row.Labels", "Nontraditional Housing", "Alcohol Commercial", "Auto Commercial", "Banks and Lending", 
                              "Care Commercial", "Community Safety", "Convenience Commercial", "Education","Entertainment",
                              "Food Commercial","Gasoline Commercial","Health and Wellness", "Hotel and Motel", "Industrial",
                              "Infrastructure", "Legal", "Manufacturing", "Mixed Use", "Office Space", 
                              "Public Space and Services", "Religious", "Residential", "Retail Commercial", "Shopping Center",
                              "Storage Commercial", "Vacant Building", "Grand Total", "Population (16 Plus)", "MOE_Population_16Plus",
                              "Percent of Population in the Labor Force", "MOE_Population_LaborForce", "Percent of Population Employed", "MOE_Population_Employed", "Unemployment Rate", 
                              "MOE_UnemploymentRate", "Total Number of Households", "Median Income (in dollars)", "MOE_MedianIncome_dollars", "Mean Income (in dollars)",
                              "MOE_MeanIncome_dollars", "# of Owner Occupied Households", "# of Owner Occupants with NO Vehicle", "% of Owner Occupants with NO Vehicle", 
                              "# Rental Occupied Households", "RONoVehicle", "PercentRONoVehicle", "Total # of Households", "Percent of Households with No Vehicle")



#Store features and actual class in seprate variables
featureList2 <- colnames(Dat.AssetPercent)[c(2:27)]
featureList3 <- colnames(Dat.AssetPercent)[c(29,31,33,35,37,38,40,42,45,49)]

CensusTractC <- Dat.AssetCount$Row.Labels
CensusTractP <- Dat.AssetPercent$Row.Labels

# SHAPEFILE - CENSUS INFORMATION
#Download Onondaga County Tracts, Onondaga County = 67
shape.Tracts <- tracts(state = 36, county = 67, cb = TRUE)

#Create a geographic set to grab tabular data (acs)
shape.Syracuse <- shape.Tracts[shape.Tracts$NAME == 1 | shape.Tracts$NAME == 2 |
                                 shape.Tracts$NAME == 3 | shape.Tracts$NAME == 4 |
                                 shape.Tracts$NAME == 5.01 | shape.Tracts$NAME == 6 |
                                 shape.Tracts$NAME == 7 | shape.Tracts$NAME == 8 |
                                 shape.Tracts$NAME == 9 | shape.Tracts$NAME == 10 |
                                 shape.Tracts$NAME == 14 | shape.Tracts$NAME == 15 |
                                 shape.Tracts$NAME == 16 | shape.Tracts$NAME == 17.01 |
                                 shape.Tracts$NAME == 17.02 | shape.Tracts$NAME == 18 |
                                 shape.Tracts$NAME == 19 | shape.Tracts$NAME == 20 |
                                 shape.Tracts$NAME == 21.01 | shape.Tracts$NAME == 23 |
                                 shape.Tracts$NAME ==  24 | shape.Tracts$NAME == 27 |
                                 shape.Tracts$NAME == 29.01 | shape.Tracts$NAME == 30 |
                                 shape.Tracts$NAME == 32 | shape.Tracts$NAME == 34 |
                                 shape.Tracts$NAME == 35 | shape.Tracts$NAME == 36.01 |
                                 shape.Tracts$NAME == 36.02 | shape.Tracts$NAME == 38 |
                                 shape.Tracts$NAME == 39 | shape.Tracts$NAME == 40 |
                                 shape.Tracts$NAME == 42 | shape.Tracts$NAME == 43.01 |
                                 shape.Tracts$NAME == 43.02 | shape.Tracts$NAME == 44 |
                                 shape.Tracts$NAME == 45 | shape.Tracts$NAME == 46 |
                                 shape.Tracts$NAME == 48 | shape.Tracts$NAME == 49 |
                                 shape.Tracts$NAME ==  50 | shape.Tracts$NAME == 51 |
                                 shape.Tracts$NAME == 52 | shape.Tracts$NAME == 53 |
                                 shape.Tracts$NAME == 54 | shape.Tracts$NAME == 55 |
                                 shape.Tracts$NAME == 56.01 | shape.Tracts$NAME == 56.02 |
                                 shape.Tracts$NAME == 57 | shape.Tracts$NAME == 58 | 
                                 shape.Tracts$NAME == 59 | shape.Tracts$NAME == 60 | 
                                 shape.Tracts$NAME == 61.01 | shape.Tracts$NAME == 61.02 |
                                 shape.Tracts$NAME == 61.03,]

#Merge shapefile with ACS 2014 data
shape.Syracuse <- merge(shape.Syracuse, ACS14, by.x = "NAME",by.y = "CensusTract3")

# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("united"),
  mainPanel(
    tabsetPanel(
      tabPanel(h4("Methodology"), 
               tabsetPanel(
                 tabPanel("Framework",
                          h4("Query Purpose"),
                          tags$ol(
                            tags$li("What is the problem/question you are trying to solve?"),
                            tags$li("Where did it come from? Who or what initiated the query?"),
                            tags$li("Where does the problem exist - public/private domain. Who is currently responsible for an intervention and what might that look like?"),
                            tags$li("How is this analysis helpful?"),
                            tags$li("What data sets do you have access to relevant to the problem?"),
                            tags$li("What fields are in each of the data sources?"),
                            tags$li("How many people/addresses/faciliites/entities/jursidictions does the data contain?"),
                            tags$li("For this problem, what % of entities are at risk or have resources to be intervened?")),
                          h4("Data Governance & Maturity"),
                          tags$ol(
                            tags$li("What analysis has been done thus far and what conclusions has it drawn? Give credit where credit is due."),
                            tags$li("For the data sets that you have access to - who is responsible for the data (which organization and person)?"),
                            tags$li("Is the data accessible outside the department? Is there a VPN?"),
                            tags$li("What security policies and considerations need to be in place for each of the data sources? (HIPPA, FERPA, etc)"),
                            tags$li("How is the data stored?"),
                            tags$li("How accessible is the data that's required"),
                            tags$li("Do you have data that is both relevant and sufficient to solve the problem?"),
                            tags$li("How is the data quality?"),
                            tags$li("How often is the data collected?"),
                            tags$li("What is the level of granularity for the data sources?"),
                            tags$li("How much history is stored? How are updates handled?"),
                            tags$li("How integrated are the different data sources?"),
                            tags$li("What data privacy policies are in place?"),
                            tags$li("How well documented are the data?")),
                          h4("Next Steps"),
                          tags$ol(
                            tags$li("What (if any) questions did this analysis answer or bring insight to?"),
                            tags$li("What (if any) deeper questions did this analysis come to demand? Did this analysis require a step back or out?"),
                            tags$li("This should link back up to the 'problem definition' and 'leads' for the next step or tab.")),
                          h5("This Frameworkf was taken from the University of Chicago's Center for Data Science & Public Policy's Data Maturity Framework Questionnaire")))),
      tabPanel(h4("Two Red Bananas"),
               tabsetPanel(
                 tabPanel("Tab Overview",
                          h4("Problem Definition"),
                          tags$ol(
                            tags$li(c("Problem Definition: Current analysis has effectively shown the concentration of poverty 
                                    and lack of resources throughout the city of Syracuse. When poverty, unemployment, lack 
                                    of access to a car, vacancy, crime, etc. is visualized across the face of Syracuse, a 
                                    common trend appears: two large, curvature areas arc across the North and Southwest. In 
                                    meetings, these areas are often described as", tags$div(HTML(paste(tags$span(style="color:red", 
                                    "two red bananas")))), "(hence the tab name). Although accurate in relaying where social 
                                    issues exist, the proportion of public resources to the areas in red seems greatly unbalanced. Is there 
                                    a way to use data to further differentiate and delineate the needs within these neighborhoods?")),
                            tags$li("Leads: A preliminary review of the analysis done was humbling. Analysts from the Syracuse University's 
                                    Community Geography Department, CNY Fair Housing, the City of Syracuse's Dept. of Neighborhood 
                                    and Business Development, Home HeadQuarters and numerous other sources (a small portion summarised below) captures
                                    the long and dedicated conversation taken place thus far. The above question came out of 
                                    numerous meetings with J. Robinson and S. Edelstein about how to further and not simply duplicate this conversation."),
                            tags$li("This tab's analysis hopes to (2) further differntiate and define physical assets and need in specific geographical
                                    areas and (2) offer an overview of access points for interventions.")),
                          h4("Data Governance & Maturity"),
                          tags$ol("Physical Assets: TheSPAD data is stored in excel form at the SOCPA office. SPAD data is 
                                  recorded by Syracuse Police Department data, permitting data, Syracuse.com data and drive around 
                                  data. SPAD data is on the asset level. Ex) DestiNY may exist within one parcel but has hundreds of 
                                  assets within it. SPAD data captures every asset on a parcel. SPAD data is point in time data. Date 
                                  of when the asset was last updated or addes is recorded within the excel file."),
                          tags$ol("City's Access Points: "),
                          fixedRow(
                            column(12, selectInput(inputId = "redBananasSelect", 
                                                   label = h4("Snapshots of Previous Research"), 
                                                   choices = c("Opportunity Indices Part 1", "Opportunity Indices Part 2", "Banks and Lending", "Redlining"), 
                                                   selected = "Redlining"))),
                          
                          #Vertical space
                          fixedRow(column(12, imageOutput("redBananasPic")))),
                 
                 tabPanel("Physical Assets", 
                          h4("CLICK & DRAG over points of interest for further information!"),
                          tags$ol(
                            tags$li("Neighborhood Asset: Data taken from the NYS Property Type Classification Codes."),
                            tags$li("Census Information: Data taken from the ACS 2014 5 Year Estimate.")),
                          
                          # Vertical space
                          tags$hr(),
                          
                          # Feature selection
                          fixedRow(
                            column(6, selectInput(inputId = "Input1", 
                                                  label = "Neighborhood Asset", 
                                                  choices = featureList2, 
                                                  selected = "Banks and Lending")),
                            column(6, selectInput(inputId = "Input2", 
                                                  label = "Census Information", 
                                                  choices = featureList3, 
                                                  selected = "Median Income (in dollars)"))),
                          #column(4, radioButtons("picture", "Syracuse Census Tracts:", c("Median H.H. Income", "Unemployment", "Reference Sheet")))),
                          
                          # First row
                          fixedRow(
                            column(6, plotlyOutput("Plot1", height = "400px"), 
                                   verbatimTextOutput("click1"), 
                                   plotlyOutput("Plot2", height="400px")),
                            column(6, leafletOutput("AssetMap1", height = "800px")))),
                 tabPanel("City's Access Points",
                          h4("Problem Definition: What neighborhoods are accessible for public, place-based intervention?"),
                          h4("Property Data"),
                          fixedRow(column(4, selectInput(inputId = "Accessible", label = "Accessible Properties", choices = c("SIDA", "OSIDA", "Land Bank", "City-owned", "Seizable"))),
                                   column(4, selectInput(inputId = "Inaccessible", label = "Inaccessible Properties", choices = c("Zombie", "Vacant & Tax Current"))),
                                   column(4, selectInput(inputId = "Problem", label = "Problem Properties", c("Dilapidated", "Lead", "Underused Corner Properties")))),
                          h4("Dis/Investment Data"),
                          fixedRow(
                            column(4, selectInput(inputId = "Census", label = "Census Information", choices = c("Unemployment", "Affordability", "Income"))),
                            column(4, selectInput(inputId = "Investment", label = "Neighborhood Investment and Assets", choices = c("Affordable Housing", "Commercial Corridors")))),
                          fixedRow(
                            column(12, leafletOutput("AccessMap1", height = "575px")))))))))

# server.R definition
server <- function(input, output, session){
  
  #########ABOUT##############
  
  #########METHODOLOGY########
  output$myMethod1 <- renderImage({list(
    src = "Understanding-Syracuse/Images/Method1.png",
    filetype = "image/png",
    alt = "Drats! Something went wrong D:"
  )})
  
  # Create a space for maps
  output$myMethod2 <- renderImage({list(
    src = "Understanding-Syracuse/Images/Method2.png",
    filetype = "image/png",
    alt = "Drats! Something went wrong D:"
  )})
  
  output$redBananasPic <- renderImage({
    if (input$redBananasSelect == "Opportunity Indices Part 1") {
      return(list(
        src = "Understanding-Syracuse/Images/Opportunity Indices_Page1.png",
        contentType = "image/png",
        alt = "Drats! Something went wrong D:"
      ))
    } else if (input$redBananasSelect == "Opportunity Indices Part 2") {
      return(list(
        src = "Understanding-Syracuse/Images/Opportunity Indices_Page2.png",
        filetype = "image/png",
        alt = "Drats! Something went wrong D:"
      ))
    }
    else if (input$redBananasSelect == "Banks and Lending") {
      return(list(
        src = "Understanding-Syracuse/Images/Banks and Lending.png",
        filetype = "image/png",
        alt = "Drats! Something went wrong D:"
      ))
    }
    else if (input$redBananasSelect == "Redlining") {
      return(list(
        src = "Understanding-Syracuse/Images/Redlining.png",
        filetype = "image/png",
        alt = "Drats! Something went wrong D:"
      ))
    }
    
  }, deleteFile = FALSE)
  
  
  #########PHYSICAL ASSETS####
  # Observes the second feature input for a change
  observeEvent(c(input$Input2, input$Input1),{
    # Create a convenience data.frame which can be used for charting
    plot1.df <- data.frame(Dat.AssetPercent[,input$Input2],
                           Dat.AssetPercent[,input$Input1],
                           CensusTract = Dat.AssetPercent$Row.Labels,
                           Income = Dat.AssetPercent$`Median Income (in dollars)`)
    
    plot2.df <- data.frame(Dat.AssetCount[,input$Input2],
                           Dat.AssetCount[,input$Input1],
                           CensusTract = Dat.AssetCount$Row.Labels,
                           Income = Dat.AssetCount$`Median Income (in dollars)`)
    # Add column names
    colnames(plot1.df) <- c("x", "y", "CensusTract", "MedianIncome")
    colnames(plot2.df) <- c("x", "y", "CensusTract", "MedianIncome")
    
    #fitted lines
    fit1 <- lm(y ~ x, data = plot1.df)
    fit2 <- lm(y ~ x, data = plot2.df )
    
    # Do a plotly contour plot to visualize the two featres with
    # the number of malignant cases as size
    # Note the use of 'source' argument
    output$Plot1 <- renderPlotly({
      plot_ly(plot1.df, x = x, y = y, 
              key = CensusTract, 
              hoverinfo = "text", 
              text = paste("X Axis:", x,",", "Y Axis:", y,",", "Census Tract:", CensusTract), 
              color = MedianIncome, 
              colors = "RdYlBu",
              mode = "markers", 
              source = "subset",
              marker = list(size = 12, outliercolor = "black")) %>%
        add_trace(data = plot1.df, x = x, y = fitted(fit1), mode = "lines")
      layout(title = paste("% of", input$Input1, "vs ", input$Input2),
             xaxis = list(title = input$Input2),
             yaxis = list(title = paste("% of Assets: ", input$Input1)),
             dragmode =  "select",
             showlegend = FALSE)
    })
    
    
    output$Plot2 <- renderPlotly({
      plot_ly(plot2.df, x = x, y = y, 
              key = CensusTract, 
              hoverinfo = "text", 
              text = paste("X Axis:", x,",", "Y Axis:", y,",", "Census Tract:", CensusTract), 
              color = MedianIncome, 
              colors = "RdYlBu", 
              mode = "markers", 
              source = "subset",
              marker = list(size = 12)) %>%
        add_trace(data = plot2.df, x = x, y = fitted(fit2), mode = "lines")
      layout(title = paste("# of", input$Input1, "vs ", input$Input2),
             xaxis = list(title = input$Input2),
             yaxis = list(title = paste("# of Assets: ", input$Input1)),
             dragmode =  "select",
             showlegend = FALSE)
    })
    
    output$click1 <- renderPrint({
      dat.hover <- event_data("plotly_selected", source = "subset")
      if (is.null(dat.hover)) "Click and drag over points of interest" 
      else 
        names(dat.hover) <- c("1", "2", "X Axis", "Y Axis", "Census Tract")
      dat.hover[c(3,4,5)]
    })
    
    output$AssetMap1 <- renderLeaflet({
      
      NonResSubset <- Dat.NonRes[Dat.NonRes$Entity_Category == input$Input1,]
      pal <- colorQuantile("RdYlBu", shape.Syracuse$`Percent of Households with No Vehicle` , n = 5)
      
      leaflet(shape.Syracuse) %>%
        setView(lng= -76.1474, lat=43.0481, zoom = 12) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                    color = ~colorQuantile("YlOrRd", shape.Syracuse$`Percent of Households with No Vehicle`)(shape.Syracuse$`Percent of Households with No Vehicle`)) %>%
        addCircleMarkers(lng = NonResSubset$Lon, lat = NonResSubset$Lat, popup = NonResSubset$Entity2, radius = 4, color = NonResSubset$Color) %>%
        addLegend("bottomright", colors= c("blue", "red", "gray"), labels=c("Occupied", "Vacant", "No Information"), title="Property Status") %>%
        addLegend("bottomleft", pal = pal, values = shape.Syracuse$`Percent of Households with No Vehicle`, title="Percent No Vehicle")
      })
      
    #########CITY ACCESSIBILITY####
    output$AccessMap1 <- renderLeaflet({
      leaflet(shape.Syracuse) %>%
        setView(lng= -76.1474, lat=43.0481, zoom = 12) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                    color = ~colorQuantile("YlOrRd", shape.Syracuse$`Percent of Households with No Vehicle`)(shape.Syracuse$`Percent of Households with No Vehicle`))
    })
  })
  
}

shinyApp(ui = ui, server = server)
