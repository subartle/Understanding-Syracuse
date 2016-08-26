library(shiny)
library(mlbench)
library(plotly)
library(shinythemes)
library(dplyr)
library(png)
library(jpeg)
library(ggplot2)

# Load data
Dat.AssetCount <- read.csv("https://raw.githubusercontent.com/subartle/hello-world/master/AssetCatCount.csv")
Dat.AssetPercent <- read.csv("https://raw.githubusercontent.com/subartle/hello-world/master/AssetCatPercent.csv")

#clean up colnames
colnames(Dat.AssetPercent) <- c("Row.Labels", "Affordable Housing", "Alcohol Commercial", "Auto Commercial", "Banks and Lending", 
                                "Care Commercial", "Community Safety", "Convenience Commercial", "Education","Entertainment",
                                "Food Commercial","Gasoline Commercial","Health and Wellness", "Hotel and Motel", "Industrial",
                                "Infrastructure", "Legal", "Manufacturing", "Mixed Use", "Office Space", 
                                "Public Space and Services", "Religious", "Residential", "Retail Commercial", "Shopping Center",
                                "Storage Commercial", "Vacant Building", "Grand Total", "Population (16 Plus)", "MOE_Population_16Plus",
                                "Percent of Population in the Labor Force", "MOE_Population_LaborForce", "Percent of Population Employed", "MOE_Population_Employed", "Unemployment Rate", 
                                "MOE_UnemploymentRate", "Total Number of Households", "Median Income (in dollars)", "MOE_MedianIncome_dollars", "Mean Income (in dollars)",
                                "MOE_MeanIncome_dollars") 

colnames(Dat.AssetCount) <- c("Row.Labels", "Affordable Housing", "Alcohol Commercial", "Auto Commercial", "Banks and Lending", 
                              "Care Commercial", "Community Safety", "Convenience Commercial", "Education","Entertainment",
                              "Food Commercial","Gasoline Commercial","Health and Wellness", "Hotel and Motel", "Industrial",
                              "Infrastructure", "Legal", "Manufacturing", "Mixed Use", "Office Space", 
                              "Public Space and Services", "Religious", "Residential", "Retail Commercial", "Shopping Center",
                              "Storage Commercial", "Vacant Building", "Grand Total", "Population (16 Plus)", "MOE_Population_16Plus",
                              "Percent of Population in the Labor Force", "MOE_Population_LaborForce", "Percent of Population Employed", "MOE_Population_Employed", "Unemployment Rate", 
                              "MOE_UnemploymentRate", "Total Number of Households", "Median Income (in dollars)", "MOE_MedianIncome_dollars", "Mean Income (in dollars)",
                              "MOE_MeanIncome_dollars") 

#Round up Percents
Dat.AssetPercent[,2] <- round(100*Dat.AssetPercent[,2], 2)
Dat.AssetPercent[,3] <- round(100*Dat.AssetPercent[,3], 2)
Dat.AssetPercent[,4] <- round(100*Dat.AssetPercent[,4], 2)
Dat.AssetPercent[,5] <- round(100*Dat.AssetPercent[,5], 2)
Dat.AssetPercent[,6] <- round(100*Dat.AssetPercent[,6], 2)
Dat.AssetPercent[,7] <- round(100*Dat.AssetPercent[,7], 2)
Dat.AssetPercent[,8] <- round(100*Dat.AssetPercent[,8], 2)
Dat.AssetPercent[,9] <- round(100*Dat.AssetPercent[,9], 2) 
Dat.AssetPercent[,10] <- round(100*Dat.AssetPercent[,10], 2)
Dat.AssetPercent[,11] <- round(100*Dat.AssetPercent[,11], 2)
Dat.AssetPercent[,12] <- round(100*Dat.AssetPercent[,12], 2)
Dat.AssetPercent[,13] <- round(100*Dat.AssetPercent[,13], 2)
Dat.AssetPercent[,14] <- round(100*Dat.AssetPercent[,14], 2)
Dat.AssetPercent[,15] <- round(100*Dat.AssetPercent[,15], 2)
Dat.AssetPercent[,16] <- round(100*Dat.AssetPercent[,16], 2)
Dat.AssetPercent[,17] <- round(100*Dat.AssetPercent[,17], 2)
Dat.AssetPercent[,18] <- round(100*Dat.AssetPercent[,18], 2)
Dat.AssetPercent[,19] <- round(100*Dat.AssetPercent[,19], 2)
Dat.AssetPercent[,20] <- round(100*Dat.AssetPercent[,20], 2)
Dat.AssetPercent[,21] <- round(100*Dat.AssetPercent[,21], 2)
Dat.AssetPercent[,22] <- round(100*Dat.AssetPercent[,22], 2)
Dat.AssetPercent[,23] <- round(100*Dat.AssetPercent[,23], 2)
Dat.AssetPercent[,24] <- round(100*Dat.AssetPercent[,24], 2)
Dat.AssetPercent[,25] <- round(100*Dat.AssetPercent[,25], 2)
Dat.AssetPercent[,26] <- round(100*Dat.AssetPercent[,26], 2)
Dat.AssetPercent[,27] <- round(100*Dat.AssetPercent[,27], 2)
Dat.AssetPercent[,28] <- round(100*Dat.AssetPercent[,28], 2)
Dat.AssetPercent[,31] <- round(100*Dat.AssetPercent[,31], 2)
Dat.AssetPercent[,32] <- round(100*Dat.AssetPercent[,32], 2)
Dat.AssetPercent[,33] <- round(100*Dat.AssetPercent[,33], 2)
Dat.AssetPercent[,34] <- round(100*Dat.AssetPercent[,34], 2)
Dat.AssetCount[,31] <- round(100*Dat.AssetCount[,31], 2)
Dat.AssetCount[,32] <- round(100*Dat.AssetCount[,32], 2)
Dat.AssetCount[,33] <- round(100*Dat.AssetCount[,33], 2)
Dat.AssetCount[,34] <- round(100*Dat.AssetCount[,34], 2)

#Cut of Totals
Dat.AssetCount <- Dat.AssetCount[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55),]
Dat.AssetPercent <- Dat.AssetPercent[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55),]

#Store features and actual class in seprate variables
featureList2 <- colnames(Dat.AssetPercent)[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)]
featureList3 <- colnames(Dat.AssetPercent)[c(29,31,33,35,37,38,40)]

CensusTractC <- Dat.AssetCount$Row.Labels
CensusTractP <- Dat.AssetPercent$Row.Labels

#Setting Color Scheme for Median Income
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 14970, "red", "black")
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 20424 & Dat.AssetCount$`Median Income (in dollars)` > 14969, "sienna3", Dat.AssetCount$Group1)
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 22709 & Dat.AssetCount$`Median Income (in dollars)` > 20423, "sienna1", Dat.AssetCount$Group1)
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 27227 & Dat.AssetCount$`Median Income (in dollars)` > 22708, "lightgoldenrod2", Dat.AssetCount$Group1)
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 33721 & Dat.AssetCount$`Median Income (in dollars)` > 27226, "darkolivegreen3", Dat.AssetCount$Group1)
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 41677 & Dat.AssetCount$`Median Income (in dollars)` > 33720, "lightcyan3", Dat.AssetCount$Group1)
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` < 50324 & Dat.AssetCount$`Median Income (in dollars)` > 41676, "steelblue", Dat.AssetCount$Group1)
Dat.AssetCount$Group1 <- ifelse(Dat.AssetCount$`Median Income (in dollars)` > 50323, "steelblue4", Dat.AssetCount$Group1)

Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 14970, "red", "black")
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 20424 & Dat.AssetPercent$`Median Income (in dollars)` > 14969, "sienna3", Dat.AssetPercent$Group1)
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 22709 & Dat.AssetPercent$`Median Income (in dollars)` > 20423, "sienna1", Dat.AssetPercent$Group1)
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 27227 & Dat.AssetPercent$`Median Income (in dollars)` > 22708, "lightgoldenrod2", Dat.AssetPercent$Group1)
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 33721 & Dat.AssetPercent$`Median Income (in dollars)` > 27226, "darkolivegreen3", Dat.AssetPercent$Group1)
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 41677 & Dat.AssetPercent$`Median Income (in dollars)` > 33720, "lightcyan3", Dat.AssetPercent$Group1)
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` < 50324 & Dat.AssetPercent$`Median Income (in dollars)` > 41676, "steelblue", Dat.AssetPercent$Group1)
Dat.AssetPercent$Group1 <- ifelse(Dat.AssetPercent$`Median Income (in dollars)` > 50323, "steelblue4", Dat.AssetPercent$Group1)

#Setting Color Scheme for Unemployment
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .29, "red", "black")
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .20 & Dat.AssetCount$`Unemployment Rate` < .30, "sienna3", Dat.AssetCount$Group1)
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .16 & Dat.AssetCount$`Unemployment Rate` < .21, "sienna1", Dat.AssetCount$Group1)
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .13 & Dat.AssetCount$`Unemployment Rate` < .17, "lightgoldenrod2", Dat.AssetCount$Group1)
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .09 & Dat.AssetCount$`Unemployment Rate` < .14, "darkolivegreen3", Dat.AssetCount$Group1)
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .06 & Dat.AssetCount$`Unemployment Rate` < .10, "lightcyan3", Dat.AssetCount$Group1)
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` > .04 & Dat.AssetCount$`Unemployment Rate` < .08, "steelblue", Dat.AssetCount$Group1)
Dat.AssetCount$Group2 <- ifelse(Dat.AssetCount$`Unemployment Rate` < .05, "steelblue4", Dat.AssetCount$Group1)

Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .29, "red", "black")
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .20 & Dat.AssetPercent$`Unemployment Rate` < .30, "sienna3", Dat.AssetCount$Group1)
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .16 & Dat.AssetPercent$`Unemployment Rate` < .21, "sienna1", Dat.AssetCount$Group1)
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .13 & Dat.AssetPercent$`Unemployment Rate` < .17, "lightgoldenrod2", Dat.AssetCount$Group1)
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .09 & Dat.AssetPercent$`Unemployment Rate` < .14, "darkolivegreen3", Dat.AssetCount$Group1)
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .06 & Dat.AssetPercent$`Unemployment Rate` < .10, "lightcyan3", Dat.AssetCount$Group1)
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` > .04 & Dat.AssetPercent$`Unemployment Rate` < .08, "steelblue", Dat.AssetCount$Group1)
Dat.AssetPercent$Group2 <- ifelse(Dat.AssetPercent$`Unemployment Rate` < .05, "steelblue4", Dat.AssetCount$Group1)

# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("united"),
  mainPanel(
    tabsetPanel(
      tabPanel(h4("About"), 
               tabsetPanel(
                 tabPanel("Introduction"),
                 tabPanel("Methodology"), imageOutput("methodology"))
      ),
      tabPanel(h4("Step 1: Two Red Bananas"),
               tabsetPanel(
                 tabPanel("Problem Definition"),
                 tabPanel("Physical Assets", 
                          h4("CLICK & DRAG over points of interest for further information!"),
                          tags$ol(
                            tags$li("Neighborhood Asset: Data taken from the NYS Property Type Classification Codes."),
                            tags$li("Neighborhood Health Indicator: Data taken from the ACS 2014 5 Year Estimate.")),
                          
                          # Vertical space
                          tags$hr(),
                          
                          # Feature selection
                          fixedRow(
                            column(4, selectInput(inputId = "Input1", label = "Neighborhood Asset", choices = featureList2, selected = "Banks and Lending")),
                            column(4, selectInput(inputId = "Input2", label = "Neighborhood Health Indicator", choices = featureList3, selected = "Median Income (in dollars)")),
                            column(4, radioButtons("picture", "Syracuse Census Tracts:", c("Median H.H. Income", "Unemployment", "Reference Sheet")))),
                          
                          # First row
                          fixedRow(
                            column(6, plotlyOutput("Plot1", height = "450px"), verbatimTextOutput("click1"), plotlyOutput("Plot2", height="450px")),
                            column(6, imageOutput("myImage")))))))))

# server.R definition
server <- function(input, output, session){
  
  #########ABOUT##############
  
  #########METHODOLOGY########
  output$methodology <- renderImage({})
    
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
    
    # Create a contour plot of the number of malignant cases
    output$myImage <- renderImage({
      if (input$picture == "Median H.H. Income") {
        return(list(
          src = "Picture1.png",
          contentType = "image/png",
          alt = "Drats! Something went wrong D:"
        ))
      } else if (input$picture == "Unemployment") {
        return(list(
          src = "Picture.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        ))
      }
      else if (input$picture == "Reference Sheet") {
        return(list(
          src = "Local_Assets.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        ))
      }
      
    }, deleteFile = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)