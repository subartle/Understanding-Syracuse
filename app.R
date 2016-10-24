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
library(magrittr)
library(highcharter)
library(geojsonio)
library(gistr)
library(reshape)
library(gmapsdistance)
library(RCurl)
library(dygraphs)
library(DT)
library(d3heatmap)
library(reshape)
library(networkD3)
#devtools::install_github("rodazuero/gmapsdistance")

# Datasets - ASSETS
#Load Datasets (Assets)
Dat.AssetCount <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/Grouped_Count.csv"
  )
Dat.AssetPercent <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/Grouped_Percent.csv"
  )
Dat.NonRes <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/NonResAssets.csv"
  )
Dat.Tract <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Raw/CensusTract.csv"
  )
Dat.Accessibility <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/Accessibility_09-06-16.csv"
  )
Dat.ProblemProps <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/Problems_09-06-16.csv"
  )
Dat.Investment <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Raw/NBD_Investment.csv"
  )
Dat.AssetDensity <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/DensityRate.csv"
  )
Dat.CCAssets <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/CCAssets.csv"
  )
Dat.CountyCentroStops <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Raw/CountyCentro_BusStops.csv"
  )
Dat.CentroStopsCounts <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/CentroStops_Counts_ByCensusTracts.csv"
  )
Dat.Violations <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Raw/ViolationReport.csv"
  )
Dat.CleanedViolations <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/ViolationReportArcs.csv"
  )
#API Key for transportation time and distance
APIkey <- ('AIzaSyCoHparOPrgG4hU6QFUR4yEOkkfj53IcZ0')

#Shapefile load (problematic atm)
##Converting to a jso
#download.file("https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Cleaned/commcorridors.geojson", "commcorridors.geojson")
#commcorridors <- readShapePoly(fn="AssetParcelCounts", proj4string=CRS("+proj=longlat +datum=WGS84"))
#commcorridors <- spTransform(commcorridors, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#geojson_write(commcorridors, geometry="polygon", file="commcorridors.geojson" )

#commcorridors <- geojson_read("commcorridors.geojson", method="local", what="sp" )

#Colors for Dat.NonRes
Dat.NonRes$Color <-
  ifelse(Dat.NonRes$Status == "Vacant", "red", "black")
Dat.NonRes$Color <-
  ifelse(Dat.NonRes$Status == "Occupied", "blue", Dat.NonRes$Color)
Dat.NonRes$Color <-
  ifelse(Dat.NonRes$Status == "No Information", "gray", Dat.NonRes$Color)

#Round up Percents
Dat.AssetPercent[, c(2:28, 31:34)] <-
  round(100 * Dat.AssetPercent[, c(2:28, 31:34)], 2)

#Cut off Totals
Dat.AssetCount <- Dat.AssetCount[c(1:55),]
Dat.AssetPercent <- Dat.AssetPercent[c(1:55),]

#as numeric
Dat.Accessibility$lat <- as.numeric(Dat.Accessibility$lat)
Dat.Accessibility$lon <- as.numeric(Dat.Accessibility$lon)
Dat.AssetCount$Row.Labels <-
  as.numeric(as.character(Dat.AssetCount$Row.Labels))
Dat.AssetPercent$Row.Labels <-
  as.numeric(as.character(Dat.AssetPercent$Row.Labels))

#as character
Dat.Accessibility$Accessibility <-
  as.character(Dat.Accessibility$Accessibility)

# DATAFRAME - CENSUS INFORMATION
#Download ACS 2014 Data
ACS14 <-
  read.csv(
    "https://raw.githubusercontent.com/subartle/Understanding-Syracuse/master/Raw/ACS14.csv"
  )
ACS14$CensusTract3 <- as.character(ACS14$CensusTract3)
ACS14 <-
  merge(ACS14, Dat.Tract, by.x = "CensusTract3", by.y = "NAME")

#clean up colnames
colnames(ACS14) <-
  c(
    "CensusTract",
    "CensusTract2",
    "CensusTract3",
    "Population (16 Plus)",
    "MOE_Population_16Plus",
    "% of Population in the Labor Force",
    "MOE_Population_LaborForce",
    "% of Population Employed",
    "MOE_Population_Employed",
    "Unemployment Rate",
    "MOE_UnemploymentRate",
    "# of Households",
    "Median Income (in dollars)",
    "MOE_MedianIncome_dollars",
    "Mean Income (in dollars)",
    "MOE_MeanIncome_dollars",
    "# of Owner Occupied Households",
    "# of Owner Occupants with NO Vehicle",
    "% of Owner Occupants with NO Vehicle",
    "# Rental Occupied Households",
    "RONoVehicle",
    "%RONoVehicle",
    "Total # of Households",
    "% of Households with No Vehicle",
    "lon",
    "lat"
  )

#ACS INFO Merged
Dat.AssetCount <-
  merge(Dat.AssetCount, ACS14, by.x = "Row.Labels", by.y = "CensusTract2")
Dat.AssetCount <- Dat.AssetCount[, c(1:41, 57:64)]

Dat.AssetPercent <-
  merge(Dat.AssetPercent, ACS14, by.x = "Row.Labels", by.y = "CensusTract")
Dat.AssetPercent <- Dat.AssetPercent[, c(1:41, 57:64)]

#clean up colnames
colnames(Dat.AssetPercent) <-
  c(
    "Row.Labels",
    "Nontraditional Housing",
    "Alcohol Commercial",
    "Auto Commercial",
    "Banks and Lending",
    "Care Commercial",
    "Community Safety",
    "Convenience Commercial",
    "Education",
    "Entertainment",
    "Food Commercial",
    "Gasoline Commercial",
    "Health and Wellness",
    "Hotel and Motel",
    "Industrial",
    "Infrastructure",
    "Legal",
    "Manufacturing",
    "Mixed Use",
    "Office Space",
    "Public Space and Services",
    "Religious",
    "Residential",
    "Retail Commercial",
    "Shopping Center",
    "Storage Commercial",
    "Vacant Building",
    "Grand Total",
    "Population (16 Plus)",
    "MOE_Population_16Plus",
    "% of Population in the Labor Force",
    "MOE_Population_LaborForce",
    "% of Population Employed",
    "MOE_Population_Employed",
    "Unemployment Rate",
    "MOE_UnemploymentRate",
    "# of Households",
    "Median Income (in dollars)",
    "MOE_MedianIncome_dollars",
    "Mean Income (in dollars)",
    "MOE_MeanIncome_dollars",
    "# of Owner Occupied Households",
    "# of Owner Occupants with NO Vehicle",
    "% of Owner Occupants with NO Vehicle",
    "# Rental Occupied Households",
    "RONoVehicle",
    "%RONoVehicle",
    "Total # of Households",
    "% of Households with No Vehicle"
  )

colnames(Dat.AssetCount) <-
  c(
    "Row.Labels",
    "Nontraditional Housing",
    "Alcohol Commercial",
    "Auto Commercial",
    "Banks and Lending",
    "Care Commercial",
    "Community Safety",
    "Convenience Commercial",
    "Education",
    "Entertainment",
    "Food Commercial",
    "Gasoline Commercial",
    "Health and Wellness",
    "Hotel and Motel",
    "Industrial",
    "Infrastructure",
    "Legal",
    "Manufacturing",
    "Mixed Use",
    "Office Space",
    "Public Space and Services",
    "Religious",
    "Residential",
    "Retail Commercial",
    "Shopping Center",
    "Storage Commercial",
    "Vacant Building",
    "Grand Total",
    "Population (16 Plus)",
    "MOE_Population_16Plus",
    "% of Population in the Labor Force",
    "MOE_Population_LaborForce",
    "% of Population Employed",
    "MOE_Population_Employed",
    "Unemployment Rate",
    "MOE_UnemploymentRate",
    "# of Households",
    "Median Income (in dollars)",
    "MOE_MedianIncome_dollars",
    "Mean Income (in dollars)",
    "MOE_MeanIncome_dollars",
    "# of Owner Occupied Households",
    "# of Owner Occupants with NO Vehicle",
    "% of Owner Occupants with NO Vehicle",
    "# Rental Occupied Households",
    "RONoVehicle",
    "%RONoVehicle",
    "Total # of Households",
    "% of Households with No Vehicle"
  )

Dat.CCAssets$Entity_Category <-
  as.character(Dat.CCAssets$Entity_Category)

#Store features and actual class in seprate variables
featureList1 <-
  c(
    "Blank",
    "City-Owned",
    "Seizable",
    "SCSD",
    "Greater Syracuse Land Bank",
    "City Park",
    "SMNC",
    "SURA",
    "Community Center",
    "O/SIDA"
  )
featureList2 <- colnames(Dat.AssetPercent)[c(2:27)]
featureList3 <-
  colnames(Dat.AssetPercent)[c(29, 37, 42, 45, 31, 49, 35, 38, 40)]
featureList4 <- c("Blank", "Suspected Zombie Property")
featureList5 <-
  c(
    "Blank",
    "Acquisition and Rehabilitation",
    "Demolition + New Construction",
    "Demolition Only",
    "Distressed Property Program",
    "Incomplete Info",
    "New Construction",
    "Rehabilitation",
    "Rental Rehabilitation",
    "Reprogrammed 1% (36+38+39)",
    "Special Housing Project",
    "Syracuse Lead Project",
    "Tax Credit",
    "Vacant Property Program"
  )
featureList6 <- c("transit", "bicycling", "walking", "driving")
featureList7 <- as.character(unique(Dat.CCAssets$Corridor))
featureList8 <-
  c(
    "TransitCount",
    "% of Households with No Vehicle",
    "Population (16 Plus)",
    "Median Income (in dollars)"
  )
featureList9 <-
  paste(unique(Dat.Violations$Complaint.Status))
featureList10 <-
  paste(unique(Dat.CleanedViolations$Complaint.Status))

CensusTractC <- Dat.AssetCount$Row.Labels
CensusTractP <- Dat.AssetPercent$Row.Labels

# SHAPEFILE - CENSUS INFORMATION
#Download Onondaga County Tracts, Onondaga County = 67
shape.Tracts <- tracts(state = 36, county = 67, cb = TRUE)

#Create a geographic set to grab tabular data (acs)
shape.Syracuse <-
  shape.Tracts[shape.Tracts$NAME == 1 | shape.Tracts$NAME == 2 |
                 shape.Tracts$NAME == 3 |
                 shape.Tracts$NAME == 4 |
                 shape.Tracts$NAME == 5.01 |
                 shape.Tracts$NAME == 6 |
                 shape.Tracts$NAME == 7 |
                 shape.Tracts$NAME == 8 |
                 shape.Tracts$NAME == 9 |
                 shape.Tracts$NAME == 10 |
                 shape.Tracts$NAME == 14 |
                 shape.Tracts$NAME == 15 |
                 shape.Tracts$NAME == 16 |
                 shape.Tracts$NAME == 17.01 |
                 shape.Tracts$NAME == 17.02 |
                 shape.Tracts$NAME == 18 |
                 shape.Tracts$NAME == 19 |
                 shape.Tracts$NAME == 20 |
                 shape.Tracts$NAME == 21.01 |
                 shape.Tracts$NAME == 23 |
                 shape.Tracts$NAME ==  24 |
                 shape.Tracts$NAME == 27 |
                 shape.Tracts$NAME == 29.01 |
                 shape.Tracts$NAME == 30 |
                 shape.Tracts$NAME == 32 |
                 shape.Tracts$NAME == 34 |
                 shape.Tracts$NAME == 35 |
                 shape.Tracts$NAME == 36.01 |
                 shape.Tracts$NAME == 36.02 |
                 shape.Tracts$NAME == 38 |
                 shape.Tracts$NAME == 39 |
                 shape.Tracts$NAME == 40 |
                 shape.Tracts$NAME == 42 |
                 shape.Tracts$NAME == 43.01 |
                 shape.Tracts$NAME == 43.02 |
                 shape.Tracts$NAME == 44 |
                 shape.Tracts$NAME == 45 |
                 shape.Tracts$NAME == 46 |
                 shape.Tracts$NAME == 48 |
                 shape.Tracts$NAME == 49 |
                 shape.Tracts$NAME ==  50 |
                 shape.Tracts$NAME == 51 |
                 shape.Tracts$NAME == 52 |
                 shape.Tracts$NAME == 53 |
                 shape.Tracts$NAME == 54 |
                 shape.Tracts$NAME == 55 |
                 shape.Tracts$NAME == 56.01 |
                 shape.Tracts$NAME == 56.02 |
                 shape.Tracts$NAME == 57 |
                 shape.Tracts$NAME == 58 |
                 shape.Tracts$NAME == 59 |
                 shape.Tracts$NAME == 60 |
                 shape.Tracts$NAME == 61.01 |
                 shape.Tracts$NAME == 61.02 |
                 shape.Tracts$NAME == 61.03,]


nhoodIcon <- makeIcon(
  iconUrl = "Understanding-Syracuse/Images/neighbourhood-icon.png",
  iconWidth = 10,
  iconHeight = 10,
  iconAnchorX = 5,
  iconAnchorY = 5
)

#Asset Density Sorting
Dat.AssetDensity <-
  Dat.AssetDensity[order(Dat.AssetDensity$RatioLength),]
Dat.AssetDensity2 <-
  Dat.AssetDensity[order(Dat.AssetDensity$RatioParcel),]
Dat.DensityLength <- Dat.AssetDensity[, c(1, 3, 7, 4)]
colnames(Dat.DensityLength) <-
  c("Corridor", "Occupied Assets", "Length (ft)", "# Asset/Length")
Dat.DensityParcels <- Dat.AssetDensity2[, c(1, 3, 6, 5)]
colnames(Dat.DensityParcels) <-
  c("Corridor",
    "Occupied Assets",
    "All Parcels",
    "# of Asset/# of Parcels")

#Assets along commercial corridors clean
Dat.CCAssets$Count <- 1

#Investment Data Clean
Dat.Investment$DollarAmount <-
  as.numeric(as.character(Dat.Investment$DollarAmount))
Investment1 <-
  as.data.frame(tapply(
    Dat.Investment$DollarAmount,
    Dat.Investment$CensusTract3,
    sum
  ))
Investment2 <-
  as.data.frame(tapply(
    Dat.Investment$DollarAmount,
    Dat.Investment$CensusTract3,
    length
  ))

Investment2$CensusTract <- row.names(Investment2)

investmentSummary <- data.frame(Investment1, Investment2)
colnames(investmentSummary) <-
  (c("ProjectSum", "ProjectCount", "CensusTract"))
investmentSummary$ProjectSum <-
  as.numeric(investmentSummary$ProjectSum)
investmentSummary$ProjectCount <-
  as.numeric(investmentSummary$ProjectCount)
investmentSummary <-
  merge(ACS14, investmentSummary, by.x = "CensusTract", by.y = "CensusTract")

#Transit Counts and Census Data
TransitCounts <-
  merge(Dat.CentroStopsCounts,
        ACS14,
        by.x = "CensusTract3",
        by.y = "CensusTract3")
TransitCounts <- TransitCounts[, c(1:6, 9, 18, 23:24, 26:29)]
#Class of first Census Tract
TransitCounts$CensusTract1 <-
  as.character(TransitCounts$CensusTract1)
#Sort Transit Data
TransitCounts <- TransitCounts[order(TransitCounts$TransitCount),]
TransitCounts$CT <-
  factor(TransitCounts$CensusTract1, levels = TransitCounts$CensusTract1[order(TransitCounts$TransitCount)])

#transit mini table of coordinates
Dat.Tract$Concat <- paste(Dat.Tract$lat, "+", Dat.Tract$lon)
Dat.Tract$Concat <- gsub(" ", "", Dat.Tract$Concat, fixed = TRUE)
origin = Dat.Tract$Concat
mode = "transit"
destination = "43.0479517596+-76.1505944348"

results = gmapsdistance(origin,
                        destination,
                        mode = mode,
                        key = APIkey,
                        shape = "long")
results <- as.data.frame(results)
results2 <- results[, c(1:3, 6)]
colnames(results2) <-
  c("Origin", "Destination", "Time_Seconds", "Dist_Meters")
results2$Dist_Miles <- round(results2$Dist_Meters * .000621371, 2)
results2$Time_Minutes <- round(results2$Time_Seconds * .0166667, 2)

Dat.Tract_Dist <-
  merge(Dat.Tract, results2, by.x = "Concat", by.y = "Origin")
Dat.Tract_Dist$TimeOverDist <-
  Dat.Tract_Dist$Time_Minutes / Dat.Tract_Dist$Dist_Miles

Dat.DistTime <-
  merge(shape.Syracuse,
        Dat.Tract_Dist,
        by.x = "NAME",
        by.y = "NAME")

Dat.Tract_Dist2 <- Dat.Tract_Dist[, c(2, 8:10)]
Dat.Tract_Dist2 <-
  Dat.Tract_Dist2[order(Dat.Tract_Dist2$TimeOverDist, decreasing = TRUE),]

#Cleaning data for Dat.Violations
Dat.Violations$Violation.Date <-
  as.Date(Dat.Violations$Violation.Date, "%m/%d/%Y")

complaint.date <- Dat.Violations$Violation.Date
post.2012 <- complaint.date > "2011-12-31"
Dat.Violations <- Dat.Violations[post.2012 ,]
complaint.date <- Dat.Violations$Violation.Date

month.year <- cut(complaint.date, breaks = "month")
# this creates a factor for month-year

# this creates pretty names
month.year.name <- format(complaint.date, "%b-%Y")

# table( dat$Complaint.Type, month.year )
Dat.Violations$month.year <- month.year

#Heat Map Data Clean
dat.VHM <-
  as.data.frame(
    tapply(
      Dat.CleanedViolations$Violation.Description,
      Dat.CleanedViolations$Violation.Description,
      FUN = length
    )
  )
colnames(dat.VHM) = "Count"
dat.VHM$Violation.Description <- row.names(dat.VHM)
Dat.CleanedViolations1 <-
  merge(Dat.CleanedViolations,
        dat.VHM,
        by.x = "Violation.Description",
        by.y = "Violation.Description")
Dat.CleanedViolations2 <-
  Dat.CleanedViolations1[Dat.CleanedViolations1$Count > 99,]
Dat.CleanedViolations2$Count <- 1
dat.VHM1 <-
  as.data.frame(
    cast(
      Dat.CleanedViolations2,
      Violation.Description ~ Complaint.Status,
      fun.aggregate = sum
    )
  )

row.names(dat.VHM1) <- dat.VHM1$Violation.Description

dat.VHM2 <- dat.VHM1[, c(2:ncol(dat.VHM1))]

#Cleaning for complaint status stacked bar graphic
Dat.Violations$Year <-
  as.numeric(format(Dat.Violations$Violation.Date, '%Y'))
Dat.Violations$Count <- 1

# ui.R definition
ui <- fluidPage(# Set theme
  theme = shinytheme("united"),
  mainPanel(tabsetPanel(
    ###########METHODOLOGY#########
    tabPanel(h4("Methodology"),
             tabsetPanel(
               tabPanel(
                 "Framework",
                 h4("Query Purpose"),
                 tags$ol(
                   tags$li("What is the problem/question you are trying to solve?"),
                   tags$li("Where did it come from? Who or what initiated the query?"),
                   tags$li(
                     "Where does the problem exist - public/private domain. Who is currently responsible for an intervention and what might that look like?"
                   ),
                   tags$li("How is this analysis helpful?"),
                   tags$li("What data sets do you have access to relevant to the problem?"),
                   tags$li("What fields are in each of the data sources?"),
                   tags$li(
                     "How many people/addresses/faciliites/entities/jursidictions does the data contain?"
                   ),
                   tags$li(
                     "For this problem, what % of entities are at risk or have resources to be intervened?"
                   )
                 ),
                 h4("Data Governance & Maturity"),
                 tags$ol(
                   tags$li(
                     "What analysis has been done thus far and what conclusions has it drawn? Give credit where credit is due."
                   ),
                   tags$li(
                     "For the data sets that you have access to - who is responsible for the data (which organization and person)?"
                   ),
                   tags$li("Is the data accessible outside the department? Is there a VPN?"),
                   tags$li(
                     "What security policies and considerations need to be in place for each of the data sources? (HIPPA, FERPA, etc)"
                   ),
                   tags$li("How is the data stored?"),
                   tags$li("How accessible is the data that's required"),
                   tags$li(
                     "Do you have data that is both relevant and sufficient to solve the problem?"
                   ),
                   tags$li("How is the data quality?"),
                   tags$li("How often is the data collected?"),
                   tags$li("What is the level of granularity for the data sources?"),
                   tags$li("How much history is stored? How are updates handled?"),
                   tags$li("How integrated are the different data sources?"),
                   tags$li("What data privacy policies are in place?"),
                   tags$li("How well documented are the data?")
                 ),
                 h4("Next Steps"),
                 tags$ol(
                   tags$li(
                     "What (if any) questions did this analysis answer or bring insight to?"
                   ),
                   tags$li(
                     "What (if any) deeper questions did this analysis come to demand? Did this analysis require a step back or out?"
                   ),
                   tags$li(
                     "This should link back up to the 'problem definition' and 'leads' for the next step or tab."
                   )
                 ),
                 h5(
                   "This Frameworkf was taken from the University of Chicago's Center for Data Science & Public Policy's Data Maturity Framework Questionnaire. All data and code is stored on Github: https://github.com/subartle/Understanding-Syracuse."
                 )
               ),
               tabPanel(
                 "Contacts and Acknowledgements",
                 h5(
                   "The following analysis required a tremendous amount of guidance and assistance from the following. Furthermore, several graphics were
                   copied from previous research done by fellow teammates and partners. Please find their names and contact information below and look for
                   acknowledgements throughout the text. If you have questions regarding a specific graphic or piecec of analysis, please contact Susannah
                   Bartlett with the City of Syracuse's Department of Innovation for detailed information."
                 ),
                 h4(
                   "City of Syracuse Department of Neighbrohood and Business Development"
                 ),
                 h5("Paul Driscoll: Commisioner - pdriscoll@syrgov.net"),
                 h5(
                   "Stephanie Pasquale: Deputy Commissioner - spasquale@syrgov.net."
                 ),
                 h5("Belen Cordon: Planner 1 - bcordon@syrgov.net"),
                 h5("Michelle Sczpanski: NBD Planner - msczpanski@sygovn.net"),
                 h4("Syracuse Community Geography Department"),
                 h5(
                   "Jonnell Robinson: Syracuse Geography Director - jdallen@maxwell.syr.edu"
                 ),
                 h4("City of Syracuse Department of Innovation"),
                 h5("Sam Edelstein: Cheif Data Officer - sedelstein@syrgov.net"),
                 h5("Samantha Linnett: Program Coordinator - slinnett@syrgov.net"),
                 h5("Adria Finch: Project Manager - afinch@syrgov.net"),
                 h5("Cassie Schmitt: Syracuse I-Team Intern - cschmitt@syrgov.net"),
                 h4("Contact Community Services"),
                 h5(
                   "Cheryl Giarrusso: Director of the Crisis Intervention Services - cgiarrusso@contactsyracuse.org"
                 ),
                 h4("Syracuse-Onondaga County Planning Agency"),
                 h5(
                   "Edward Hart: GIS Program Manager with SOCPA - EdwardHart@ongov.net"
                 ),
                 h4("Thank you!")
                 )
             )),
    
    ###########PLACED-BASED APPROACH######
    tabPanel(
      h4("Place-Based Approach"),
      tabsetPanel(
        tabPanel(
          "Tab Overview",
          h4("Problem Definition"),
          tags$ol(
            tags$li(
              c(
                "Problem Definition: Current analysis has effectively shown the concentration of poverty
                and lack of resources throughout the city of Syracuse. When poverty, unemployment, lack
                of access to a car, vacancy, crime, etc. is visualized across the face of Syracuse, a
                common trend appears: two large, curvature areas arc across the North and Southwest. In
                meetings, these areas are often described as 'two red bananas'. Although accurate in relaying where social
                issues exist, the proportion of public resources to the areas in red seems greatly unbalanced. Is there
                a way to use data to further differentiate and delineate the needs within these neighborhoods?"
              )
              ),
            tags$li(
              "Leads: A preliminary review of the analysis done was humbling. Analysts from the Syracuse University's
              Community Geography Department, CNY Fair Housing, the City of Syracuse's Dept. of Neighborhood
              and Business Development, Home HeadQuarters and numerous other sources (a small portion summarised below) captures
              the long and dedicated conversation taken place thus far. The above question came out of
              numerous meetings with J. Robinson and S. Edelstein about how to further and not simply duplicate this conversation."
            ),
            tags$li(
              "This tab's analysis hopes to (2) further differntiate and define physical assets and need in specific geographical
              areas and (2) offer an overview of access points for interventions."
            )
            ),
          h4("Data Governance & Maturity"),
          tags$ol(
            "Physical Assets: The SPAD data is stored in excel form at the SOCPA office. SPAD data is
            recorded by Syracuse Police Department data, permitting data, Syracuse.com data and drive around
            data. SPAD data is on the asset level. Ex) DestiNY may exist within one parcel but has hundreds of
            assets within it. SPAD data captures every asset on a parcel. SPAD data is point in time data. Date
            of when the asset was last updated or addes is recorded within the excel file."
            ),
          tags$ol(
            "Place-Based Approach: Data was provided by the Department of Neighborhood and Business Development. The suspected
            zombie properties list was taken from the City of Syracuse Application for 'Zombie' and Vacant Properties Remediation
            and Prevention Initiative (prepared by Stephanie Pasquale and Michelle Sczpanski) "
          ),
          h4("Snapshots of Previous Research"),
          tags$ol(
            "Fact Sheets: Samantha Linnett (Program Coordinator for the Syracuse Innovation Team) took on the task of defining
            Syracuse's population using census data and her awesome design skills."
          ),
          tags$ol(
            "Opportunity Indicies: Alys Mann and CNY Fair Housing defined what opportunity was and where it existed
            throughout Onondaga County in the 2014 Analysis of Impediments to Fair Housing Report
            (http://cnyfairhousing.org/wp-content/uploads/2014/11/CNY-Fair-Housing-sm2.pdf). Summaries of their work can be
            found below."
          ),
          tags$ol(
            "Access to Capital: Jonnell Robinson and staff at the Syracuse Community Geography Department have done extensive
            research into the underlying causes of poverty and lack of opportunity. the 'Redlining' and 'Banks and Lending' tabs
            are brief summaries of their research into access to capital."
          ),
          fixedRow(column(
            12,
            selectInput(
              inputId = "redBananasSelect",
              label = " ",
              choices = c(
                "General Factsheet",
                "Business Factsheet",
                "Crime Factsheet",
                "Education Factsheet",
                "Health Factsheet",
                "Housing Factsheet",
                "Opportunity Indices Part 1",
                "Opportunity Indices Part 2",
                "Banks and Lending",
                "Redlining"
              ),
              selected = "General"
            )
          )),
          
          #Vertical space
          fixedRow(column(12, imageOutput("redBananasPic")))
          ),
        
        ###########PHYSICAL ASSETS UI#########
        tabPanel(
          "Physical Assets",
          h4(
            "Question: Is there a relationship between certain types of property based community assets and neighborhood health?"
          ),
          tags$ol(
            "HOW TO:",
            h5(
              "Step 1: The drop down labeled 'Neighborhood Asset' includes data from the SPAD database (see Tab Overview for more information). For example if 'Banks and Lending' is selected, the plots below will group the total # of banks and lending institutions by census tract and the map will populate with a dot for every property that has a bank and lending institution within it."
            ),
            h5(
              "Step 2: The drop down labeled 'Census Information' includes data from the ACS 2014 5 Year Estimate. For example if 'Median Income (in dollars)' is selected, the plots will alter the x-axis accordingly and the map will shade by census tract. Be mindful of the map's legend."
            ),
            h5(
              "Step 3: Click and drag over the plots' census dots. More detailed information will populate the gray table."
            ),
            h5(
              "Step 4: Hover over the map's asset points for more detailed informaiton"
            )
          ),
          h4(
            "Observations: Census tracts with a low unemployment rate also have a higher total # of households and a lower percentage of households with no vehicle. Furthermore, there is a positive relationship between these census tracts and residential assets and a negative relationship between these census tracts and all other, non-residential assets."
          ),
          h4(
            "Conclusions: The below plots do NOT represent a causal relationship. However, they bring doubt to the hypothesis that census tracts with a higher # or % of non-residential services are more likely to have higher economic opportunity."
          ),
          
          # Vertical space
          tags$hr(),
          
          # Feature selection
          fixedRow(column(
            6,
            selectInput(
              inputId = "Input1",
              label = "Neighborhood Asset",
              choices = featureList2,
              selected = "Banks and Lending"
            )
          ),
          column(
            6,
            selectInput(
              inputId = "Input2",
              label = "Census Information",
              choices = featureList3,
              selected = "Median Income (in dollars)"
            )
          )),
          
          # First row
          fixedRow(
            column(
              6,
              plotlyOutput("Plot1", height = "400px"),
              verbatimTextOutput("click1"),
              plotlyOutput("Plot2", height = "400px")
            ),
            column(6, leafletOutput("AssetMap1", height = "800px"))
          ),
          tags$hr(),
          h5(
            "This app is for planning purposes only. Please contact Susannah Bartlett at sbartlett@syrgov.net with any questions, concerns or insights."
          )
        ),
        
        ##############COMMERCIAL CORRIDORS UI############
        tabPanel(
          "Commercial Corridors",
          h4(
            "What is the breakdown of each commercial corridor? What is the diversity of Use?"
          ),
          fixedRow(column(
            12, leafletOutput("CCAssetMap", height = "500px")
          )),
          fixedRow(column(
            4,
            selectInput(
              inputId = "CCorridor",
              label = "Commercial Corridor",
              choices = featureList7
            )
          )),
          fixedRow(column(
            12, plotlyOutput("CCVarietyPlot", height = "500px")
          )),
          h4(
            "What is the density of each commercial corridor? Does the density of a corridor play a part in the health of a commercial corridor and its surrounding neighborhoods?"
          ),
          fixedRow(
            column(8, plotlyOutput("CCDensityPlot", height = "500px")),
            column(
              4,
              h6(
                "The ratio to the left looks at the total # of assets in each corridor (not including vacant assets) over the total length (in feet) of that corridor."
              ),
              numericInput("DensityObs1", "# of rows:", 7),
              tableOutput("CCLengthRatio")
            )
          ),
          fixedRow(
            column(8, plotlyOutput("CCDensityPlot2",  height = "500px")),
            column(
              4,
              h6(
                "The ratio to the left looks at the total # of assets in each corridor (not including vacant assets) over the total # of parcels (including vacant parcels & lots) in that corridor"
              ),
              numericInput("DensityObs2", "# of rows:", 7),
              tableOutput("CCParcelRatio")
            )
          ),
          h5(
            "This app is for planning purposes only. Please contact Susannah Bartlett at sbartlett@syrgov.net with any questions, concerns or insights."
          )
        ),
        
        ##########RESIDENTIAL PROJECTS UI#############
        tabPanel(
          "Residential Projects",
          h4(
            "Question: What has been the City’s place-based approach? Where has money been invested and in what way?"
          ),
          h4(
            "Observations: Lead dollars have been evenly distributed throughout the city. However, other projects seem more focused in specific areas"
          ),
          fixedRow(column(
            4,
            selectInput(
              inputId = "Census2",
              label = "Census Information",
              choices = featureList3
            )
          )),
          fixedRow(column(
            8, plotlyOutput("PlotInvested", height = "400px")
          )),
          tags$ol(
            "HOW TO:",
            h5(
              "Step 1: The drop down labeled 'Accessible Properties' displays points where the City or a City partner currently has or has potential to take parcel ownership. Therefore, there is potential for a place based project."
            ),
            h5(
              "Step 2: The drop down labeled 'Inaccessible Properties' displays points where the City would struggle to gain access to a parcel."
            ),
            h5(
              "Step 3: The drop down labeled 'Census Information' colors the map with data from the ACS 2014 5 Year Estimate. For example if 'Median Income (in dollars)' is selected, the map will shade by census tract. Be mindful of the map's legend and click the gold houses to identify census tract #s."
            ),
            h5(
              "Step 4: The drop down labeled 'Property Investments' show where the City or a City partner has invested CDBG, HOME or Lead money over the past 5+ years (more detailed metadata to come). Click the purple points to identify the grant dollar amount invested in the property."
            ),
            h5(
              "Step 5: Dont forget to click over golden houses and purple dots for more detailed info!"
            )
          ),
          fixedRow(column(
            4,
            selectInput(
              inputId = "Accessible",
              label = "Accessible Properties",
              choices = featureList1
            )
          ),
          column(
            4,
            selectInput(
              inputId = "Inaccessible",
              label = "Inaccessible Properties",
              choices = featureList4
            )
          )),
          fixedRow(column(
            4,
            selectInput(
              inputId = "Census",
              label = "Census Information",
              choices = featureList3
            )
          ),
          column(
            4,
            selectInput(
              inputId = "Investment",
              label = "Property Investments",
              choices = featureList5
            )
          )),
          fixedRow(column(
            12, leafletOutput("AccessMap1", height = "700px")
          ))
        )
          )
    ),
    
    
    ##############COMMUNITY CONNECTION UI##############
    tabPanel(h4("Community Connection"),
             tabsetPanel(
               tabPanel(
                 "Tab Overview",
                 h4("Problem Definition"),
                 tags$ol(tags$li(
                   c(
                     "Problem Definition: Throughout the interview process, service providers and constituents alike say that there
                     is no lack of services in Syracuse, the problem lies instead with connecting people to those services.
                     When we ask how constituents hear about available services, the answer has been almost exclusively 'word-
                     of-mouth.' Additionally, interviewees have listed transportation to services as being problematic.
                     An additional theme is that service providers do not 'meet people where they are at' and instead
                     expect people to travel to them. Is there a 'service connection system' in Syracuse? Is it effective?"
                   )
                   ),
                   tags$li(
                     c(
                       "Leads: Samantha Linnett and I met with Cheryl Giarrusso of Central New York's 211. They have offered to share call
                       data and service provider data."
                     )
                     )),
                 h4("Data Governance & Maturity"),
                 tags$ol(
                   tags$li(
                     "Transportation: Bus Transits: https://maps.bts.dot.gov/arcgis/apps/webappviewer/index.html?id=b06d206bcae840d58fb3d0af36e7ee16"
                   ),
                   tags$li(
                     "Available Services: Over the duration of CNY 211's 30 years, they have tracked and logged any service provider within the
                     5 contiguous counties. They are going to provide the full list of programs/services throughout the city of Syracuse and Onondaga
                     County, their hours of operations, the categories of services they provide, address and the # of referrals per service."
                   ),
                   tags$li(
                     "Calls: CNY 211 has offered to provide their call data at the zip code level for Syracuse: # of calls, subject of the call,
                     time and date of the call. We are also looking into police call data and cityline call data."
                   )
                   ),
                 fixedRow(column(
                   12,
                   selectInput(
                     inputId = "CommunityConncetionPics",
                     label = h4("Snapshots of Previous Research"),
                     choices = c("Public Transportation Report", "Community Centers")
                   )
                 )),
                 fixedRow(imageOutput("CommunityConnectionPic"))
                 ),
               
               ################PUBLIC TRANSPORTATION UI##############
               tabPanel(
                 "Public Transportation",
                 fixedRow(column(
                   12, leafletOutput("TransportationMap1", height = "700px")
                 )),
                 tags$hr(),
                 fixedRow(column(
                   4,
                   selectInput(
                     inputId = "TransitCensus",
                     label = "Census Level Data",
                     choices = featureList8
                   )
                 )),
                 fixedRow(column(
                   6, plotlyOutput("TransportationGraph1", height = "700px")
                 ),
                 column(
                   6, leafletOutput("TransportationMap2", height = "700px")
                 )),
                 tags$hr(),
                 fixedRow(
                   column(8, leafletOutput("TransportationMap3", height = "700px")),
                   column(
                     4,
                     h6(
                       "The map to the left looks at the minutes it takes to get from the center of each census tract to a specific location (i.e. downtown) using public transportation divided
                       by the # of miles from the center of each census tract to a specific location (i.e. downtown). If services are evenly distributed throughout the city, there should be little
                       to no range and a small standard deviation."
                     ),
                     numericInput("TransportObs1", "# of rows:", 16),
                     tableOutput("TransportTable1")
                     )
                   )
               )
                 )),
    
    ##############DOCE UI############
    tabPanel(h4("DOCE"),
             tabsetPanel(
               tabPanel(
                 "Tab Overview",
                 h4("Problem Definition"),
                 tags$ol(tags$li(
                   c(
                     "Problem Definition: Several interviews have brought the teams attention to the Division
                     of Code Enforcement (DOCE) as a tool to address poverty. Conversations with the current Director of
                     Code Enforcement (as well as previous Directors) and other members of the Department of
                     Neighborhood and Business Development list the DOCE as having great potential for impacting
                     unsafe living conditions. What is this potential? What is the current capacity? What can be done
                     to increase the capacity to meet this potential?"
                   )
                   ),
                   tags$li("Leads: ")),
                 h4("Data Governance & Maturity"),
                 fixedRow(column(
                   12,
                   selectInput(
                     inputId = "CodeViolationSelect",
                     label = h4("Snapshots of Previous Research"),
                     choices =
                       c(
                         "Life of a Code Violation",
                         "Code Violation Heat Map",
                         "Neighborhood Compliance Rate",
                         "Demolition Strategy",
                         "Demolition Candidates",
                         "Housing Vulnerable Case Load",
                         "Bed Bug Breakdown",
                         "Legal Streamline Weighting System"
                       ),
                     selected = "Neighborhood Compliance Rate"
                   )
                 )),
                 #Vertical space
                 fixedRow(column(12, imageOutput("CodeViolationPic")))
                 ),
               
               #########COMPLAINTS V VIOLATION DATA UI##################
               tabPanel(
                 "Bottlenecks",
                 h4(
                   "How well does the current DOCE function? Are there bottlenecks in the process?"
                 ),
                 fixedRow(column(
                   3,
                   selectInput(
                     inputId = "ComplaintStatusSelect",
                     label = h4("Complaint Status:"),
                     choices = featureList9,
                     selected = "Referred to Law"
                   )
                 )),
                 fixedRow(column(
                   6, dygraphOutput("ComplaintGraph1", height = "500px")
                 ),
                 column(
                   6, plotlyOutput("ComplaintGraph2", height = "500px")
                 )),
                 fixedRow(column(
                   12,
                   selectInput(
                     inputId = "HeatMapSort",
                     label = "Which case status would you like to sort by?",
                     choices = featureList10,
                     selected = "High Priority Review"
                   )
                 )),
                 fixedRow(column(
                   12, d3heatmapOutput("violationHeatmap", height = "1000px")
                 ))
               )
                 ))
               )))

# server.R definition
server <- function(input, output, session) {
  #########ABOUT SERVER##############
  
  #########METHODOLOGY SERVER########
  output$redBananasPic <- renderImage({
    if (input$redBananasSelect == "Opportunity Indices Part 1") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Opportunity Indices_Page1.png",
          contentType = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    } else if (input$redBananasSelect == "Opportunity Indices Part 2") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Opportunity Indices_Page2.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Banks and Lending") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Banks and Lending.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Redlining") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Redlining.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "General Factsheet") {
      return(
        list(
          src = "Understanding-Syracuse/Images/General.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Business Factsheet") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Business.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Crime Factsheet") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Crime.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Education Factsheet") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Education.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Health Factsheet") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Health.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
    else if (input$redBananasSelect == "Housing Factsheet") {
      return(
        list(
          src = "Understanding-Syracuse/Images/Housing.png",
          filetype = "image/png",
          alt = "Drats! Something went wrong D:"
        )
      )
    }
  }, deleteFile = FALSE)
  
  
  #########PHYSICAL ASSETS SERVER####
  # Observes the second feature input for a change
  observeEvent(
    c(
      input$Input2,
      input$Input1,
      input$Census,
      input$TransitCensus,
      input$TransportMode
    ),
    {
      # Create a convenience data.frame which can be used for charting
      plot1.df <- data.frame(
        Dat.AssetPercent[, input$Input2],
        Dat.AssetPercent[, input$Input1],
        CensusTract = Dat.AssetPercent$Row.Labels,
        Income = Dat.AssetPercent$`Median Income (in dollars)`
      )
      
      plot2.df <- data.frame(
        Dat.AssetCount[, input$Input2],
        Dat.AssetCount[, input$Input1],
        CensusTract = Dat.AssetCount$Row.Labels,
        Income = Dat.AssetCount$`Median Income (in dollars)`
      )
      
      censusInfo <- data.frame(ACS14[, input$Input2],
                               ACS14$CensusTract,
                               ACS14$lon,
                               ACS14$lat)
      
      censusInfo2 <- data.frame(ACS14[, input$Census],
                                ACS14$CensusTract,
                                ACS14$lon,
                                ACS14$lat)
      
      censusInfo3 <- data.frame(ACS14$CensusTract,
                                ACS14$lon,
                                ACS14$lat)
      
      censusInfo4 <- data.frame(
        TransitCounts[, input$TransitCensus],
        TransitCounts$CensusTract1,
        TransitCounts$lon.x,
        TransitCounts$lat.x
      )
      
      # Add column names
      colnames(plot1.df) <-
        c("x", "y", "CensusTract", "MedianIncome")
      colnames(plot2.df) <-
        c("x", "y", "CensusTract", "MedianIncome")
      colnames(censusInfo) <- c("x", "CensusTract3", "lon", "lat")
      colnames(censusInfo2) <- c("x", "CensusTract3", "lon", "lat")
      colnames(censusInfo3) <- c("CensusTract3", "lon", "lat")
      colnames(censusInfo4) <- c("x", "CensusTract3", "lon", "lat")
      
      #Merge shapefile with ACS 2014 data
      shape.asset <-
        merge(shape.Syracuse,
              censusInfo,
              by.x = "NAME",
              by.y = "CensusTract3")
      shape.transit <-
        merge(shape.Syracuse,
              censusInfo4,
              by.x = "NAME",
              by.y = "CensusTract3")
      shape.access <-
        merge(shape.Syracuse,
              censusInfo2,
              by.x = "NAME",
              by.y = "CensusTract3")
      shape.ccasset <-
        merge(shape.Syracuse,
              censusInfo3,
              by.x = "NAME",
              by.y = "CensusTract3")
      
      #fitted lines
      fit1 <- lm(y ~ x, data = plot1.df)
      fit2 <- lm(y ~ x, data = plot2.df)
      
      # Do a plotly contour plot to visualize the two featres with
      # the number of malignant cases as size
      # Note the use of 'source' argument
      output$Plot1 <- renderPlotly({
        plot_ly(
          plot1.df,
          x = x,
          y = y,
          key = CensusTract,
          hoverinfo = "text",
          text = paste(
            "X Axis:",
            x,
            ",",
            "Y Axis:",
            y,
            ",",
            "Census Tract:",
            CensusTract
          ),
          color = MedianIncome,
          colors = "RdYlBu",
          mode = "markers",
          source = "subset",
          marker = list(size = 12, outliercolor = "black")
        ) %>%
          add_trace(
            data = plot1.df,
            x = x,
            y = fitted(fit1),
            mode = "lines"
          )
        layout(
          title = paste("% of", input$Input1, "vs ", input$Input2),
          xaxis = list(title = input$Input2),
          yaxis = list(title = paste("% of Assets: ", input$Input1)),
          dragmode =  "select",
          showlegend = FALSE
        )
      })
      
      
      output$Plot2 <- renderPlotly({
        plot_ly(
          plot2.df,
          x = x,
          y = y,
          key = CensusTract,
          hoverinfo = "text",
          text = paste(
            "X Axis:",
            x,
            ",",
            "Y Axis:",
            y,
            ",",
            "Census Tract:",
            CensusTract
          ),
          color = MedianIncome,
          colors = "RdYlBu",
          mode = "markers",
          source = "subset",
          marker = list(size = 12)
        ) %>%
          add_trace(
            data = plot2.df,
            x = x,
            y = fitted(fit2),
            mode = "lines"
          )
        layout(
          title = paste("# of", input$Input1, "vs ", input$Input2),
          xaxis = list(title = input$Input2),
          yaxis = list(title = paste("# of Assets: ", input$Input1)),
          dragmode =  "select",
          showlegend = FALSE
        )
      })
      
      output$click1 <- renderPrint({
        dat.hover <- event_data("plotly_selected", source = "subset")
        if (is.null(dat.hover))
          "Click and drag over points of interest"
        else
          names(dat.hover) <-
            c("1", "2", "X Axis", "Y Axis", "Census Tract")
        dat.hover[c(3, 4, 5)]
      })
      
      output$AssetMap1 <- renderLeaflet({
        NonResSubset <-
          Dat.NonRes[Dat.NonRes$Entity_Category == input$Input1,]
        
        leaflet(shape.asset) %>%
          setView(lng = -76.1474,
                  lat = 43.0481,
                  zoom = 12) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            stroke = FALSE,
            fillOpacity = 0.7,
            smoothFactor = 0.5,
            color = ~ colorNumeric("Blues", shape.asset$x)(shape.asset$x)
          ) %>%
          addMarkers(
            ~ lon,
            ~ lat,
            icon = nhoodIcon,
            popup = paste("Census Tract: ", shape.access$NAME)
          ) %>%
          addCircleMarkers(
            lng = NonResSubset$Lon,
            lat = NonResSubset$Lat,
            popup = NonResSubset$Entity2,
            radius = 4,
            color = NonResSubset$Color
          ) %>%
          addLegend(
            "bottomright",
            colors = c("blue", "red", "gray", "orange"),
            labels = c("Occupied", "Vacant", "No Information", "Census Tract #"),
            title = "Property Status"
          ) %>%
          addLegend(
            "bottomleft",
            pal = colorNumeric("Blues", shape.asset$x, n = 5),
            values = shape.asset$x,
            title = input$Input2
          )
      })
      
      #########COMMERCIAL CORRIDORS SERVER#####
      output$CCAssetMap <- renderLeaflet({
        Dat.CCAssets$GeneralCategories <-
          as.factor(Dat.CCAssets$GeneralCategories)
        
        factpal <-
          colorFactor(terrain.colors(4), Dat.CCAssets$GeneralCategories)
        
        
        leaflet(shape.ccasset) %>%
          setView(lng = -76.1474,
                  lat = 43.0481,
                  zoom = 12) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            stroke = FALSE,
            fillOpacity = 0.7,
            smoothFactor = 0.5,
            color = "gray"
          ) %>%
          addMarkers(
            ~ lon,
            ~ lat,
            icon = nhoodIcon,
            popup = paste("Census Tract: ", shape.ccasset$NAME)
          ) %>%
          addCircleMarkers(
            lng = Dat.CCAssets$Lon,
            lat = Dat.CCAssets$Lat,
            radius = 3,
            color = ~ factpal(Dat.CCAssets$GeneralCategories),
            popup = paste(
              "Corridor: ",
              Dat.CCAssets$Corridor,
              "; Status: ",
              Dat.CCAssets$GeneralCategories
            )
          ) %>%
          addLegend(
            "bottomright",
            colors = c("white", "orange", "yellow", "green"),
            labels = c(
              "Vacant",
              "Residential",
              "Mixed Use",
              "Commercial and Public Spaces"
            ),
            title = "Asset Use"
          )
      })
      
      output$CCDensityPlot <- renderPlotly({
        pp <-
          ggplot(data = Dat.AssetDensity, aes(x = Corridor, y = RatioLength)) +
          geom_bar(stat = "identity",
                   colour = "orange",
                   fill = "orange") +
          ggtitle("Asset to Length Ratio: # of Assets/Length of Corridor (ft)") +
          ylab("Ratio") +
          xlab(" ") +
          coord_flip()
      })
      
      output$CCLengthRatio <- renderTable({
        head(Dat.DensityLength, n = input$DensityObs1)
      })
      
      output$CCDensityPlot2 <- renderPlotly({
        ppp <-
          ggplot(data = Dat.AssetDensity2, aes(x = Corridor, y = RatioParcel)) +
          geom_bar(stat = "identity",
                   colour = "orange",
                   fill = "orange") +
          ggtitle("Asset to Parcel Ratio: # of Assets/# of Parcels") +
          ylab("Ratio") +
          xlab(" ") +
          coord_flip()
      })
      
      output$CCParcelRatio <- renderTable({
        head(Dat.DensityParcels, n = input$DensityObs2)
      })
      
      output$CCVarietyPlot <- renderPlotly({
        CCSubset <- Dat.CCAssets[Dat.CCAssets$Corridor == input$CCorridor,]
        CCSubset$GeneralCategories <-
          as.character(CCSubset$GeneralCategories)
        CCSubset$Count <- 1
        CCSubset1 <-
          as.data.frame(tapply(CCSubset$Count, CCSubset$Entity_Category, FUN = sum))
        CCSubset1$Category1 <-
          tapply(CCSubset$Entity_Category,
                 CCSubset$Entity_Category,
                 FUN = unique)
        CCSubset1$Category <-
          tapply(CCSubset$GeneralCategories,
                 CCSubset$Entity_Category,
                 FUN = unique)
        colnames(CCSubset1) <- c("Count", "Category1", "Category")
        
        CCSubset1$Count <- as.numeric(CCSubset1$Count)
        CCSubset1$Category1 <- as.character(CCSubset1$Category1)
        CCSubset1$Category <- as.factor(CCSubset1$Category)
        
        
        p <-
          ggplot(data = CCSubset1, aes(x = Category1, y = Count, fill = Category)) +
          scale_fill_brewer(palette = "YlGn") +
          geom_bar(stat = "identity") +
          ggtitle(paste("Asset Breakdown of ", input$CCorridor)) +
          ylab("Asset Count") +
          xlab(" ") +
          coord_flip()
        ggplotly(p)
      })
      
      
      #########RESIDENTIAL PROJECTS SERVER####
      output$AccessMap1 <- renderLeaflet({
        accessSubset <-
          Dat.Accessibility[Dat.Accessibility$Accessibility == input$Accessible,]
        InaccessSubset <-
          Dat.Accessibility[Dat.Accessibility$Accessibility == input$Inaccessible,]
        ProblemSubset <-
          Dat.ProblemProps[Dat.ProblemProps$Problems == input$Problem, ]
        Investment <-
          Dat.Investment[Dat.Investment$Activity == input$Investment, ]
        
        leaflet(shape.access) %>%
          setView(lng = -76.1474,
                  lat = 43.0481,
                  zoom = 12) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addMarkers(
            ~ lon,
            ~ lat,
            icon = nhoodIcon,
            popup = paste("Census Tract: ", shape.access$NAME)
          ) %>%
          addPolygons(
            stroke = FALSE,
            fillOpacity = 0.7,
            smoothFactor = 0.5,
            color = ~ colorNumeric("Blues", shape.access$x)(shape.access$x)
          ) %>%
          addCircleMarkers(
            lng = accessSubset$lat,
            lat = accessSubset$lon,
            radius = 4,
            color = "green"
          ) %>%
          addCircleMarkers(
            lng = InaccessSubset$lat,
            lat = InaccessSubset$lon,
            radius = 4,
            color = "gray"
          ) %>%
          addCircleMarkers(
            lng = Investment$lat,
            lat = Investment$lon,
            popup = paste("Amount Invested: $", Investment$DollarAmount),
            radius = 4,
            color = "purple"
          ) %>%
          addLegend(
            "bottomright",
            colors = c("green", "gray", "purple", "orange"),
            labels = c(
              "Accessible Properties",
              "Inaccessibile Properties",
              "Neighborhood Investment",
              "Census Tract #"
            ),
            title = "Property Status"
          ) %>%
          addLegend(
            "bottomleft",
            pal = colorNumeric("Blues", shape.access$x, n = 5),
            values = shape.access$x,
            title = input$Census
          )
      })
      
      
      observeEvent(c(input$Census2), {
        # Create a convenience data.frame which can be used for charting
        plot.investSummary <-
          data.frame(
            ProjectSum = investmentSummary$ProjectSum,
            ProjectCount = investmentSummary$ProjectCount,
            CensusTract = investmentSummary$CensusTract,
            CensusInformation = investmentSummary[, input$Census2]
          )
        
        output$PlotInvested <- renderPlotly({
          plot_ly(
            plot.investSummary,
            x = ProjectSum,
            y = ProjectCount,
            key = CensusTract,
            hoverinfo = "text",
            text = paste(
              "Census Tract:",
              CensusTract,
              "# of Projects:",
              ProjectCount,
              ",",
              "Total Fed $:",
              ProjectSum
            ),
            color = CensusInformation,
            colors = "PRGn",
            mode = "markers",
            source = "subset",
            marker = list(size = 12)
          )
        })
        
        output$investClick <- renderPrint({
          dat.investhover <- event_data("plotly_selected", source = "subset")
          if (is.null(dat.investhover))
            "Click and drag over points of interest"
          else
            names(dat.investhover) <-
              c("1", "2", "X Axis", "Y Axis", "Census Tract")
          dat.investhover[c(3, 4, 5)]
        })
      })
      #########DOCE SERVER####
      output$CodeViolationPic <- renderImage({
        if (input$CodeViolationSelect == "Life of a Code Violation") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Life of a Code Violation.png",
              contentType = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        } else if (input$CodeViolationSelect == "Code Violation Heat Map") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Code Violation Heat Map.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Neighborhood Compliance Rate") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Neighborhood Compliance Rate.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Demolition Strategy") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Demolition Strategy.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Demolition Candidates") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Demolition Candidates.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Housing Vulnerable Case Load") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Housing Vulnerable Task Force Case Load.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Bed Bug Breakdown") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Bed Bug PNG.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Legal Streamline Weighting System") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Legal Streamline Weighting System.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
      }, deleteFile = FALSE)
      ###############COMMUNITY CONNECTION SERVER#########
      ###############PUBLIC TRANSPORTATION SERVER############
      
      output$TransportationMap1 <- renderLeaflet({
        leaflet(shape.Syracuse) %>%
          setView(lng = -76.1474,
                  lat = 43.0481,
                  zoom = 12) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addCircleMarkers(
            lng = Dat.CountyCentroStops$stop_lon,
            lat = Dat.CountyCentroStops$stop_lat,
            radius = 1,
            color = "mediumseagreen"
          ) %>%
          addLegend(
            "bottomright",
            colors = "mediumseagreen",
            labels = "Centro Bus Stop",
            title = "Legend"
          )
      })
      
      output$TransportationGraph1 <- renderPlotly({
        tranp <-
          ggplot(data = TransitCounts, aes(x = CT , y = TransitCount)) +
          geom_bar(stat = "identity",
                   colour = "mediumseagreen",
                   fill = "mediumseagreen") +
          ggtitle("# of Public Transit Stops within each Census Tract") +
          ylab("Count of Transits") +
          xlab(" ") +
          coord_flip()
      })
      
      output$TransportationMap2 <- renderLeaflet({
        leaflet(shape.transit) %>%
          setView(lng = -76.1474,
                  lat = 43.0481,
                  zoom = 12) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            stroke = FALSE,
            fillOpacity = 0.7,
            smoothFactor = 0.5,
            color = ~ colorNumeric("Greens", shape.transit$x)(shape.transit$x)
          ) %>%
          addMarkers(
            ~ lon,
            ~ lat,
            icon = nhoodIcon,
            popup = paste("Census Tract: ", shape.transit$NAME)
          ) %>%
          addLegend(
            "bottomleft",
            pal = colorNumeric("Greens", shape.transit$x, n = 5),
            values = shape.transit$x,
            title = input$TransitCensus
          )
      })
      
      output$TransportationMap3 <- renderLeaflet({
        leaflet(Dat.DistTime) %>%
          setView(lng = -76.1474,
                  lat = 43.0481,
                  zoom = 12) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addMarkers(
            ~ lon,
            ~ lat,
            icon = nhoodIcon,
            popup = paste("Census Tract: ", Dat.DistTime$NAME)
          ) %>%
          addPolygons(
            stroke = FALSE,
            fillOpacity = 0.7,
            smoothFactor = 0.5,
            color = ~ colorNumeric("Greens", Dat.DistTime$TimeOverDist)(Dat.DistTime$TimeOverDist),
            popup = paste(
              "Time to Destination: ",
              Dat.DistTime$Time_Minutes,
              "Miles to Destination: ",
              Dat.DistTime$Dist_Miles
            )
          )
      })
      
      output$TransportTable1 <- renderTable({
        head(Dat.Tract_Dist2, n = input$TransportObs1)
      })
      
      #########DOCE SERVER####
      output$CodeViolationPic <- renderImage({
        if (input$CodeViolationSelect == "Life of a Code Violation") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Life of a Code Violation.png",
              contentType = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        } else if (input$CodeViolationSelect == "Code Violation Heat Map") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Code Violation Heat Map.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Neighborhood Compliance Rate") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Neighborhood Compliance Rate.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Demolition Strategy") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Demolition Strategy.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Demolition Candidates") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Demolition Candidates.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Housing Vulnerable Case Load") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Housing Vulnerable Task Force Case Load.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Bed Bug Breakdown") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Bed Bug PNG.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
        else if (input$CodeViolationSelect == "Legal Streamline Weighting System") {
          return(
            list(
              src = "Understanding-Syracuse/Images/Legal Streamline Weighting System.png",
              filetype = "image/png",
              alt = "Drats! Something went wrong D:"
            )
          )
        }
      }, deleteFile = FALSE)
      
      #########COMPLAINTS V VIOLATION DATA SERVER##################
      #output$ComplaintGraph1 <- renderDygraph({
      # dat.sub <-
      #  Dat.Violations[Dat.Violations$Complaint.Type %in% input$ComplaintSelect , ]
      
      # Dropping months with zero complaints
      #  ncomps <- 0
      # comp.checks <- as.data.frame(input$ComplaintSelect)
      #ncomps <- length(input$comp.checks)
      
      # Create chart for a subset of data
      # complaint.sub <-
      #    tapply(dat.sub$Complaint.Type, dat.sub$month.year, length)
      #  complaint.sub[is.na(complaint.sub)] <- 0
      
      # dygraph(complaint.sub) %>%
      #  dyRangeSelector()
      
      #  })
      
      output$ComplaintGraph1 <- renderDygraph({
        dat.sub <-
          Dat.Violations[Dat.Violations$Complaint.Status %in% input$ComplaintStatusSelect , ]
        
        # Dropping months with zero complaints
        ncomps <- 0
        comp.checks <- as.data.frame(input$ComplaintStatusSelect)
        ncomps <- length(input$comp.checks)
        
        # Create chart for a subset of data
        complaint.sub <-
          tapply(dat.sub$Complaint.Status, dat.sub$month.year, length)
        complaint.sub[is.na(complaint.sub)] <- 0
        
        dygraph(complaint.sub) %>%
          dyRangeSelector()
        
      })
      
      output$ComplaintGraph2 <- renderPlotly({
        Dat.Violations$Complaint.Status <-
          as.factor(Dat.Violations$Complaint.Status)
        Dat.Violations$Year <- as.factor(Dat.Violations$Year)
        
        compstatgraph <-
          ggplot(data = Dat.Violations, aes(x = Year, fill = Complaint.Status)) +
          geom_bar() +
          theme(legend.position = 'none') +
          scale_fill_manual(
            values = c(
              "aquamarine4",
              "azure2",
              "aquamarine",
              "azure",
              "cyan3",
              "darkseagreen",
              "aquamarine3",
              "azure1",
              "aquamarine1",
              "antiquewhite",
              "cyan2",
              "darkseagreen1",
              "aquamarine2",
              "azure2",
              "aquamarine1",
              "azure3",
              "cyan1",
              "darkseagreen2",
              "aquamarine1",
              "azure4",
              "aquamarine1",
              "antiquewhite2",
              "cyan4",
              "darkseagreen3"
            )
          ) +
          theme(panel.background = element_blank()) +
          labs(list(title = "Breakdown of Case Statuses by Year", x = "Year Case Opened", y = " "))
        
        ggplotly(compstatgraph)
      })
      
      output$violationHeatmap <- renderD3heatmap({
        dat.VHM3 <-
          dat.VHM2[order(dat.VHM2[input$HeatMapSort], decreasing = TRUE),]
        d3heatmap(
          dat.VHM3,
          colors = "BuGn",
          scale = "column",
          dendrogram = "none"
        )
      })
    }
  )
}

shinyApp(ui = ui, server = server)
