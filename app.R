
### Setup and Data


require (RCurl)
library(shiny)
library(DT)
library (dplyr)
library (leaflet)

code.violations <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")

parcels <- read.csv( "https://raw.githubusercontent.com/hectorlca/Code-Violations/master/data/parcels.csv" )

cv <- code.violations[ !( is.na(code.violations$lon) | is.na(code.violations$lat)) , ]

merged <- merge( cv, parcels, by.x="Identifier", by.y="SBL" )





### Data Table Explorer



explorer <- select(merged, Complaint.Type, Violation.Date, Comply.By.Date, 
                  Violation.Status, Complaint.Status, Owner, Nhood, LandUse, Address)


### mashed ###

      by.ownerv <- group_by (merged, Owner)
      by.ownerp <- group_by (parcels, Owner)
      
      dist.ownerv <- summarise (by.ownerv, violations = n())
      dist.ownerp <- summarise (by.ownerp, properties = n())
      
      mashed <- merge (dist.ownerp, dist.ownerv, by = "Owner")
      
      ### Add # of Open Violations
      
      only.open <- merged [ merged$Violation.Status == "Open" , ]
      by.owneropen <- group_by (only.open, Owner)
      dist.ownerop <- summarise (by.owneropen, open = n())
      
      mashed <- merge (mashed, dist.ownerop, by = "Owner", all = TRUE)
      mashed$open [is.na(mashed$open)] <- 0
      
      ### Add Acres Owned ###
      
      acres.owned <- summarise (by.ownerp, Acres = sum(Acres))
      mashed <- merge (mashed, acres.owned, by = "Owner")
      
      ### Add Square Feet Owned ###
      
      sqft.owned <- summarise (by.ownerp, sqft = sum(SqFt))
      mashed <- merge (mashed, sqft.owned, by = "Owner")
      
      
      ### Add Total Assessed Value ###
      
      total.value <- summarise (by.ownerp, value = sum(AssessedVa))
      mashed <- merge (mashed, total.value, by = "Owner")
      
      colnames (mashed) <- c("Owner", "Properties", "Violations", "Open Violations", 
                             "Acres Owned", "Square Feet Owned", "Assessed Value")
      
      mashed$`Acres Owned` <- round (mashed$`Acres Owned`, digits = 2)
      mashed$`Square Feet Owned` <- round (mashed$`Square Feet Owned`, digits = 2)

### prop.mash ###
      
      # Create Property Profiles
      
      props <- select (merged, Address, LandUse, Owner, AssessedVa)
      
      by.prop <- group_by (props, Address)
      only.open <- merged [ merged$Violation.Status == "Open" , ]
      by.propen <- group_by (only.open, Address)
      
      
      ### Add Violations
      prop.v <- summarise (by.prop, violations = n())  
      
      ### Add Open Cases
      dist.propen <- summarise (by.propen, open = n())  
      prop.mash <- merge (prop.v, dist.propen, by = "Address", all = TRUE)
      
      ### Finalize Table
      prop.mash <- merge (prop.mash, props, by = "Address")
      prop.mash <- unique (prop.mash)
      
      prop.mash$open [is.na(prop.mash$open)] <- 0
      
      colnames (prop.mash) <- c("Property Address", "Violations", 
                                "Open", "Property Type", "Owner", "Assessed Value")
      
      prop.mash$`Assessed Value` <- format (prop.mash$`Assessed Value`, digits = 2, scientific = FALSE)
      
     
      

rm (acres.owned, by.owneropen, by.ownerp, by.ownerv, by.prop, 
    by.propen, dist.ownerop, dist.ownerp, dist.ownerv, dist.propen,
    only.open, prop.v, props, sqft.owned, total.value)

most.wanted <- merged$Owner
 





###  SERVER
      
my.server <- (function(input, output) {
  
  
  output$explorer <- DT::renderDataTable(DT::datatable({
    data <- explorer
    
    if (input$status != "All") {
      data <- data[data$Violation.Status == input$status,]
      
    }
    if (input$use != "All") {
      data <- data[data$LandUse == input$use,]
     
    }
    
    if (input$complaint != "All") {
          data <- data[data$Complaint.Status == input$complaint,]
       
    } 
    
    data
    
    }))
    
  output$summaries <- DT::renderDataTable(DT::datatable({
    data <- explorer    
    
    if (input$category == "owner") { 
          data <- mashed
      
    }
    
    if (input$category == "property") {   
          data <- prop.mash
      
    } 
    
    data
    
  }))
  
    output$mostmap <- renderLeaflet({
    
    # build base map on load
    
    syr.map <- leaflet(data=explorer ) %>% ####
      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
      setView(lng=-76.13, lat=43.03, zoom=13) %>%
      setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)
    
    
    syr.map <- addCircleMarkers( syr.map, lng = merged$lon, lat = merged$lat, col="orange", popup = most.wanted )
    
  })
  
})

 
  



############################################
###### HTML Specs for Checkbox Widgets #####
###########################################

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                 height: 280px; width: 400px;
                                 -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 2;    /* Firefox */ 
                                 column-count: 2; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 } 
                                 ")) 
  ))

###########################
### Widgets as Variables###
###########################

use.boxes <-
  list(h3("Select property type"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'use', 
                                   label    = NULL, 
                                   choices  = c("All", unique(as.character(merged$LandUse))),
                                   selected = "All",
                                   inline   = FALSE))) 

status.boxes <-
  list(h3("Choose Complaint Status"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'complaint', 
                                   label    = NULL, 
                                   choices  = c("All", unique(as.character(merged$Complaint.Status))),
                                   selected = "All",
                                   inline   = FALSE)))

dropdown <- 
  list(h3("Choose Violation Status"), 
       tags$div(align = 'left', 
                class = 'dropdown',
                selectInput("status", 
                            "", 
                            c("All",unique(as.character(merged$Violation.Status))))))

summary.drop <- 
  list(h3("Choose Violation Status"), 
       tags$div(align = 'left', 
                class = 'dropdown',
                
                selectInput ("category",
                             "",
                             choices = list("By Owner" = "owner",
                                            "By Property" = "property"),
                             selected = "property")
       )
  )







### UI


my.ui <- navbarPage("Most Wanted Dashboard",
                             tabPanel("Owner and Property Profiles",
                                      titlePanel("Summary Tables for Owners and Properties"),
                                      fluidRow(
                                        column (3,
                                                summary.drop),
                                        column (7,
                                                
                                                mainPanel(
                                                 DT::dataTableOutput("summaries"),
                                                 align = "left",
                                                 width = 12
                                                          )
                                      
                                                )
                                            )
                                      ),
                             
                             tabPanel("Searchable",tweaks,
                                      titlePanel("Most Wanted List"),
                                      fluidRow(
                                        
                                        #### The Three Widgets ####   
                                        column(3,
                                               dropdown,
                                               use.boxes,
                                               status.boxes),
                                        
                                        
                                        #### The Table ####
                                        
                                        column(6,
                                               
                                               
                                               mainPanel(
                                                 DT::dataTableOutput("explorer"),
                                                 align = "center",
                                                 width = 12
                                                        )
                                              )
                                        )
                                      ),
                    
                                        tabPanel( "Map",  
                              titlePanel("Most Wanted Map"),
                                fluidRow(
                                  column(3,
                                         wellPanel(
                                           h4("Filter"),
                                           dropdown
                                         )
                                         
                                  ),
                                  column(6,
                                         leafletOutput("mostmap", height=500)
                                           )
                    
                    )
                    )
            
        )


shinyApp( ui = my.ui, server = my.server )

