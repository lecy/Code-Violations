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




################################################################
################################################################


my.ui <- navbarPage("Most Wanted Dashboard",
                             tabPanel("Owner and Property Profiles",
                                      titlePanel("Summary Tables for Owners and Properties"),
                                      fluidRow(
                                        
                                        column (3,
                                                
                                                summary.drop),
                                                
                                        column (6,
                                                
                                                mainPanel(
                                                 DT::dataTableOutput("summaries"),
                                                 align = "center",
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
                                        )))
