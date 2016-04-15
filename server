```{r}
      
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
  
})
