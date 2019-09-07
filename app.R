#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(dplyr)
library(rsconnect)

energy<-read.csv("energy.csv")
energy<-energy[,-c(10:14,16:18,20,23,25:26,30)]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Energy Potentials Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        hr(),

         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         hr(),
  
         
         sliderInput(inputId = "energy_potential", 
                     label = "energy_potential_minimum", 
                     min = 0, max =3000 , 
                     value = 1400),

        hr(),

    
         selectInput(inputId = "y", 
                     label = "y-axis:",
                     choices = c("Urban Gigawatts" = "urbanUtilityScalePV_GW", 
                                 "Rural Gigawatts" = "ruralUtilityScalePV_GW", 
                                 "Urban Kilometers" = "urbanUtilityScalePV_km2", 
                                 "Rural Kilometers" = "ruralUtilityScalePV_km2" 
                                 ), 
                     selected = "urbanUtilityScalePV_GW"),
         
         selectInput(inputId = "x", 
                     label = "x-axis:",
                     choices = c("rooftop Gigawatts" = "rooftopPV_GW", 
                                 "Onshore Wind_km2" = "onshoreWind_km2", 
                                 "biopowerSolid" = "biopowerSolid_GWh", 
                                 "biopowerGaseous" = "biopowerGaseous_GWh"
                                  ), 
                     selected = "urbanUtilityScalePV_GW"),
           hr(),
        
         sliderInput(inputId = "alpha", 
                     label = "Alpha:", 
                     min = 0, max = 1, 
                     value = 0.5),
        
        hr(),
   
         
         checkboxInput(inputId = "show_data",
                       label = "Show energy table",
                       value = TRUE),
        
        hr(),
         
         numericInput(inputId = "n_sample", 
                      label = "Sample size:", 
                      min = 0, max = nrow(energy), 
                      value = 10),
         
        hr(),
        
         downloadButton("2019 Energy Potential of Each State", "Download")
      ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
        
         plotOutput("hist"),
         
         plotOutput("barplot"),
         
         plotOutput("scatterplot"),
         
         DT::dataTableOutput(outputId = "energytable")
         
      )
   )
)



server <- function(input, output, session) {
  
  energy_subset<- reactive({
    req(input$energy_potential)
    filter(energy,urbanUtilityScalePV_GWh >input$energy_potential)
  })
  
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(1, nrow(energy_subset())),
                       max = nrow(energy_subset())
    )
  })
  
  
  energy_sample<- reactive({
    req(input$n_sample)
    sample_n(energy_subset(), input$n_sample)
    
  })
  
  #barplot
  output$barplot <- renderPlot({
    bar_sample   <- energy_sample()[,c(1,3)] 
    qplot(data=bar_sample, y=urbanUtilityScalePV_GW, x=X,xlab="States")+ 
      geom_bar(position="dodge",stat = "identity",fill="orange")+ coord_flip() +
      ggtitle("Energy Potential for each State") +theme(text=element_text(family="serif",size=20))
    })
   
  #histogram
   output$hist <- renderPlot({
      x    <- energy_sample()[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgreen', border = 'white', main = "Urban Utility Scale PV Gigawatts", xlab="PV Gigawatts", ylab = "Number of States")
   })
   
   #scatter plot
   output$scatterplot <- renderPlot({
     ggplot(data =energy_sample(), aes_string(x = input$x, y = input$y)) +
       geom_point(alpha = input$alpha) +
       labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
            y = toTitleCase(str_replace_all(input$y, "_", " "))
            )
   })
   
   #datatable
   output$energytable <- DT::renderDataTable(
     if(input$show_data){
       DT::datatable(data = energy_sample()[,1:7], 
                     options = list(pageLength = 25), 
                     rownames = TRUE)})
   
   #dowload button
   output$downloadData <- downloadHandler(filename = "energy.csv", 
         content = function(file){write.csv(energy, file)})
   
}

# Run the application 
shinyApp(ui = ui, server = server)

