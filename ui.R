#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Solar and Hybrid Analysis on ATC sites"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("sliderSiteload","Value of Site Load:", min = 0.5, max = 6, value = 2),
       checkboxInput("showModel1", "Show/Hide Model", value = TRUE),
       checkboxInput("includeOutliers", "Include Outliers in Boxplot", value = FALSE),
       textInput(inputId = "comments1", label = "Report 1 from Analysis:", value = "Analysis Report"), ## input for comments
       #textInput(inputId = "comments2", label = "Report 2 from Analysis:", value = "Analysis Report"),
       
       ## Code For the brush on slope
       h4("Slope of selected points in second scatter plot"), 
       textOutput("slope_output"), ## slope value output as a text
       h4("Intercept of selected points in second scatter plot"), 
       textOutput("intercept_output"), ## intercept value output as a text
       h4("Selected sites in second scatter plot"), 
       ## Text for selected points
       tableOutput("selected_sites")
       #submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Report 1", br(), textOutput("comment1")),
                    #tabPanel("Report 2", br(), textOutput("comment2"))
                    tabPanel("Documentation", br(), textOutput("comment3"))
                    ),
       plotOutput("plotloadhrs"),
       h4("Predicted DG Hours according to Model(Yellow): "),
       textOutput("pred1"), ## Prediction for Linear regression
       h4("Saved DG hours per Day: "),
       textOutput("savedhrs"),
       h4("Predicted DG Hours according to ModelRF(Blue): "), 
       textOutput("predRF"), ## Prediction for model 2 using RF
       #h4("boxplot for the DG hours:"),
       plotOutput("plotBoxplot"),
       
       ## Plot with brush
       plotOutput("plot_with_brush", brush = brushOpts(id = "brush1")), ## Refer to brush 1 in server.R
       ## Boxplot with brush
       plotOutput("plot_bxplotwith_brush")
    )
  )
))
