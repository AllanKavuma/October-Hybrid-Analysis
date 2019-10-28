library(shiny)
library(miniUI)


analysisGadget <- function(){
        
ui <- miniPage(
        ## Title for the gadget
        gadgetTitleBar("SiteLoad and DG run hours Analysis"),
        ## Contents for the gadget
        miniContentPanel(
                plotOutput("plot1", width = "100%", height = "100%",
                           brush = "brush1")
        )
)

server <- function(input, output, session) {
        ## Load the data. Note, this is done in the server specifically
        load("allanHy.Rda")
        ## output the plot
        output$plot1 <- renderPlot({
        plot(siteload ~ usagetimedg1, data=allanHy, main = "Site Load Vs DG Hours",
             xlab = "usagetimedg1 (hrs)", ylab = "siteload (kW)", cex = 2, pch = 19,
             col="lightblue")
        text(siteload ~ usagetimedg1, labels=allanHy$generalinformationsitename,
             data=allanHy, cex=0.9, font=2)
        })
        
        ## Observe session, capture data points and end when done
        observeEvent(eventExpr = input$done, {
                stopApp(brushedPoints(df = allanHy, brush = input$brush1,
                                      xvar = "usagetimedg1", yvar = "siteload"))
        })
}

runGadget(ui, server)
}

