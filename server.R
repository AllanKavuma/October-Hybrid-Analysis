#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        library(randomForest)
        library(caret)
        load("allanHy.Rda")
        
        allanData <- allanHy ## create allanData to carry the original dataset
        
        gridsiteIndex <- c("TL_UG_BUNKER_ONGRID",
                           "TL_UG_BUNKER_ONGRID_HYBRID", 
                           "TL_UG_BUNKER_ONGRID_HYBRID_REV03", 
                           "TL_UG_BUNKER_ONGRID_HYBRID_SOLAR", 
                           "TL_UG_INDOOR_ONGRID_HYBRID", 
                           "TL_UG_OUTDOOR_ONGRID", 
                           "TL_UG_OUTDOOR_ONGRID_HYBRID") ## Index for grid sites
        
        gridIndex <- c("Kyarukobwa",
                       "Rwenkonbwa", 
                       "Nyakajumo", 
                       "Nyongozi", 
                       "Ryensibo",
                       "Kabutsye",
                       "Nagoje",
                       "Butoha",
                       "Kihanga")
        
        #allanHy <- allanHy[!(allanHy$generalinformationsitename %in% gridIndex),] ## Remove grid sites
        
        
        outlierIndex <- c("Bugongi", "Makanga") ## Outliers
        allanHyNoOL <- allanHy[!(allanHy$generalinformationsitename %in% outlierIndex),]
        
        model1 <- lm(usagetimedg1 ~ siteload, data = allanHy) ## Model for predicting site load
        
        model1pred <- reactive({
                loadInput <- input$sliderSiteload
                predict(model1, newdata = data.frame(siteload = loadInput))
        })
        
        output$pred1 <- renderText({
                model1pred()
        })
        
        output$savedhrs <- renderText({
                24 - model1pred()
        })
        
        ## Building the random forest model
        inTrain <- createDataPartition(allanHy$usagetimedg1, p = 0.75, list = FALSE)
        training <- allanHy[inTrain,]
        testing <- allanHy[-inTrain,]
        trainingCleaned <- training[,c(6, 9, 11, 12, 13, 15, 16, 18, 19, 21)] ## Select the important variables
        trainingCleaned$availtimesolar <- as.numeric(trainingCleaned$availtimesolar) ## Change the more than 200 factors to numeric
        
        #modelRF <- train(usagetimedg1 ~siteload + sitelayout + extrafield8, data = trainingCleaned, method = "rf") 
        modelRF <- train(usagetimedg1 ~siteload, data = trainingCleaned, method = "rf") 
        modelRFpred <- reactive({
                loadInput <- input$sliderSiteload
                predict(modelRF, newdata = data.frame(siteload = loadInput))
        })
        
        output$predRF <- renderText({
                modelRFpred()
        })
        
        ## Plot the graph
        output$plotloadhrs <- renderPlot({
                if(input$includeOutliers){
                plot(siteload ~ usagetimedg1, data=allanHy, main = "Site Load Vs DG Hours",
                     xlab = "usagetimedg1 (hrs)", ylab = "siteload (kW)", cex = 2, pch = 19,
                     col="lightblue")
                } else{
                        plot(siteload ~ usagetimedg1, data=allanHyNoOL, main = "Site Load Vs DG Hours",
                             xlab = "usagetimedg1 (hrs)", ylab = "siteload (kW)", cex = 2, pch = 19,
                             col="lightblue")
                }
                text(siteload ~ usagetimedg1, labels=allanHy$generalinformationsitename,
                     data=allanHy, cex=0.9, font=2)
                fitSiteload2 <- lm(allanHy$siteload ~ allanHy$usagetimedg1)
                if(input$showModel1){
                        abline(fitSiteload2, col = "green", lwd =2)
                }
                abline(v = 10, col = "red", lwd = 2)
                abline(h = 4, col = "red", lwd = 2)
                loadvalue <- reactive({
                        input$sliderSiteload
                })
                points(x = model1pred(), loadvalue(), col = "yellow", pch = 16, cex = 2)
                points(x = modelRFpred(), loadvalue(), col = "blue", pch = 16, cex = 2)
        })
        
        ## Boxplot for the DG hours
        output$plotBoxplot <- renderPlot({
                if (input$includeOutliers){
                boxplot(allanHy$usagetimedg1, ylab = "hours", main = "Summary Boxplot for DG hours")
                } else{
                        boxplot(allanHyNoOL$usagetimedg1, ylab = "hours", main = "Boxplot for DG hours")
                }
        })
        
        output$comment1 <- renderText({input$comments1})
        output$comment2 <- renderText({input$comments2})
        
        ## Code for the plot with brush
        ## Create model that gets input from brush 1
        model_brush <- reactive({
                ## Create data of brushed points
                brushed_data <- brushedPoints(df = allanData, brush = input$brush1, yvar = "usagetimedg1", xvar = "siteload")
                ## If brushed data points are less than 2, return NULL; else return model for brushed points
                if(nrow(brushed_data) < 2){
                        return(NULL)
                }
                lm(usagetimedg1 ~ siteload, brushed_data)
        })
        ## Ouput for the slope value
        output$slope_output <- renderText({
                ## if model is not NULL, return slope component
                if(is.null(model_brush())){
                        "No points selected in scatter plot 2!"
                } else{
                        model_brush()[[1]][2]
                }
        }) 
        ## Output for the intercept value
        output$intercept_output <- renderText({
                ## if model is not NULL, return intercept component
                if(is.null(model_brush())){
                        "No points selected in scatter plot 2!"
                } else{
                        model_brush()[[1]][1]
                }
        })
        ## Output Plot with brush
        output$plot_with_brush <- renderPlot({
                plot(usagetimedg1 ~ siteload, data=allanHyNoOL, main = "Site Load Vs DG Hours",
                     ylab = "usagetimedg1 (hrs)", xlab = "siteload (kW)", cex = 2, pch = 19,
                     col="lightblue")
                text(usagetimedg1 ~ siteload, labels=allanHy$generalinformationsitename,
                     data=allanHy, cex=0.9, font=2)
                if(!is.null(model_brush())){
                        abline(model_brush(), col = "yellow", lwd = 2)
                }
        })
        
        ## CREATE A BRUSHED BOXPLOT
        ## Create boxplot that gets input from brush_boxplot
        boxplot_brush <- reactive({
                ## Create data of brushed points
                brushed_data <- brushedPoints(df = allanData, brush = input$brush1, 
                                                     xvar = "siteload", yvar = "usagetimedg1")
                if(nrow(brushed_data) < 1){
                        return(NULL)
                }
                boxplot(brushed_data$usagetimedg1, ylab = "hours", main = "Boxplot for DG hours on selected Site Loads")
        })
        ## Output boxplot from the brushed points
        output$plot_bxplotwith_brush <- renderPlot({
                boxplot_brush()
        })
        
        ## CREATE LIST OF SELECTED SITES
        selectedlist_brush <- reactive({
                brushed_data <- brushedPoints(df = allanData, brush = input$brush1, 
                                              yvar = "usagetimedg1", xvar = "siteload")
                if(nrow(brushed_data) < 1){
                        return("No sites selected")
                }
                data.frame(Site_ID = as.integer(brushed_data$generalinformationsiteid),
                           Site_Name = c(brushed_data$generalinformationsitename))
        })
        
        ## Output the list of selected sites
        output$selected_sites <- renderTable({
                selectedlist_brush()
        })
        
        ## Output the documentation
        output$comment3 <- renderText({
                "This Shiny app does analysis of different hybrid setups on sites.  

                Application loads data and plots a scatter graph of Diesel Generator (DG) run hours
                against the site loads(kW) on various sites.  
                It also plots a summary box plot for the data.  
                
                Using the \"Value of Site Load\" slider, 2 predictions (one using Linear programming and other using random forest method) can be made of the Diesel Generator (DG) run hours for the value of site load at slider.
                The result is displayed below the first scatter plot for 2 models and also on scatter plot (\"yellow\" - linear programming; \"blue\" - random forest). 
                
                Reports from running the analysis are written in the text input boxes.
                
                On the second scatter plot, using the mouse to brush/select points will produce their summary boxplot below the graph. 
                It will also show list of selected sites, slope and intercept of the the selected points in side panel.
                It will also draw a linear model basing on the selected points."
        })
        
        
})
