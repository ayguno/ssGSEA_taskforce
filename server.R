library(googleVis)
library(shiny)
library(shinydashboard)

server<-function(input, output, session) {
        
        options(shiny.maxRequestSize=30*1024^2) 
        # this would increase the upload limit to 30MB 
        
        #######################################################################
        # Reactive values to store and access across a session
        #######################################################################
        
        global.values <- reactiveValues(task = NULL)
        
        
        ######################################################################
        # Entry Page
        ######################################################################
        
        observe(
        if(is.null(global.values$task)){
        output$mainbody <- renderUI(
                box(title="Welcome to ssGSEA taskforce!",status = "primary", 
                    background = "navy",width = 12,height = "100%",
                    h4(column(3,{}),"Do you want to run ssGSEA or analyze existing ssGSEA results?"),
                    column(2,{}),
                    actionLink("link_to_run.GSEA",label = uiOutput("run.GSEA.box",width = 4)),
                    actionLink("link_to_analyze.GSEA",label = uiOutput("analyze.GSEA.box",width = 4))
                    )
        )# End of renderUI
        }
        )
       
        ######################################################################
        # Task : run.GSEA
        ######################################################################
        
        # Box link for run.GSEA
        output$run.GSEA.box <-renderUI({
                valueBox(value="Run ssGSEA",color = "purple", icon = icon("cogs"),
                        subtitle = "Click here to run ssGSEA")
        })
        
        # Actual observer for run.GSEA
        observeEvent(input$link_to_run.GSEA, {
                global.values$task = "run.GSEA"
                output$mainbody <- renderUI(
                        
                        box(title = "Welcome to ssGSEA run wizard!",status = "primary",
                            background = "navy", width = 12, height = "100%",
                            h3("Step1: Load your data")
                            )
                        
                )# End of renderUI 
                
        })# End of link_to_run.GSEA observer                   
        
        
        ######################################################################
        # Task : analyze.GSEA
        ######################################################################
        
        # Box link for analyze.GSEA
        output$analyze.GSEA.box <-renderUI({
                valueBox(value="Analyze ssGSEA",color = "blue", icon = icon("line-chart"),
                         subtitle = "Click here to analyze ssGSEA")
        })
        
        # Actual observer for analyze.GSEA
        observeEvent(input$link_to_analyze.GSEA, {
                global.values$task = "analyze.GSEA"
                output$mainbody <- renderUI(
                        
                        box(title = "Welcome to ssGSEA analysis wizard!",status = "primary",
                            background = "navy", width = 12, height = "100%",
                            h3("Step1: Load your data")
                        )
                        
                )# End of renderUI 
                
        })# End of link_to_analyze.GSEA observer 
        
        
}# End of server        