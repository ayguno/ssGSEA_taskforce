library(googleVis)
library(shiny)
library(shinydashboard)

server<-function(input, output, session) {
        
        options(shiny.maxRequestSize=30*1024^2) 
        # this would increase the upload limit to 30MB 
        
        #######################################################################
        # Reactive values to store and access across a session
        #######################################################################
        
        global.values <- reactiveValues(task = NULL,results.gct = NULL,
                                        fdr.gct = NULL)
        
        global.errors <- reactiveValues(analysis.step1 = NULL)
        
        
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
        
        #################################
        # UI definition for analyze.GSEA
        #################################
        
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
                            h3("Step1: Load your data"), br(),
                            fileInput(inputId = "results.gct",width = '400px',
                                      label = "Select to upload your results.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "fdr.gct",width = '400px',
                                      label = "Select to upload your Results-fdr-pvalues.gct file",
                                      multiple = FALSE,
                                      accept = ".gct")
                        )
                        
                        
                        
                )# End of renderUI 
                
        })# End of link_to_analyze.GSEA observer 
        
        ######################
        # In the case of error
        ######################
        observeEvent(global.errors$analysis.step1,
        if(global.errors$analysis.step1 == "error"){
        output$mainbody <- renderUI(

                        box(title = "Welcome to ssGSEA analysis wizard!",status = "primary",
                            background = "navy", width = 12, height = "100%",
                            h3("Step1: Load your data"), br(), 
                            fileInput(inputId = "results.gct",width = '400px',
                                      label = "Select to upload your results.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "fdr.gct",width = '400px',
                                      label = "Select to upload your Results-fdr-pvalues.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            box(title = "Error", status = "danger",
                                background = "navy", width = 12,height = "100%",
                                h2("Your files are not suitable for the analysis, please correct them and reload")
                            )
                        )
                        
                        
                              
               )# End of renderUI
           }
        )# End of analysis.step1.error observer
        
        ######################################
        # Actual computation for analyze.GSEA
        ######################################
        
        # Extract data from results and fdr files
        observe(
       
        # Read results.gct and fdr.gct
        if(!is.null(input$results.gct) & !is.null(input$fdr.gct) ){ 
                cat("Is it working here?")
        # First check the files to decide if they are readable        
        line.gct <- length(readLines(input$results.gct$datapath))        
        line.fdr <- length(readLines(input$fdr.gct$datapath))
        if(line.gct < 4 | line.fdr < 4){
                global.errors$analysis.step1 = "error"       
        }
        else{
        
         results.gct <<- data.frame(MSIG.Gct2Frame(filename = input$results.gct$datapath)$ds,
                                   urls= MSIG.Gct2Frame(filename = input$results.gct$datapath)$descs)
         
         fdr.gct <<- data.frame(MSIG.Gct2Frame(filename = input$fdr.gct$datapath)$ds,
                                    urls= MSIG.Gct2Frame(filename = input$fdr.gct$datapath)$descs)
         
        #Update the global if not null
        if(!is.null(results.gct) | !is.null(fdr.gct) ) {
                global.values$results.gct <- results.gct
                global.values$fdr.gct <- fdr.gct
                }
            }
        }
        
        )
        
        # Initiate the ui change to Step2 
        
        # Compute the plots/heatmaps required for the analysis 
        
}# End of server        