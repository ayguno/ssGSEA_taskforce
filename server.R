library(googleVis)
library(shiny)
library(shinydashboard)

server<-function(input, output, session) {
        
        options(shiny.maxRequestSize=30*1024^2) 
        # this would increase the upload limit to 30MB 
        
        #######################################################################
        # Reactive values to store and access across a session
        #######################################################################
        
        global.values <- reactiveValues(task = NULL,
                                        input.gct = NULL,
                                        results.gct = NULL,
                                        p.values.gct = NULL,
                                        fdr.gct = NULL,
                                        gene.sets = NULL,
                                        features = NULL,
                                        feature = NULL,
                                        fdr.cutoff = NULL
                                        )
        
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
        
        output$sidebar <- renderUI(
                
                sidebarMenu(id="tabitems",  
                            h5(column(1,{}),icon("power-off"),"Powered by:"),
                            tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 )        
                            
                            
                )#End of sidebarMenu
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
                
                output$sidebar <- renderUI(
                        
                        sidebarMenu(id="tabitems",  
                                    h5(column(1,{}),icon("power-off"),"Powered by:"),
                                    tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 )        
                                    
                                    
                        )#End of sidebarMenu
                )# End of renderUI
                
        })# End of link_to_run.GSEA observer                   
        
        
        ######################################################################
        # Task : analyze.GSEA
        ######################################################################
        
        #################################
        # UI definition for analyze.GSEA
        #################################
        
        #############
        # 
        #  STEP1
        #
        #############
        
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
                            
                            fileInput(inputId = "input.gct",width = '400px',
                                      label = "Select to upload your input.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "results.gct",width = '400px',
                                      label = "Select to upload your results.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "p.values.gct",width = '400px',
                                      label = "Select to upload your pvalues.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "fdr.gct",width = '400px',
                                      label = "Select to upload your Results-fdr-pvalues.gct file",
                                      multiple = FALSE,
                                      accept = ".gct")
                        )
                        
                        
                        
                )# End of renderUI 
                
                output$sidebar <- renderUI(
                        
                        sidebarMenu(id="tabitems",  
                                    h5(column(1,{}),icon("power-off"),"Powered by:"),
                                    tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 )        
                                    
                                    
                        )#End of sidebarMenu
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
                            
                            fileInput(inputId = "input.gct",width = '400px',
                                      label = "Select to upload your input.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "results.gct",width = '400px',
                                      label = "Select to upload your results.gct file",
                                      multiple = FALSE,
                                      accept = ".gct"),
                            fileInput(inputId = "p.values.gct",width = '400px',
                                      label = "Select to upload your pvalues.gct file",
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
        
        
        output$sidebar <- renderUI(
                
                sidebarMenu(id="tabitems",  
                            h5(column(1,{}),icon("power-off"),"Powered by:"),
                            tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 )        
                            
                            
                )#End of sidebarMenu
        )# End of renderUI
        
        
           }
        )# End of analysis.step1.error observer
        
        
        #########################################################
        # 
        #  STEP2: GSEAplot and GSEAheatmap
        #
        #########################################################
        
        #######################################
        #
        #
        # Actual computation for analyze.GSEA
        #
        #
        #######################################
        
        ####################################################################
        # Extract data from gene expression data set, results and fdr files
        ####################################################################
        
        observe(
                
                # Read results.gct and fdr.gct
                if(!is.null(input$results.gct) & !is.null(input$p.values.gct) & !is.null(input$fdr.gct) & !is.null(input$input.gct) ){ 
                        
                        # First check the files to decide if they are readable        
                        line.gct <- length(readLines(input$results.gct$datapath)) 
                        line.p.values <- length(readLines(input$p.values.gct$datapath))
                        line.fdr <- length(readLines(input$fdr.gct$datapath))
                        line.input <- length(readLines(input$input.gct$datapath))
                        
                        if(line.gct < 4 | line.fdr < 4 | line.p.values < 4 | line.input < 4){
                                global.errors$analysis.step1 = "error"       
                        }
                        else{
                                
                                # Work here to read expression input!!
                                input.gct <<- data.frame(MSIG.Gct2Frame(filename = input$input.gct$datapath)$ds)
                                
                                
                                results.gct <<- data.frame(MSIG.Gct2Frame(filename = input$results.gct$datapath)$ds,
                                                           urls= MSIG.Gct2Frame(filename = input$results.gct$datapath)$descs)
                                
                                p.values.gct <<- data.frame(MSIG.Gct2Frame(filename = input$p.values.gct$datapath)$ds,
                                                            urls= MSIG.Gct2Frame(filename = input$p.values.gct$datapath)$descs)
                                
                                fdr.gct <<- data.frame(MSIG.Gct2Frame(filename = input$fdr.gct$datapath)$ds,
                                                       urls= MSIG.Gct2Frame(filename = input$fdr.gct$datapath)$descs)
                                
                                
                                #Update the global if not null
                                if(!is.null(results.gct) | !is.null(fdr.gct) | !is.null(p.values.gct) | !is.null(input.gct) ) {
                                        global.values$input.gct <- input.gct
                                        global.values$results.gct <- results.gct
                                        global.values$p.values.gct <- p.values.gct
                                        global.values$fdr.gct <- fdr.gct
                                        global.values$features <- names(results.gct)[1:(length(names(results.gct))-1)]
                                        global.values$gene.sets <-row.names(global.values$results.gct)
                                        
                                        # When files make sense, Initiate the ui change to Step2 
                                        # Move to the next step once file upload is complete
                                        global.values$task = "analyze.GSEA.step2"
                                }
                        }
                }
                
        )
        
        
        
        #######################################################
        #   
        #  Prepare the analysis outputs for analyze.GSEA.step2 
        # 
        #######################################################
        
        observeEvent(global.values$task,
                
                     ########################################
                     #
                     # UI definition for analyze.GSEA.step2 
                     #
                     ########################################
                     
                     
                if(global.values$task == "analyze.GSEA.step2" ){
                        
                
                        
                        output$mainbody <- renderUI(
                               
                                        tabItems(
                                                        # analyze tab
                                                        tabItem(tabName = "analyze", class = "active",
                                                        box(title="Analyze your ssGSEA data",status = "primary", 
                                                            background = "navy",width = 12,height = "100%",
                                                            h3("Step2: Explore your ssGSEA data by using different tools"),
                                                            actionLink("link_to_GSEAplot",label = uiOutput("GSEAplot.box",width = 4)),
                                                            actionLink("link_to_GSEAheatmap",label = uiOutput("GSEAheatmap.box",width = 4))
                                                                )
                                                        
                                                        ),# End of analyze tab
                                                        
                                                        # GSEAplot tab
                                                        tabItem(tabName = "GSEAplot", 
                                                                box(title="ssGSEAplot",status = "primary",solidHeader = TRUE,
                                                                    background = "navy",width = 11, height = "100%",
                                                                    
                                                                    selectInput("feature",choices = global.values$features,
                                                                                selected = global.values$features[1],
                                                                                label = "Select a sample to display:",width = 300),
                                                                
                                                                plotOutput(outputId = "ssGSEAplot", width = "100%", height = "800px")
                                                                )
                                                        ),# End of GSEAplot tab
                                                        
                                                        # GSEAheatmap tab
                                                        tabItem(tabName = "GSEAheatmap", 
                                                                h5("GSEAheatmap will be here!"),
                                                                box(title="ssGSEAheatmap",status = "primary",solidHeader = TRUE,
                                                                    background = "navy",width = 11, height = "100%",
                                                                    
                                                                    selectInput("features",choices = global.values$features,
                                                                                selected = global.values$features[1:length(global.values$features)],
                                                                                multiple = TRUE,label = "Select samples to display:",width = 300),
                                                                    
                                                                plotOutput(outputId = "ssGSEAheatmap", width="100%", height = "700px")
                                                                )
                                                        )# End of GSEAheatmap tab 
                                                )
                                        
                        )# End of renderUI
                        
                        output$sidebar <- renderUI(

                                sidebarMenu(id="tabitems",  
                                            h5(column(1,{}),icon("power-off"),"Powered by:"),
                                            tags$img(src='BroadProteomicsLogo.png', height = 90, width =220),
                                            menuItem("Analyze ssGSEA", tabName = "analyze",icon = icon("thumbs-o-up"),badgeLabel = "start here",badgeColor = "blue"),
                                            menuItem("GSEA plot", tabName = "GSEAplot"),
                                            menuItem("GSEA heatmap", tabName = "GSEAheatmap"),
                                            sliderInput("FDR",max = 0.25, min = 0.001, value = 0.2,label = "FDR cutoff for Gene Sets:"),
                                            selectInput("gene.set",choices = global.values$gene.sets,
                                                        selected = global.values$gene.sets[1:10], multiple = TRUE,
                                                        label = "Select genesets to filter:")
                                        )#End of sidebarMenu
                                
                        )# End of renderUI
                    
                        # Box link for GSEAplot
                        output$GSEAplot.box <-renderUI({
                                valueBox(value="Generate GSEA plots",color = "blue", icon = icon("line-chart"),
                                         subtitle = "Click here")
                        })
                        
                        
                        observeEvent(input$link_to_GSEAplot, {
                                newvalue <- "GSEAplot"
                                updateTabsetPanel(session, "tabitems",selected= newvalue)
                        })
                        
                        
                        # Box link for GSEAheatmap
                        output$GSEAheatmap.box <-renderUI({
                                valueBox(value="Generate GSEA heatmaps",color = "blue", icon = icon("line-chart"),
                                         subtitle = "Click here")
                        })
                        
                        observeEvent(input$link_to_GSEAheatmap, {
                                newvalue <- "GSEAheatmap"
                                updateTabsetPanel(session, "tabitems",selected= newvalue)
                        })
                        
                        #########################################################################
                        # Compute the plots/heatmaps required for the analysis 
                        #########################################################################
                        
                        ###########################
                        # Prepare the ssGSEAplot
                        ###########################
                        
                        
                        
                        observeEvent(c(input$FDR,input$feature),{
                                ######################################################################
                                # Need to make this fully reactive to feature selection: input$feature
                                #######################################################################
                                withProgress(message = "Updating gene set selection ", value = 1, {
                                        
                                        fdr.cutoff <- input$FDR
                                        feature.index <- which(names(fdr.gct) == global.values$feature)
                                        new.gene.set <- row.names(global.values$fdr.gct)[which(fdr.gct[,feature.index] < fdr.cutoff)]
                                        
                                        updateSelectInput(session,inputId = "gene.set",
                                                          label = "Select genesets to filter:", 
                                                          choices = new.gene.set, selected = new.gene.set[1:10])  
                                }) # End of withProgress

                        })
                        
                        
                       observeEvent(c(input$FDR,input$feature, input$gene.set),{
                               
                                
                               
                               fdr.cutoff <- input$FDR
                               feature <- input$feature
                               gene.set <- input$gene.set 
                               global.values$fdr.cutoff <- fdr.cutoff
                               global.values$feature <- feature 
                                   
                                       
                              withProgress(expr= { 
                               
                        output$ssGSEAplot <- renderPlot({
                               
                                   isolate({
                                        
                                                  
                                           
                                           
                                        input.gct <- global.values$input.gct
                                        results.gct <- global.values$results.gct
                                        p.values.gct <- global.values$p.values.gct
                                        fdr.gct <- global.values$fdr.gct        
                                    
                                        
                                        
                                        cat("--fdr cut off:", fdr.cutoff,"\n")
                                        cat("--feature selected:", feature,"\n")

                                        
                                        
                                        
                                        #Next, aim to make these two user-selectible, enable FDR filtering        
                                        ##################################################################        
                                        feature.index <- which(names(fdr.gct) == feature) # Only one value, selected feature
                                        gene.set.index <- which((fdr.gct[,feature.index] < fdr.cutoff) & (row.names(fdr.gct) %in% gene.set )) # Can be multiple values, selected genesets
                                        ##################################################################
                                        
                                        cat("--feature.index",feature.index,"\n")
                                        cat("--gene.set.index",gene.set.index,"\n")
                                        
                                       
                                               
                                        feature.exp <- input.gct[,feature.index]; names(feature.exp) <- row.names(input.gct)
                                        feature.geneset <- data.frame(gset = row.names(results.gct)[gene.set.index],
                                                                      NES = results.gct[gene.set.index,feature.index],
                                                                      P.value = p.values.gct[gene.set.index,feature.index],
                                                                      FDR = fdr.gct[gene.set.index,feature.index])
                                        
                                        feature.name <- names(input.gct)[feature.index]

     
                                })
                                
                                cat("--",feature.name,"\n") 
                                cat("--",head(feature.exp),"\n") 
                                
                                generate.GSEAplot(feature.name,feature.exp,feature.geneset,genesets)
                                cat("--GSEAplot executed\n")
                                
                             
                        })
                              },message = "Preparing ssGSEAplot")
                        
                        
                       },ignoreNULL = FALSE) # End of c(input$fdr,input$feature) for the reactive ssGSEAplot
                       
                       
                        ###########################
                        # Prepare the ssGSEAheatmap
                        ###########################
                        output$ssGSEAheatmap <- renderPlot({
                                
                                
                                isolate({
                                        
                                        
                                        ####################
                                        # Dev. purpose only
                                        ####################
                                        
                                        
                                        #Next, aim to make these two user-selectible, enable FDR filtering        
                                        ##################################################################        
                                        feature.index <- 1:(ncol(results.gct)-1) # Can be one value or all available features
                                        gene.set.index <- 1:nrow(results.gct) # Can be multiple values, selected genesets
                                        ##################################################################
                                        
                                        results.gct <- global.values$results.gct
                                        fdr.gct <- global.values$fdr.gct
                                        
                                        
                                        sub.results.gct <- results.gct[gene.set.index,feature.index]
                                        
                                        FDR.cut.off <- ""
                                        
                                        
                                })
                                
                                
                                generate.ssGSEAheatmap(sub.results.gct, cluster.rows = FALSE, 
                                                       cluster.columns = FALSE, scale = "none",
                                                       FDR.cut.off = "")
                                cat("--Heatmap executed\n")
                                
                                
                        })      
                        
                }
        )# End of analyze.GSEA.step2 observer
        

        
       
        
        
        
      
        
                        
        
         
       
        
}# End of server        