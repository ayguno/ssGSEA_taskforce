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
                                           fluidRow(     
                                                box(title = "Welcome to ssGSEA run wizard!",status = "primary",
                                                    background = "navy", width = 6, height = "100%",
                                                    h4("Load your data and define parameters."),
                                                    
                                                    h6(icon("exclamation-triangle"),"First column (Name) must contain gene symbols."),
                                                fluidRow(
                                                column(6,        
                                                    fileInput(inputId = "input.gct.ssGSEA",width = '200px',
                                                              label = "Select to upload your input.gct file:",
                                                              multiple = FALSE, accept = ".gct")
                                                      ),column(6,
                                                               br(),br(),
                                                    actionButton(inputId = "next.step", label = "Next step")
                                                      )
                                                ),  
                                                    hr(),
                                                    h6(icon("exclamation-triangle"),"Your results can be e-mailed to you."), 
                                                    textInput(inputId = "email.address", width = "400px",
                                                          label = "Provide a valid e-mail address:"),
                                                          
                                                    hr(),
                                                    textInput(inputId = "output.prefix", width = "400px",
                                                              label = "Give a name for your project:",
                                                              value = "My.ssGSEA.Output"),
                                                    hr(),
                                                    h6(icon("exclamation-triangle"),"Currently defaults to: MSigDB C2."),
                                                    selectInput(inputId = "gene.set.databases", width = "400px",
                                                                label = "List of genesets (in gmt format) to evaluate enrichment on",
                                                                selected = 'c2.all.v4.0.symbols.gmt',choices = 'c2.all.v4.0.symbols.gmt' ),
                                                    hr(),
                                                    h6(icon("exclamation-triangle"),"By default it will include ALL gene sets available."),
                                                    textAreaInput(inputId  = "gene.set.selection", width = "400px",
                                                                  label = "List with names of gene sets",
                                                                  value = "ALL"),
                                                    hr(),
                                                    selectInput(inputId = "sample.norm.type", width = "400px",
                                                                label = "Sample Normalization Type",
                                                                selected = "rank", choices = c("rank", "log", "log.rank","none")),
                                                    hr(),
                                                    h6(icon("exclamation-triangle"),"When weight is equal to 0, all genes have the same weight; if weight is >0, 
                                                       actual values matter, and can change the resulting score"),
                                                    numericInput(inputId = "weight", width = "100px",
                                                                 label = "Weight", value = 0)
                        
                                                ), # End of the "Welcome to ssGSEA run wizard!" box
                                                
                                                box(title = "Tips for running ssGSEA:Click Here ->",status = "success",
                                                    background = "navy", width = 6, height = "100%",collapsible = TRUE,
                                                    collapsed = TRUE,
                                                    
                                                    h5("For results similar to the Java version:Use weight=0"),
                                                    h5(icon("exclamation-triangle"),"when weight=0, sample.norm.type and correl.type do not matter"),
                                                    h5(icon("exclamation-triangle"),"when weight > 0, the combination of sample.norm.type and correl.type
                                                           dictate how the gene expression values in input.ds are transformed
                                                           to obtain the score -- use this setting with care (the transformations
                                                           can skew scores towards +ve or -ve values)"),
                                                    h5(icon("thumbs-o-up"),"sample.norm.type='none' uses actual expression values; 
                                                       combined with correl.type ='rank', genes are weighted by actual values"),
                                                    h5(icon("thumbs-o-up"),"sample.norm.type ='rank' weights genes proportional to rank"),
                                                    h5(icon("thumbs-o-up"),"sample.norm.type ='log' can be used for log-transforming input data"),
                                                    h5(icon("thumbs-o-up"),"correl.type ='z.score' standardizes the (normalized) input values before using them to calculate scores")
                                                  
                                                        
                                                ),
                                                
                                                box(status = "primary",solidHeader = FALSE,
                                                    background = "navy", width = 6, height = "100%",
                                                    
                                                    selectInput(inputId = "statistic", width = "400px",
                                                                label = "Test statistic",
                                                                selected = "area.under.RES", 
                                                                choices = c("Kolmogorov-Smirnov", "area.under.RES")),
                                                    hr(),
                                                    h6(icon("exclamation-triangle"),"ES: Enrichment Score",br(),"NES: Normalized Enrichment Score"),
                                                    selectInput(inputId = "output.score.type", width = "400px",
                                                                label = "Output score type",
                                                                selected = "NES",choices = c("ES","NES")),
                                                    sliderInput(inputId = "nperm", width = "400px",
                                                                label = "For NES output: number of random permutations",
                                                                min = 100, max = 1000, value = 1000),
                                                    hr(),
                                                    h6(icon("exclamation-triangle"),"combine.off: do not combine *_UP and *_DN versions in a single score",br(),
                                                       "combine.replace: combine *_UP and *_DN versions in a single score",br(),
                                                       "combine.add: combine *_UP and *_DN versions in a single score and add it but keeping the individual *_UP and *_DN versions."),
                                                    selectInput(inputId = "combine.mode",width = "200px",
                                                                label = "Combination Mode", selected = "combine.off",
                                                                choices = c("combine.off", "combine.replace", "combine.add")
                                                                ),
                                                    hr(),
                                                    selectInput(inputId = "correl.type", label = "Correlation type",width = "200px",
                                                                selected = "rank", choices = c("rank", "z.score", "symm.rank")),
                                                    hr(),
                                                    radioButtons(inputId = "global.fdr",label = "FDR adjustment", 
                                                                 choices = c("Calculate FDR sample-by-sample",
                                                                             "Calculate global FDR"),
                                                                 selected = "Calculate FDR sample-by-sample")
                                                    
                                                        
                                                )
                                                
                                           )        
                                               
                                        )# End of renderUI output$mainbody
                                        
                
                                        
                
                                        output$sidebar <- renderUI(
                                                
                                                sidebarMenu(id="tabitems",  
                                                            h5(column(1,{}),icon("power-off"),"Powered by:"),
                                                            tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 )        
                                                            
                                                            
                                                )#End of sidebarMenu
                                        )# End of renderUI output$sidebar
                
                
                
                
                
                ##################################
                #
                # Actual computation for ssGSEA
                #
                ##################################
                observeEvent(input$next.step,{
                        
                        # Read input.gct
                        if(!is.null(input$input.gct.ssGSEA) ){ 
                                
                                # First check the file to decide if it is readable        
                        
                                line.input <- length(readLines(input$input.gct.ssGSEA$datapath))
                               
                                if( line.input < 4){
                                        # If input doesn't make sense
                                        # Throw an error to enforce user
                                        # global.errors$analysis.step1 = "error"       
                                }
                                else{
                                        # If input makes sense
                                        # read expression input and other parameters
                                        input.gct <<- data.frame(MSIG.Gct2Frame(filename = input$input.gct.ssGSEA$datapath)$ds)
                                        
                                        output.prefix <- input$output.prefix
                                        gene.set.databases <- input$gene.set.databases
                                        gene.set.selection <- input$gene.set.selection
                                        sample.norm.type <- input$sample.norm.type
                                        weight <- input$weight
                                        statistic <- input$statistic
                                        output.score.type <- input$output.score.type
                                        nperm <- input$nperm
                                        combine.mode <- input$combine.mode
                                        correl.type <- input$correl.type
                                        global.fdr <- input$global.fdr
                                        
                                        user.directory <- paste(APPNAME,gsub(" |:|-","_",output.prefix),gsub(" |:|-","_",Sys.time()),sep = "_")
                                        
                                        
                                        user.email <- input$email.address
                                        ###########################
                                        #
                                        # Re-structure UI 
                                        #
                                        ###########################
                                        
                                        output$mainbody <- renderUI({
                                            
                                                fluidRow(shinyjs::useShinyjs(),
                                                column(1,{}),         
                                                box(title= "You are about to run ssGSEA with the selected parameters"
                                                    ,status = "primary",
                                                background = "navy", width = 10, height = "100%",
                                                
                                        fluidRow(
                                        column(6,        
                                                h5(icon("tags"),"Project Name:",column(1,{}),output.prefix),
                                                h5(icon("tags"),"Gene Set Database:",column(1,{}),gene.set.databases),
                                                h5(icon("tags"),"Selected Gene Sets:",column(1,{}),gene.set.selection),
                                                h5(icon("tags"),"Sample Normalization Type:",column(1,{}),sample.norm.type),
                                                h5(icon("tags"),"Weight:",column(1,{}),weight),
                                                h5(icon("tags"),"Test Statistic:",column(1,{}),statistic),
                                                h5(icon("tags"),"Type of Output Score:",column(1,{}),output.score.type),
                                                h5(icon("tags"),"Number of Permutations:",column(1,{}),nperm),
                                                h5(icon("tags"),"Combination mode:",column(1,{}),combine.mode),
                                                h5(icon("tags"),"Correlation Type:",column(1,{}),correl.type),
                                                h5(icon("tags"),"FDR Calculation mode:",column(1,{}),global.fdr)
                                        ), column(6, 
                                                br(),br(),br(),
                                                h4(icon("hourglass-start"), "Ready to start?"),
                                                br(),
                                                actionButton(inputId = "run.ssGSEA",label = "Run ssGSEA")
                                                )
                                        )    
                                                
                                                
                                                ),
                                                column(2,{}),
                                                 
                                                box(title= "Realtime ssGSEA Run Console",status = "primary",
                                                     background = "navy", width =8, height = "100%",
                                                
                                                
                                                    
                                                wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 300px",
                                                          textOutput("text",container = pre))
                                                 ),
                                                column(2,{}) 
                                                )
      
                                         })# End of renderUI mainbody
                                         
                                        
                                        

                                        
                                         observeEvent(input$run.ssGSEA,{
                                                 
                                                 #############
                                                 # Run ssGSEA
                                                 #############
                                                 
                                                 ###################################################
                                                 # Next step: configure e-mail interaction with user
                                                 ###################################################

                                                 dir.create(paste0("./",user.directory))
                                                 setwd(paste0("./",user.directory))
                                                
                                                 withProgress({

                                                        withCallingHandlers({
                                                                shinyjs::html("text", "")
                                                                
                                                                message(paste("Your ssGSEA job ID is:",user.directory, "\n", sep = " "))
                                                                
                                                                if(!is.null(user.email)){
                                                                message(paste("Your ssGSEA results will be e-mailed to:",user.e-mail, "\n", sep = " "))        
                                                                }
                                                                
                                                                # ssGSEA(input.ds = input$input.gct.ssGSEA$datapath,
                                                                #        'Combined_.gct_Results',
                                                                #        gene.set.databases='./c2.all.v4.0.symbols.gmt',
                                                                #        sample.norm.type="rank",
                                                                #        weight=0,
                                                                #        nperm=1000,
                                                                #        min.overlap=10,
                                                                #        correl.type='z.score')
                                                        ssGSEA(        
                                                                input.ds = input$input.gct.ssGSEA$datapath,                      
                                                                output.prefix = output.prefix,                
                                                                gene.set.databases= '../c2.all.v4.0.symbols.gmt',  
                                                                gene.set.selection  = gene.set.selection,  
                                                                sample.norm.type    = sample.norm.type,  
                                                                weight              = weight,      
                                                                statistic           = statistic,
                                                                output.score.type   = output.score.type,   
                                                                nperm               = nperm,    
                                                                combine.mode        = combine.mode,  
                                                                min.overlap         = 10,
                                                                correl.type         = correl.type, 
                                                                fdr.pvalue          = TRUE,    
                                                                global.fdr          = ifelse(global.fdr == "Calculate FDR sample-by-sample", FALSE,TRUE)    
                                                        )
                                                        
                                                        message(paste("Completed Job ID:",user.directory, "\n", sep = " "))
                                                        
                                                        if(!is.null(user.email)){
                                                                message(paste("E-mailing your ssGSEA results to:",user.e-mail, "\n", sep = " "))        
                                                        }        
                                                        
                                                        },
                                                        message = function(m) {
                                                                shinyjs::html(id = "text", html = m$message, add = TRUE)
                                                        })
                                                        
                                                                
                                                        
                                                }, message = "Running ssGSEA, be patient...")
                                              
                                                setwd("../")
                                                   
                                         })        
                                        
                                } # end of: else line.input > 4 control
                                
                        }# end of: if(!is.null(input$input.gct) )       
                        
                })
                
                
                
                
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
        
        observeEvent(global.values$task,{
                
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
                                                                box(title="ssGSEAplot (adapted from Karsten Krug)",status = "primary",solidHeader = TRUE,
                                                                    background = "navy",width = 11, height = "100%",
                                                             
                                                                column(6,       
                                                                    selectInput("feature",choices = global.values$features,
                                                                                selected = global.values$features[1],
                                                                                label = "Select a sample to display:",width = 300)
                                                                ),
                                                                
                                                                column(6,
                                                                    h5("Do you like what you generated?"),
                                                                    downloadButton(outputId = "download.ssGSEAplot",label = "Download PDF") 
                                                                ),
                                                                
                                                                plotOutput(outputId = "ssGSEAplot", width = "100%", height = "800px")
                                                                )
                                                        ),# End of GSEAplot tab
                                                        
                                                        # GSEAheatmap tab
                                                        tabItem(tabName = "GSEAheatmap", 
                                                                
                                                                box(title="Sample selection",status = "primary",solidHeader = TRUE,
                                                                    background = "navy",width = 6, height = "100%",
                                                                 
                                                                     radioButtons("all.features",label = "Which samples you want to use?",
                                                                                 choices = c("Use all samples","Filter samples below"),
                                                                                 selected = "Use all samples" ),
                                                                    
                                                                     selectInput("features",choices = global.values$features,
                                                                                selected = global.values$features[1:2],
                                                                                multiple = TRUE,label = "Select samples to display:",width = 300)
                                                                    ),
                                                                
                                                                box(title="Clustering and scaling options",status = "primary",solidHeader = TRUE,
                                                                    background = "navy",width = 6, height = "100%",
                                                                    
                                                                    column(6,
                                                                    radioButtons("cluster.columns",label = "Cluster samples(columns)?",
                                                                                 choices = c("No","Cluster columns"),
                                                                                 selected = "No"),
                                                                    
                                                                    radioButtons("cluster.rows",label = "Cluster gene sets(rows)?",
                                                                                 choices = c("No","Cluster rows"),
                                                                                 selected = "No"),
                                                                    
                                                                    radioButtons("scaling",label = "Scale and center rows or columns?",
                                                                                 choices = c("none","row","column"),
                                                                                 selected = "none")
                                                                    ),
                                                                    
                                                                    #column(1,{}),
                                                        
                                                                    column(6,
                                                                            
                                                                            h5("Do you like what you generated?"),
                                                                            downloadButton(outputId = "download.heatmap",label = "Download PDF")    
                                                                            
                                                                    )
                                                                    
                                                                    ),
                                                                
                                                                    br(),
                                                                
                                                                box(title="ssGSEAheatmap: Heatmap of Normalized Enrichment Scores",status = "primary",solidHeader = TRUE,
                                                                    background = "navy",width = 12, height = "100%",
                                                                   
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
                                            sliderInput("FDR",max = 0.25, min = 0.001, value = 0.2,label = "Global FDR cutoff for Gene Sets:"),
                                            radioButtons("all.gene.sets",label = "Which gene sets you want to use?",
                                                         choices = c("Use selected gene sets","Use all genesets"),
                                                         selected = "Use selected gene sets"),
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
                             
                                withProgress(message = "Updating gene set selection ", value = 1, {
                                        
                                        fdr.cutoff <- input$FDR
                                        feature.index <- which(names(fdr.gct) == input$feature)
                                        fdr.gct <- global.values$fdr.gct
                                        
                                        new.gene.set <- row.names(fdr.gct)[which(fdr.gct[,feature.index] < fdr.cutoff)]
                                        display.length <- ifelse(length(new.gene.set)>10,10,length(new.gene.set))
                                        
                                        global.values$gene.set <- new.gene.set 
                                        
                                        
                                        updateSelectInput(session,inputId = "gene.set",
                                                          label = "Select genesets to filter:", 
                                                          choices = new.gene.set, selected = new.gene.set[1:display.length])  
                                }) # End of withProgress

                        })
                        
                        
                       observeEvent(c(input$FDR,input$feature, input$gene.set,input$all.gene.sets ),{
                               
                                
                               
                               fdr.cutoff <- input$FDR
                               feature <- input$feature
                               gene.set <- input$gene.set 
                               global.values$fdr.cutoff <- fdr.cutoff
                               global.values$feature <- feature 
                               all.gene.sets <- input$all.gene.sets    
                                       
                          withProgress(expr= { 
                               
                             output$ssGSEAplot <- renderPlot({
                               
                                   plotter <<- function(){  
                                     
                                                   isolate({
                                                        
                                                                  
                                                           
                                                           
                                                        input.gct <- global.values$input.gct
                                                        results.gct <- global.values$results.gct
                                                        p.values.gct <- global.values$p.values.gct
                                                        fdr.gct <- global.values$fdr.gct        
                                                    
                                                        
                                                        
                                                        cat("--fdr cut off:", fdr.cutoff,"\n")
                                                        cat("--feature selected1:", feature,"\n")
                                                      
                                                        # these are two key user-selectible attributes, enable FDR filtering        
                                                        ##################################################################        
                                                        feature.index <- which(names(fdr.gct) == feature) # Only one value, selected feature
                                                        
                                                        if(all.gene.sets == "Use all genesets"){
                                                                gene.set.index <- which(fdr.gct[,feature.index] < fdr.cutoff)         
                                                        } else{
                                                                gene.set.index <- which(row.names(fdr.gct) %in% gene.set ) # Can be multiple values, selected genesets
                                                        }
                                                        
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
                                   
                                        }# End of the plotter() definition
                                   
                                   plotter() # Call plotter once to print the plot on the screen
                                   
                                   
                             
                                })  # End of ssGSEAplot renderPlot
                        
                           },message = "Preparing ssGSEAplot")
                          
                          output$download.ssGSEAplot <- downloadHandler(
                                  
                                  filename = function(){
                                          paste(APPNAME,"ssGSEAplot",gsub(" |:|-","_",Sys.time()),".pdf",sep = "_")
                                  },
                                  
                                  content = function(file){
                                          pdf(file = file, width = 12, height = 14)
                                          plotter() # Acccess plotter here to print into pdf device to reactive download
                                          dev.off()}
                                  
                                  
                          )# End of the ssGSEA downloadhandler
     
                         },ignoreNULL = FALSE) # End of c(input$fdr,input$feature) for the reactive ssGSEAplot
                       
                      
                        observeEvent(c(input$FDR,input$features),{
                            
                               withProgress(message = "Updating gene set selection ", value = 1, {
                                       all.features <- input$all.features
                                       features <- input$features
                                       fdr.cutoff <- input$FDR
                                       if(all.features == "Use all samples"){
                                               features.index <- names(fdr.gct)[1:(length(names(fdr.gct))-1)]
                                       }else{
                                               features.index <- which(names(fdr.gct) %in% features) # Can be one value or all available features  
                                       }
                                       new.gene.set <- row.names(global.values$fdr.gct)[which(fdr.gct[,features.index] < fdr.cutoff)]
                                       display.length <- ifelse(length(new.gene.set)>10,10,length(new.gene.set))
                                       
                                       global.values$gene.set <- new.gene.set 
                                       
                                       
                                       updateSelectInput(session,inputId = "gene.set",
                                                         label = "Select genesets to filter:", 
                                                         choices = new.gene.set, selected = new.gene.set[1:display.length])  
                               }) # End of withProgress
                               
                       })   
                       
                        observeEvent(input$all.features,{
                                all.features <- input$all.features
                                if(all.features == "Use all samples"){
                                        
                                        updateSelectInput(session, inputId = "features",choices = global.values$features,
                                                    selected = NULL,label = "Select samples to display:")
                                        
                                }else{
                                        updateSelectInput(session, inputId ="features",choices = global.values$features,
                                                    selected = global.values$features[1:2],label = "Select samples to display:")
                                                   
                                }
                                
                        })        
             
                        
                        
                        ###########################
                        # Prepare the ssGSEAheatmap
                        ###########################           
                                        
             observeEvent(c(input$FDR,input$features,input$gene.set,input$all.features,
                            input$all.gene.sets,input$cluster.columns,input$cluster.rows,
                            input$scaling),{ 
                     
                     
                     
                 output$ssGSEAheatmap <- renderPlot({
                                 
                         heatmapper <<- function(){  
                                 
                                                 isolate({
                                                         
                                                         fdr.cutoff <- input$FDR
                                                         all.features <- input$all.features
                                                         features <- input$features
                                                         global.values$fdr.cutoff <- fdr.cutoff
                                                         all.gene.sets <- input$all.gene.sets  
                                                         cluster.columns <- ifelse(input$cluster.columns == "No" , FALSE, TRUE)
                                                         cluster.rows <- ifelse(input$cluster.rows == "No" , FALSE, TRUE)
                                                         scaling <- input$scaling
                                                         
                                                         input.gct <- global.values$input.gct
                                                         results.gct <- global.values$results.gct
                                                         p.values.gct <- global.values$p.values.gct
                                                         fdr.gct <- global.values$fdr.gct
                                                         gene.set <- input$gene.set
                                                          
                                                         
                                                         cat("--fdr cut off:", fdr.cutoff,"\n")
                                                         cat("--feature selected2:", features,"\n")
                                                         
                                                         cat("--all.features:", all.features,"\n")
                                                         
                                                         #Next, aim to make these two user-selectible, enable FDR filtering        
                                                         ##################################################################        
                                                         if(all.features == "Use all samples"){
                                                                 features.index <- names(fdr.gct)[1:(length(names(fdr.gct))-1)]
                                                         }else{
                                                                 features.index <- which(names(fdr.gct) %in% features) # Can be one value or all available features  
                                                         }        
                                                         
                                                         ########################################################
                                                         # Pick any gene set if it is lower than the specified
                                                         # FDR cut off in any of the specified features
                                                         ########################################################
                                                         
                                                         cat("--features index:", features.index,"\n")
                                                         # Make the selection matrix
                                                         
                                                         temp.select <- apply(fdr.gct[,features.index],2, function(x) x < fdr.cutoff)
                                                         temp.select.any <- apply( temp.select,1, function(x) any(x))
                                                         
                                                         if(all.gene.sets == "Use all genesets"){
                                                                 gene.set.index <- which(temp.select.any)         
                                                         } else{
                                                                 gene.set.index <- which(row.names(fdr.gct[,features.index]) %in% gene.set ) # Can be multiple values, selected genesets
                                                         }
                                                         
                                                          # Can be multiple values, selected genesets
                                                         ##################################################################
                                                         
                                                         
                                                         
                                                         
                                                         sub.results.gct <<- results.gct[gene.set.index,features.index]
                                                         
                                                         cat("--sub.results.gct nrows: ",nrow(sub.results.gct),"\n")
                                                         cat("--sub.results.gct ncols: ",ncol(sub.results.gct),"\n")
                                                         
                                                 })
                                                 
                                                 
                                                 generate.ssGSEAheatmap(sub.results.gct, cluster.rows = cluster.rows, 
                                                                        cluster.columns = cluster.columns, scale = scaling,
                                                                        FDR.cut.off = fdr.cutoff)
                                                 cat("--Heatmap executed\n")
                                 
                         } #End of the heatmapper() definition 
                         
                         heatmapper() # Call heatmapper() to generate the heatmap on the screen
                                 
                         }) # End of ssGSEAheatmap renderPlot
                 
                 output$download.heatmap <- downloadHandler(
                         
                         filename = function(){
                                 paste(APPNAME,"ssGSEAheatmap",gsub(" |:|-","_",Sys.time()),".pdf",sep = "_")
                         },
                         
                         content = function(file){
                                 pdf(file = file, width = 18, height = 10)
                                 heatmapper() # Acccess heatmapper here to print into pdf device to reactive download
                                 dev.off()}
                         
                         
                 )# End of the ssGSEA heatmap downloadhandler
                         
                         
                   },ignoreNULL = FALSE) # End of c(input$fdr,input$features) for the reactive ssGSEAheatmap     
                    
                 
                                 
                }# End of: if(global.values$task == "analyze.GSEA.step2" )
                
        })# End of: analyze.GSEA.step2 observer
        

        
       
        
        
        
        
}# End of server        