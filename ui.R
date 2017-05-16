library(googleVis)
library(shiny)
library(shinydashboard)


shinyUI( dashboardPage( skin = "red",
                      
   dashboardHeader(title = "ssGSEA taskforce Version 0.1",
                titleWidth = 350),
                       
   ########################################################################
   # dashboardSidebar definition of the ui
   ########################################################################
   
   dashboardSidebar(
          
           uiOutput("sidebar")
           
   ),#End of dashboardSidebar
   
   
   ########################################################################
   # dashboardBody definition of the ui
   ########################################################################
   
   dashboardBody(
           
           uiOutput("mainbody")
                   
   )#End of dashboardBody
   
   
) #End of dashboardPage

)#End pf ShinyUI