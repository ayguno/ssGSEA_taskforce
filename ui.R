library(googleVis)
library(shiny)
library(shinydashboard)


shinyUI( dashboardPage( skin = "red",
                      
   dashboardHeader(title = "ssGSEA taskforce",
                titleWidth = 300),
                       
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