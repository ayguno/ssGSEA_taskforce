library(googleVis)
library(shiny)
library(shinydashboard)


shinyUI( dashboardPage(skin = "red",
      
   dashboardHeader(title = "ssGSEA taskforce",
                titleWidth = 300),
                       
   ########################################################################
   # dashboardSidebar definition of the ui
   ########################################################################
   
   dashboardSidebar(
           sidebarMenu(id="tabitems",  
                       h5(column(1,{}),icon("power-off"),"Powered by:"),
                       tags$img(src='BroadProteomicsLogo.png', height = 90, width =220 )        
                       
                       
           )#End of sidebarMenu
           
   ),#End of dashboardSidebar
   
   
   ########################################################################
   # dashboardBody definition of the ui
   ########################################################################
   
   dashboardBody(
   
           uiOutput("mainbody")
                   
   )#End of dashboardBody
   
   
) #End of dashboardPage

)#End pf ShinyUI