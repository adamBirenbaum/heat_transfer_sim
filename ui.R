library(shiny)
library(ggplot2)
library(shinydashboard)

material_df <<- read.csv("D:/abire/Documents/heat_transfer_simulations/conduction_material.csv",stringsAsFactors = F)

header <-  dashboardHeader(
  title = "Numerical Methods in Heat Transfer"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Steady-State",tabName = "steady")
  )
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "steady",
            fluidRow(
              column(width = 5,
                     box(title = NULL,status = "primary",solidHeader = F,width = NULL,
                         fluidRow(
                           column(width = 4,
                                  sliderInput(inputId = "dim_x",label = h4("Width (cm)"),min = 1,max = 100,value = 10),
                                  sliderInput(inputId = "dim_y",label = h4("Height (cm)"),min = 1,max = 100,value = 10)
                           ),
                           column(width = 4,
                                  sliderInput("n_nodes",h4("# Nodes"), min = 50, max = 1000,value = 500)
                           ),
                           column(width = 4,
                                  selectizeInput("material",h4("Material"),choices = material_df$material,selected = "Aluminum"),
                                  textOutput("material_conduct")
                                  
                           )
                         )
                         
                     ),
                     box(title = NULL,width = NULL,
                         
                         
                         fluidRow(
                           column(width = 6,
                                  sliderInput("T_up",h6("Temp. Above"), min = -273, max = 1000, value = 100),
                                  sliderInput("h_up",h6("Heat Transfer coef. Above"), min = 0, max = 500, value = 20)
                           ),
                           column(width = 6,
                                  sliderInput("T_down",h6("Temp. Below"), min = -273, max = 1000, value = 22),
                                  sliderInput("h_down",h6("Heat Transfer coef. Below"), min = 0, max = 500, value = 20)
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(width = 6,
                                  sliderInput("T_left",h6("Temp. Left"), min = -273, max = 1000, value = 22),
                                  sliderInput("h_left",h6("Heat Transfer coef. Left"), min = 0, max = 500, value = 20)
                           ),
                           column(width = 6,
                                  sliderInput("T_right",h6("Temp. Right"), min = -273, max = 1000, value = 22),
                                  sliderInput("h_right",h6("Heat Transfer coef. Right"), min = 0, max = 500, value = 20)
                           )
                           
                           
                         )
                         
                         
                         
                     )
                     
                     
              ),
              column(width = 7,
                     plotOutput("steady_state"),
                     br(),

                     h3("Initial Setup"),
                     
                     plotOutput("draw_setup")
              )
              
            )
            
    )
  )
  
)




dashboardPage(header, sidebar, body)
