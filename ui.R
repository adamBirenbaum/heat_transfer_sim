library(shiny)
library(ggplot2)
library(shinydashboard)
library(gganimate)




common_path <- "public/project/heat-transfer-sim/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_main <<- paste0("/var/www/adambirenbaum.com/",common_path)
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_main <<- "D:/abire/Documents/heat_transfer_sim/"
}else{
  path_to_main <<- paste0("~/adambirenbaum.com/",common_path)
}



material_df <<- read.csv(paste0(path_to_main,"conduction_material.csv"),stringsAsFactors = F)

header <-  dashboardHeader(
  title = "Heat Transfer Simulation on a Rectangular Plate",titleWidth = "100%"
)
sidebar <- dashboardSidebar(
  # sidebarMenu(
  #   menuItem("Steady-State",tabName = "steady")
  # )
  # 
  NULL
)

body <- dashboardBody(
  
  fluidRow(
    column(width = 5,
           box(title = NULL,status = "primary",solidHeader = F,width = NULL,
               fluidRow(
                 column(width = 4,
                        sliderInput(inputId = "dim_x",label = h4("Width (cm)"),min = 1,max = 100,value = 10),
                        sliderInput(inputId = "dim_y",label = h4("Height (cm)"),min = 1,max = 100,value = 10)
                 ),
                 column(width = 4,
                        sliderInput("n_nodes",h4("# Nodes"), min = 20, max = 1000,value = 100),
                        radioButtons("sim_type",h4("Simulation Type"),choices = c("Steady State", "Transient"),
                                     selected = "Steady State")
                        
                 ),
                 column(width = 4,
                        selectizeInput("material",h4("Material"),choices = material_df$material,selected = "Aluminum"),
                        textOutput("material_conduct"),
                        uiOutput("ui_transient")
                        
                 )
               )
               
           ),
           
           uiOutput("ui_transient_button"),
           
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
           
           
           uiOutput("ui_transient_image"),
           
           
           
           plotOutput("draw_setup")
    )
    
  )
  
  
)




dashboardPage(header, dashboardSidebar(disable = T), body)
