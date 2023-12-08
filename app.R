###############################################
######  UI for clients(PI and students)  ######
######         Author: Li Liu            ######
######         Date: 12-06-2023          ######
###############################################
library(RSQLite)
library(DBI)
library(tidyverse)
library(dbplyr)
library(dplyr)
library(shiny)
library(shinyWidgets)

con = dbConnect(RMySQL::MySQL(),
                            dbname='BCL',
                            host='localhost',
                            port=3306,
                            user='Li',
                            password='***')
dbListTables(con)

# It is a good practice to close connection to a database when you no longer need to read/write data from/to it.
# dbDisconnect(con)

# Insert a new row into the table
# dbExecute(con, "INSERT into client (v_number, project_id, role, name, school, department, title, sex, email) 
#                 VALUES (10003, 1236, 'Student', 'Bob James', 'Medicibe','Family Medicine', '2nd-year MD student','M','bob12@vcu.edu')")


### Retrieve the tables that will be needed 
table_client <- dbGetQuery(con, "select * from client")                     #          delete, add
table_work_on_project  <- dbGetQuery(con, "select p.project_id, p.title, p.date_received, p.deadline, p.p_type, sf.name, sf.email 
                                           from project as p, student_faculty as sf, work_on as wo
                                           where p.project_id = wo.project_id and wo.v_number = sf.v_number") # observe
table_project_progress <- dbGetQuery(con, "select * from project_progress") # observe
table_grants <- dbGetQuery(con, "select * from grants")                     # observe, delete, add
table_output <- dbGetQuery(con, "select * from output")                     # observe, delete, add
### Retrieve the lists that will be needed
vector_project_number  <- dbGetQuery(con,"select project_id from client")
vector_client_v_number <- dbGetQuery(con,"select v_number from client")
vector_grant_number <- dbGetQuery(con, "select number from grants")

###########################################################################
# user interface (ui) object controls the layout and appearance of your app
###########################################################################
ui <- fluidPage(
  titlePanel("Biostatistical Consulting Laboratory"),
  
  sidebarLayout(
   
     sidebarPanel(
      textInput("input_project_number",
                  label = h4("Input your project number")),
      checkboxGroupInput("input_client_role",
                         label = h4("Select the role in your team"),
                         choices = c("PI","Student", "Other"), selected = "PI"),
      checkboxGroupInput("input_progress",
                         label = h4("Select the progress type"),
                         choices = c("SOW finished","SAP finished", "Analysis report finished"), selected = "SOW finished"),
      dateInput("input_date",
                label = h4("See the progress updates of your project after this date"),
                value = "2021-01-01"),
      p("Want to learn more about BCL? Visit the ",a("BCL homepage.", href = "https://biostatistics.vcu.edu/research/bcl/"))
      ),
    
    mainPanel(
      textOutput("output_text"),
      h4("See the basic information of your project"),
      tableOutput("output_selected_project"),
      br(),
      h4("See your team members"),
      tableOutput("output_selected_client"),
      br(),
      h4("Check the progress of your project"),
      tableOutput("output_selected_progress"),
      br(),
      h4("Check the grant status"),
      tableOutput("output_selected_grant"),
      br(),
      h4("Check the output of your project"),
      dataTableOutput("output_selected_output")
      )
    
    )
)
  

######################################################################################
# server function contains the instructions that your computer needs to build your app
######################################################################################
server <- function(input, output) {
  output$output_text <- renderText({paste("You are checking information of project: ", input$input_project_number)})
  output$output_selected_project   <- renderTable({table_work_on_project[table_work_on_project$project_id == input$input_project_number,]})
  output$output_selected_client    <- renderTable({table_client[table_client$project_id == input$input_project_number & table_client$role %in% input$input_client_role,]})
  output$output_selected_progress  <- renderTable({table_project_progress[table_project_progress$project_id == input$input_project_number 
                                                                            & table_project_progress$status %in% input$input_progress 
                                                                            & table_project_progress$date > input$input_date,]})
  output$output_selected_grant     <- renderTable({table_grants[table_grants$project_id == input$input_project_number,]})
  output$output_selected_output    <- renderDataTable({table_output[table_output$project_id == input$input_project_number,]})
}

#############################################################################
# ShinyApp function creates Shiny app objects from an explicit UI/server pair
#############################################################################
shinyApp(ui, server)













