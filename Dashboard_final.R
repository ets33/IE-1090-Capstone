#install.packages("shiny")
#install.packages("dplyr")

library(reshape)
library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
  
  num_rows <- 74
  num_columns <- 10
  random_values <- matrix(sample(0:100, num_rows * num_columns, replace = TRUE), ncol = num_columns)
df <- data.frame(
  hospital = rep(c("Presbyterian", "Shadyside"), c(48, 26)),
  location = c('10C', '10F', '11F', '9F', '3F', '4F', '4G', '5F', '5S', '6F', '6GF','CT10', 'CT11', 'SICU', '3E', '4D', '5D', '7D', '9D', '10D', '10G', '5G', '6D', '8D', '8G', '743F', '7F', '12D', '9G', '7G', '2D', '8N', '10E', '10W', '10S', '11S', '12S', '12N', '8W', '9N', '10N', '11N', '5W', '5E', '6NE', 'R7SN', 'R7SS', 'RHAB', '2P', '2S','3E','3M', '3P', '3W', '4E','4M','4P','4S','4WB','5M','5P','6M','6P','7P','6W','7M','7W','ICTA','ISWA','ISWB','ICTB','INS','IMS','I1MT'),
  q1 = random_values[, 1],
  q2 = random_values[, 2],
  q3 = random_values[, 3],
  q4 = random_values[, 4],
  q5 = random_values[, 5],
  q6 = random_values[, 6],
  q7 = random_values[, 7],
  q8 = random_values[, 8],
  q9 = random_values[, 9],
  q10 = random_values[, 10])
  


#In Unit Questionnaire
moduleServer <- function(id, module) {
  callModule(module, id)
}

button_ui <- function(id) {
  actionButton(NS(id, "btn"), label = "Finish")
}

button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$btn)
  })
}

#Unit Stay Questions
module_server <- function(id, button) {
  moduleServer(id, function(input, output, session) {
    modal <- modalDialog(
      h4("Questionnaire"),
      radioButtons("q1", "1. Was the Nurse/HUC notified of the patients ESO status?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q2", "2.  Was EVS notified of the terminal clean?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q3", "3. Did off unit team recieve cleaning slip?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q4", "4. Did the Nurse/HUC complete the ticket to ride?", choices = c("Yes", "No"), selected = "No"),
      footer = actionButton(NS(id, "submit"), label = "Submit"),
      textInput("comments", "5. If answered 'No' to any of the above, elaborate below:")
    )
    observeEvent(input$submit, {
      removeModal()
    })
    observeEvent(button(), {
      showModal(modal)
    })
  })
}

#sign in Questionnaire pop ups
moduleServer_signin <- function(id_signin, module_signin) {
  callModule(module_signin, id_signin)
}

button_ui_signin <- function(id_signin) {
  actionButton(NS(id_signin, "btn_signin"), label = "Sign In")
}

button_server_signin<- function(id_signin) {
  moduleServer(id_signin, function(input, output, session) {
    reactive(input$btn_signin)
  })
}

module_server_signin <- function(id_signin, button_signin) {
  moduleServer(id_signin, function(input, output, session) {
    modal_signin <- modalDialog(
      h4("Sign in Questionnaire"),
      textInput("entrant", "Entrant: "),
      textInput("role", "Role: "),
      textInput("equipment", "Reusable medical equipment used:"),
      footer = actionButton(NS(id_signin, "submit_signin"), label = "Submit")
      
    )
    observeEvent(input$submit_signin, {
      removeModal()
    })
    observeEvent(button_signin(), {
      showModal(modal_signin)
    })
  })
}

#Sign Out Questionnaire
moduleServer_signout <- function(id_signout, module_signout) {
  callModule(module_signout, id_signout)
}

button_ui_signout <- function(id_signout) {
  actionButton(NS(id_signout, "btn_signout"), label = "Sign Out")
}

button_server_signout<- function(id_signout) {
  moduleServer(id_signout, function(input, output, session) {
    reactive(input$btn_signout)
  })
}

module_server_signout <- function(id_signout, button_signout) {
  moduleServer(id_signout, function(input, output, session) {
    modal_signout <- modalDialog(
      h4("Sign Out Questionnaire"),
      textInput("entrant_signout", "Entrant: "),
      radioButtons("q5", "1.  Did entrant dawn gowning upon entry? ", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q6", "2.  Did entrant doff gowning upon exit?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q7", "3. Did entrant perform proper handwashing required upon entry?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q8", "4. Did entrant perform proper handwashing required upon exit?", choices = c("Yes", "No"), selected = "No"),
      radioButtons("q9", "5. Was reusable medical equipment properly cleaned after use on patient?", choices = c("Yes", "No"), selected = "No"),
      footer = actionButton(NS(id_signout, "submit_signout"), label = "Submit"),
      textInput("commentsignout", "6. If answered 'No' to any of the above, elaborate below:")
      
    )
    observeEvent(input$submit_signout, {
      removeModal()
    })
    observeEvent(button_signout(), {
      showModal(modal_signout)
    })
  })
}

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = "#3E1151",
    fg = "#EBE8E5",
    base_font = "Helvetica"
  ),
  navbarPage("UPMC App",
             tabPanel("Observer Portal",
                      
                      fluidRow(
                        
                        
                        column(4,
                          # Hospital dropdown menu
                          selectInput("hospital", 
                                      label = "Choose Hospital", 
                                      choices = sort(unique(df$hospital)),
                                      multiple = F),
                          uiOutput('location'),
                          selectInput("PatientStatus", "Select Patient Status:", 
                                      choices = c("In Unit", "In Transport"),
                                      selected = "In Unit"),
                          
                          textInput("HCW", "HCW in Charge:"),
                          actionButton("patientstay", "Begin Patient Stay"),
                          
                          
                          #output of Begin Patient Stay button
                          textOutput("outhosp"),
                          textOutput("outloc"),
                          textOutput("outstat"),
                          textOutput("outHCW"),
                          #finish button
                          uiOutput("button1")
                        ),
                        #key reminders
                        column(8,
                               titlePanel("Key Reminders"),
                               conditionalPanel(
                                 condition = "input.PatientStatus == 'In Unit'",
                                 h5(textOutput("textUnit", container = pre))),
                               conditionalPanel(
                                 condition = "input.PatientStatus == 'In Transport'",
                                 h5(textOutput("textTransport", container = pre)))
                      ),
                      #sign in sheet
                      fluidRow(
                        column(6,
                               titlePanel("Sign In Sheet"),
                               button_ui_signin("mod1_signin"),
                              button_ui_signout("mod1_signout"),
                              ),
                        column(6,
                               titlePanel("ESO Info Guide"),
                               )
                      ),
                      fluidRow(
                        column(6,),
                        column(6,
                               actionButton("reportlapse", "Report A Lapse"))
                        
                      )
             ),
             ),
             tabPanel("Tracking Portal",
                      selectInput("hospitaltrack", 
                                  label = "Choose Hospital", 
                                  choices = sort(unique(df$hospital)),
                                  multiple = F),
                      #graph output based off hospital
                      plotOutput("heatmap")
  )
))

# Define server
server <- function(input, output, session) {
  
  
  #Key Reminder Text based off Patient Status
  output$textUnit <- shiny::renderText({
    print("- Hand Hygiene, with hand sanitizer or soap/water,is 
effective at preventing transmission.
- Maintain Standard and Contact Precautions appropriately
to reduce risk of exposure of blood and bodily fluids
- HCWs having contact with the patient should be limited
to only those providing necessary care
- If possible, dedicate reusable medical equipment
(WOW, vitals machine, HD support, etc.) to the patient's 
room ")
  })
  output$textTransport <- shiny::renderText({
    print("- You will always travel with the patient 
during their admission:
- You will handle the patient's chart and not allow it 
on the bed/stretcher or wheelchair.
- You will communicate to the new location your role and
record name of HCW in charge
- Alert EVS of terminal clean requirement at off unit 
destinations
	. SHY EVS: 412-623-2112
	. PUH EVS: 412-647-6997
	. MUH EVS: 412-648-6002")
  })
  

  #location UI output
  output$location = renderUI({
    req(input$hospital)
    
    selectInput("location_choice", 
                label = "Choose Unit:", 
                choices = df$location[df$hospital == input$hospital]
    )
  })
  
  #Sign in Button activation
  button_signin <- button_server_signin("mod1_signin")
  module_server_signin("mod_2_signin", button_signin)
  
  #Sign Out Button activation
  button_signout <- button_server_signout("mod1_signout")
  module_server_signout("mod_2_signout", button_signout)
  
  #Finish Button Activation
  button <- button_server("mod1")
  module_server("mod_2", button)
  
 
  
  #Display Current Patient Tracking status after Begin Patient Stay Button Hit
  observeEvent(input$patientstay, {
  stringone <- reactive(paste0("Hospital: ", input$hospital))
  output$outhosp <- renderText(stringone())
  
  stringtwo <- reactive(paste0("Location: ", input$location_choice))
  output$outloc <- renderText(stringtwo())
  
  stringthree <- reactive(paste0("Patient Status: ", input$PatientStatus))
  output$outstat <- renderText(stringthree())
  
  stringfour <- reactive(paste0("HCW in Charge: ", input$HCW))
  output$outHCW <- renderText(stringfour())

  output$button1 <- renderUI({
    button_ui("mod1")
    
  })
  
  
  })

  
  
  data_melt <- melt(df)
  colnames(data_melt)[4] <- "Frequency"

    
  filtered_data <- reactive({
    data_melt %>%
      filter(hospital == input$hospitaltrack)
  })
  
  
  output$heatmap <- renderPlot({
    
    ggplot(filtered_data(), aes(x = location, y = variable, fill = Frequency)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "Hospital Unit", y = "Questionnaire Step", title = "Procedural Breakdown by Location") +
      theme(axis.text.x = element_text(angle = 90))
  })
}


# Run the application
shinyApp(ui, server)
