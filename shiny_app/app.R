library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("A Cybernetic Solution to Optimize Blood Allocation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Copy the line below to make a date selector 
      dateInput("date", label = h3("Date input"), value = "2021-02-07"),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h1("Blood units required (predicted)"),
      # Create a new row for the table.
      DT::dataTableOutput("table"),
      plotOutput("daily_plot")
      
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  library(tidyverse)
  library(lubridate)

  res <- readRDS("data/dataset.rds")

# You can access the value of the widget with input$date, e.g.
  output$value <- renderPrint({ input$date }) 
  
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data = 
      res %>% 
      filter(donation_expiry > input$date & donation_date <= input$date) %>% 
      group_by(donor_bloodtype) %>% 
      summarise(available_units = n()) %>% 
      rename("Blood type" = "donor_bloodtype", "Required Units" = "available_units")

  },options = list(dom = 't'), rownames = FALSE))
  
  output$daily_plot <- renderPlot({
    res %>% 
      filter(donation_expiry > input$date & donation_date <= input$date) %>% 
      group_by(donor_bloodtype) %>% 
      summarise(n = n(), xlab = input$date) %>% 
      ggplot(aes(x = xlab, y = n))+
      geom_col(aes(fill = donor_bloodtype), position = position_fill())+
      scale_fill_viridis_d()+
      labs(fill = "Type", x = "", y = "Units required by type (%)")+
      theme_minimal()+
      coord_flip()+
      theme(
        aspect.ratio = 0.2,
        legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold")
      )
    })
    
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)