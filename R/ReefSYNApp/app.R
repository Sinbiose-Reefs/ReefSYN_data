# load packages
library(shiny)
library(tidyverse)
library(leaflet)
library (here)
library (openxlsx)

# load functions
source (here ("R", "function_load_dwc_data.R"))

# Load Darwin core datasets
# identify the place of dirs and files
interesting_dirs_fish <- list.files (here("\\.","Pos_Doc_Sinbiose", 
                                          "ReefSYN_data", 
                                          "DwC_output"))[c(2,5,6,9,12,13,14,15)]

# interesting files
interesting_files_fish <- lapply (interesting_dirs_fish, function (i) 
  
  list.files (here("\\.","Pos_Doc_Sinbiose", 
                   "ReefSYN_data", 
                   "DwC_output",
                   i)))
# filtering fish
interesting_files_fish <- lapply (interesting_files_fish, function (i) {
  
  i [grep("benthos",i, invert=T)]
  
})

# benthos
# identify the place of dirs and files
interesting_dirs_benthos <- list.files (here("\\.","Pos_Doc_Sinbiose", 
                                             "ReefSYN_data", 
                                             "DwC_output"))[c(1,5,8,11,12)]

# interesting files
interesting_files_benthos <- lapply (interesting_dirs_benthos, function (i) 
  
  list.files (here("\\.","Pos_Doc_Sinbiose", 
                   "ReefSYN_data", 
                   "DwC_output",
                   i)))
# filtering fish
interesting_files_benthos <- lapply (interesting_files_benthos, function (i) {
  
  i [grep("fish",i, invert=T)]
  
})

# load fish data
fish_data <- lapply (seq (1,length(interesting_files_fish)), function (i)  
  
  function_load_data(dir=interesting_dirs_fish[[i]],
                     file=interesting_files_fish[[i]])
)

# load benthic data
benthic_data <- lapply (seq (1,length(interesting_files_benthos)), function (i)  
  
  function_load_data(dir=interesting_dirs_benthos[[i]],
                     file=interesting_files_benthos[[i]])
)

# bind 
all_data <- c(fish_data,benthic_data)

# remove duplicated names
all_data <- lapply (all_data, function (i) 
  
  i[, !duplicated(colnames(i), fromLast = TRUE)] 
)

# get interesting cols of each dataset

all_data <- lapply (all_data, function (i) 
  
  i %>%
    
    select (#"measurementValue",
      #"higherGeography",
      "site",
      "locality",
      "year",
      "decimalLatitude",
      "decimalLongitude",
      "scientificNameAccepted") 
)



all_data[[1]] %>%
  slice (0) %>%
  glimpse()

# Load datasets
data1 <- all_data[[1]]
data2 <- all_data[[2]]
data3 <- all_data[[3]]



# --------------------------------------------- 
# UI

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("ReefSYN App"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Dataset I", "Dataset II", "Dataset III")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of species to view:",
                   value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Dataset I" = data1,
           "Dataset II" = data2,
           "Dataset III" = data3)
    
    
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    dataset %>%
       group_by(sp) %>%
      summarize (val=n()) %>%
      arrange (-val)
    
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    
    
    #head(datasetInput(), n = input$obs)
    dataset <- datasetInput()
    unique(dataset$sp)[order(unique(dataset$sp))]
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
