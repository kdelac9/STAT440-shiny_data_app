#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(readr)
library(jsonlite)
library(DT)
library(stringr)
#install.packages("readxl")
library(readxl)
library(shinythemes)
library(Dict)

#created the basic UI with the functions to let users enter the URL
ui = navbarPage(title = "440 Data Engineers",
                tabPanel(title = "Data File Importing",
                         #main page
                         fluidPage(
                           theme = shinytheme("sandstone"),
                           setBackgroundColor(
                             color = "Tan",
                             gradient = "linear",
                             direction = "bottom"),
                           uiOutput("style"),
                           #https://stackoverflow.com/questions/61632272/r-shiny-light-dark-mode-switch
                           tags$head(
                             tags$style(
                               HTML(
                                 "
                                .dataTables_length label,
                                .dataTables_filter label,
                                .dataTables_info {
                                    color: white!important;
                                    }
                        
                                .paginate_button {
                                    background: white!important;
                                }
                        
                                thead {
                                    color: white;
                                    }
                        
                                "
                               )
                             )
                           ),
                           h1("Data File Importing", style = "font-family:Arial"),
                           h4("Welcome to the 440 Data Engineers' RShiny App! We understand that data engineering can be time-consuming and tedious, so we made an app to do the work for you. First, upload your data or choose one of the data sets already included in the app. Then using the app, either validate your data, clean your data, or both.", style = "font-family:Arial"),
                           sidebarLayout(
                             
                             sidebarPanel(
                               # Enter URL or choose from existing datasets
                               
                               selectizeInput("url",
                                              label = "Enter your URL or choose from one of the below datasets",
                                              selected = NULL,
                                              options = list(create = TRUE),
                                              choices = c("Use the dropdown list" = "",
                                                          str_sort(names(c(
                                                          "Chicago Food Inspection Data 1" = "https://uofi.box.com/shared/static/lwwzlv2sulip1p8qoqqghmw0pouso0wk.csv",
                                                          "Chicago Food Inspection Data 2" = "https://uofi.box.com/shared/static/k6h51rdj07jmmiv11q3vmehgr2200eua.csv",
                                                          "Chicago Food Inspection Data 3" = "https://uofi.box.com/shared/static/zyn1kls0xxpjl3ld0m9w6cl2s3343gov.csv",
                                                          "CCSO Jail Data" = "https://uofi.box.com/shared/static/lurfpdqk63au11yzra4a88sxizda0iz5.dat",
                                                          #I changed CCSO Jail Data's link to a box one.
                                                          "ADS Demo" = "https://uofi.box.com/shared/static/9b9ecldtxkr23wb3uc36wwbn2l5ylpyx.csv",
                                                          "Speed Dating" = "https://uofi.box.com/shared/static/w8vjq29rig8pdvussyg3997h1gwhkimq.csv",
                                                          "US Presidential Elections" = "https://uofi.box.com/shared/static/qmhzgom3e1y0lnqd720rnbhp34xyfd6d.csv",
                                                          "Rental Inspection Grade (Data Batch #1)" = "https://uofi.box.com/shared/static/l9o50efbnemdnaxury4hg45cj8b2truu.csv", 
                                                          "Rental Inspection Grade (Data Batch #2)" = "https://uofi.box.com/shared/static/2e2s3l35l2johejjvf0an39g9ucpxfw2.txt",
                                                          #I changed these two Rental inspection grade link into box one
                                                          "Rental Inspection Grade (Data Batch #3)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data03.txt?token=AAAB4T35EAM5ZMHFNTXWTUDBTQTXW", 
                                                          "Rental Inspection Grade (Data Batch #4)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data04.csv?token=AAAB4TYNNCVBY2SG3UTSDHDBTQTX4", 
                                                          "Rental Inspection Grade (Data Batch #5)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data05.json?token=AAAB4TZPKMXO4ABLDZCEC73BTQTYA", 
                                                          "Rental Inspection Grade (Data Batch #6)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data06.json?token=AAAB4T3SUCZTP2J3J7LOQU3BTQTYE",
                                                          "NBA stint1"="https://uofi.box.com/shared/static/fk1q6htmes532jz2obi89zj2v7bobh37.json",
                                                          "NBA stint2"="https://uofi.box.com/shared/static/aix57cgzs4bhzsvorpv924rrv1w57wod.json",
                                                          "NBA stint3"="https://uofi.box.com/shared/static/6a4nbioxh8yek5oppb06jn4429n7fekr.json",
                                                          "NBA stint4"="https://uofi.box.com/shared/static/9ap7efzs42xldrhr1cpt1ql53ip1v25u.json",
                                                          "Baby Names Iowa" = "https://uofi.box.com/shared/static/yye6p5kgq0l1dtz6bqz364mr6iz3c3bw.txt",
                                                          "Baby Names Illinois" = "https://uofi.box.com/shared/static/ttc01y8x71eadmzc9ramd16ksuq2xckk.txt",
                                                          "Baby Names Indiana" = "https://uofi.box.com/shared/static/0ymjj08wid1l19ehy5tx9hjq2xnbse64.csv",
                                                          "Baby Names Kentucky" = "https://uofi.box.com/shared/static/nhfxo34324uratwl3fezx08fcdoknayj.txt",
                                                          "Baby Names Missouri" = "https://uofi.box.com/shared/static/p2synnz72z6se94int717w9jhgflmynb.csv",
                                                          "Baby Names Wisconsin" = "https://uofi.box.com/shared/static/arv2amejhnyglkc684mgeyp0mcom27tl.txt",
                                                          "Baby Names National" = "https://uofi.box.com/shared/static/c2whskv4x21w9pok3qxfh2kdz7jxk1g4.csv",
                                                          "Speed Dating" = "https://uofi.box.com/shared/static/w8vjq29rig8pdvussyg3997h1gwhkimq.csv",
                                                          "US Presidential Elections" = "https://uofi.box.com/shared/static/qmhzgom3e1y0lnqd720rnbhp34xyfd6d.csv",
                                                          "Rental Inspection Grade (Data Batch #1)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data01.csv?token=AAAB4T72YRIH5APM44Y6WX3BTQTMA", 
                                                          "Rental Inspection Grade (Data Batch #2)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data02.txt?token=AAAB4T3WPJ6GOBWUUN5ENNDBTQTXU",
                                                          "Rental Inspection Grade (Data Batch #3)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data03.txt?token=AAAB4T35EAM5ZMHFNTXWTUDBTQTXW", 
                                                          "Rental Inspection Grade (Data Batch #4)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data04.csv?token=AAAB4TYNNCVBY2SG3UTSDHDBTQTX4", 
                                                          "Rental Inspection Grade (Data Batch #5)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data05.json?token=AAAB4TZPKMXO4ABLDZCEC73BTQTYA", 
                                                          "Rental Inspection Grade (Data Batch #6)" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/rental-inspections-grades-data06.json?token=AAAB4T3SUCZTP2J3J7LOQU3BTQTYE",
                                                          "stint1"="https://uofi.box.com/shared/static/fk1q6htmes532jz2obi89zj2v7bobh37.json",
                                                          "stint2"="https://uofi.box.com/shared/static/aix57cgzs4bhzsvorpv924rrv1w57wod.json",
                                                          "stint3"="https://uofi.box.com/shared/static/6a4nbioxh8yek5oppb06jn4429n7fekr.json",
                                                          "stint4"="https://uofi.box.com/shared/static/9ap7efzs42xldrhr1cpt1ql53ip1v25u.json",
                                                          "US Congress Data"="https://theunitedstates.io/congress-legislators/legislators-current.json",
                                                          "Delivery Stores"="https://uofi.box.com/shared/static/o2bqah52ioj0p0q4msvna7ixk8d9lsh4",
                                                          "other" = "other",
                                                          "Owners Address" = "https://github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/blob/master/data/owners-addresses.csv",
                                                          "Trips" = "https://uofi.box.com/shared/static/0pyqnfpl40kr5do7fue8t85vva11ynjm.txt",
                                                          "Covid Data" = "https://github.com/nytimes/covid-19-data/raw/master/us-states.csv",
                                                          "Election" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/us-presidential-election-data.csv?token=AAAC6SDY2PW3HICWLYTM6B3BVU3R6",
                                                          "Nuisance Complaints" = "https://data.urbanaillinois.us/api/views/tsn9-95m3/rows.tsv?accessType=DOWNLOAD&bom=true",
                                                          "Urbana Market at the Square Vendor Products" = "https://data.urbanaillinois.us/api/views/6gtk-bwms/rows.csv?accessType=DOWNLOAD",
                                                          #added the data set Daily Crime Log
                                                          "Daily Crime Log" = "https://uofi.box.com/shared/static/7uah46fdogn8gdc1zy06efuxsfpqif4g.csv",
                                                          #I added "zoom script"  data
                                                          "Zoom Script" = "https://raw.github-dev.cs.illinois.edu/stat440-fa21/stat440-fa21-course-content/master/data/zoom_transcript.vtt?token=AAAC6CKNTKIUAGL5YBENCGDBW7UFE"
                                                          
                                              ))))),
                               uiOutput("urlother"),
                               selectizeInput("type",
                                              label = "What is the file extension?",
                                              selected = NULL,
                                              options = list(create = TRUE),
                                              choices = c("Choose the file extension" = "", str_sort(c("CSV", "TXT", "JSON", "XLSX","TSV")),"Others")),
                               #Can be ignored if the file extension is JSON
                               
                               selectizeInput("delim",
                                              label = "Choose delimiter",
                                              selected = NULL,
                                              options = list(create = TRUE), 
                                              choices = c("Choose delimiter" = "", str_sort(c("Comma", "Semicolon", "Tab", "Whitespace", "Colon","Slash")))),
                               checkboxInput("checkbox", "Data has column name?", value = TRUE),
                               numericInput("num", h3("Please select the rows you want to display!"), value = 10),
                               actionButton("Submit" , label = "Submit")
                             ),
                             mainPanel(
                               dataTableOutput("table"),
                               uiOutput("style_checkbox")
                             )
                           )
                         )
                ),
                
                #for validating strategies
                tabPanel(title = "Data Validating Strategies",
                         fluidPage(
                           h1("Data Validation Strategies", style = "font-family:Arial"),
                           sidebarLayout(
                             sidebarPanel(
                               # Enter URL or choose from existing datasets
                               
                               selectInput("val_type",
                                           label = "Choose your validation strategies:",
                                           choices = c("Use the dropdown list" = "", str_sort(c("Filtering and Arranging", "Counting Frequencies and Duplicates", "Computing Summary Statistics", "Visualizing Distributions", "Column Selection", "Row Selection")))),
                               
                               selectInput("variable", "Which variable do you want to validate?", choices = NULL),
                              
                               
                               selectInput("col", "Which column(s) do you want to select? Please correctly spell the column(s) name, Separated by \",\" :", choices = NULL, multiple = TRUE),
                            
                               
                               actionButton("Submit" , label = "Submit")
                             ),
                             mainPanel(
                               dataTableOutput("validation")
                             )
                           )
                           
                         )
                ),
                
                #for cleaning approach
                tabPanel(title = "Data Cleaning Approaches",
                         fluidPage(
                           h1("Data Cleaning Approaches", style = "font-family:Arial"),
                           sidebarLayout(
                             sidebarPanel(
                               # Enter URL or choose from existing datasets
                               
                               selectInput("clean_approach",
                                           label = "Choose your cleaning approaches:",
                                           choices = c("Use the dropdown list" = "", str_sort(c("Removing Duplicate Observations", "Fixing Rounding Errors and Inconsistent Units of Measurement",
                                                       "Removing or Replacing Missing Values", "Limiting a Distribution to its Realistic Set of Observations",
                                                       "Correcting and Subsetting with Dates", "Correcting Misspelled Words, Abbreviations, or Text Cases")))),
                               textInput("variable_clean", "Which variable do you want to clean the dataset with? Please correctly spell the variable name:"),
                               
                               actionButton("Submit" , label = "Submit")
                             ),
                             mainPanel(
                               dataTableOutput("cleaning")
                             )
                           )
                           
                         )
                ),
                navbarMenu("More",
                           tabPanel(title = "Data Summarization",
                                    fluidPage(
                                      h1("Data Summarization", style = "font-family:Arial"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput(inputId = "function",
                                                      label = "Choose your summarize function:",
                                                      choices = c("Use the dropdown list" = "", str_sort(c("Sum", "Mean",
                                                                  "Median", "Variance",
                                                                  "Standard Deviation")))),
                                          textInput(inputId = "group",
                                                    label = "Group data by:"),
                                          actionButton("Submit" , label = "Submit")
                                        ),
                                        mainPanel(
                                          dataTableOutput("summarize")
                                        )
                                      )
                                      
                                    )),                 
                           tabPanel("Data Structure Information",
                                    fluidPage(
                                      h1("Data Structure Information", style = "font-family:Arial"),
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("variable", "Which variable you want to check? Please correctly spell the variable name:"),
                                          actionButton("Submit" , label = "Submit")
                                        ),
                                        mainPanel(
                                          dataTableOutput("structure_var")
                                        )
                                      ),
                                      verbatimTextOutput("structure", placeholder = TRUE))),
                           tabPanel("App Theme settings", 
                                    actionButton("Black mode", "Black mode"),
                                    actionButton("+", "Larger font size", icon("plus")),
                                    actionButton("-", "Smaller font size", icon("minus")))))
                
                #Creating pages that does other functions? Hopefully?



server = function(input, output, session) {
  
  current_theme <- reactiveVal(FALSE)
  
  output$style_checkbox <- renderUI({
    checkboxInput("style", "Dark theme", value = current_theme())
  })
  
  output$style <- renderUI({
    if (!is.null(input$style)) {
      current_theme(input$style)
      if (input$style) {
        includeCSS("css-files/darkly.min.css")
      } else {
        includeCSS("css-files/flatly.min.css")
      }
    }
  })
  
  importedData = NULL
  observe({
    if(input$url == "other")
    {
      output$urlother <- renderUI({
        textInput("urlother", "If the url you want is not on our list, type it here:" , value = " ")
      })
    }
  })
  
  
  data1 =reactive({
    
    if (input$url != "") {
      # Fix "Chicago Food Inspection Data 3" (CSV file but delimiter is Tab) to display properly in the  table  
      if (input$type == "CSV" & input$delim == "Tab") {   
        print("The file extension is CSV, and delimitor is Tab")
        delim = "\t"
        data = read_delim(input$url, delim = delim)
      } else if (input$type == "CSV" ) {
        print("Data type is CSV")
        data = read_csv(input$url)
      } else if (input$type == "JSON") {
        print("Data type is JSON")
        data = fromJSON(input$url)
      } else if (input$type == "others") {
        print("Data type is others")
        if (input$delim == "Comma")
          delim = ","
        else if (input$delim == "Semicolon")
          delim = ";"
        else if (input$delim == "Tab")
          delim = "\t"
        else if (input$delim == "Whitespace")
          delim = " "
        else if (input$delim == "Slash")
          delim = "/"
        else
          delim = input$delim
        data = read_delim(input$url, delim = delim)
      } else {
        data = NA
      }
      data = as.data.frame(data)
      for (j in 1:ncol(data)) {
        if (is.character(data[1,j])) {
          for (i in 1:nrow(data)) {
            if (!is.na(data[i,j]) & nchar(data[i,j]) > 30) 
              data[i,j] = paste(strtrim(data[i,j], 30), "...", sep = "")
          }
        }
      }
      if (!is.na(data)) 
        data
    }
    
  })
  
  
  observeEvent(input$Submit,{
    if(input$url != "other"){
      output$table = renderDataTable({
        data = if (input$url != "") {
          # Fix "Chicago Food Inspection Data 3" (CSV file but delimiter is Tab) to display properly in the  table  
          if (input$type == "CSV" & input$delim == "Tab") {   
            print("The file extension is CSV, and delimiter is Tab")
            delim = "\t"
            if (input$checkbox == TRUE){
              data = read_delim(input$url, delim = delim)
            } else if (input$checkbox == FALSE){
              data = read_delim(input$url, delim = delim, col_names=FALSE)
            }
          } else if (input$type == "CSV" ) {
            print("Data type is CSV")
            if (input$checkbox == TRUE){
              data = read_csv(input$url)
            } else if (input$checkbox == FALSE){
              data = read_csv(input$url, col_names=FALSE)
            }
          } else if (input$type == "JSON") {
            print("Data type is JSON")
            data = fromJSON(input$url)
          } else if (input$type == "TXT") {
            print("Data type is TXT")
            data = read_csv(input$url)
          } else if (input$type == "XLSX") {
            print("Data type is XLSX")
            data = read_xlsx(input$url)
          } else if (input$type == "TSV"){
            print("Data type is TSV")
            data = read_tsv(input$url)
          } else if (input$type == "others") {
            print("Data type is others")
            if (input$delim == "Comma")
              delim = ","
            else if (input$delim == "Semicolon")
              delim = ";"
            else if (input$delim == "Tab")
              delim = "\t"
            else if (input$delim == "Whitespace")
              delim = " "
            else if (input$delim == "Colon")
              delim = ":"
            else if (input$delim == "Pipe")
              delim = "|"
            else if (input$delim == "Slash")
              delim = "/"
            else
              delim = input$delim
            data = read_delim(input$url, delim = delim)
          } else {
            data = NA
          }
          data = as.data.frame(data)
          for (j in 1:ncol(data)) {
            if (is.character(data[1,j])) {
              for (i in 1:nrow(data)) {
                if (!is.na(data[i,j]) & nchar(data[i,j]) > 30) 
                  data[i,j] = paste(strtrim(data[i,j], 30), "...", sep = "")
              }
            }
          }
          if (sum(is.na(data)) != sum(dim(data)[1]* dim(data)[2]))
            head(data, n = input$num) # Show the number of rows users want to show
          importedData <<- data
          
        }
        # add a filter for each column
        DT::datatable(data,
                      extensions = "Scroller",
                      filter = "top",
                      # add horizontal scrolling for datatable
                      options = list(scrollX = TRUE)
        )
      })
    } 
  }

  )
  
  
  observe({
    # Code from above to retrieve imported dataset
    data = if (input$url != "") {
      # Fix "Chicago Food Inspection Data 3" (CSV file but delimiter is Tab) to display properly in the  table  
      if (input$type == "CSV" & input$delim == "Tab") {   
        print("The file extension is CSV, and delimiter is Tab")
        delim = "\t"
        if (input$checkbox == TRUE){
          data = read_delim(input$url, delim = delim)
        } else if (input$checkbox == FALSE){
          data = read_delim(input$url, delim = delim, col_names=FALSE)
        }
      } else if (input$type == "CSV" ) {
        print("Data type is CSV")
        if (input$checkbox == TRUE){
          data = read_csv(input$url)
        } else if (input$checkbox == FALSE){
          data = read_csv(input$url, col_names=FALSE)
        }
      } else if (input$type == "JSON") {
        print("Data type is JSON")
        data = fromJSON(input$url)
      } else if (input$type == "TXT") {
        print("Data type is TXT")
        data = read_csv(input$url)
      } else if (input$type == "XLSX") {
        print("Data type is XLSX")
        data = read_xlsx(input$url)
      } else if (input$type == "TSV"){
        print("Data type is TSV")
        data = read_tsv(input$url)
      } else if (input$type == "others") {
        print("Data type is others")
        if (input$delim == "Comma")
          delim = ","
        else if (input$delim == "Semicolon")
          delim = ";"
        else if (input$delim == "Tab")
          delim = "\t"
        else if (input$delim == "Whitespace")
          delim = " "
        else if (input$delim == "Colon")
          delim = ":"
        else if (input$delim == "Pipe")
          delim = "|"
        else if (input$delim == "Slash")
          delim = "/"
        else
          delim = input$delim
        data = read_delim(input$url, delim = delim)
      } else {
        data = NA
      }
      data = as.data.frame(data)
      for (j in 1:ncol(data)) {
        if (is.character(data[1,j])) {
          for (i in 1:nrow(data)) {
            if (!is.na(data[i,j]) & nchar(data[i,j]) > 30) 
              data[i,j] = paste(strtrim(data[i,j], 30), "...", sep = "")
          }
        }
      }
      if (sum(is.na(data)) != sum(dim(data)[1]* dim(data)[2]))
        head(data, n = input$num) # Show the number of rows users want to show
      importedData <<- data
      
      # Creates variable selection for Data validation page
      updateVarSelectInput(session, "variable", "Which variable do you want to validate?", data = importedData)
      # Make variable selection easier
      updateVarSelectInput(session, "col", "Which column(s) do you want to select?", data = importedData)
    }
  })
  
  
  output$validation = renderDataTable({
    # Bring down imported data from Data File Importing
    # data = ...
    if (input$val_type != '') {
      # Check that data is not empty and validation strategy is selected
      # if (!is.na(data) && input$val_type != '') {
      if (input$val_type == "Filtering and Arranging") {
        print('Filtering and Arranging')
        importedData %>% #arranging
          arrange(input$variable)
        
      } else if (input$val_type == "Counting Frequencies and Duplicates") {
        print('Counting Frequencies and Duplicates (Categorical)')
        # add code here
        duplicate <- vector()
        frequencies <- vector()
        check <- data1()
        for (i in length(check)) {
          if (check[i] %in% frequencies) {
            frequencies$check[i] = frequencies$check[i] + 1
            if (check[i] %in% duplicate) {
              print("Here")
            } else {
              duplicate <- append(duplicate, check[i])
            }
          } else {
            frequencies$check[i] = 1
          }
        }

        print("Duplicates:", duplicate)
        print("Frequency:", frequencies)
      } else if (input$val_type == "Computing Summary Statistics") {
        print('Computing Summary Statistics (Numeric)')
        data_temp=data1()
        summary(data_temp)
      } else if (input$val_type == "Visualizing Distributions") {
        print('Visualizing Distributions')
        # add code here
      } else if(input$val_type == "Column Selection"){
        selectedColumn <- unlist(strsplit(input$col, ","))
        importedData <- importedData  %>% 
          select(c(selectedColumn))
      } else if(input$val_type == "Row Selection"){
        selectedRow <- unlist(strsplit(input$row, ","))
        importeddata <- importeddata %>%
          select(c(selectedRow))
        } else {
        # other methods to be added?
      }
      
    }
  })
  
  output$cleaning = renderDataTable({
    importedData
    
    
    # Notifies the User if the Data File has not been imported
    if (input$clean_approach != " " && is.null(importedData)) {
      data.frame("EMPTY_DATA_FILE" = "Please Import a Data File")
      
      #Removing Missing Values
    } else if (input$clean_approach == "Removing or Replacing Missing Values") {
      importedData %>%
        drop_na()
      #Remove Duplicate Observations
    } else if (input$clean_approach == "Removing Duplicate Observations") {
      importedData %>%
        distinct()
    } else if (input$clean_approach == "Fixing Rounding Errors and Inconsistent Units of Measurement"){
      print('Fixing Rounding Errors and Inconsistent Units of Measurement')
      # Correcting and Subsetting with Dates
      importedData %>%
        mutate_if(is.numeric, round, digits = 3)
    } else if (input$clean_approach == "Correcting and Subsetting with Dates") {
      i <- which(sapply(importedData, function(x)
        ! all(is.na(as.Date(
          as.character(x),
          format = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d")
        )))) == TRUE)
      n = length(i)
      for (p in 1:n) {
        importedData[,i[p]] <-lubridate::mdy(importedData[,i[p]])
      }
      importedData
    } # add code here
    else {
      # other methods to be added?
    }
    
  })
  
  # print the structure of the data
  output$structure <- renderPrint({
    str(importedData)
  })

}
# Run the application
shinyApp(ui, server)
