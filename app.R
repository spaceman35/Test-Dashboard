# This R Shiny Dashboard is...
# Survey tool POC: Maj Sean Kelly, AFOTEC Det 5, sean.kelly.33@spaceforce.mil
# Database POC: Lt Tim Dawson, AFOTEC Det 5, timothy.dawson.7@us.af.mil

## ---------------------------------------------------------------------------------------- ##
# Before we get to the server.R and ui.R portions, these are things that could go in a global.R file, but to minimize the number of files, I included these sections here.

library(shiny)
library(shinydashboard)
library(shinysurveys) #uses the {shinysurveys} package which adds more flexibility to {shiny}
library(sass)
library(digest) #digest() Create hash function digests for R objects
library(readr)
library(readxl) #allows the app to read in .xlx files instead of parsing .csv files
library(dplyr) #not sure if I'm still using this at all, might delete
library(aws.s3)
library(httr)
library(jsonlite)
library(DBI)
library(shinyTime)
library(DT) #datatable package
library(timevis) #for cool timeline visualizations via https://github.com/daattali/timevis
#library(semantic.dashboard) #This might be useful if we want to make it look much prettier/customizable than just a skin color; The themes are at https://semantic-ui-forest.com/themes

############################################################################################
### This section reads in each survey file and needs to be duplicated for each survey excel file that we have. It also pulls the latest version of the master database for viewing/analysis
UMUXSurvey <- read_excel("surveys/UMUXSurvey.xlsx", na=c("", "NA"))
simpleSurvey <- read_excel("surveys/simple_survey.xlsx", na=c("", "NA"))
dataset <- read.csv("data/datatest.csv") #reads in the dataset csv file - this will need to call whatever our final dataset database looks like
sched <- read_excel("schedule.xlsx", na=c("", "NA")) #Reads in the schedule file for the timeline visualization

############################################################################################
### Get S3 Credentials from Databricks Job

#db_token_value = "dapicba79e0fc2955fcf407d15c163f8382a"
#db_job_id = 866

# Turn off SSL verification (!! Only do this for trusted URLs !!)
#httr::set_config(httr::config(ssl_verifypeer = 0L))

# Get list of completed runs for the specified job:
#response_job = httr::GET("https://databricks.afdatalab.af.mil/api/2.0/jobs/runs/list",
#                         add_headers(Authorization=paste0("Bearer ",db_token_value)),
#                         query=list(job_id=db_job_id, completed_only=TRUE)
#)
#my_runs=httr::content(response_job, "parsed")

# extract the most recent completed run_id
#last_run_id=my_runs$runs[[1]]$run_id

# Get results of most recent completed run
#response_run = httr::GET("https://databricks.afdatalab.af.mil/api/2.0/jobs/runs/get-output",
#                         add_headers(Authorization=paste0("Bearer ", db_token_value)),
#                         query=list(run_id=last_run_id)
#)

# extract credentials from results of most recent run
#my_creds=httr::content(response_run, "parsed")
#creds=jsonlite::fromJSON(my_creds$notebook_output$result)

############################################################################################
### Environment Variables

formName <- "Survey Data"
resultsDir <- file.path("data", formName)
dir.create(resultsDir, recursive = TRUE, showWarnings = FALSE)

## Environment variables for S3 credentials
#Sys.setenv(
#  "AWS_ACCESS_KEY_ID" = creds$AccessKeyId,
#  "AWS_SECRET_ACCESS_KEY" = creds$SecretAccessKey,
#  "AWS_SESSION_TOKEN" = creds$Token,
#  "AWS_DEFAULT_REGION" = "us-gov-west-1"
#)

# fix for `Warning: Error in <-: 'names' attribute [1] must be the same length as the vector [0]`
getRequired_internal <- function(questions) {
  out <- as.data.frame(
    do.call(
      rbind,
      lapply(questions, getID)
    ),
    stringsAsFactors = FALSE
  )

  ## NK 8/18/2021
  if (NROW(out)) {
    names(out) <- "required_id"
    out <- out$required_id
  }
  return(out)
}

getFormattedTimestamp <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

############################################################################################
### This section adds some "exted Input types" to create custom inputs from the survey excel file.

extendInputType("check", {
  shiny::checkboxGroupInput(
    inputId = surveyID(),
    label = surveyLabel(),
    choices = surveyOptions(),
    inline = TRUE
  )
})

# extendInputType("matrix", {
#   shinysurveys::radioMatrixInput(
#     inputId = surveyID(),
#     choices = surveyOptions(),
#     selected = 1
#   )
# })

extendInputType("umuxradio", {
  shiny::radioButtons(
    inputId = surveyID(),
    label = surveyLabel(),
    #choices = surveyOptions(),
    selected = character(0),
    inline = TRUE,
    choiceNames = surveyOptions(),
    choiceValues = list(1,2,3,4,5)
  )
})

extendInputType("radio", {
  shiny::radioButtons(
    inputId = surveyID(),
    label = surveyLabel(),
    choices = surveyOptions(),
    selected = NULL,
    inline = TRUE
  )
})

extendInputType("slider", { #this gives a new input type called "slider" for survey questions. I think I prefer the matrixed radio icons instead, but this could be useful.
  shiny::sliderInput(
    inputId = surveyID(),
    label = surveyLabel(),
    min = 1,
    max = 10, #should probably change these to better match the survey scales we typically use (1-7)
    value = 5
  )
})

extendInputType("date", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-10,
    max = Sys.Date()+10
  )
})

extendInputType("time", {
  shinyTime::timeInput(
    inputId = surveyID(),
    label = surveyLabel(),
    value = Sys.time()
  )
})


############################################################################################
### This is the UI section of the app.R file. This tells the shiny server how to display the survey.


ui <- dashboardPage(skin = "black", #if using shinydashboard
# ui <- dashboardPage( theme = "slate", #if using semantic.dashboard
  dashboardHeader(title = "Test Dashboard"), #dashboardHeader has more potential, I just didn't use it yet
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Data Visualization", tabName = "data_vis", icon = icon("th")),
      menuItem("Test Schedule", tabName = "schedule", icon = icon("calendar-alt")),
      menuItem("Surveys", tabName = "surveys", icon = icon("poll")),
      menuItem("Data Import", tabName = "data_import", icon = icon("wrench")),
      menuItem("Paper Surveys", tabName = "surveys_paper", icon = icon("poll")),
      menuItem("Links", tabName = "links", icon = icon("wrench"))
    )
  ),
# Icons can be found here: https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free

############################################################################################
### This section defines what is in each tab. It uses R markdown, but html tags work as well.
  dashboardBody( 
    tabItems(
      tabItem("home",
              fluidPage(
                h1("Home page"),
                img(src = "det5.png", height = "25%", width = "25%", align = "center"),
                br(),
                p("Welcome to the test dashboard! This is a work in progress by AFOTEC Det 5/MTO analysts, but once it's in a stable, generic form, we plan on exporting it for wider use."),
                p("On the left-hand sidebar, you'll see various tabs for different areas of the dashboard. This is meant to be a mostly all-in-one place to add data, view existing data, and visualize the data however you need to. An in-depth readme or how-to guide will follow once the kinks are worked out and once we have a working prototype with real data.")
              )

      ),


    tabItem("data",
              fluidPage(
              h1("Test Data"),
              div(style = 'overflow-x:scroll; overflow-y:scroll', dataTableOutput("datatable", width="100%")) #displays the datatable called in the server section and adds scroll functionality 
              
              )
    ),
    
    tabItem("data_vis",
            fluidPage(
              h1("Data Visualization"),

            )
    ),
    
    tabItem("schedule",
            h1("Test Schedule"),
            fluidPage(
            p("To edit this schedule, edit the schedule.xlsx file in the code repository."),
            timevisOutput("timeline"),
            dataTableOutput("timelineTable")
            
            )
    ),
    
    tabItem("surveys",
            fluidPage(
            h1("Survey tool"),
            p("The links below take you to the different survey or data collection forms for this program. After submitting each survey, the data will make its way to the database. It may take a few minutes for the data to show up, or it may need to be adjudicated first."),
            #selectInput("surveySelection", label = h5("Select a survey"), choices=c("UMUX Survey", "Simple Survey"))
            selectInput("surveySelection", "Survey Selection",
                        c("UMUX Survey" ,
                          "Simple Survey")),
            tableOutput("Survey")
            #fluidRow(uiOutput("Survey"))
            )
    ),
    
    tabItem("data_import",
            fluidPage(
            #h1("Data Import"),
            p("Currently, this tool just allows you to temporarily upload a csv file for analysis, but it does not store it in the AWS S3 Cloud where the rest of our data is stored."),
            #fileInput("file1", "Choose CSV File", accept = ".csv", buttonLabel="Upload data here")
            #)
              titlePanel("File Input Tool"),
              sidebarLayout(
                sidebarPanel(
                  fileInput("file","Upload a CSV file here"), 
                  helpText("Default max. file size is 5MB"),
                  tags$hr(),
                  h5(helpText("Select the read.table parameters below")),
                  checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                  checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                  br(),
                  radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                ),
                mainPanel(
                  uiOutput("tb")
                )
              )
            )
    ),
    
    tabItem("surveys_paper",
            fluidPage(
            h1("Backup surveys"),
            div("If the survey tool isn't working or you otherwise need to print out paper copies of surveys, use the links below to get the documents."),
            br(),
            a("Survey name", href="", target="_blank"),
            br(),
            a("Survey name", href="", target="_blank"),
            br(),
            a("Survey name", href="", target="_blank"),
            br(),
            a("Survey name", href="", target="_blank"),
            br()
            )
    ),
    
    tabItem("links",
            fluidPage(
            h1("Helpful links "),
            div("Copy the hyperlinks below and open them in your web browser."),
            br(),
            a("url.com", href="url.com", target="_blank"),
            br(),
            a("code repo", href = "", target="_blank"),
            br(),
            a("AFOTEC Det 5 Sharepoint: ", href = "", target="_blank"),
            br(),
            a("Program Team link: ", href = "", target="_blank")
            )
    )
    )
)
)

############################################################################################
### This is the server section of the R app from here on out.

server <- function(input, output, session) {

output$datatable <- renderDataTable(dataset) #This calls our master dataset pulled from the csv at the very top of this code. There are a lot of other things we can do to clean up this dataframe using mutates, filters, rename_all(~str_to_title(.x)), etc.

############################################################################################
### This renders the timeline based off the timeline data in the beginning of this code
output$timeline <- renderTimevis({
  timevis(sched)
})
sched_simple = subset(sched, select = -c(type, title)) #This hides the type, title, and group columns that I don't need to see in table form
output$timelineTable <- renderDataTable(sched_simple)

############################################################################################
### This section is to choose which survey to render and then renders it from the source .xlsx file it read in earlier

 surveyInput <- reactive({
   if (input$surveySelection == "UMUX Survey"){
     dataset <- UMUXSurvey
   }
   else if (input$surveySelection == "Simple Survey"){
     dataset <- simpleSurvey
   }
   return(dataset)
 })
 
output$Survey <- renderUI({
  surveyOutput(df = surveyInput(), #the dataframe is the appropriate excel file called earlier
               theme="blue", #can take normal colors or hex codes
               survey_title = "Test Survey", #this goes on the top of the page
               survey_description = "Welcome! Please fill out this test survey to check how data is stored in the database." #add some instructions, etc. for the survey
  )
})

############################################################################################
### This section defines what happens when a user submits the survey.

observeEvent(input$submit, {
  results <- getSurveyData()
  print(results)
  #add time and adjudication rows to results dataframe
  survey_time <- data.frame("NO_USER_ID", "survey_time", "time", format(Sys.time(), "%Y%m%d-%H%M%OS"))
  names(survey_time) <- c("subject_id", "question_id", "question_type", "response")
  results_withtime <-rbind(results, survey_time)
  #filter columns and tranpose dataframe
  results_responses <- results_withtime %>% select(question_id, response)
  results_resp_transpose <- as.data.frame(t(as.matrix(results_responses)))
  #--function to convert first row to header
  header.true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
  }
  #--
  results_final <- header.true(results_resp_transpose)
  print(results_final)
  
  # generate a file name based on timestamp and a unique hash
  hash_seed <- "hello world"
  isolate(
    fileName <- paste0(
      paste(
        getFormattedTimestamp(),
        digest(hash_seed, algo = "md5"),
        sep = "_"
      ),
      ".csv"
    )
  )


############################################################################################
### This code chunk writes the csv file and will have to change based on where we store persistent data
  
  write.csv(x = results_final, file = file.path(resultsDir, fileName),
            row.names = FALSE)
  #    write.csv(x = results_final, file = file.path(tempdir(), fileName),
  #             row.names = FALSE)
  #    aws.s3::s3write_using(results_final,
  #                         object = fileName,
  #                         FUN = write.csv,
  #                         bucket = "usaf-data-tenant-afac/det5/shinywritetest/1-transient")
  

############################################################################################
### After submitting the survey, it'll thank the user here.
  
  shiny::showModal(
    shiny::modalDialog(
      p("We appreciate your participation."),
      p("Your survey results have been saved."),
      title = paste("Thank you for taking the  '",currentSurvey,"' !",sep=""),
    )
  )
})

############################################################################################
### This section is for the file upload tab. Essentially, I wanted a way to upload a .csv file in various formats and analyze the data

data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
})
# this reactive output contains the summary of the dataset and display the summary in table format
output$filedf <- renderTable({
  if(is.null(data())){return ()}
  input$file
})
# this reactive output contains the summary of the dataset and display the summary in table format
output$sum <- renderTable({
  if(is.null(data())){return ()}
  summary(data())
})
# This reactive output contains the dataset and display the dataset in table format
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
# the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
output$tb <- renderUI({
    tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
})

}

############################################################################################
### #This negates the need to have a server.R and ui.R file.

shinyApp(ui, server) 