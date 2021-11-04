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
#library(semantic.dashboard) #This might be useful if we want to make it look much prettier/customizable than just a skin color; The themes are at https://semantic-ui-forest.com/themes

##This section reads in each survey file and needs to be duplicated for each survey excel file that we have.
UMUXSurvey <- read_excel("surveys/UMUXSurvey.xlsx", na=c("", "NA"))
#simpleSurvey <- read_excel("surveys/simple_survey.xlsx", na=c("", "NA"))
#cat("loaded UMUXSurvey:\n") # displays what's happening in the console just to make sure it's all working nicely
#print(UMUXSurvey) # displays the data in the console just to make sure it's all working nicely
#cat("loaded simpleSurvey:\n") # displays what's happening in the console just to make sure it's all working nicely
#print(simpleSurvey) # displays the data in the console just to make sure it's all working nicely
#currentSurvey <- NA # not being used yet, but might be later

dataset <- read.csv("data/datatest.csv") #reads in the dataset csv file - this will need to call whatever our final dataset database looks like
#surveys <- data.frame(name=c("UMUX", "Simple"), nr=c('UMUX','Simple'))

######################################S3 STUFF###############################################
###------Get S3 Credentials from Databricks Job -----------------------------------------------

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

#####################################MAIN CODE###############################################
### -----Environment variables-----------------------------------------------------------------

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

# names of the fields on the form we want to save
fieldNames <- c("Uniform"
)

getFormattedTimestamp <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


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



## ---------------------------------------------------------------------------------------- ##
# This is the UI section of the app.R file. This tells the shiny server how to display the survey.
# For now, I have it displaying a very simple .html file, and the survey the user selects is the "survey" output
ui <- dashboardPage(skin = "black", #if using shinydashboard
# ui <- dashboardPage( theme = "slate", #if using semantic.dashboard
# Icons can be found here: https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free
  dashboardHeader(title = "Test Dashboard"), #dashboardHeader has more potential, I just didn't use it yet
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Test Schedule", tabName = "schedule", icon = icon("calendar-alt")),
      menuItem("Surveys", tabName = "surveys", icon = icon("poll")),
      menuItem("Mx Data", tabName = "mx_data", icon = icon("wrench")),
      menuItem("Paper Surveys", tabName = "surveys_paper", icon = icon("poll")),
      menuItem("Links", tabName = "links", icon = icon("wrench"))
    )
  ),

#This section defines what is in each tab. It uses markdown, but html tags work as well.
  dashboardBody( 
    tabItems(
      tabItem("home",
              fluidPage(
                h1("Home page")
              )

      ),


    tabItem("data",
              fluidPage(
              h1("Test Data"),
              div(style = 'overflow-x:scroll',dataTableOutput("datatable",width="100%")) #displays the datatable called in the server section and adds scroll functionality 
              
              )
    ),
    
    tabItem("schedule",
            fluidPage(
            h1("Test Schedule")
            )
    ),
    
    tabItem("surveys",
            fluidPage(
            h1("Survey tool"),
            p("The links below take you to the different survey or data collection forms for this program. After submitting each survey, the data will make its way to the database. It may take a few minutes for the data to show up, or it may need to be adjudicated first."),
            #fluidRow(selectInput("surveySelection", label = h5("Select a survey"), choices=c('UMUX','Simple'))),
            #fluidRow(htmlOutput("frame"))
            #fluidRow(htmlOutput("surveyFrame")),
            uiOutput("survey")
            )
    ),
    
    tabItem("mx_data",
            fluidPage(
            h1("Mx data import"),
            p("Put in the mx data importer tool here once it's ready.")
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


## ---------------------------------------------------------------------------------------- ##
# This is the server half of the shiny app.
server <- function(input, output, session) {

 output$datatable <- renderDataTable(dataset) #This calls our dataset pulled from the csv at the very top of this code. There are a lot of other things we can do to clean up this dataframe using mutates, filters, rename_all(~str_to_title(.x)), etc.


# #This renders a chosen survey into the frame in the survey tab.
#  observe({ 
#    query <- surveys[which(surveys$nr==input$surveySelection),2]
#    test <<- paste0("http://news.scibite.com/scibites/news.html?q=GENE$",query)
#  })
#  output$frame <- renderUI({
#    input$surveySelection
#    display <- tags$iframe(src=test, height="100%", width="100%")
#    # print(my_test)
#    display
#  })
#  
output$survey <- renderUI({
  #input$surveySelection
  surveyOutput(df = UMUXSurvey, #the dataframe is the appropriate excel file called earlier
               theme="blue", #can take normal colors or hex codes
               survey_title = "Test Survey", #this goes on the top of the page
               survey_description = "Welcome! Please fill out this test survey to check how data is stored in the database." #add some instructions, etc. for the survey
  )
})
cat("Survey rendering...\n") #just useful to see in the R console
#renderSurvey()
#currentSurvey <<- "UMUX Survey" #name of the survey in plain english for use in displays



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
  
  ## attempt to save response as local text file
  # read the info into a dataframe
  isolate(
    infoList <- t(sapply(fieldNames, function(x) x = input[[x]]))
  )
  
  # generate a file name based on timestamp, user name, and form contents
  isolate(
    fileName <- paste0(
      paste(
        getFormattedTimestamp(),
        # input$lastName,
        # input$firstName,
        digest(infoList, algo = "md5"),
        sep = "_"
      ),
      ".csv"
    )
  )
  
  # write out the results
  ### This code chunk writes a response and will have to change
  ### based on where we store persistent data
  write.csv(x = results_final, file = file.path(resultsDir, fileName),
            row.names = FALSE)
  #    write.csv(x = results_final, file = file.path(tempdir(), fileName),
  #             row.names = FALSE)
  #    aws.s3::s3write_using(results_final,
  #                         object = fileName,
  #                         FUN = write.csv,
  #                         bucket = "usaf-data-tenant-afac/det5/shinywritetest/1-transient")
  
  ### End of writing data
  
  shiny::showModal(
    shiny::modalDialog(
      p("We appreciate your participation."),
      p("Your survey results have been saved."),
      title = paste("Thank you for taking the  '",currentSurvey,"' !",sep=""),
    )
  )
})

}
## ---------------------------------------------------------------------------------------- ##
shinyApp(ui, server) #This negates the need to have a server.R and ui.R file.