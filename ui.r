########## Load libraries 
#library(party)
#library(rpart.plot)
#library(rpart)

#install.packages('shiny')
library(shiny)

#install.packages("scatterplot3d", dependencies = TRUE)
#library(scatterplot3d)


# A read-only data set that will load once, when Shiny starts, and will be
# available to each user session

result_library <- read.csv("E4.1G4.1B2.8M_OpTesultUnderRep.csv")

act_list <- seq(1, 36, by=1)
gpa_list <- seq(0, 4.9, by=0.1)
ethnicity_list <- sort(unique(result_library$ETHNICITY.DESC))
tier_list<-sort(unique(result_library$HS.COUNTY.TIER))
EFC_list<-unique(result_library$APP.YEAR.EFC)
Raider_list<-sort(unique(result_library$HS.RAIDER.COUNTRY.CATEGORY))
GENDER_list<-sort(unique(result_library$GENDER.CODE))
PELL_list<-sort(unique(result_library$PellGrant))
program_list <- sort(unique(result_library$APP.MAJOR))
college_list <- sort(unique(result_library$APP.INTENDING.COLLEGE.CODE))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Enrollment Managment: Select GPA, ACT, EFC, Program to Award Scholarship"),
  
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 200px; }"),
      tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      tags$style(type='text/css', ".well { max-width: 250px; }"),
      tags$style(type='text/css', ".span4 { max-width: 250px; }")
    ),
    
   
    selectInput(inputId = "act", label = "ACT scores:", choices = act_list, selected = 24),
    selectInput(inputId = "gpa", label = "High School GPA Score",  choices = gpa_list,  selected = 3.5),
    
    helpText("The following are for analysis in respective tabs after GPA/ACT"),
    selectInput(inputId = "ethnity", label = "Ethnity",   choices = ethnicity_list),
    selectInput(inputId = "tier", label = "Tier",  choices = tier_list),
    # selectInput(inputId = "EFC", label = "EFC",choices = EFC_list),
    selectInput(inputId = "raider", label = "Raider",  choices = Raider_list),
    selectInput(inputId = "pell", label = "Pell--Derived",   choices = PELL_list),
    selectInput(inputId = "college", label = "College",   choices = college_list),
    selectInput(inputId = "program", label = "Program",   choices = program_list)
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Composite Score Summary", plotOutput(outputId = "coms_plot", height = "800px")),
      tabPanel("GPA ACT Average Summary", dataTableOutput("gpa_act_table")), 
      tabPanel("Suggested Scholarship Award", dataTableOutput("scholarship_table")), 
      tabPanel("The Complete Table", dataTableOutput("complete_table")) ,
      tabPanel("Tier Summary", plotOutput(outputId = "tier_plot", height = "800px")), 
      tabPanel("Raider Summary", plotOutput(outputId = "Raider_plot", height = "800px")),
      tabPanel("IN/OUT State Summary", plotOutput(outputId = "state_plot", height = "800px")),
      tabPanel("Ethnicity Summary", plotOutput(outputId = "ethnicity_plot", height = "800px")), 
      tabPanel("Gender Summary", plotOutput(outputId = "gender_plot", height = "800px")),
      tabPanel("College Summary", plotOutput(outputId = "college_plot", height = "800px")), 
      tabPanel("Program Summary", plotOutput(outputId = "program_plot", height = "800px")), 
      tabPanel("Cluster Summary", plotOutput(outputId = "cluster_plot", height = "800px"))
    )
    
  )
  
))

