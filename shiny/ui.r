library(shiny)
library(quid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(brio)
library(data.table)
library(rsconnect)
library(plotly)
library(RColorBrewer)
load("plottable.rda")

shinyUI(
  navbarPage("Qualitative individual differences in cognitive inhibition tasks",
             
             tabPanel("Datasets",
                      sidebarLayout(
                        
                        sidebarPanel(
                          selectInput("Dataset", "Select dataset",
                                      choices = c("Select a dataset", 
                                                  "Dataset 1: Stroop (Von Bastian, Souza, & Gade, 2015)", 
                                                  "Dataset 2: Stroop (Pratte, Rouder, Morey, & Feng, 2010)", 
                                                  "Dataset 3: Stroop (Pratte, Rouder, Morey, & Feng, 2010)", 
                                                  "Dataset 4: Stroop (Rey-Mermet, Gade, & Oberauer, 2018)", 
                                                  "Dataset 5: Stroop (Rey-Mermet, Gade, & Oberauer, 2018)", 
                                                  "Dataset 6: Stroop (Hedge, Powell, & Sumner, 2018)", 
                                                  "Dataset 7: Simon (Von Bastian, Souza, & Gade, 2015)", 
                                                  "Dataset 8: Simon (Pratte, Rouder, Morey, & Feng, 2010)", 
                                                  "Dataset 9: Simon (Pratte, Rouder, Morey, & Feng, 2010)", 
                                                  "Dataset 10: Flanker (Von Bastian, Souza, & Gade, 2015)", 
                                                  "Dataset 11: Flanker (Rey-Mermet, Gade, & Oberauer, 2018)", 
                                                  "Dataset 12: Flanker (Rey-Mermet, Gade, & Oberauer, 2018)", 
                                                  "Dataset 13: Flanker (Hedge, Powell, & Sumner, 2018)", 
                                                  paste(rep("Dataset ", 21), 14:34, rep(": Stroop (Cebersole et al., 2016)", 21), sep = "")))
                        ),
                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Instructions", verbatimTextOutput("instructions1"),
                                               tags$style(type = 'text/css', '#instructions1         
                        {font-size: 16px; font-family: helvetica; background-color: rgba(255,255,255,0.40); color: grey; border-style: none;}')),
                                      tabPanel("Summary", tableOutput("stats")),
                                      tabPanel("Plot", imageOutput("myPlot"))
                          )
                        )
                      )),
             tabPanel("Properties",
                      sidebarLayout(
                        
                        sidebarPanel(
                          radioButtons("quid", "Select y-axis input",
                                      choices = c("Bayes Factor", "Preferred model")),
                          radioButtons("property", "Select x-axis input",
                                       choices = c("Cognitive Inhibition Task",
                                                   "Mean effect (ms)",
                                                   "Standard deviation effect (ms)",
                                                   "Mean age",
                                                   "Percentage female",
                                                   "Percentage male",
                                                   "Number of participants",
                                                   "Number of experimental blocks",
                                                   "Number of trials",
                                                   "Usage of neutral trials",
                                                   "Percentage incongruent trials",
                                                   "Feedback",
                                                   "Fixation cross",
                                                   "Time limit",
                                                   "Attenuation factor η"))
                          ),
                        
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Instructions", verbatimTextOutput("instructions2"),
                                                 tags$style(type = 'text/css', '#instructions2         
                        {font-size: 16px; font-family: helvetica; background-color: rgba(255,255,255,0.40); color: grey; border-style: none;}')),
                                        tabPanel("Plot", plotlyOutput("myPlot2"))
                          )
                      )
             )
  )
  )
)