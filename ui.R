library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)


# Define UI for application that draws a histogram
shinyUI(
    
    dashboardPage(
        
        dashboardHeader(title = "Home Automation"),
        dashboardSidebar(
            actionButton("fan", "Fan"),
            actionButton("light", "Light"),
            actionButton("refridgrator", "Refrigerator"),
            actionButton("oven", "Oven"),
            actionButton("ac", "AC"),
            actionButton("tv", "TV"),
            actionButton("mixer", "Mixer"),
            actionButton("washingmachine", "Washing Machine"),
            actionButton("cleaner", "Vacuume Cleaner"),
            actionButton("data", "LoadProfile")
        ),
        dashboardBody(
            # Boxes need to be put in a row (or column)
            
            
            dataTableOutput("load"),
            textOutput("text"),
            useShinyalert()
            
        )
        
    )
)
