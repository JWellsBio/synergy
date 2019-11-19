library(shiny)
library(shinyLP)
library(shinyBS)
library(shinythemes)
#shinyLP::runExample()

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyLP)
library(shinythemes)

# Define UI for application
shinyUI(
  
  # Include a fliudPage above the navbar to incorporate a icon in the header
  # Source: http://stackoverflow.com/a/24764483
  fluidPage(
    
    list(tags$head(HTML('<link rel="icon", href="logo.png",
                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="Window Tab title"
        )
    ),
    
    navbarPage(title=div(img(src="dpine_lochup_40x31.jpg"), "Different Ways to Use the App"),
               inverse = F, # for diff color view
               theme = shinytheme("slate"),
               
               tabPanel("Home Page", icon = icon("home"),
                        
                        jumbotron("This will be the home page for the App", "A brief introduction will go here\nincluding link to the original publication\nas well as this publication",
                                  buttonLabel = "Click Me"),
                        fluidRow(
                          column(6, panel_div(class_type = "success", panel_title = "Directions",
                                              content = "Description of different types of experiments and the tabs for those")),
                          column(6, panel_div("success", "Application Maintainer",
                                              HTML("Email Me: <a href='mailto:wellsjasond@gmail.com?Subject=Synergy_App%20Help' target='_top'>Jason Wells</a>")))
                        ),  # end of fluidRow
                        fluidRow(
                          column(6, panel_div("success", "App Status", "Include text with status, version and updates")),
                          column(6, panel_div("success", "Security and License", "Copyright 2019")),
                          
                          #### FAVICON TAGS SECTION ####
                          tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                          
                          bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
                                  p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
                                  iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
                          )
                          
                        )),
               tabPanel("Single Dose", icon = icon("dna"),
                        wells(content = "Here is where you can analyze data from an experiment where a single dose of each treatment was used",
                              size = "default"),
                        h1("Hey There!", align = "center"),
                        hr(),
                        
                        list_group(div(list_item("Maybe Number of Users", badge_value = 270000),
                                       list_item("Number of Updates", badge_value = 24)))
                        
                        
               ),
               
               tabPanel("All Designs", icon = icon("dna"),
                        
                        jumbotron("Hey There!", "Could possibly link to all different experiment apps here??",
                                  button = FALSE),
                        hr(),
                        fluidRow(
                          column(4, thumbnail_label(image = 'Rlogo.png', label = 'Single Dose',
                                                    content = 'This would be the location of the first experimental design
                                                    with single dose treatments of each drug',
                                                    button_link = 'C:/Users/jwells/Desktop/app.R', button_label = 'Click me')
                          ),
                          column(4, thumbnail_label(image = 'Rlogo.png', label = 'With Control Group',
                                                    content = 'This would be the location of the second experimental design
                                                    with comparisons to control groups',
                                                    button_link = 'http://getbootstrap.com/', button_label = 'Click me')),
                          column(4, thumbnail_label(image = 'Rlogo.png', label = 'Tumor Growth',
                                                    content = 'This would be the location of the third experimental design
                                                    that analyzes results of tumor growth experiments',
                                                    button_link = 'http://getbootstrap.com/', button_label = 'Click me'))
                          
                          )))
    
                          ) # end of fluid page
                        ) # end of shiny