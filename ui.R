# user interface for central composite design app

library(shiny)
library(shinythemes)

ui = navbarPage("AC 3.0: Introduction to Response Surfaces",
     theme = shinytheme("journal"),
     header = tags$head(
       tags$link(rel = "stylesheet",
                 type = "text/css",
                 href = "style.css") 
     ),
     
     tabPanel("Introduction",
      fluidRow(
        column(width = 6,
          wellPanel(
            includeHTML("text/introduction.html") 
      )),
        column(width = 6, 
          align = "center",
          plotOutput("intro_plot1a", height = "350px"),
          plotOutput("intro_plot1b", height = "350px")
          )
      )),
     
     tabPanel("Visualizing the Response Surface",
      fluidRow(
        column(width = 6,
          wellPanel(
            includeHTML("text/activity1.html")
          )),
        column(width = 6,
          align = "center",
          splitLayout(
            sliderInput("beta0","beta 0", min = 0, max = 100,
                       step = 0.1, value = 3, 
                       width = "100px", ticks = FALSE),
            sliderInput("beta1", "beta 1",
                        min = -3, max = 3, step = 0.1, value = 1.7,
                        width = "100px", ticks = FALSE),
            sliderInput("beta2", "beta 2",
                        min = -3, max = 3, step = 0.1, value = 2.2,
                        width = "100px", ticks = FALSE),
            sliderInput("beta11", "beta 11",
                        min = -0.02, max = 0.02, step = 0.001, value = -0.013,
                        width = "100px", ticks = FALSE),
            sliderInput("beta22", "beta 22",
                        min = -0.02, max = 0.02, step = 0.001, value = -0.018,
                        width = "100px", ticks = FALSE),
            sliderInput("beta12", "beta 12",
                        min = -0.2, max = 0.2, step = 0.001, value = -0.009,
                        width = "100px", ticks = FALSE)
          ),
          splitLayout(
            sliderInput("turn", "rotate the xy plane",
                        min = -180, max = 180, step = 1, value = 45,
                        width = "200px", ticks = FALSE),
            sliderInput("tilt", "tilt the z axis",
                        min = -180, max = 180, step = 1, value = 35,
                        width = "200px", ticks = FALSE),
          ),
          plotOutput("act1_plot", height = "500px")
          )
        )),
     tabPanel("Modeling the Response Surface",
              fluidRow(
                column(width = 6,
                       wellPanel(
                         includeHTML("text/activity2.html")
                       )),
                column(width = 6,
                       align = "center",
                       
                       splitLayout(
                         radioButtons("act2_show", "display response surface?",
                                      choices = c("no","yes"), selected = "no"),
                         sliderInput("act2_turn", "rotate the xy plane",
                                     min = -180, max = 180, step = 1, value = 45,
                                     width = "200px", ticks = FALSE),
                         sliderInput("act2_tilt", "tilt z axis",
                                     min = -180, max = 180, step = 1, value = 35,
                                     width = "200px", ticks = FALSE),
                       ),
                       plotOutput("act2_plot", height = "600px")
                )
              )),
     tabPanel("Wrapping Up",
              fluidRow(
                column(width = 6,
                       wellPanel(id = "wrapuppanel",
                                 style = "overflow-y:scroll; max-height: 750px",
                                 includeHTML("text/wrapup.html"))),
                column(width = 6,
                       align = "center",
                       plotOutput("wrapup_plot", height = "700px"),
                       verbatimTextOutput("summary")
                )
                
              ))

   
     ) 

