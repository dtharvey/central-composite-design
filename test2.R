library(shiny)
library(shinythemes)
library(graph3d)

ui = navbarPage("AC 3.0: Central Composite Experimental Designs",
                theme = shinytheme("journal"),
                header = tags$head(
                  tags$link(rel = "stylesheet",
                            type = "text/css",
                            href = "style.css") 
                ),
                
                tabPanel("Introduction",
                         fluidRow(
                           column(width = 6, # left column that holds text
                                  wellPanel(
                                    includeHTML("text/introduction.html") 
                                  )),
                           column(width = 6, 
                                  align = "center",
                                  graph3dOutput("intro_plot", height = "400px")
                                  # plotOutput("intro_plot", height = "400px")
                           )
                         )) # close introduction tabPanel
                
                
) # close navbarpage

# place for data files and scripts used in server file
x = seq(0,100,4)
y = seq(0,100,4)
xy = expand.grid(x = x, y = y)
model1 = function(a,b){
  3.349 + 1.683*a + 2.183*b - 0.0125*a^2 - 0.01750*b^2 - 0.008660*a*b
}
z = outer(x,y,model1)
xyz = transform(xy, z = model1(x,y))

# set color scheme

palette("Okabe-Ito")

server = function(input, output, session){
  
  output$intro_plot = renderGraph3d({
    graph3d(data = xyz, showLegend = FALSE)
  })
  
} # close the servers

shinyApp(ui, server)
