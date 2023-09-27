# server for <title>

library(shiny)
library(shinythemes)
library(fields)

# set color scheme

palette("Okabe-Ito")

shinyServer(function(input, output, session){
  
  output$intro_plot1a = renderPlot({
    par(mar = c(1,1,1,1))
    x = seq(0,100,4)
    y = seq(0,100,4)
    model_1 = function(a,b){
      3.349 + 1.683*a + 2.183*b - 0.0125*a^2 - 0.01750*b^2 - 0.008660*a*b
    }
    z1 = outer(x,y,model_1)
    nrz = nrow(z1)
    ncz = ncol(z1)
    nbcol = 100
    col.pal<-colorRampPalette(c(5,3))
    color<-col.pal(nbcol)
    z1facet = (z1[-1, -1] + z1[-1, -ncz] + z1[-nrz, -1] + z1[-nrz, -ncz])/4
    facet1col = cut(z1facet,nbcol)
    persp(x,y,z1,scale = TRUE, shade = NA, col = color[facet1col],
          theta = 45, phi = 35, ticktype = "detailed",
          xlab = "extraction time", ylab = "solid-to-solvent ratio",
          zlab = "extraction yield")
    imagePlot(legend.only=T, zlim=range(z1facet), col = color)
  })
  
  output$intro_plot1b = renderPlot({
    par(mar = c(1,1,1,1))
    x = seq(0,100,4)
    y = seq(0,100,4)
    model_3 = function(a,b){
      93.30 - 0.3660*a - 1.366*b - 0.0050*a^2 + 0.0050*b^2 + 0.01732*a*b
    }
    z3 = outer(x,y,model_3)
    nrz = nrow(z3)
    ncz = ncol(z3)
    nbcol = 100
    col.pal<-colorRampPalette(c(5,3))
    color<-col.pal(nbcol)
    z3facet = (z3[-1, -1] + z3[-1, -ncz] + z3[-nrz, -1] + z3[-nrz, -ncz])/4
    facet3col = cut(z3facet,nbcol)
    persp(x,y,z3,scale = TRUE, shade = NA, col = color[facet3col],
          theta = 45, phi = 35, ticktype = "detailed",
          xlab = "extraction time", ylab = "solid-to-solvent ratio",
          zlab = "extraction yield")
    imagePlot(legend.only=T, zlim=range(z3facet), col=color)
  })

  output$act1_plot = renderPlot({
    
    x = seq(0,100,4)
    y = seq(0,100,4)
    polymodel = function(a,b){
      input$beta0 + input$beta1*a + input$beta2*b + input$beta11*a^2 + input$beta22*b^2 + input$beta12*a*b
    }
    z = outer(x,y,polymodel)
    nrz = nrow(z)
    ncz = ncol(z)
    nbcol = 100
    col.pal<-colorRampPalette(c(5,3))
    color<-col.pal(nbcol)
    zfacet = (z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz])/4
    facetcol = cut(zfacet,nbcol)
    par(mar = c(1,1,1,1))
    z_range = range(z, na.rm = TRUE)
    if ((z_range[2] - z_range[1]) != 0) {
    persp(x,y,z,scale = TRUE, shade = NA, col = color[facetcol],
          theta = input$turn, phi = input$tilt, ticktype = "detailed",
          xlab = "factor 1", ylab = "factor 2", 
          zlab = "response")
    imagePlot(legend.only=T, zlim=range(zfacet), col = color)
    }
  })
  
  output$act2_plot = renderPlot({
    peroxide = c(20,20,10,10,15,22,8,15,15,15,15,15)
    sulfuric = c(20,10,20,10,15,15,15,22,8,15,15,15)
    absorbance = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000
    lm.r = lm(absorbance ~ peroxide * sulfuric + I(peroxide^2) + I(sulfuric^2))
    
    if (input$act2_show == "yes"){
    xmod = seq(0,30,1)
    ymod = seq(0,30,1)
    ccmod = function(a,b){
      lm.r$coefficients[1] + lm.r$coefficients[2]*a + lm.r$coefficients[3]*b + lm.r$coefficients[4]*a^2 + lm.r$coefficients[5]*b^2 + lm.r$coefficients[6]*a*b
    }
    zmod = outer(xmod,ymod,ccmod)
    nrz = nrow(zmod)
    ncz = ncol(zmod)
    nbcol = 100
    col.pal<-colorRampPalette(c(5,3))
    color<-col.pal(nbcol)
    zfacet = (zmod[-1, -1] + zmod[-1, -ncz] + zmod[-nrz, -1] + zmod[-nrz, -ncz])/4
    facetcol = cut(zfacet,nbcol)
    p = persp(xmod, ymod, zmod, scale = TRUE, shade = NA, 
              col = color[facetcol],
              theta = input$act2_turn, phi = input$act2_tilt, 
              ticktype = "detailed",
              xlab = "drops of peroxide (x)", 
              ylab = "drops of sulfuric acid (y)", 
              border = NULL,
              zlab = "response (z)", zlim = c(0,1))
    points(trans3d(x = peroxide, y = sulfuric, absorbance, pmat = p), 
           col = 1, pch = 19, type = "p")
    imagePlot(legend.only=T, zlim=range(zfacet), col = color)
    } else {
      xadj = c(2,2,-2,-2,2,3,-3,0,0,-2,-2,2)
      yadj = c(2,-2,2,-2,2,0,0,2,-2,2,-2,-2)
      
      plot(x = peroxide, y = sulfuric, pch = 19, col = c(rep(4,4),rep(8,8)), 
           xlab = "drops of peroxide", xlim = c(0,30),
           ylab = "drops of sulfuric acid", ylim = c(0,30),
           asp = 1, cex = 2)
      text(x = peroxide + xadj, y = sulfuric + yadj, labels = absorbance, cex = 1)
      rect(xleft = 10, xright = 20, ybottom = 10, ytop = 20, lwd = 4, border = 4)
      lines(x = c(15,15), y = c(8,22), lwd = 4, col = 8)
      lines(x = c(8,22), y = c(15,15), lwd = 4, col = 8)
      grid()
    }
  })
  
 output$wrapup_plot = renderPlot({
   peroxide = c(20,20,10,10,15,22,8,15,15,15,15,15)
   sulfuric = c(20,10,20,10,15,15,15,22,8,15,15,15)
   absorbance = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000
   lm.r = lm(absorbance ~ peroxide * sulfuric + I(peroxide^2) + I(sulfuric^2))
   
   xmod = seq(0,30,1)
   ymod = seq(0,30,1)
   ccmod = function(a,b){
     lm.r$coefficients[1] + lm.r$coefficients[2]*a + lm.r$coefficients[3]*b + lm.r$coefficients[4]*a^2 + lm.r$coefficients[5]*b^2 + lm.r$coefficients[6]*a*b
   }
   zmod = outer(xmod,ymod,ccmod)
   
   contour(x = xmod, y = ymod, z = zmod,
           levels = seq(0,1,0.05), asp = 1,
           xlim = c(0,30), xlab = "drops of peroxide",
           ylim = c(0,30), ylab = "drops of sulfuric acid",
           lwd = 4, col = seq(1,9,1), labcex = 1.25)
   points(x = sulfuric, y = peroxide, pch = 19, col = "black", cex = 2)
   grid()
 }) 

}) 
