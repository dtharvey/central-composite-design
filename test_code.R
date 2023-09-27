# test code for central-composite designs

x = c(1,1,-1,-1,0,2^(2/4),-2^(2/4),0,0,0,0,0)
y = c(1,-1,1,-1,0,0,0,2^(2/4),-2^(2/4),0,0,0)
z = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000

lm.r_coded = lm(z ~ x * y + I(x^2) + I(y^2))
summary(lm.r_coded)

# use cc data to create model
xuc = c(20,20,10,10,15,22,8,15,15,15,15,15)
yuc = c(20,10,20,10,15,15,15,22,8,15,15,15)
z = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000
lm.r_uncoded = lm(z ~ xuc * yuc + I(xuc^2) + I(yuc^2))
summary(lm.r_uncoded)

# build persp() plot
palette("Okabe-Ito")
xmod = seq(0,30,1)
ymod = seq(0,30,1)
ccmod = function(a,b){
  lm.r_uncoded$coefficients[1] + lm.r_uncoded$coefficients[2]*a + lm.r_uncoded$coefficients[3]*b + lm.r_uncoded$coefficients[4]*a^2 + lm.r_uncoded$coefficients[5]*b^2 + lm.r_uncoded$coefficients[6]*a*b
}
zmod = outer(xmod,ymod,ccmod)
nrz = nrow(zmod)
ncz = ncol(zmod)
nbcol = 100
col.pal<-colorRampPalette(c(5,3))
color<-col.pal(nbcol)
zfacet = (zmod[-1, -1] + zmod[-1, -ncz] + zmod[-nrz, -1] + zmod[-nrz, -ncz])/4
facetcol = cut(zfacet,nbcol)

  persp(xmod,ymod,zmod,scale = TRUE, shade = NA, col = color[facetcol],
        theta = 45, phi = 35, ticktype = "detailed",
        xlab = "factor 1", ylab = "factor 2", border = NULL,
        zlab = "response")
  imagePlot(legend.only=T, zlim=range(zfacet), col = color)

  p = persp(xmod,ymod,zmod,scale = TRUE, shade = NA, col = color[facetcol],
        theta = 25, phi = 35, ticktype = "detailed",
        xlab = "factor 1", ylab = "factor 2", border = NULL,
        zlab = "response", zlim = c(0,1))
  points(trans3d(x = xuc, y = yuc, z, pmat = p), col = 1, pch = 19, type = "p")
  points(trans3d(x = xuc, y = yuc, z, pmat = p), col = 1, pch = 19, type = "h")
  
  
  library(plotly)
  # build model
  xuc = c(20,20,10,10,15,22,8,15,15,15,15,15)
  yuc = c(20,10,20,10,15,15,15,22,8,15,15,15)
  z = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000
  lm.r_uncoded = lm(z ~ xuc * yuc + I(xuc^2) + I(yuc^2))
  summary(lm.r_uncoded)
  
  # create data for surface
  xmod = seq(0,30,1)
  ymod = seq(0,30,1)
  ccmod = function(a,b){
    lm.r_uncoded$coefficients[1] + lm.r_uncoded$coefficients[2]*a + lm.r_uncoded$coefficients[3]*b + lm.r_uncoded$coefficients[4]*a^2 + lm.r_uncoded$coefficients[5]*b^2 + lm.r_uncoded$coefficients[6]*a*b
  }
  zmod = outer(xmod,ymod,ccmod)
  
  # points
  xuc = c(20,20,10,10,15,22,8,15,15,15,15,15)
  yuc = c(20,10,20,10,15,15,15,22,8,15,15,15)
  z = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000
  
  # prepare plotly surface
  plot_ly(x = xmod, y = ymod, z = zmod) %>% 
    add_surface() %>%
    layout(scene = list(
      xaxis = list(title = "drops of H2O2"),
      yaxis = list(title = "drops of H2SO4"),
      zaxis = list(title = "absorbance", range = c(0,1), nticks = 5)
    )) %>%
    add_trace(type = "scatter3d", mode = "markers", 
              x = xuc, y = yuc, z = z) 
  
  
# central composite design

peroxide = c(20,20,10,10,15,22,8,15,15,15,15,15)
sulfuric = c(20,10,20,10,15,15,15,22,8,15,15,15)
absorbance = c(330,293,359,420,334,327,397,319,367,336,346,323)/1000
xadj = c(2,2,-2,-2,2,3,-3,0,0,-2,-2,2)
yadj = c(2,-2,2,-2,2,0,0,2,-2,2,-2,-2)

plot(x = peroxide, y = sulfuric, pch = 19, col = 6, 
     xlab = "drops of peroxide", xlim = c(0,30),
     ylab = "drops of sulfuric acid", ylim = c(0,30),
     asp = 1)
text(x = peroxide + xadj, y = sulfuric + yadj, labels = absorbance, cex = 0.75)
rect(xleft = 10, xright = 20, ybottom = 10, ytop = 20)
lines(x = c(15,15), y = c(8,22))
lines(x = c(8,22), y = c(15,15))
grid()



The data in the table below provides one example

# ```{r, echo=FALSE, results="asis"}
# library(knitr)
# col_one = seq(1,6,1)
# col_two = c(+1,+1,-1,-1,0,+1.414)
# col_three = c(+1,-1,+1,-1,-1.414,0)
# col_four = c(0.330,0.293,0.359,0.420,0.367,0.327)
# col_five = seq(7,12,1)
# col_six = c(-1.414,0,0,0,0,0)
# col_seven = c(0,+1.414,0,0,0,0)
# col_eight = c(0.397,0.319,0.334,0.336,0.346,0.323)
# df = data.frame(col_one,col_two,col_three, col_four, 
#                 col_five, col_six, col_seven, col_eight)
# colnames(df) = c("std", "x", "y", "response", 
#                  "std", "x", "y", "response")
# 
# kable(df, align = "cccccccc", digits = 3)
# ```

