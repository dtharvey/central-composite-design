# file: persp_code.R

x = seq(0,100,4)
y = seq(0,100,4)

model_1 = function(a,b){
  3.349 + 1.683*a + 2.183*b - 0.0125*a^2 - 0.01750*b^2 - 0.008660*a*b
}
model_2 = function(a,b){
  96.65 - 1.683*a - 2.183*b + 0.0125*a^2 + 0.01750*b^2 + 0.008660*a*b
}
model_3 = function(a,b){
  93.30 - 0.3660*a - 1.366*b - 0.0050*a^2 + 0.0050*b^2 + 0.01732*a*b
}

z1 = outer(x,y,model_1)
z2 = outer(x,y,model_2)
z3 = outer(x,y,model_3)

nrz = nrow(z1)
ncz = ncol(z1)
nbcol = 100

col.pal<-colorRampPalette(c(5,3))
colors<-col.pal(nbcol)
zfacet = (z1[-1, -1] + z1[-1, -ncz] + z1[-nrz, -1] + z1[-nrz, -ncz])
facetcol = cut(zfacet,nbcol)


persp(x,y,z1,scale = TRUE, shade = NA, col = colors[facetcol],
      theta = 45, phi = 35, ticktype = "detailed")
# persp(x,y,z2,scale = TRUE, shade = NA, col = colors[facetcol],
#       theta = 45, phi = 35, ticktype = "detailed")
# persp(x,y,z3,scale = TRUE, shade = NA, col = colors[facetcol],
#       theta = 45, phi = 35, ticktype = "detailed")




# library(fields)
# imagePlot(x, y, z1)
