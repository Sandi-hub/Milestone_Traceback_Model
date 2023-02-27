
library(sf)
library(spatstat)
sf_outbreak <- st_read("./Data/Outbreak/Aldi_Outbreak.shp")
ppp_outbreak <- as.ppp(X=sf_outbreak$geometry, W=owin(c(4303150,4342650), c(3365250,3403550)))
im_population <- readRDS("./Data/Population Data/im_population.rds")
sf_stores <- st_read("./Data/Potential Pattern/Aldi_store_6.shp")
ppp_stores <- as.ppp(X=sf_stores$geometry, W=owin(c(4303150,4342650), c(3365250,3403550)))
ppp_stores
fit0 <- ppm(ppp_outbreak ~ offset(log(im_population)), gcontrol = glm.control(maxit = 10000))



d2source <- function(x, y, xPOS= 4316914, yPOS=3385953) {
  (x - xPOS)^2 + (y - yPOS)^2}
raisin <- function(x, y, alpha, beta) {
  1 + alpha * exp(-beta * d2source(x, y))
}
fit1 <- ippm(ppp_outbreak ~ offset(log(im_population) + log(raisin) ),gcontrol = glm.control(maxit = 10000),
             start = list(alpha = 5, beta = 1) )




