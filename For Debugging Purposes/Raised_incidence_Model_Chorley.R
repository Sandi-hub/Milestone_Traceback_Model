library(spatstat)
S <- split(chorley)
larynx <- S$larynx # cases
lung <- S$lung # controls
smo <- density(lung, sigma = 0.15, eps = 0.1, positive = TRUE)
smo <- eval.im(pmax(smo, 1e-10))
Q <- quadscheme(larynx, eps = 0.1)

# Fitting the null model
chorley0fit <- ppm(Q ~ offset(log(smo)))

d2incin <- function(x, y, xincin = 354.5, yincin = 413.6) {
  (x - xincin)^2 + (y - yincin)^2
}
raisin <- function(x, y, alpha, beta) {
  1 + alpha * exp(-beta * d2incin(x, y))
}
# Fitting the one model
chorleyDfit <- ippm(Q ~ offset(log(smo) + log(raisin)),
                    start = list(alpha = 5, beta = 1)
)


