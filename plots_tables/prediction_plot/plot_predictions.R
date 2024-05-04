library(spatialfil)
library(tmvtnorm)
library(reshape2)
library(grid)
library(gridExtra)
library(ggdmc)

# We need to modify the applyFilter()-Funktion aus dem spatialfil-Package
source("plots_tables/prediction_plot/applyFilter.R")

figMod <- function(mat, top, x.title, y.title, title = NULL, rowtitle = " ", alpha = 0,  ...){
  mat <- melt(mat)
  p <- ggplot(mat, aes(x = Var1, y = Var2)) + 
    geom_raster(aes(fill=value)) +
    scale_fill_gradient(limits = c(0, top),
                        low="white", high="black") +
    geom_point(aes(x = 80.5, y = 80.5), size = 1, alpha = alpha) +
    geom_hline(yintercept = 80.5, linetype = "dashed", color = "lightgrey") +
    geom_vline(xintercept = 80.5, linetype = "dashed", color = "lightgrey") +
    scale_x_continuous(breaks = c(0, 40.25, 80.5, 120.75, 161),
                       labels = c(-2, -1, 0, 1, 2),
                       expand = c(0.01,0)) +
    scale_y_continuous(breaks = c(0, 40.25, 80.5, 120.75, 161),
                       labels = c(-2, -1, 0, 1, 2),
                       expand = c(0.01, 0)) +
    theme_bw() +
    coord_cartesian(clip = "off") +
    labs(x = x.title, 
         y = y.title, title = title) +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks.length = unit(.2, "cm"),
          plot.title = element_text(hjust = .5),
          legend.position = "none") +
    theme(...)
  arrangeGrob(p, left = textGrob(rowtitle, rot = 90, vjust = 0))
}


# Create Plot -------------------------------------------------------------

sd0 <- .5
eta = .5

gamma <- seq(-2, 2, .025)

kern <- convKernel(sigma = 5, k = "gaussian")

nrmlz <- function(mat)
{
  tot <- sum(mat)
  mat/tot
}


#Conditional model specification
norm0 <- function(theta1, theta2, Sigma) dnorm(theta1, 0,Sigma) * dnorm(theta2, 0, Sigma)
norm <- function(theta1, theta2, Sigma) dmvnorm(cbind(theta1, theta2), c(0,0), Sigma)
normT1 <- function(theta1, theta2, Sigma, l, u) dtmvnorm(cbind(theta1, theta2)
                                                         , c(0,0)
                                                         , Sigma
                                                         , lower = rep(l, 2)
                                                         , upper = rep(u, 2))
normT <- function(theta1, theta2, Sigma, l , u){
  dtnorm(theta1, 0, Sigma, lower = l, upper = u) * dtnorm(theta2, 0, Sigma, lower = l, upper = u)
}

Null <- outer(gamma, gamma, norm0, Sigma = .002)
Null <- nrmlz(Null)
One <- outer(gamma
             , gamma
             , normT1
             , Sigma = matrix(c(sd0^2, sd0^2.001, sd0^2.001, sd0^2)
                              , nrow = 2)
             , l = -Inf
             , u = Inf) 
One <- nrmlz(One)
Pos <- outer(gamma
             , gamma
             , normT
             , sd0
             , l = 0
             , u = Inf)
Pos <- nrmlz(Pos)
General <- outer(gamma
                 , gamma
                 , norm
                 , Sigma = matrix(c(sd0^2, 0, 0, sd0^2)
                                  , nrow = 2))
General <- nrmlz(General)

priorPos1 <- outer(gamma
                   , gamma
                   , normT1
                   , Sigma = matrix(ncol = 2, c(sd0^2, 0, 0, .005^2))
                   , l = 0
                   , u = Inf)
priorPos1 <- nrmlz(priorPos1)

priorPos2 <- outer(gamma
                   , gamma
                   , normT1
                   , Sigma = matrix(ncol = 2, c(.005^2, 0, 0, sd0^2))
                   , l = 0
                   , u = Inf)
priorPos2 <- nrmlz(priorPos2)

priorSpike <- outer(gamma
                    , gamma
                    , normT1
                    , Sigma = matrix(ncol = 2, c(.005^2, 0, 0, .005^2))
                    , l = 0
                    , u = Inf)
priorSpike <- nrmlz(priorSpike)

#Marginal model specification
GeneralH <- outer(gamma
                  , gamma
                  , norm
                  , Sigma = matrix(c(sd0^2, eta*sd0^2, eta*sd0^2, sd0^2)
                                   , nrow = 2))
GeneralH <- nrmlz(GeneralH)

PosH <- 4 * GeneralH
index <- gamma < 0
PosH[index, ] <- 0
PosH[, index] <- 0
PosH <- nrmlz(PosH)

#Model Predictions
NullP <- nrmlz(applyFilter(Null, kern))
OneP <- nrmlz(applyFilter(One, kern))
PosP <- nrmlz(applyFilter(PosH, kern))
GeneralP <- nrmlz(applyFilter(GeneralH, kern))

#####Figure
top1 <- max(One, PosH)
top2 <- max(Pos)
top3 <- max(NullP)

true1 <- expression(paste("True ", theta[1]))
true2 <- expression(paste("True ", theta[2]))
est1 <- expression(paste("Estimated ", theta[1]))
est2 <- expression(paste("Estimated ", theta[2]))

a <- figMod(Null, top1, true1, true2, 
            title = "Model", rowtitle = "\nNull", alpha = 1)
b <- figMod(NullP, top3, est1, est2, 
            title = "Prediction")
c <- figMod(One, top1, true1, true2, 
            title = NULL, rowtitle = "\nCommon")
d <- figMod(OneP, top2, est1, est2, 
            title = NULL)
e <- figMod(Pos, top2, true1, true2, 
            title = NULL, rowtitle = "\nPositive")
f <- figMod(PosP, top2, est1, est2, 
            title = NULL)
g <- figMod(General, top2, true1, true2, 
            title = NULL, rowtitle = "\nUnconstrained")
h <- figMod(GeneralP, top2, est1, est2, 
            title = NULL)
cowplot::plot_grid(c, d, e, f, g, h,
          ncol = 2, rel_heights = c(1.1, .95, .95, .95),
          rel_widths = c(1.1, 1))
