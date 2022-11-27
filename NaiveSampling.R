HMC = function (U, grad_U, epsilon, L, current_q)
{
  q = current_q
  p = rnorm(length(q), 0, 1)  # independent standard normal variates
  current_p = p

  # Make a half step for momentum at the beginning

  p = p - epsilon * grad_U(q) / 2

  # Alternate full steps for position and momentum

  for (i in 1:L)
  {
    # Make a full step for the position

    q = q + epsilon * p

    # Make a full step for the momentum, except at end of trajectory

    if (i != L) p = p - epsilon * grad_U(q)
  }

  # Make a half step for momentum at the end.

  p = p - epsilon * grad_U(q) / 2

  # Negate momentum at end of trajectory to make the proposal symmetric

  p = -p

  # Evaluate potential and kinetic energies at start and end of trajectory

  current_U = U(current_q)
  current_K = sum(current_p^2) / 2
  proposed_U = U(q)
  proposed_K = sum(p^2) / 2

  # Accept or reject the state at end of trajectory, returning either
  # the position at the end of the trajectory or the initial position

  if (runif(1) < exp(current_U - proposed_U + current_K - proposed_K))
  {
    return (list(q = q, p = p) ) # accept
  }
  else
  {
    return (list(q = current_q, p = p))  # reject
  }
}

##############################
HMCstep = function (U, grad_U, epsilon, L, current_q)
{ # same as previous HMC code but keep track of each step
  Q <- q <- current_q
  ## p = 0.3845861
  p = rnorm(length(q), 0, 1)
  P <- current_p <- p
  p = p - epsilon * grad_U(q) / 2
  P <- cbind(P, p)
  Q <- cbind(Q, q)
  for (i in 1:L)
  {
  q = q + epsilon * p
    Q <- cbind(Q, q)
  if (i != L) {
    p = p - epsilon * grad_U(q)
    P <- cbind(P, p)
    }
  }
  p = p - epsilon * grad_U(q) / 2
  P <- cbind(P, p)
  p = -p
  P <- cbind(P, p)
  Q <- cbind(Q, q)
  current_U = U(current_q)
  current_K = sum(current_p^2) / 2
  proposed_U = U(q)
  proposed_K = sum(p^2) / 2
  ## if (0.5
  ##     < exp(current_U - proposed_U + current_K - proposed_K))
  ## {
  ##   return (list(q = q, p = p, Q = Q, P = P) ) # accept
  ## }
  ## else
  ## {
  ##   return (list(q = current_q, p = p, Q = Q, P = P))  # reject
  ## }
  if (runif(1)
      < exp(current_U - proposed_U + current_K - proposed_K))
  {
    return (list(q = q, p = p, Q = Q, P = P) ) # accept
  }
  else
  {
    return (list(q = current_q, p = p, Q = Q, P = P))  # reject
  }
}

U = function(q) { 3 * log(1 + (q^2) / 5) }

gradU = function(q) { 3 * (2 * q / 5) / (1 + (q^2) / 5) }
###############################
x <- y <- seq(-5, 5, len = 50)
H <- outer(U(x), 0.5 * y * y, "+")

library(ggplot2)
library(reshape2)

pq_plot <- function(H, x, y, n_levels = 25, transparency = 0.4, col = "white",
                    xtitre = "position q", ytitre = "momentum p") {
  theme_set(theme_bw(base_size = 16))
  dat <- reshape2::melt(H)
  dat$Var1 <- x[dat$Var1]
  dat$Var2 <- y[dat$Var2]
  g <- ggplot() +
    geom_tile(data = dat,
              aes(x = Var1, y = Var2, fill = value)
              ) +
    scale_fill_gradientn(name = "H", colors = viridisLite::viridis(256)) +
    geom_contour(data = dat,
                 aes(x = Var1, y = Var2, z = value),
                 bins = n_levels, color = col, alpha = transparency
                 ) +
    xlab(xtitre) + ylab(ytitre) +
    theme(legend.position = "top",
          legend.text = element_text(size = 10),
          plot.title = element_text(lineheight = 0.8, face = "bold"),
          axis.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()
          )
  return(g)
}
pq_plot(H = H, x = x, y = y)

##################################STEP by STEP
epsilon = 0.3
L = 15
Q <- current_q <- 4
###########################CHANGER pour voir le leapfrog
lstep = HMCstep(U, gradU, epsilon, L, current_q)
pq_plot(H = H, x = x, y = y) +
    geom_path(data = data.frame(x = drop(lstep$Q), y = drop(lstep$P)),
              aes(x = x, y = y), color = "white"
              ) +
    geom_point(data = data.frame(x = drop(lstep$Q), y = drop(lstep$P)),
               aes(x = x, y = y), color = "white"
               ) +
    coord_cartesian(xlim = range(x), ylim = range(y))

##################################One GO
epsilon = 0.3
L = 10
Q <- current_q <- 0
p <- q <- NULL

for (n in 1:10000) {
  l = HMC(U, gradU, epsilon, L, current_q)
  current_q = l$q
  Q <- c(Q, current_q)
  p <- c(p, l$p)
  q <- c(q, l$q)
  # points(l$q, l$p, pch = 19, cex = 0.5, col = 'white')
}; rm(n)

pq_plot(H = H, x = x, y = y) +
  geom_point(data = data.frame(x = q, y = p),
             aes(x = x, y = y), col = "white", alpha = 0.3
             ) +
    coord_cartesian(xlim = range(x), ylim = range(y))


dfMCMC <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/haskSampsMCMC1000.csv", header=FALSE)
dfMCMC$Group <- rep("MCMC", dim(dfMCMC)[1])

dfHMC  <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/haskSampsHMC1000.csv",  header=FALSE)
dfHMC$Group <- rep("HMC", dim(dfHMC)[1])

df <- rbind(dfMCMC, dfHMC)

dt5 <- function (x) {
    dt(x, 5)
}

ggplot(df, aes(x = V2, fill = Group)) +
    geom_density(alpha = 0.2) +
    ## geom_histogram(aes(y= ..density..), alpha = 0.6, binwidth = 0.1) +
    geom_function(fun = dt5, colour = "red") +
    labs(title = paste("Sampling Student's t ", print(dim(dfHMC)[1]), " Samples"),
        subtitle = "True density and via MCMC and HMC") +
    theme( plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    xlab("x")
    ## ggtitle("Sampling Student's t: True density and via MCMC and HMC")

ggsave("diagrams/barChartGenericMCMC1.png")

hist(Q, freq = F, breaks = 100, col=rgb(0,0,1,0.2), add=FALSE)
lines(x, dt(x, 5), col = "red")

hist(dfMCMC$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
hist(dfHMC$V2, freq = F, breaks = 100, col=rgb(0,0,1,0.2), add=FALSE)
lines(x, dt(x, 5), col = "red")
legend("topright", legend=c("Haskell","R"), col=c(rgb(1,0,0,0.2), rgb(0,0,1,0.2)), pt.cex=2, pch=15 )


dfMB <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/haskSampsMB10000.csv", header=FALSE)
dfMC <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/haskSampsMCMC10002.csv", header=FALSE)

dfMS <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/SingleObs.csv", header=FALSE)

hist(dfMS$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dnorm(x, 3.2, 0.4472135954999579), col = "red")

hist(dfMC$V2, freq = F, breaks = 100, col=rgb(0,1,0,0.2), add=FALSE)
lines(x, dt(x, 5), col = "red")

lines(x, dnorm(x, 3.2, 0.4472135954999579), col = "red")


dfMP <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/Rwm100000.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/Rwm.png", width=600, height=350)
hist(dfMP$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dnorm(x, 2.0, 0.7071067811865476), col = "red")
dev.off()

dfMQ <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/Mb100000.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/Mb.png", width=600, height=350)
hist(dfMQ$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dnorm(x, 2.0, 0.7071067811865476), col = "red")
dev.off()

dfMStudentRwm <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/StudSampsRwm100002.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/StudRwm.png", width=600, height=350)
hist(dfMStudentRwm$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dt(x, 5), col = "red")
dev.off()

dfMStudentMb <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/StudSampsMB10003.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/StudMb.png", width=600, height=350)
hist(dfMStudentMb$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dt(x, 5), col = "red")
dev.off()

dfMQ <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/MbPrime1000000.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/MbPrime.png", width=600, height=350)
hist(dfMQ$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dnorm(x, 2.0, 0.7071067811865476), col = "red")
dev.off()

dfMQ <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/StudSampsHMC10000.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/Hmc.png", width=600, height=350)
hist(dfMQ$V2, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dt(x, 5), col = "red")
dev.off()

dfMR <- read.csv("/Users/dom/Dropbox/Tidy/monad-bayes-maria/J.csv", header=FALSE)
png(file="/Users/dom/Dropbox/Tidy/monad-bayes-maria/diagrams/PplNormal.png", width=600, height=350)
hist(dfMR$V1, freq = F, breaks = 100, col=rgb(1,0,0,0.2), add=FALSE)
lines(x, dnorm(x, 2.0, 0.7071067811865476), col = "red")
dev.off()

