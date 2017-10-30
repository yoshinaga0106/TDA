# -----------------------------------
# Analysis_TDA_Blog.R
# Author : Takahiro Yoshinaga
# 2017-10-30 Ver0.1
# -----------------------------------

## Reset
rm(list = ls())
gc();gc();

## Library
if(!require(TDA)){
  install.packages("TDA", quiet = TRUE)
}
require(TDA)
if(!require(ggplot2)){
  install.packages("ggplot2", quiet = TRUE)
}
require(ggplot2)

## JapanR
setwd("./")

# -----------------------------------------------------------------------------------
# Circle

X1 <- circleUnif(100) * 2 + rnorm(100, 0, 0.03)
X2 <- circleUnif(50) * 0.3 + c(1.65,1.65) + + rnorm(50, 0, 0.02)
X <- rbind(X1, X2)
X <- data.frame(x = X[, 1], y = X[, 2])

ggplot(X) + 
  geom_point(aes(x = x, y = y))

# PD
Diag_Cir <- ripsDiag(X = X, maxdimension = 1, maxscale = 5)

dimension_Cir <- Diag_Cir$diagram[,1]
birth_Cir <- Diag_Cir$diagram[,2]
death_Cir <- Diag_Cir$diagram[,3]
Diag_Cir_DF <- data.frame(cbind(Dimension = dimension_Cir, 
                              Birth = birth_Cir,
                              Death = death_Cir))
# plot
PD_Cir <- ggplot(Diag_Cir_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))

PD_Cir

# alpha complex
# PD_Cir_alpha <- alphaComplexDiag(X = X, printProgress = FALSE)
# plot(PD_Cir_alpha[["diagram"]])
