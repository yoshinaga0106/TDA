# -----------------------------------
# Analysis_TDA.R
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
if(!require(dplyr)){
  install.packages("dplyr", quiet = TRUE)
}
require(dplyr)
if(!require(ggplot2)){
  install.packages("ggplot2", quiet = TRUE)
}
require(ggplot2)
if(!require(grid)){
  install.packages("grid", quiet = TRUE)
}
require(grid)

## JapanR
setwd("./")

# -----------------------------------------------------------------------------------
# J
image_J <- data.frame(x = c(2,3,4,5,6,7,8,6,6,6,6,6,6,4,2,2.5,5.5,3   ,5),
                      y = c(9,9,9,9,9,9,9,8,7,6,5,4,3,1,3,2  ,2  ,1.25,1.25))
plt_J <- ggplot(image_J) + 
  geom_point(mapping = aes(x = x, y = y), size = 4) +
  xlim(0, 10) + ylim(0, 10)

# PD
Diag_J <- ripsDiag(X = image_J, maxdimension = 1, maxscale = 5)
dimension_J <- Diag_J$diagram[,1]
birth_J <- Diag_J$diagram[,2]
death_J <- Diag_J$diagram[,3]
Diag_J_DF <- data.frame(cbind(Dimension = dimension_J, 
                              Birth = birth_J,
                              Death = death_J))
# plot
PD_J <- ggplot(Diag_J_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))

# -----------------------------------------------------------------------------------
# A
image_A <- data.frame(x= c(2,2.5 ,3   ,3.5,4   ,4.5 ,5,5.5 ,6   ,6.5,7   ,7.5 ,8,4.25,5,5.75),
                      y =c(1,2.33,3.67,5  ,6.33,7.67,9,7.67,6.33,5  ,3.67,2.33,1,5   ,5,5))
plt_A <- ggplot(image_A) + 
  geom_point(mapping = aes(x = x, y = y), size = 4) +
  xlim(0, 10) + ylim(0, 10)

# PD
Diag_A <- ripsDiag(X = image_A, maxdimension = 1, maxscale = 5)
dimension_A <- Diag_A$diagram[,1]
birth_A <- Diag_A$diagram[,2]
death_A <- Diag_A$diagram[,3]
Diag_A_DF <- data.frame(cbind(Dimension = dimension_A, 
                              Birth = birth_A,
                              Death = death_A))
# plot
PD_A <- ggplot(Diag_A_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))

# -----------------------------------------------------------------------------------
# P
image_P <- data.frame(x = c(2,2,2,2,2,2,2,2,2,3,3,4,4,5,5,6,6,7   ,7),
                      y = c(1,2,3,4,5,6,7,8,9,5,9,5,9,5,9,5,9,7.77,6.33))
plt_P <- ggplot(image_P) + 
  geom_point(mapping = aes(x = x, y = y), size = 4) +
  xlim(0, 10) + ylim(0, 10)

# PD
Diag_P <- ripsDiag(X = image_P, maxdimension = 1, maxscale = 5)
dimension_P <- Diag_P$diagram[,1]
birth_P <- Diag_P$diagram[,2]
death_P <- Diag_P$diagram[,3]
Diag_P_DF <- data.frame(cbind(Dimension = dimension_P, 
                              Birth = birth_P,
                              Death = death_P))
# plot
PD_P <- ggplot(Diag_P_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))

# -----------------------------------------------------------------------------------
# N
image_N <- data.frame(x = c(2,2,2,2,2,2,2,2,2,3   ,4   ,5,6   ,7   ,8,8,8,8,8,8,8,8,8), 
                      y = c(1,2,3,4,5,6,7,8,9,7.67,6.33,5,3.67,2.33,1,2,3,4,5,6,7,8,9))
plt_N <- ggplot(image_N) + 
  geom_point(mapping = aes(x = x, y = y), size = 4) +
  xlim(0, 10) + ylim(0, 10)

# PD
Diag_N <- ripsDiag(X = image_N, maxdimension = 1, maxscale = 5)
dimension_N <- Diag_N$diagram[,1]
birth_N <- Diag_N$diagram[,2]
death_N <- Diag_N$diagram[,3]
Diag_N_DF <- data.frame(cbind(Dimension = dimension_N, 
                              Birth = birth_N,
                              Death = death_N))
# plot
PD_N <- ggplot(Diag_N_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))

# -----------------------------------------------------------------------------------
# R
image_R <- data.frame(x = c(2,2,2,2,2,2,2,2,2,3,3,4,4,5,5,6,6,7   ,7   ,2.5,3  ,3.5,4  ,4.5,5  ,5.5,6  ,6.5,7),
                 y = c(1,2,3,4,5,6,7,8,9,5,9,5,9,5,9,5,9,7.77,6.33,4.6,4.2,3.8,3.4,3.0,2.6,2.2,1.8,1.4,1))
plt_R <- ggplot(image_R) + 
  geom_point(mapping = aes(x = x, y = y), size = 4) +
  xlim(0, 10) + ylim(0, 10)

# PD
Diag_R <- ripsDiag(X = image_R, maxdimension = 1, maxscale = 5)
dimension_R <- Diag_R$diagram[,1]
birth_R <- Diag_R$diagram[,2]
death_R <- Diag_R$diagram[,3]
Diag_R_DF <- data.frame(cbind(Dimension = dimension_R, 
                              Birth = birth_R,
                              Death = death_R))
# plot
PD_R <- ggplot(Diag_R_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))

# -----------------------------------------------------------------------------------


## Merge and Save
# Plot
png("JAPANR.png", width = 1000, height = 600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 3)))
print(plt_J, vp = viewport(layout.pos.row=1,layout.pos.col=1))
print(plt_A, vp = viewport(layout.pos.row=1,layout.pos.col=2))
print(plt_P, vp = viewport(layout.pos.row=1,layout.pos.col=3))
print(plt_A, vp = viewport(layout.pos.row=2,layout.pos.col=1))
print(plt_N, vp = viewport(layout.pos.row=2,layout.pos.col=2))
print(plt_R, vp = viewport(layout.pos.row=2,layout.pos.col=3))
dev.off()

## PD
png("PD_JAPANR.png", width = 1000, height = 600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 3)))
print(PD_J, vp = viewport(layout.pos.row=1,layout.pos.col=1))
print(PD_A, vp = viewport(layout.pos.row=1,layout.pos.col=2))
print(PD_P, vp = viewport(layout.pos.row=1,layout.pos.col=3))
print(PD_A, vp = viewport(layout.pos.row=2,layout.pos.col=1))
print(PD_N, vp = viewport(layout.pos.row=2,layout.pos.col=2))
print(PD_R, vp = viewport(layout.pos.row=2,layout.pos.col=3))
dev.off()

