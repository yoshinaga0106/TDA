# -----------------------------------
# TDA_Hadley.R
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
if(!require(ggmap)){
  install.packages("ggmap", quiet = TRUE)
}
require(ggmap)
if(!require(ggplot2)){
  install.packages("ggplot2", quiet = TRUE)
}
require(ggplot2)
if(!require(grid)){
  install.packages("grid", quiet = TRUE)
}
require(grid)

## 平均単純法
Hadley_gray <- (hadley[,,1] + hadley[,,2] + hadley[,,3]) / 3
plt_Had <- ggimage(Hadley_gray)



## 単純にTDA
Diag_Had <- ripsDiag(X = Hadley_gray, maxdimension = 1, maxscale = 5)

dimension_Had <- Diag_Had$diagram[,1]
birth_Had <- Diag_Had$diagram[,2]
death_Had <- Diag_Had$diagram[,3]
Diag_Had_DF <- data.frame(cbind(Dimension = dimension_Had, 
                              Birth = birth_Had,
                              Death = death_Had))
# plot
PD_Had <- ggplot(Diag_Had_DF) + 
  geom_point(aes(x = Birth, y = Death, colour = as.factor(Dimension)), size = 4) + 
  xlim(0,5) + ylim(0,5) + 
  geom_abline(slope = 1) + 
  labs(color = "Dimension") + 
  scale_color_manual(name="Dimension",
                     values=c("0"="black","1"="#F8766D"))


png("PD_HADLEY.png", width = 1000, height = 600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 2)))
print(plt_Had, vp = viewport(layout.pos.row=1,layout.pos.col=1))
print(PD_Had, vp = viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()

