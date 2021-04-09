library(ggplot2)
library(openxlsx)
library(purrr)
library(raster)
setwd("D:\\TEMP")


############################################################
############# DATA


dane<-read.xlsx("D:\TEMP\\data.xlsx",sheet = 1)
col_y<-"Y" 
col_x<-c(32:ncol(dane)) #predictors

corr_tab<-as.data.frame(matrix(nrow=length(col_x),ncol=10))
names(corr_tab)<-c("Zmienna","Min","Max","Mean","Median","SD","CV","Corr_Pearsona","Corr_Spearmana", "Corr_Tau_Kendalla")
for (i in col_x){
  j=i-min(col_x)+1
  print(sprintf('Zmienna %i z %i',j,length(col_x)))
  cor.p<-cor.test(dane[,col_y],dane[,i])
  cor.s<-cor.test(dane[,col_y],dane[,i], method = "spearman")
  cor.t<-cor.test(dane[,col_y],dane[,i], method = "kendall")
 
  corr_tab[j,"Zmienna"]<-names(dane)[i]
  corr_tab[j,"Min"]<-min(dane[,i])
  corr_tab[j,"Max"]<-max(dane[,i])
  corr_tab[j,"Mean"]<-mean(dane[,i])
  corr_tab[j,"Median"]<-median(dane[,i])
  corr_tab[j,"SD"]<-sd(dane[,i])
  corr_tab[j,"CV"]<-sd(dane[,i])/mean(dane[,i])
  corr_tab[j,"Corr_Pearsona"]<-cor.p$estimate
  corr_tab[j,"Corr_Spearmana"]<-cor.s$estimate
  corr_tab[j,"Corr_Tau_Kendalla"]<-cor.t$estimate
  
  remove(cor.p,cor.s,cor.t)
  
}

write.xlsx(corr_tab,sprintf('Statystyki_opisowe i_korelacje_%s.xlsx',col_y))


