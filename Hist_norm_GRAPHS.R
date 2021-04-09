library(ggplot2)
library(openxlsx)
library(purrr)
setwd("D:\\TEMP")


############################################################
############# DANE


dane<-read.xlsx("D:\\TEMP\\data.xlsx",sheet = 1)
col_y<-"Y" 
col_x<-c(32:ncol(dane)) ##predictors




#############################################################
############# HISTOGRAMY i QQ 
y<-set_names(col_y)
x<-set_names(names(dane)[col_x])



for (i in col_x){
  print(i)
  
  ## QQ plot - wykres normalnosci
  p <- ggplot(dane, aes(sample = dane[,i])) +
        stat_qq() +
        stat_qq_line() +
        labs(title=sprintf("A q-q plot - %s",names(dane)[i])) +
        theme_bw()
  
  ## Histogram
 
  hist_func = function(dane,x){
      ggplot(dane, aes(x = .data[[x]])) +
        geom_histogram(bins = floor(sqrt(nrow(dane))),
                       col="black",
                       fill="white",
                       alpha=0.2) +
        labs(title=sprintf("Histogram - %s",names(dane)[i]), x=x) +
        theme_bw()
  }

  h<-hist_func(dane,x[i-min(col_x)+1])

  
  
  ## Wykres rozrzutu - Scatter plot
  scatter_fun = function(dane,x, y) {
        ggplot(dane, aes(x = .data[[x]], y = .data[[y]]) ) +
          geom_point() +
          labs(x = x,
              y = y, 
              title=sprintf("Wykres rozrzutu - %s vs %s", names(dane)[i],y)) +
          geom_smooth(method = "loess", se = FALSE, color = "grey74") + #lm for linear smooths, glm for generalised linear smooths, loess for local smooths
          theme_bw() 
  }
  
  r<-scatter_fun(dane,x[i-min(col_x)+1],y)
  
  
  
  png(sprintf("%i_Norm-%s.png",i-min(col_x)+1,names(dane)[i]))
  print(p)
  dev.off()

  png(sprintf("%i_Hist-%s.png",i-min(col_x)+1,names(dane)[i]))
  print(h)
  dev.off()

  
  png(sprintf("%i_Wykres_rozrzutu_%s_vs_%s.png",i-min(col_x)+1,names(dane)[i],y))
  print(r)
  dev.off()
}


















###########################################################
############ ZMIENNE

# x<-names(dane)[col_x]
# 
# x_fe<-x[which(grepl("^fe_",x)==TRUE)]
# x_le<-x[which(grepl("^le_",x)==TRUE)]
# x_all<-x[which(grepl("^all_",x)==TRUE)]
# 
# x_int<-x[which(grepl("_int_",x)==TRUE)]
# x_int_all<-x[which(grepl("(^all_{1})(\\w+)(_int_)",x)==TRUE)]
# x_int_fe<-x[which(grepl("(^fe_{1})(\\w+)(_int_)",x)==TRUE)]
# x_int_le<-x[which(grepl("(^le_{1})(\\w+)(_int_)",x)==TRUE)]
# 
# x_rgb<-x[which(grepl("_r$|_g$|_b$",x)==TRUE)]
# 
# x_h<-x[!x %in% c(x_rgb,x_int)]
# x_h_all<-x_h[which(grepl("^all_",x_h))]
# x_h_fe<-x_h[which(grepl("^fe_",x_h))]
# x_h_le<-x_h[which(grepl("^le_",x_h))]
# x_h_inne<-x_h[!x_h %in% c(x_all,x_fe, x_le)]
