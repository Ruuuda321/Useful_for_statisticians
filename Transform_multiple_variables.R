#library(openxlsx)

dane<-read.csv2("D:\\TEMP\\data.csv",stringsAsFactors = F)
col_y<-"h100_z" # kolumna 9
col_x<-names(dane)[c(1:10)] # predictor names

out<-"D:\\TEMP\\"


dane_iloczyny<-dane[,c("numer",col_y,col_x)]

for (i in 1:length(col_x)){
  for (j in 1:length(col_x)){
    if (j>i){
    dane_iloczyny[,sprintf('%s_x_%s',col_x[i],col_x[j])]<-dane_iloczyny[,col_x[i]]*dane_iloczyny[,col_x[j]]
    } else if (j==i) {
      dane_iloczyny[,sprintf('%s^2',col_x[j])]<-dane_iloczyny[,col_x[i]]*dane_iloczyny[,col_x[j]]
      dane_iloczyny[,sprintf('%s^-1',col_x[j])]<-1/dane_iloczyny[,col_x[i]]
      dane_iloczyny[,sprintf('sqrt(|%s|)',col_x[j])]<-sqrt(abs(dane_iloczyny[,col_x[i]]))
    }
  }
}

write.xlsx(dane_iloczyny,paste(out,col_y,"_Predictors_transformacje_iloczyny_i_inne_v2.xlsx"))
