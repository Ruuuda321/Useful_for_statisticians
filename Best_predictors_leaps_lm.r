library(leaps)
library(olsrr)

cecha= 'Y'
id_pow= 'id'
projekt= 'Project1'

dir= 'D:\\TEMP\\'
data_dir= 'D:\\DATA\\data.csv'
omit_cols<-c(1:31)  #Columns without predictors - only description of observation
predictors_col<-c(32:800,813:896) #Columns with predictors


#####################################################################
all_data= read.csv2(data_dir, stringsAsFactors = F)
opis_pow= names(all_data)[omit_cols]



sel_cols<-names(all_data)[predictors_col]


stat_tab= all_data[,c(id_pow,cecha, sel_cols)]


set.seed(100)

var_model <- regsubsets(stat_tab[,cecha] ~. ,stat_tab[,-c(1,2)],nbest=3,nvmax=6,method='seqrep')
var_plot <- plot(var_model, scale="r2")
var_summary= summary(var_model)
var_plot

r2df= data.frame(matrix(nrow=length(var_summary$adjr2), ncol=2))
names(r2df) = c('no','adjR2')
r2df$no= seq(1,nrow(r2df))
r2df$adjR2 = var_summary$adjr2
r2df= r2df[order(-r2df$adjR2),]
best_groups= r2df$no[1:8]

var_summary$adjr2[best_groups]


best_predictors= list()
adjr2= list()

for(b in 1:length(best_groups)){
  best_subset= which(var_summary$which[best_groups[b],])
  best_names= names(best_subset)
  best_names= best_names[-1]
  best_predictors[[b]] = best_names
  adjr2[[b]] = var_summary$adjr2[best_groups[b]]
  
}

best_predictors
adjr2



#bp= unique(unlist(best_predictors[[1]]))
bp = unique(unlist(best_predictors, use.names = F))


best_stats_tab= all_data[,c(id_pow,cecha,bp)]

################################################################
################################################################
#Uwaga! Tu zmien parametr glowny - Y

mod1= lm(Y~ ., data= best_stats_tab[,-1])
summary(mod1)



best_stats_tab$predicted= mod1$fitted.values
best_stats_tab$dif= best_stats_tab[,cecha] - best_stats_tab$predicted
best_stats_tab$abs_dif= abs(best_stats_tab$dif)
best_stats_tab$abs_dif_pr= best_stats_tab$abs_dif / best_stats_tab[,cecha] *100



#best_stats_tab_opis= merge(best_stats_tab,opis_pow, by="numer", suffixes = c("",".y")) 

write.csv2(best_stats_tab, paste(dir,'best_results_',cecha,'.csv',sep=''), row.names = F)


### transformacje

power2= best_stats_tab[,3:length(bp)]^2
power3= best_stats_tab[,3:length(bp)]^3
ln= log(best_stats_tab[,3:length(bp)])
log10= log10(best_stats_tab[,3:length(bp)])
log2= log2(best_stats_tab[,3:length(bp)])
expo= exp(best_stats_tab[,3:length(bp)])
d1_= 1/best_stats_tab[,3:length(bp)]

names(power2)= paste(names(best_stats_tab[,3:length(bp)]),'_t1_pow2',sep='')
names(power3)= paste(names(best_stats_tab[,3:length(bp)]),'_t2_pow3',sep='')
names(ln)= paste(names(best_stats_tab[,3:length(bp)]),'_t3_ln',sep='')
names(log10)= paste(names(best_stats_tab[,3:length(bp)]),'_t4_log10',sep='')
names(log2)= paste(names(best_stats_tab[,3:length(bp)]),'_t5_log2',sep='')
names(expo)= paste(names(best_stats_tab[,3:length(bp)]),'_t6_exp',sep='')
names(d1_) = paste(names(best_stats_tab[,3:length(bp)]),'_t7_d_1',sep='')


best_stats_tab_transformacje<-cbind(best_stats_tab, power2, power3, ln, log10, log2, expo, d1_)
col_NaN<-names(best_stats_tab_transformacje)[colSums(is.na(best_stats_tab_transformacje)) > 0]
#best_stats_tab_transformacje<-best_stats_tab_transformacje[,-c(col_NaN)]

match()

var_model_trans <- regsubsets(best_stats_tab_transformacje[,cecha] ~. ,best_stats_tab_transformacje[,-c(1,2)],nbest=3,nvmax=6,method='seqrep')
var_plot <- plot(var_model, scale="r2")
var_summary= summary(var_model)

test_vec<-as.vector(test)
