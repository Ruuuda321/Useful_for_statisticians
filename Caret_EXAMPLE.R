#library(installr)

#install.packages("caret", dependencies = TRUE, type = "binary")

library(caret)

df<-read.csv2("E:\\TEMP\\data.csv")


# Define training control
train.control <- trainControl(method = "LOOCV")

# Put a model here and train
model <- train(Y ~ x1 + x2 + x3, data = df, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)




###RF
df2<-read.csv2("E:\\TEMP\\data2.csv")

model_rf<-train(Y ~ ., data=df2, method='rf', trControl = train.control)

print(model_rf)



#70/30 - random

mod <- train(Y ~ x1 + x2 + x3, data = df, method = "lm",
             trControl = trainControl(method = "LGOCV", p = 0.7, number = 900,
                                      savePredictions = T))


mod_rf<-train(predicted_value ~ ., data=your_data, method='rf', trControl = trainControl(method = "LGOCV", p = 0.8, number = 100, savePredictions = T))


#70/30 na tych samych zbiorach
train_idx<-createDataPartition(df$Vref_m3ha_p,p=0.7,times=900)

##HERE CONTINUE
