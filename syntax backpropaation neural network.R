library(readxl) #import data
library(cluster) #untuk kelompok data
library(ggplot2) #untuk memunculkan plot
library(factoextra) # untuk memunculkan biplot cluster
library(neuralnet) #untuk backpropagation nerual network
library(caret) #untuk training dan testing
library(rtweet) #untuk menyimpan data dalam bentuk tabel
library(caTools) #untuk melihat tingkat akurasi
#import data
datanya<-read_excel("D:/S K R I P S I/DATA INDIKATOR KESEJAHTERAAN RAKYAT - Copy.xlsx", sheet = "Sheet1")
summary(datanya)
#Standarisasi data
data2<-scale(datanya[,2:7]) #menstandarisasi data dari kolom 2 sampai 7
#menentukan jumlah cluster terbaik dengan metode silhoutte
fviz_nbclust(data,kmeans) #memunculkan plot pada metode silhouette
#Pengelompokan
kluster<-kmeans(data2,3) #mengelompokkan data menjadi 3 karena grafik tertinggi pada silhoette
h<-data.frame(data2,kluster[1]) #untuk memunculkan data dalam bentuk tabel
#Clustering Biplot
rownames(data2)<-datanya$`Nama Wilayah`
pca<-princomp(data2,cor=T,score=T)
kluster<-kmeans(data2,3)
h<-data.frame(data2,kluster[1])
fviz_pca_biplot(pca,geom=c("point","text"),
                geom.var = c("arrow","text"),label="all",
                habillage = h$cluster,palette = palette(rainbow(12)),
                addEllipses = T,title = "Pengelompokan Indikator Kesejahteraan Rakyat Kabupaten/kota di Indonesia")

#Data Training & Testing 50%
set.seed(1234) #kunci
split=sample.split(h$cluster,SplitRatio = 0.5)
training=subset(h,split==TRUE)
testing=subset(h,split==FALSE)
lengths(training)
lengths(testing)
#fungsi aktivasi sigmoid
sigmoid=function(x){1/(1+exp(-x))} #fungsi sigmoid
set.seed(1234) 
nn11<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000) #hidden dan learningrate berdasarkan trial error, act.fct untuk fungsi simoid, stepmax untuk perulangan berapa kali
plot(nn11)
pred<-predict(nn11,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn12<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn12)
pred<-predict(nn12,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn13<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn13)
pred<-predict(nn13,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn14<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn14)
pred<-predict(nn14,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn15<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn15)
pred<-predict(nn15,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn16<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn16)
pred<-predict(nn16,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn17<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn17)
pred<-predict(nn17,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn18<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn18)
pred<-predict(nn18,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn19<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn19)
pred<-predict(nn19,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn110<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn110)
pred<-predict(nn110,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn111<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn111)
pred<-predict(nn111,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn112<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn112)
pred<-predict(nn112,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn113<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.004,
                linear.output = T,stepmax = 10000000)
plot(nn113)
pred<-predict(nn113,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn114<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn114)
pred<-predict(nn114,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn115<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn115)
pred<-predict(nn115,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn116<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn116)
pred<-predict(nn116,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn117<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(7),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn117)
pred<-predict(nn117,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn118<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn118)
pred<-predict(nn118,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn119<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn119)
pred<-predict(nn119,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn120<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn120)
pred<-predict(nn120,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))


#Data Training & Testing 60%
set.seed(1234)
split=sample.split(h$cluster,SplitRatio = 0.6)
training=subset(h,split==TRUE)
testing=subset(h,split==FALSE)
lengths(training)
lengths(testing)
#fungsi aktivasi sigmoid
sigmoid=function(x){1/(1+exp(-x))}
set.seed(1234)
nn21<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn21)
pred<-predict(nn21,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn22<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn22)
pred<-predict(nn22,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn23<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn23)
pred<-predict(nn23,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn24<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn24)
pred<-predict(nn24,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn25<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn25)
pred<-predict(nn25,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn26<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn26)
pred<-predict(nn26,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn27<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn27)
pred<-predict(nn17,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn28<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn28)
pred<-predict(nn28,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn29<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn29)
pred<-predict(nn29,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn210<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn210)
pred<-predict(nn210,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn211<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn211)
pred<-predict(nn211,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn212<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn212)
pred<-predict(nn212,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn213<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(7),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn213)
pred<-predict(nn213,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn214<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn214)
pred<-predict(nn214,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn215<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn215)
pred<-predict(nn215,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn216<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn216)
pred<-predict(nn216,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn217<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(7),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn217)
pred<-predict(nn217,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn218<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn218)
pred<-predict(nn218,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn219<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn219)
pred<-predict(nn219,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn220<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn220)
pred<-predict(nn220,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))


#Data Training & Testing 70%
set.seed(1234)
split=sample.split(h$cluster,SplitRatio = 0.7)
training=subset(h,split==TRUE)
testing=subset(h,split==FALSE)
lengths(training)
lengths(testing)
#fungsi aktivasi sigmoid
sigmoid=function(x){1/(1+exp(-x))}
set.seed(1234)
nn31<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
              data = training,hidden =c(7),algorithm="backprop",
              act.fct=sigmoid,learningrate=0.001,
              linear.output = T,stepmax = 10000000)
plot(nn31)
pred<-predict(nn31,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn32<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
              data = training,hidden =c(5,3),algorithm="backprop",
              act.fct=sigmoid,learningrate=0.001,
              linear.output = T,stepmax = 10000000)
plot(nn32)
pred<-predict(nn32,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn33<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
              data = training,hidden =c(5,9,4),algorithm="backprop",
              act.fct=sigmoid,learningrate=0.001,
              linear.output = T,stepmax = 10000000)
plot(nn33)
pred<-predict(nn33,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn34<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn34)
pred<-predict(nn34,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn35<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn35)
pred<-predict(nn35,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn36<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn36)
pred<-predict(nn36,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn37<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn37)
pred<-predict(nn37,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn38<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn38)
pred<-predict(nn38,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn39<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn39)
pred<-predict(nn39,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn310<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn310)
pred<-predict(nn310,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn311<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn311)
pred<-predict(nn311,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn312<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn12)
pred<-predict(nn12,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn313<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.004,
                linear.output = T,stepmax = 10000000)
plot(nn313)
pred<-predict(nn313,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn314<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn314)
pred<-predict(nn314,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn315<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn315)
pred<-predict(nn315,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn316<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn316)
pred<-predict(nn316,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn317<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.005,
                linear.output = T,stepmax = 10000000)
plot(nn317)
pred<-predict(nn317,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn318<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn318)
pred<-predict(nn318,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn319<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn319)
pred<-predict(nn319,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn320<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn20)
pred<-predict(nn20,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))


#Data Training & Testing 80%
set.seed(1234)
split=sample.split(h$cluster,SplitRatio = 0.8)
training=subset(h,split==TRUE)
testing=subset(h,split==FALSE)
lengths(training)
lengths(testing)
#fungsi aktivasi sigmoid
sigmoid=function(x){1/(1+exp(-x))}
set.seed(1234)
nn41<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn41)
pred<-predict(nn41,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn42<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn42)
pred<-predict(nn42,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn43<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn43)
pred<-predict(nn43,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn44<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.001,
                linear.output = T,stepmax = 10000000)
plot(nn44)
pred<-predict(nn44,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn45<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn45)
pred<-predict(nn45,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn46<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,3),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn46)
pred<-predict(nn46,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn47<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(5,9,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn47)
pred<-predict(nn47,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn48<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(8,6,3,4),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.002,
                linear.output = T,stepmax = 10000000)
plot(nn48)
pred<-predict(nn48,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn49<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                data = training,hidden =c(7),algorithm="backprop",
                act.fct=sigmoid,learningrate=0.003,
                linear.output = T,stepmax = 10000000)
plot(nn49)
pred<-predict(nn49,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn410<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn410)
pred<-predict(nn410,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn411<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn411)
pred<-predict(nn411,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn412<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.003,
                 linear.output = T,stepmax = 10000000)
plot(nn412)
pred<-predict(nn412,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn413<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(7),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn413)
pred<-predict(nn413,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn414<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn414)
pred<-predict(nn414,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn415<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn415)
pred<-predict(nn415,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn416<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.004,
                 linear.output = T,stepmax = 10000000)
plot(nn416)
pred<-predict(nn416,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn417<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(7),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 9999)
plot(nn417)
pred<-predict(nn417,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn418<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,3),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn418)
pred<-predict(nn418,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn419<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(5,9,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn419)
pred<-predict(nn419,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
set.seed(1234)
nn420<-neuralnet((cluster=="1")+(cluster=="2")+(cluster=="3")~.,
                 data = training,hidden =c(8,6,3,4),algorithm="backprop",
                 act.fct=sigmoid,learningrate=0.005,
                 linear.output = T,stepmax = 10000000)
plot(nn420)
pred<-predict(nn420,testing)
prediksi<-as.vector(apply(pred,1,which.max))
confusionMatrix(as.factor(prediksi),as.factor(testing$cluster))
