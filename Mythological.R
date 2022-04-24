install.packages("resample")
library(resample)
install.packages("scorecard")
library(scorecard)
library(MASS)
library(ggplot2)
install.packages("EBImage")
library("EBImage")
install.packages("BiocManager")
BiocManager::install("EBImage")
library("EBImage")
b1 <- readImage("/Users/aqibjunaid/Downloads/Machine Learning/Image/123.jpeg")
#z1 <- readImage(file.path(path,"s08_14.jpg"))
r1 <- resize(b1,64,64)
display(r1)
path <- "/Users/aqibjunaid/Downloads/Machine Learning/Image"
pathToSaveNewImages <- "/Users/aqibjunaid/Downloads/Machine Learning/ProcessedImage"
data.files <- list.files(path=path, pattern="*.jpg")
#results <- mode = "integer", length =length(files)
#resize_image <- resize(s1, w=256, h= 256)
#data.files
#browser()
install.packages("tidyverse")
library(tidyverse)
num_k=0
new_df = tibble(m.cx=rep(0, num_k),
                  m.cy=rep(0, num_k),
                  m.majoraxis=rep(0, num_k),
                  m.eccentricity=rep(0, num_k),
                  m.theta=rep(0, num_k))
num_k=0
new_df1 = tibble(b.mean=rep(0, num_k),
                  b.sd=rep(0, num_k),
                  b.mad=rep(0, num_k),
                  b.q001=rep(0, num_k),
                  b.q005=rep(0, num_k),
                  b.q05=rep(0, num_k),
                  b.q095=rep(0, num_k),
                  b.q099=rep(0, num_k))
num_k=0
new_df2 = tibble(s.area=rep(0, num_k),
                  s.perimeter=rep(0, num_k),
                  s.radius.mean=rep(0, num_k),
                  s.radius.sd=rep(0, num_k),
                  s.radius.min=rep(0, num_k),
                  s.radius.max=rep(0, num_k))

num_k=0
new_df3 = tibble(s.area=rep(0, num_k),
                 s.perimeter=rep(0, num_k),
                 s.radius.mean=rep(0, num_k),
                 s.radius.sd=rep(0, num_k),
                 s.radius.min=rep(0, num_k),
                 s.radius.max=rep(0, num_k))

num_k=0
new_df4 = tibble(s.area=rep(0, num_k),
                 s.perimeter=rep(0, num_k),
                 s.radius.mean=rep(0, num_k),
                 s.radius.sd=rep(0, num_k),
                 s.radius.min=rep(0, num_k),
                 s.radius.max=rep(0, num_k))





for (val  in data.files)
{
  #data.files
  #print(data.files)
  #print(val)
 
  y <-file.path(path,val)
  x <- readImage(y)[,,1]
  #x <- readImage(y)
  
  
    #print(y)
   resized<-resize(x, 100,  100)
   #save resized imaged to a location
   #z = thresh(resized, 10, 10, 0.05)
   #z = opening(z, makeBrush(5, shape='disc'))
   #fts=computeFeatures.moment(resized)
   #new_df = rbind(new_df, as.data.frame(fts))
   ftp=computeFeatures.shape(resized)
   new_df2 = rbind(new_df2, as.data.frame(ftp))

   ftb=computeFeatures.shape(x[1:100,1:100,2])
   new_df3 = rbind(new_df3, as.data.frame(ftb))
   
   ftc=computeFeatures.shape(x[1:100,1:100,3])
   new_df4 = rbind(new_df4, as.data.frame(ftc))
   
}
   
   
      ftq=computeFeatures.basic(resized,x)
   new_df1 = rbind(new_df1, as.data.frame(ftq))
   total_df <- cbind(new_df,new_df1)
}
total_df <- cbind(new_df,new_df1,new_df2)
working_df <- total_df
#zVar <- (working_df - mean(working_df)) / sd(working_df)
#zvar <- data.Normalization (working_df,type="n1",normalization="column")
#install.packages("clusterSim")
#library(clusterSim)
zvar <- scale(working_df)
data.files
myth_label <- read.csv("/Users/aqibjunaid/Downloads/machine/Myth_label.csv", header = TRUE, ",")
myth_label <- subset(myth_label,select = -New.Panel)
actual_df <- cbind(myth_label, zvar)
library(resample)
# choosing 75% of the data to be the training data
#data_split <- initial_split(zvar, prop = .75)
# extracting training data and test data as two seperate dataframes
#data_train <- training(data_split)
#data_test  <- testing(data_split)
ind <- sample(2, nrow(actual_df), replace = T, prob = c(0.75,0.25))
train_new <- actual_df[ind==1, ]
test_new <- actual_df[ind==2, ]
dim(train_new)
ggplot(train_new, aes(Label)) + geom_density(fill="blue")
ggplot(train_new, aes(log(Label))) + geom_density(fill="blue")
ggplot(train_new, aes(sqrt(Label))) + geom_density(fill="blue")
#Let’s make default model.
model1 = lm(sqrt(Label)~., data=train_new)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

# remove the less significant feature
model2 = update(model1, ~.-m.cx-m.cy-m.theta-b.mad-b.q05-b.q095-b.q099) 
summary(model2)
par(mfrow=c(2,2))
plot(model2)

#Lets  make default model and add square term in the model.
model3 = lm(sqrt(Label)~m.majoraxis+m.eccentricity+b.mean+b.sd+b.q001+b.q005+s.area+s.perimeter+s.radius.mean+s.radius.sd+s.radius.min+s.radius.max+
            I(m.majoraxis^2) +I(m.eccentricity^2) + I(b.mean^2)+ I(b.sd^2)+ I(b.q001^2)   +I(b.q005^2)+ I(s.area^2) + 
              I(s.perimeter^2)+ I(s.radius.mean^2)   +I(s.radius.sd^2)+I(s.radius.min^2)+I(s.radius.max^2)
            , data=train_new)
summary(model3)
##Removing the insignificant variables.
model4=update(model3, ~.-m.eccentricity-s.perimeter-s.radius.sd-s.radius.min-
                I(m.eccentricity^2)-I(s.area^2)-I(s.radius.sd^2))
summary(model4)

par(mfrow=c(2,2))
plot(model4)
#prediction
pred1 <- predict(model4, newdata = test_new)
rmse <- sqrt(sum((exp(pred1) - test_new$Label)^2)/length(test_new$Label))
c(RMSE = rmse, R2=summary(model4)$r.squared)

par(mfrow=c(1,1))
plot(test_new$Label, exp(pred1))




