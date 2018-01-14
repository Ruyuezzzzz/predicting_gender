---
title: "Data sicence project II"
author: "Ruyue Zhang, Phoebe Wu, Liwei Zhu, Sheng Luo"
date: "28/11/2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

####How did you develop your question and what relevant research has already been completed on this topic?

Our original idea was finding a dataset about iPhone sales. But during that process, we found this dataset interesting and adopted it. This dataset is a survey done among Slovakian young people about their movie preference, music preference, interests, habits, personality traits, views on life, opinions, phobias, and demographics. After checking every column of the dataset, we decided to take the gender data in demographics as our dependent variable. Based on the survey's theme and in case that there will be too many variables, we only kept music preferences, movie preferences, interests and phobia as our independent variables. Therefore, the question we put forward first was "How can we predict 'gender' by personal preferences and fears". To make it more specific, we developed it into" How can we predict 'gender' by personal preferences and fears in 90% accuracy?"

This dataset was published on kaggle called Young People Survey by Miroslav Sabo. The publisher himself as a scholar published two research based on the survey. One is Gender differences in the prevalence of specific phobias. Forum Statisticum Slovacum. 2014, Vol. 10, No. 6. [Differences (gender + whether people lived in village/town) in the prevalence of phobias.].  The other is Multivariate Statistical Methods with Applications. Diss. Slovak University of Technology in Bratislava, 2014.[Clustering of variables (music preferences, movie preferences, phobias) + Clustering of people w.r.t. their interests.]. But the two research are all in Slovak.

Similar research includes: 1) Ohannessian, C. M., Lerner, R. M., Lerner, J. V., & von Eye, A. (1999). Does self-competence predict gender differences in adolescent depression and anxiety?. Journal of Adolescence, 22(3), 397-411.  2) Jones, M. G., Howe, A., & Rua, M. J. (2000). Gender differences in students' experiences, interests, and attitudes toward science and scientists. Science education, 84(2), 180-192. 3)Cao, D., Chen, C., Piccirilli, M., Adjeroh, D., Bourlai, T., & Ross, A. (2011, October). Can facial metrology predict gender?. In Biometrics (IJCB), 2011 International Joint Conference on (pp. 1-8). IEEE. 4)Powell, M., Schubert, R., & Gysler, M. (2001). How to predict gender-differences in choice under risk: a case for the use of formalized models (No. 01/21). Economics Working Paper Series.


####How did you gather and prepare the data for analysis?
Before the final best model, We wanted to subset the data, find the best model for every subset and significant variable which should be used in the final model. So we separated our data into music data, movie data, interests data and phobia data and combined gender data to every group thus making it a subset.


```{r}

alldata<-read.csv("responses.csv")
head(alldata)
str(alldata)
#clean the data
alldata = na.omit(alldata)
str(alldata)

musicdata<-alldata[c(2:19)]
genderdata<-alldata$Gender
musicdata<-cbind(musicdata,genderdata)
head(musicdata)
musicdata
str(musicdata)

moviedata<-alldata[c(21:31)]
moviedata<-cbind(moviedata,genderdata)
moviedata
str(moviedata)

interestsdata<-alldata[c(32:63)]
interestsdata<-cbind(interestsdata,genderdata)
str(interestsdata)

phobiadata<-alldata[c(64:73)]
phobiadata<-cbind(phobiadata,genderdata)
str(phobiadata)

Demographicsdata<-alldata[c((73+3+7+57+1):(73+3+7+57+10))]
str(Demographicsdata)
```


####How did you select and determine the correct regression model to answer your question?

We ran the basic logistic regression to check coefficients first. Then we used the bestglm package and view the best model. Due to the long running time of bestglm, after running it once we commented it. We picked out the variable in the best model then form a new logistic model, summarizing it to compare the change in coefficients. After we regrouped the data depending on the variable we picked out, we divided it into test and train data. We used the logistic model developed by train data to predict the test data, draw the ROC of every model and calculate the AUC to see how well our model classifies. 

When all subsets were done, we selected all the variables used in subset model to form our final dataset. Then following the same procedure above to get our final best model and its AUC value. 



```{r}
# Glm & Bestglm

library(bestglm)
library(ROCR)
library(pROC)

# Misic
musicdatalogit<- glm(genderdata ~. , family = binomial(link = "logit"), data = musicdata)
summary(musicdatalogit)
#musicbglm <- bestglm(Xy=musicdata,family = binomial,IC="AIC",method = "forward")
#musicbglm$BestModel

musicdatabestglm <- glm(genderdata ~Slow.songs.or.fast.songs+Country+Classical.music+Musical+Pop+Metal.or.Hardrock+Hiphop..Rap+Alternative+Latino+Techno..Trance , family = binomial(link = "logit"), data = musicdata)
summary(musicdatabestglm)

###draw the ROC of Music Group and calculate the AUC
New_music = cbind(genderdata,musicdata[c(1,4,5,6,7,9,11,15,16,17)])
str(New_music)
#684 is the numbers of rows in the new dataset
train_music = New_music[1:547,] #684*0.8 = 547
test_music = New_music[548:684,]
new_musicglm = glm(genderdata~.,family="binomial"(link = "logit"),train_music)
predict_music = predict.glm(new_musicglm,test_music,type='response')
newpred_music <- prediction(predict_music,test_music$genderdata)
newpred_music.performance <- performance(newpred_music, measure = "tpr",x.measure = "fpr")
plot(newpred_music.performance, main= 'ROC of Music Group')
AUC_music = performance(newpred_music,measure = 'auc')
AUC_music@y.values # AUC value
#


# Movies
moviedatalogit <- glm(genderdata ~. , family = binomial(link = "logit"), data = moviedata)
summary(moviedatalogit)
#moviebglm <- bestglm(Xy=moviedata,family = binomial,IC="AIC",method = "forward")
#moviebglm$BestModel

moviedatabestglm <- glm(genderdata ~ Comedy+Romantic+Sci.fi+War+Fantasy.Fairy.tales+Western+Action, family = binomial(link = "logit"), data = moviedata)
summary(moviedatabestglm)
#AIC dropped to 549.4 from 556.31


###draw the ROC of Movie Group and calculate the AUC
New_movie = cbind(genderdata,moviedata[c(3,4,5,6,7,10,11)])
train_movie = New_movie[1:547,]
test_movie = New_movie[548:684,]
new_movieglm = glm(genderdata~.,family="binomial"(link = "logit"),train_movie)
predict_movie = predict.glm(new_movieglm,test_movie,type='response')
newpred_movie <- prediction(predict_movie,test_movie$genderdata)
newpred_movie.performance <- performance(newpred_movie, measure = "tpr",x.measure = "fpr")
plot(newpred_movie.performance, main= 'ROC of the Movie Group')
AUC_movie = performance(newpred_movie,measure = 'auc')
AUC_movie@y.values
#

# Interests
interestsdatalogit <- glm(genderdata ~. , family = binomial(link = "logit"), data = interestsdata)
summary(interestsdatalogit)
interestsdata.1 <- interestsdata[, c("History", "Psychology", "Politics", "PC", "Reading", "Foreign.languages", "Cars", "Dancing", "Active.sport", "Gardening", "Shopping", "Science.and.technology", "Theatre", "Fun.with.friends", "Pets", "genderdata")]
str(interestsdata.1)

#interestsdata.1bglm <- bestglm(Xy=interestsdata.1,family = binomial,IC="AIC",method = "forward")
#interestsdata.1bglm$BestModel
interestsdata.1bestglm <- glm(genderdata~ History+ Politics+ PC+ Reading+ Foreign.languages+ Cars+ Dancing+ Active.sport+ Gardening+ Shopping+ Science.and.technology+ Theatre+ Fun.with.friends+ Pets, family = binomial(link = "logit"), data =interestsdata.1)
summary(interestsdata.1bestglm)

#draw the ROC of Interest Group and calculate the AUC
New_interest = cbind(genderdata,interestsdata.1[c(1,3:15)])
train_interest = New_interest[1:547,]
test_interest = New_interest[548:684,]
new_interestglm = glm(genderdata~.,family="binomial"(link = "logit"),train_interest)
predict_interest = predict.glm(new_interestglm,test_interest,type='response')
newpred_interest <- prediction(predict_interest,test_interest$genderdata)
newpred_interest.performance <- performance(newpred_interest, measure = "tpr",x.measure = "fpr")
plot(newpred_interest.performance,main= 'ROC of the Interest Group')
AUC_interest = performance(newpred_interest,measure = 'auc')
AUC_interest@y.values
#




# Phobia
phobiadatalogit <- glm(genderdata ~. , family = binomial(link = "logit"), data = phobiadata)
summary(phobiadatalogit)
#pobiabglm <- bestglm(Xy=phobiadata,family = binomial,IC="AIC",method = "forward")
#phobiabglm$BestModel

phobiadatabestglm <- glm(genderdata~Heights+Storm+Darkness+Spiders+Ageing+Fear.of.public.speaking, family = binomial(link = "logit"), data = phobiadata)
summary(phobiadatabestglm)

###draw the ROC of Phobia Group and calculate the AUC
New_phobia = cbind(genderdata,phobiadata[c(2,3,4,5,8,10)])
train_phobia = New_phobia[1:547,]
test_phobia = New_phobia[548:684,]
new_phobiaglm = glm(genderdata~.,family="binomial"(link = "logit"),train_phobia)
predict_phobia = predict.glm(new_phobiaglm,test_phobia,type='response')
newpred_phobia <- prediction(predict_phobia,test_phobia$genderdata)
newpred_phobia.performance <- performance(newpred_phobia, measure = "tpr",x.measure = "fpr")
plot(newpred_phobia.performance,main='ROC of the Phobia group')
AUC_phobia = performance(newpred_phobia,measure = 'auc')
AUC_phobia@y.values

```



```{r}
#final data
newall = cbind(genderdata,musicdata[c(1,4,5,6,7,9,11,15,16,17)],moviedata[c(3,4,5,6,7,10,11)],interestsdata.1[c(1,3:15)],phobiadata[c(2,3,4,5,8,10)])
newall_glm=glm(genderdata~.,family = binomial(link = 'logit'),data= newall)
summary(newall_glm)
newall_glm1=glm(genderdata~Classical.music+Latino + Romantic +War + Fantasy.Fairy.tales+ Western +Action + PC + Reading + Foreign.languages +Cars + Dancing + Active.sport + Shopping + Storm + Heights + Spiders + + Fear.of.public.speaking,family = binomial(link = 'logit'),data= newall)
summary(newall_glm1)

###draw the ROC of the Final Group and calculate the AUC
New_all = cbind(genderdata,newall[(c(3,9,12,13,14,15,16,17,20,21,22,23,24,25,27,28,29,32,34,35,37))+1])
train_all = New_all[1:547,]
test_all = New_all[548:684,]
new_allglm = glm(genderdata~.,family="binomial"(link = "logit"),train_all)
predict_all = predict.glm(new_allglm,test_all,type='response')
newpred_all <- prediction(predict_all,test_all$genderdata)
newpred_all.performance <- performance(newpred_all, measure = "tpr",x.measure = "fpr")
plot(newpred_all.performance, main='ROC of the Final Group')
AUC_all = performance(newpred_all,measure = 'auc')
AUC_all@y.values



```




####What predictions can you make with your model? 
The prediction we can make with our model is to predict the gender based on a person's  preferences and fears. 
We can also use KNN to do the prediction. 




```{r}
###use KNN to test the Final Group.

library(class)
set.seed(3)
All_data_train_rows = sample(1:nrow(New_all),     #<- from 1 to the number of 
                                                     #   rows in the data set
                              round(0.8 * nrow(New_all), 0),  #<- multiply the 
                                                                #   number of rows
                                                                #   by 0.8 and round
                                                                #   the decimals
                              replace = FALSE) 

All_data_train = New_all[All_data_train_rows, ]  #<- select the rows identified in
                                                     #   the All_data_train_rows data
All_data_test = New_all[-All_data_train_rows, ]


chooseK = function(k, train_set, val_set, train_class, val_class){
  
  # Build knn with k neighbors considered.
  set.seed(3)
  class_knn = knn(train = train_set,    #<- training set cases
                  test = val_set,       #<- test set cases
                  cl = train_class,     #<- category for classification
                  k = k,                #<- number of neighbors considered
                  use.all = TRUE)       #<- control ties between class assignments
                                        #   If true, all distances equal to the kth 
                                        #   largest are included
  
  tab = table(class_knn, val_class)
  
  # Calculate the accuracy.
  accu = sum(tab[row(tab) == col(tab)]) / sum(tab)                         
  cbind(k = k, accuracy = accu)
}

knn_different_k = sapply(seq(1, 21, by = 2),  #<- set k to be odd number from 1 to 21
                         function(x) chooseK(x, 
                                             train_set = All_data_train[, c(-1)],
                                             val_set = All_data_test[, c(-1)],
                                             train_class = All_data_train[, c(1)],
                                             val_class = All_data_test[, c(1)]))

knn_different_k = data.frame(k = knn_different_k[1,],
                             accuracy = knn_different_k[2,])
knn_different_k

# Plot accuracy vs. k.

library(ggplot2)

ggplot(knn_different_k,
       aes(x = k, y = accuracy)) +
  geom_line(color = "orange", size = 1.5) +
  geom_point(size = 3)

All_13NN = knn(train = All_data_train[, c(-1)],  #<- training set cases
               test = All_data_test[, c(-1)],    #<- test set cases
               cl = All_data_train[, c(1)],                         #<- category for true classification
               k = 13,                                                       #<- number of neighbors considered
               use.all = TRUE)  

kNN_res = table(All_13NN,
                All_data_test$genderdata)

# Select the true positives and true negatives by selecting
# only the cells where the row and column names are the same.
kNN_res[row(kNN_res) == col(kNN_res)]

# Calculate the accuracy rate by dividing the correct classifications
# by the total number of classifications.
kNN_acc = sum(kNN_res[row(kNN_res) == col(kNN_res)]) / sum(kNN_res)
kNN_acc
```

####How reliable are your results?

The AUC of our final logistic model is 0.9449957, which is a rather high value near to 1. So our model can discriminate well between gender and classify the case right 94% of chance. And the accuracy rate in the KNN is around 0.91.


####What additional information or analysis might improve your model results or work to control limitations?

Our data is around 1000 observations but after we remove the missing data it only remains 684 observations. The test dataset only has around 100 observations.The sample size of our data can be improved.   
We can also use the decision tree to do the classification for our data.

