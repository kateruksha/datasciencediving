# load and study the data

library(data.table)
library(mlbench)
library(ggplot2)

data(PimaIndiansDiabetes2)
PimaIndiansDiabetes <- as.data.table(PimaIndiansDiabetes2)
dim(PimaIndiansDiabetes)

any(is.na(PimaIndiansDiabetes))

for (col in colnames(PimaIndiansDiabetes[,-"diabetes", with=F])) {
  print(ggplot(data = PimaIndiansDiabetes) + geom_histogram(bins = 100, mapping = aes_string(x = col)))
}

pairs.panels(PimaIndiansDiabetes2)

# As most of data is skewed, I will apply Box-Cox transformation

################################### Preprocess data ##############################################
library(caret)

set.seed(123)
trainIn <- createDataPartition(PimaIndiansDiabetes$diabetes, times = 1, p = 0.7, list=FALSE)

preprocess <- preProcess(PimaIndiansDiabetes[trainIn,], method = c("scale","center", "BoxCox", 'knnImpute'))
trainset <- predict(preprocess, PimaIndiansDiabetes[trainIn,- "diabetes", with=F])
testset <- predict(preprocess, PimaIndiansDiabetes[-trainIn,- "diabetes", with=F])

pairs.panels(trainset)

################################# principal component analysis#####################################
pca <- prcomp(trainset, scale = F, center = F)
names(pca)
pca$rotation
pca

pca2 <- princomp(trainset, cor = F, scores = T)
names(pca2)
pca2$loadings
pca2$scores # the scores of the supplied data on the principal components



###################################### check with eigenvectors####################################
cov_matrix <- cov(trainset)
library(psych)
tr(cov_matrix) #7.490629
sum(pca$sdev^2) #7.490629

evalues <- eigen(cov_matrix)$values
evalues
pca$sdev^2

evectors <- eigen(cov_matrix)$vectors
evectors[,1:3]
pca$rotation[,1:3]

evalues/tr(cov_matrix)
plot(evalues/tr(cov_matrix), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(evalues/tr(cov_matrix)), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

pairs.panels(pca$x)

###################################### Drawing biplot ##########################################
library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              groups = PimaIndiansDiabetes[trainIn, diabetes], ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

biplot(pca)


# pca in caret
trainset_caret <- preProcess(PimaIndiansDiabetes[trainIn,- "diabetes", with=F], method = c("scale","center", "BoxCox", 'knnImpute', "pca"))
pca_caret = predict(trainset_caret, PimaIndiansDiabetes[trainIn,- "diabetes", with=F])
trainset_caret$rotation[,1:3]


##################### Logistic model for preprocessed data ##################################
trainset <- cbind(trainset, diabetes = PimaIndiansDiabetes[trainIn, diabetes])
trcontrol <- trainControl("cv", 5)

alldata <- train(diabetes ~.,
                 data = trainset,
                 method = "glm",
                 family = binomial,
                 trControl = trcontrol)
summary(alldata)$coef
alldata_predict <- predict(alldata, newdata = testset)
confusionMatrix(data = alldata_predict,
                reference = PimaIndiansDiabetes[-trainIn, ]$diabetes)

library(pROC)
alldata_roc <- roc(response = as.vector(PimaIndiansDiabetes[-trainIn, ]$diabetes), 
                   predictor = predict(alldata, newdata = testset, type = 'prob')[,2])
plot(alldata_roc)
auc(alldata_roc) 


#################### Logistic model for all principal components#############################
train_data <- data.frame(diabetes = PimaIndiansDiabetes[trainIn, ]$diabetes, pca$x)
test_data <- predict(pca, newdata = testset)

allpc <- train(diabetes ~.,
                 data = train_data,
                 method = "glm",
                 family = binomial,
                 trControl = trcontrol)
summary(allpc)$coef
allpc_predict <- predict(allpc, newdata = test_data)
confusionMatrix(data = allpc_predict,
                reference = PimaIndiansDiabetes[-trainIn, ]$diabetes)

allpc_roc <- roc(response = as.vector(PimaIndiansDiabetes[-trainIn, ]$diabetes), 
                   predictor = predict(allpc, newdata = test_data, type = 'prob')[,2])
plot(allpc_roc)
auc(allpc_roc) 


############################ Logistic model for 6 components#####################################
train_data <- data.frame(diabetes = PimaIndiansDiabetes[trainIn, ]$diabetes, pca$x[,1:6])
test_data <- predict(pca, newdata = testset)

sixpc <- train(diabetes ~.,
               data = train_data,
               method = "glm",
               family = binomial,
               trControl = trcontrol)
summary(sixpc)$coef
sixpc_predict <- predict(sixpc, newdata = test_data)
confusionMatrix(data = sixpc_predict,
                reference = PimaIndiansDiabetes[-trainIn, ]$diabetes)

sixpc_roc <- roc(response = as.vector(PimaIndiansDiabetes[-trainIn, ]$diabetes), 
                 predictor = predict(sixpc, newdata = test_data, type = 'prob')[,2])
plot(sixpc_roc)
auc(sixpc_roc) 


# Linear discriminnat analysis
trainset <- predict(preprocess, PimaIndiansDiabetes[trainIn,- "diabetes", with=F])
trainset <- cbind(trainset, diabetes = PimaIndiansDiabetes[trainIn, diabetes])

testset <- predict(preprocess, PimaIndiansDiabetes[-trainIn,- "diabetes", with=F])

library(MASS)
ldaFit <- lda(diabetes ~.,
              data = trainset)
names(ldaFit)
ldaFit

lda_predict <- predict(ldaFit, newdata = testset)
confusionMatrix(data = lda_predict$class,
                reference = PimaIndiansDiabetes[-trainIn, ]$diabetes)

auc(roc(response = as.vector(PimaIndiansDiabetes[-trainIn, ]$diabetes), 
        predictor = lda_predict$posterior[,2]))


lda_cv <- train(diabetes~.,
                data = trainset,
                method = "lda",
                trControl = trcontrol)
lda_predict <- predict(lda_cv, newdata = testset)
confusionMatrix(data = lda_predict,
                reference = PimaIndiansDiabetes[-trainIn, ]$diabetes)

auc(roc(response = as.vector(PimaIndiansDiabetes[-trainIn, ]$diabetes), 
        predictor = predict(lda_cv, newdata = testset, type = 'prob')[,2]))
