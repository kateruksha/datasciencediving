# LDA multiclass example (Glass data set)

####################################### I. Load and prepare data #############################
library(mlbench)
library(caret)
library(data.table)

data(Glass)
dim(Glass)
table(Glass$Type)
head(Glass)

Glass <- as.data.table(Glass)
Glass$Type <- as.factor(Glass$Type)


set.seed(1)
ss <- sample(1:3, size = nrow(Glass), replace = T, prob = c(0.7,0.3, 0.3))
ss <- ifelse(ss==1, "train", ifelse(ss==2, "val", "test"))

preprocess <- preProcess(Glass[ss=="train",])
train_st <- predict(preprocess, newdata = Glass[ss=="train", ])
val_st <- predict(preprocess, newdata = Glass[ss=="val", ])
test_st <- predict(preprocess, newdata = Glass[ss=="test", ])


#################################### II. Linear discriminant analysis ##########################
ldaModel <- lda(x = train_st[, -"Type", with=F],
                grouping = train_st$Type)

ldaPredClasses <- predict(ldaModel, newdata = test_st[,-c("Type"), with=F])

confusionMatrix(data = ldaPredClasses$class,
                reference = test_st$Type)

#################################### III. With cross-validation ##########################

Glass$Type <- factor(paste("Class", as.numeric(Glass$Type), sep = ""))

# Cross-validation on the training set

trainControl <- trainControl(method = "cv", 
                       number = 3, 
                       returnResamp = "all",
                       classProbs = TRUE)


set.seed(1)
ldaCv <- train(Glass[ss=="train",-"Type", with=F], 
                             Glass[ss=="train",]$Type, 
                             method = "lda2", 
                             trControl = trainControl,
                             tunelength = 5,
                             preProc = c("center", "scale"))

# Model evaluation on the validation set

ldaVal <- lda(x = val_st[, -"Type", with=F],
              grouping = val_st$Type)

# Assess model performance on the test set
ldaPredClasses <- predict(ldaVal, newdata = test_st[,-c("Type"), with=F])

confusionMatrix(data = ldaPredClasses$class,
                reference = test_st$Type)

