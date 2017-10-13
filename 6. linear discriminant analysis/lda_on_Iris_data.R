# LDA multiclass example (Iris data set)

####################################### I. Load and prepare data #############################
require(MASS)
require(caret)
require(scales)
require(ggplot2)
require(ggord)


data(iris)
set.seed(123)
splitIndex <- createDataPartition(iris$Species, p = 0.5, list = FALSE, times = 1)

preprocessed <- preProcess(iris[splitIndex,])
train <- predict(preprocessed, newdata = iris[splitIndex,])
test <- predict(preprocessed, newdata = iris[-splitIndex,])

####################################### II. LDA #############################################

ldaFit <- lda(Species~., 
              train,
              prior = c(1,1,1)/3)
ldaFit 
plot(ldaFit)
ldaPredClasses <- predict(ldaFit, newdata = test)
prop.lda <- ldaFit$svd^2/sum(ldaFit$svd^2)
confusionMatrix(data = ldaPredClasses$class,
                reference = test$Species) #Accuracy = 0.9733

dataset <- data.frame(species = test$Species,
                      lda = ldaPredClasses$x)

ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

ggord(ldaFit, train$Species)

####################################### III. QDA #############################################
qdaFit <- qda(Species~., 
              train,
              prior = c(1,1,1)/3)
qdaFit 
qdaPredClasses <- predict(qdaFit, newdata = test)
confusionMatrix(data = qdaPredClasses$class,
                reference = test$Species) #Accuracy = 0.96
