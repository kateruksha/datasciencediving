library(data.table)
dt <- data.table(charact_var = sample(c("A", "B", "C"), 1000, replace = TRUE),
                 integer_var = sample(c(1:5), 1000, replace = TRUE),
                 double_var = sample(c(94.0000105, 94.000106, 86.003001), 1000, replace = TRUE, prob=c(0.49, 0.49, 0.02)),
                 logic_var = sample(c(T, F), 1000, replace = TRUE, prob = c(0.7,03)),
                 var1 = rnorm(1000, mean = 0, sd = 1),
                 var2 = rnorm(1000, mean = 58, sd = 17),
                 var3 = sample(LETTERS, 1000, replace = TRUE))
str(dt)

dt <- dt[,.(charact_var = as.factor(charact_var),
            integer_var = as.factor(integer_var),
            double_var = as.factor(double_var),
            logic_var = as.factor(logic_var),
            var1 = as.numeric(var1),
            var2 = as.numeric(var2),
            var3 = as.character(var3))]
str(dt)

#################################### 2.1 Insert a dummy for a vector ####################################

#ifelse way
dt_dummies_ifelse <- dt[,logic_dummy := ifelse(logic_var==TRUE,1,0)]
str(dt_dummies_ifelse)

#as.numeric way
dt_dummies_numeric <- dt[, logic_dummy := as.numeric(logic_var == TRUE)]
str(dt_dummies_numeric)

# mlr::createDummyFeatures
library(mlr)
dt_dummies_mlr <- createDummyFeatures(as.data.frame(dt),  method = "reference")
str(dt_dummies_mlr)

# dummies::dummy.data.frame
library(dummies)
dt_dummies_dummies <- dummy.data.frame(as.data.frame(dt), names = c("charact_var", "integer_var", "double_var", "logic_var"))
dt_dummies_dummies <- dummy.data.frame(as.data.frame(dt), dummy.classes = "factor")
str(dt_dummies_dummies)

# model.matrix way
dt_dummies_modelmatrix <- as.data.table(model.matrix(~charact_var+integer_var+double_var+logic_var - 1, data = dt))
str(dt_dummies_modelmatrix)

#################################### 3. Modeling ####################################
# mtcars example
mtcars <- as.data.table(mtcars)
eq_1 <- lm(mpg ~ wt + hp +wt*am + hp*am + am, mtcars)
summary(eq_1)
with(summary(eq_1), df[2] * sigma^2)

eq_2 <- lm(mpg ~ wt + hp, mtcars[am ==1,])
summary(eq_2)
with(summary(eq_2), df[2] * sigma^2)

eq_3 <- lm(mpg ~ wt + hp, mtcars[am ==0,])
summary(eq_3)
with(summary(eq_3), df[2] * sigma^2)

eq_4 <- lm(mpg ~ wt + hp + am, mtcars)
summary(eq_4)
with(summary(eq_4), df[2] * sigma^2)
anova(eq_4)
anova(eq_1)


eq_5 <- lm(mpg ~ wt + hp , mtcars)
summary(eq_5)
