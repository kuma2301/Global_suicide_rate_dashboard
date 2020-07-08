##installing libraries

library(ggplot2)
library(knitr)
library(imager)
library(Amelia)

#retrieving data
mdata <- read.csv(file.choose())

#summary of data set
summary(mdata)


#pattern of suicides with reference to age group
ggplot(mdata, aes(x=suicides_no, y=age))+geom_boxplot()+coord_flip()

#pattern of suicides with reference to gender.
ggplot(mdata, aes(y=suicides_no, x=sex))+geom_boxplot()+coord_flip()


#to check accuracy mean
by(mdata$suicides_no, mdata$age, mean)

#to check accuracy length
by(mdata$suicides_no, mdata$age, length)


#analysis of variance
anova<-aov(mdata$suicides_no~mdata$sex,data=mdata)
summary(anova)


#Tukey's test would be helpful to see the clear difference in the suicide rates among the age groups.
tukeystest <- TukeyHSD(anova, ordered=TRUE)
tukeystest
plot(tukeystest)
summary(tukeystest)



#assingning the variables for corelation
x<-mdata$suicides_no
x
y<-mdata$gdp_per_capita
y

#correlation, which measures a linear dependence between two variables (x and y)
cor(x,y)

#scatterplot
plot(x,y,main = "scatterplot")


#regression
mod<-lm(x~y)
summary(mod)
attributes(mod)
mod$coefficients

#It can be used to add vertical, horizontal or regression lines to a graph.
abline(mod)
abline(mod,col=2,lwd=0.5)
confint(mod,level = 0.99)
summary(mod)
anova(mod)


#multiple regreesion
result <- lm(mdata$suicides_no~mdata$gdp_per_capita + mdata$year +
                     mdata$population+mdata$sex+mdata$generation+mdata$age+mdata$suicides.100k.pop+mdata$HDI_.for_.year)

summary(result)
attributes(result)

#analysis of variance
anova(result)


tukeystest <- TukeyHSD(anova, ordered=TRUE)
tukeystest
summary(tukeystest)
plot(tukeystest)



#to check confidence level
confint(result,level = 0.99)

#To check proportion between age and gender
prop.table(table(mdata$age,mdata$sex))

# bar plot
barplot(prop.table(table(mdata$age,mdata$sex)),
        col = rainbow(6),
        ylim = c(0,0.7),
        main = "Class Distribution")

#split data set into training and test data sets

set.seed(123)
ind <- sample(2, nrow(mdata), replace = TRUE, prob = c(0.7,0.3))
train <- mdata[ind==1,]
str(train)

test <- mdata[ind==2,]
str(test)

#creating prop table and barplot for train data sets

table(train$sex,train$age)

prop.table(table(train$age,train$sex))
barplot(prop.table(table(train$age,train$sex)),
        col = rainbow(2),
        ylim = c(0,0.7),
        main = "Class Distribution")
summary(train)

#creating prop table and barplot for test data sets

table(test$sex,test$age)

prop.table(table(test$age,test$sex))
barplot(prop.table(table(test$age,test$sex)),
        col = rainbow(2),
        ylim = c(0,0.7),
        main = "Class Distribution")
summary(test)








