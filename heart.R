# SAYALI (explanation of libraries)

library(ggplot2)
  library(dplyr)
  library(tidyverse
  library(readr)
  library(corrplot)
  library(ggcorrplot)
  library(factoextra)
  library(DataExplorer)
  library(ClustOfVar)
  library(gridExtra)
  
  #there uses and what are there main functions
  
  # HITESH 
  # read file
  
  data <- read.csv("heart.csv")

glimpse(data)

View(data)

#changing name of columns 

colnames(data)[colnames(data)=='ï..age'] <- "age"

# check missing values

plot_missing(data)

# data transformation

data2 <- data %>% 
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

View(data2)

summary(data2)

colnames(data2)[colnames(data2)=='ï..age'] <- "age"

#percentage of peoples having heart disease or not

prop.table(table(data2$target))

#representation of correlations

cor_heart <- cor(data2[,10:14])
cor_heart
corrplot(cor_heart, method = "ellipse", type="upper",)

# SABA

#boxplot

boxplot(data2[,9:14])

#Bar plot for target (Heart disease) 

ggplot(data2, aes(x=data2$target, fill=data2$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))

#Counting the frequency of the values of the age

data2 %>% 
  group_by(age) %>% 
  count() %>% 
  filter(n > 10) %>% 
  ggplot()+
  geom_col(aes(age, n), fill = "navyblue")+
  ggtitle("Age Analysis") +
  xlab("Age")  +
  ylab("AgeCount")

# bar plot types of chest pain

ggplot(data2, aes(cp, fill = target))+
  geom_bar(position = "fill")+
  ggtitle("cp")

#histogram Distribution of Male and Female population across age

data2 %>%
  ggplot(aes(x=age,fill=sex))+
  geom_histogram(col="black")+
  xlab("Age") + 
  ylab("Number")+
  guides(fill = guide_legend(title = "Gender"))

#KETAKI

#scatterplot Representation of Cholestoral level 

data2 %>%
  ggplot(aes(x=age,y=chol,color=sex, size=chol))+
  geom_point(alpha=0.7)+xlab("Age") +
  ylab("Cholestoral")+
  guides(fill = guide_legend(title = "Gender"))


#box plot Comparison of Blood pressure across pain type

data2 %>%
  ggplot(aes(x=sex,y=trestbps))+
  geom_boxplot(fill="darkorange")+
  xlab("Sex")+
  ylab("BP")+
  facet_grid(~cp)

#combination of histogram and density

ggplot(data2)+
  geom_histogram(aes( x = oldpeak, y = ..density.., fill =  target), position = position_dodge2())+
  geom_density(aes( x = oldpeak), alpha =.3 ,fill = "orange")

#multiple bar graphs in single plot

grid.arrange(
  ggplot(data2, aes(x = sex, fill = target))+
    geom_bar(position = "fill"),
  
  ggplot(data2, aes(x = fbs, fill = target))+
    geom_bar(position = "fill"),
  
  ggplot(data2, aes(x = exang, fill = target))+
    geom_bar(position = "fill"), nrow = 3
)

#types of multiple bar graphs in single plot

#type1

grid.arrange(
  ggplot(data2, aes(x = cp, fill = target))+
    geom_bar(position = "fill")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
  
  ggplot(data2, aes(x = restecg, fill = target))+
    geom_bar(position = "fill")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)), ncol = 2
)

#type2

# -------------------------------------------------------------------------


grid.arrange(
  ggplot(data2, aes(x = slope, fill = target))+
    geom_bar(position = "fill"), ncol = 3,
  
  ggplot(data2, aes(x = ca, fill = target))+
    geom_bar(position = "fill"),
  
  ggplot(data2, aes(x = thal, fill = target))+
    geom_bar(position = "fill")
  
  
  