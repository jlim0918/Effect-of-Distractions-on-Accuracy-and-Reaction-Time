data_rt <- read.csv("reactionTime.csv", sep = ",", header = T, stringsAsFactors = T)
data_acc <- read.csv("accuracy.csv", sep = ",", header = T, stringsAsFactors = T)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("gplots")
install.packages("ggpubr")

library(alr3)
library(ggplot2)
library(car)
library(dplyr)
library(gplots)
library("ggpubr")

#Reordering labels
data_rt$Type <- ordered(data_rt$Type, levels = c("control", "minor", "major"))
data_acc$Type <- ordered(data_acc$Type, levels = c("control", "minor", "major"))

#Check labels
levels(data_rt$Type)
levels(data_acc$Type)

#Get Means and SD of data
group_by(data_rt, Type) %>%
  summarise(count = n(), mean = mean(Time, na.rm= TRUE), sd = sd(Time, na.rm = TRUE))

group_by(data_acc, Type) %>%
  summarise(count = n(), mean = mean(Percent, na.rm= TRUE), sd = sd(Percent, na.rm = TRUE))

#Box Plots
boxplot_rt <- ggplot(data_rt, aes(x = Type, y=Time, color=Type)) + geom_boxplot()
boxplot_acc <- ggplot(data_acc, aes(x = Type, y=Percent, color=Type)) + geom_boxplot()

boxplot_rt
boxplot_acc

#Mean Plots
ggline(data_rt, x = "Type", y = "Time", 
       add = c("mean_se", "jitter"), 
       order = c("control", "minor", "major"),
       ylab = "Time", xlab = "Type")

ggline(data_acc, x = "Type", y = "Percent", 
       add = c("mean_se", "jitter"), 
       order = c("control", "minor", "major"),
       ylab = "Percent", xlab = "Type")

# ----- ANOVA Test -----
anova_rt <- aov(Time ~ Type,data= data_rt)
anova_acc <- aov(Percent ~ Type, data= data_acc)

summary(anova_rt)
summary(anova_acc)

# Tukey comparision for each ANOVA table 
TukeyHSD(anova_rt)
TukeyHSD(anova_acc)

#Check homogeneity
plot(anova_rt, 1)
plot(anova_acc, 1)

leveneTest(Time ~ Type, data = data_rt)
leveneTest(Percent ~ Type, data = data_acc)

#Check normality
plot(anova_rt, 2)
plot(anova_acc, 2)

aov_rt_res <- residuals(object = anova_rt)
shapiro.test(x = aov_rt_res)

aov_rt_acc <- residuals(object = anova_acc)
shapiro.test(x = aov_rt_acc)

#Drop outliers
data_rt_2 <- data_rt[-c(23,44,67), ]
anova_rt_2 <- aov(Time ~ Type,data= data_rt_2)

plot(anova_rt_2, 1)
leveneTest(Time ~ Type, data = data_rt_2)

plot(anova_rt_2, 2)
aov_rt_res_2 <- residuals(object = anova_rt_2)
shapiro.test(x = aov_rt_res_2)

# ----- Pairwise Comparisons -----

#Reaction Time
rt_pair_1 <- data_rt %>% filter(Type == "control" | Type == "minor")
rt_pair_2 <- data_rt %>% filter(Type == "control" | Type == "major")
rt_pair_3 <- data_rt %>% filter(Type == "major" | Type == "minor")

rt_test_1 <- t.test(Time ~ Type, data = rt_pair_1, paired = TRUE)
rt_test_1

rt_test_2 <- t.test(Time ~ Type, data = rt_pair_2, paired = TRUE)
rt_test_2S

rt_test_3 <- t.test(Time ~ Type, data = rt_pair_3, paired = TRUE)
rt_test_3

#Accuracy
acc_pair_1 <- data_acc %>% filter(Type == "control" | Type == "minor")
acc_pair_2 <- data_acc %>% filter(Type == "control" | Type == "major")
acc_pair_3 <- data_acc %>% filter(Type == "major" | Type == "minor")

acc_test_1 <- t.test(Percent ~ Type, data = acc_pair_1, paired = TRUE)
acc_test_1

acc_test_2 <- t.test(Percent ~ Type, data = acc_pair_2, paired = TRUE)
acc_test_2

acc_test_3 <- t.test(Percent ~ Type, data = acc_pair_3, paired = TRUE)
acc_test_3

