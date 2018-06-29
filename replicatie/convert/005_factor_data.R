setwd("D:/R/replicatie/convert")
load("../data/dis_df.Rdata")

######### factorizing for presentation purposes

age_classes <- c("15-19", "20-24", "25-29", "30-34", "35-39")
bmi_classes_1 <- c("normal", "underweight", "overweight", "obese")
bmi_classes_2 <- c("normal", "underweight", "overweight", 
                   "obese 30-35", "obese 35-40", "obese 40+")
race_classes <- c("White", "Black", "Hispanic", "Other")
edu_classes <- c("0-8", "9-11", "12", "12+")
marit_classes <- c("Married", "Separated etc", "Never Married")

dis_df$sex <- factor(dis_df$sex , labels= c("M", "F"), levels=1:2)
dis_df$rural <- as.factor(c("Urban", "Rural")[dis_df$rural])
dis_df$bmi_class <- factor(dis_df$bmi_class , 
                           levels=c(1,0,2:3), 
                           labels=bmi_classes_1)
dis_df$bmi_class_2 <- factor(dis_df$bmi_class_2 , 
                             levels=c(1,0,2:5), 
                             labels=bmi_classes_2)
dis_df$age_class <- factor(dis_df$age_class, 
                           levels=1:5, labels= age_classes)
dis_df$race <- factor(dis_df$race, levels= 1:4, label= race_classes)
dis_df$edu_class <- factor(dis_df$edu_class, 
                           levels=1:4, labels= edu_classes)
dis_df$marit_class<- ifelse(dis_df$marit >6, 3, 
                            ifelse(dis_df$marit >3, 2, 1))
dis_df$marit_class <- as.factor(marit_classes[dis_df$marit_class])

# some adjustments to names in table 2
dis_df_2$sex <- as.factor(c("M", "F")[dis_df_2$sex])
dis_df_2$rural <- as.factor(c("Urban", "Rural")[dis_df_2$rural])

dis_df_2$age_class <- factor(dis_df_2$age_class, 
                             levels=1:5, labels= age_classes)
dis_df_2$race <- factor(dis_df_2$race, levels= 1:4, label= race_classes)
dis_df_2$edu_class <- factor(dis_df_2$edu_class, 
                           levels=1:4, labels= edu_classes)
dis_df_2$marit_class<- ifelse(dis_df_2$marit >6, 3, 
                            ifelse(dis_df_2$marit >3, 2, 1))
dis_df_2$marit_class <- as.factor(marit_classes[dis_df_2$marit_class])



df<-dis_df
df2<-dis_df_2

save(df, df2, file="../data/df.Rdata")