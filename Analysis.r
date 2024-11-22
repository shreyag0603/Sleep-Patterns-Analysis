data=read.csv("C:/Users/avikm/OneDrive/Documents/SleepPatterns_and_Academic_Performance - cleaned.csv")
library(dplyr)
dim(data)
summary(data)

#Average sleep a student gets in a weeknight

mean(data$Avg_sleep.weeknight.)

#Inference-We see that a student gets a sleep of 5-6 hours in a weeknight
#which is less than the required hours of sleep required for a person

#Average sleep by each gender in a weeknight

sleep_gender <- data %>%
  group_by(Gender) %>%
  summarize(mean_sleep_weeknight = mean(Avg_sleep.weeknight., na.rm = TRUE))
print(sleep_gender)

#Inference-We see that males get more sleep on an average on a weeknight
#but the difference is minimal and below the required hours nof sleep

#Table to show how many students have a healthy sleep paaterns and academic performance
# Range-
#20-30-  Weak academic performance due to a bad sleep pattern
#30-40- Moderately weak academic performance due to a bad sleep pattern
#40-50- Decent academic performance due to average sleep schedule
#50-60-Good academic performance due to good  sleep schedule
#60-67- Good academic performance due to excellent sleep schedule

ranges <- cut(data$Total, breaks = c(20, 30, 40, 50, 60, 67), right = FALSE)
table_result <- table(ranges)
print(table_result)

#Inference-In our study,most student's academic record is affected by a bad sleep schedule
#Maintaing a average sleep schedule helps improving the academic record of a student

library("moments")
#Relation between late Night studying and GPA

plot(density(data$Engagement_in_late.night_studying..cramming_for_exams), main = "Weekly Average Studies vs. GPA", 
     xlab = "Late night studying", ylab = "Density", col = "skyblue",lwd=2)
lines(density(data$GPA), col = "orange", lwd = 2)
legend("topleft", legend = c("Late Night Studying", "GPA"), fill = c("skyblue", "orange"))

#Inference- The density plot between late night studying and GPA shows that 
#students who study late night moderately have a high GPA.These are students who
#agree that that they study late in the night but they dont do it regularly

# Does a part time job affect the number of hours a student sleeps in a weeknight

library(vioplot)
vioplot(data$Avg_sleep.weeknight. ~ data$Avg_hours_per_week.part.time.job.,
        names = unique(data$Avg_hours_per_week.part.time.job.),
        main = "Average Sleep vs. Part-Time Job Hours", 
        xlab = "Average Hours per Week (Part-Time Job)", ylab = "Average Sleep (Weeknight)")

#Inference- this plot shows that the Students without part-time jobs (0 hours per week) have a higher average weeknight sleep compared to students working part-time.


#Average sleep by qualification

barplot(height = tapply(data$Avg_sleep.weeknight., data$Qualification, mean),
        names.arg = unique(data$Qualification), main = "Average Sleep by Qualification",
        xlab = "Qualification", ylab = "Average Sleep (Weeknight)")

#Inference-This plot shows that undergraduate students are having less sleep 
#than postgraduates and high school students 


#Which gender in which age group takes the highest short naps during the day 

library(ggplot2)
boxplot_short_naps <- ggplot(data, aes(x = Age, y = Short_naps_during_day, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Short Nap Duration by Age Group and Gender",
       x = "Age Group",
       y = "Short Nap Duration",
       fill = "Gender")
print(boxplot_short_naps)

#Inference- In this plot,we see that females in the age group of 18-22 take 
#the highest amount of short naps during the day.This might be due to
#not having a proper sleep pattern.

