
# Machine Learning Model - Stroke prediction code

# load ggplot
library(ggplot2)

# read the csv file
stroke <- read.csv("stroke.csv", na.strings = "N/A", stringsAsFactors = TRUE)

# get an overview of the data frame and variables
summary(stroke)
str(stroke)

# convert binary variables to factors
stroke$hypertension <- as.factor(stroke$hypertension)
stroke$heart_disease <- as.factor(stroke$heart_disease)
stroke$stroke <- as.factor(stroke$stroke)

# remove n/a values
stroke <- na.omit(stroke)

# get an updated overview of the data frame and variables
summary(stroke[, -1])
str(stroke)

## plot series 1: exploratory viz
plot1 <- ggplot(stroke, aes(ever_married))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5, width = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Marriage History", 
       caption = "Q: Has a patient ever been married?")
plot1

plot2 <- ggplot(stroke, aes(smoking_status))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Smoking Status", 
       caption = "Q: Has a patient ever smoked before?")
plot2

plot3 <- ggplot(stroke, aes(hypertension))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5, width = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Hypertension", 
       caption = "Q: Has a patient exhibit signs of hypertension?")
plot3

plot4 <- ggplot(stroke, aes(heart_disease))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5, width = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Heart Disease", 
       caption = "Q: Has a patient exhibit signs or have history of heart disease?")
plot4

plot5 <- ggplot(stroke, aes(age))+
  geom_histogram(bins = 8, fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Distribution of Patient Ages", y = "Count", title = "Histogram", subtitle = "Age")
plot5

plot6 <- ggplot(stroke, aes(avg_glucose_level))+
  geom_histogram(bins = 15, fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Distribution of Average Patient Glucose Levels", y = "Count", title = "Histogram", subtitle = "Average Glucose Levels")
plot6

plot7 <- ggplot(stroke, aes(bmi))+
  geom_histogram(bins = 25, fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Distribution of Patient BMI", y = "Count", title = "Histogram", subtitle = "BMI")
plot7
