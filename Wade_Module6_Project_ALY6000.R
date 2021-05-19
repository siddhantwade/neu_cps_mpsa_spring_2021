library(vcd)
install.packages('FSA')
library(FSA)
install.packages('FSAdata')
library(FSAdata)
install.packages('magrittr')
library(magrittr)
install.packages('dplyr')
library(dplyr)
install.packages('plotrix')
library(plotrix)
install.packages('ggplot2')
library(ggplot2)
install.packages('tidyr')
library(tidyr)
install.packages('reshape2')
library(reshape2)
install.packages('moments')
library(moments)
install.packages('plyr')
library(plyr)
install.packages("ggpubr")
library(ggpubr)
install.packages('pivottabler')
library(pivottabler)
install.packages('tidyverse')
library(tidyverse)
install.packages('here')
library(here)
install.packages('skimr')
library(skimr)
install.packages('kableExtra')
library(kableExtra)
install.packages('readxl')
library(readxl)
install.packages("corrplot")
library(corrplot)

options(max.print = .Machine$integer.max)

df <-read.csv(file = '/Users/siddhantwade/Documents/Masters/Academics/Term 1/ALY 6000 Introduction to Analytics/Assignments and Homework/Module 6 Assignment/bank_marketing_6K_sample.csv')

#dropping the unncessary index column - not required if not in source data

colnames(df)
#_________________________________________________________________________________
#_________________________________________________________________________________

# Adding a new column: duration_in_mins

df$duration_in_mins = df$duration/60


hist(df$age, breaks = 100,  main = "Age Histogram", col = 'lightpink', ann = 'True')

# Change line color by sex
ggplot(df, aes(x = age)) +
  geom_histogram(aes(color = y), fill = "white",
                 position = "identity", bins = 30) + scale_color_manual(values = c("#00AFBB", "#E7B800")) 

#############################################
# Frequency tables and plot for categorical data

count(df = df, vars = c('y'))

#freq for y

categorical_variable = 'education'  #define which variable to work with
df_cat = df #define which df you want to use
freqdf = count(df = df_cat, vars = c(categorical_variable)) #temp freqdf for any category under analysis
freqdf$percent = ((freqdf$freq)/nrow(df_cat)) * 100
View(freqdf)  

barplot(freqdf$percent,col = 'Pink', border="white", xlab="group")

## Slide 1

subset(df, age < 30 && age > 60)
subset(df, (df$age < 30) & (df$age > 60))
df[(df$age < 30) & (df$age > 60),]
dfs1 = subset(df, age < 30)
dfs2 = subset(df, age > 60)
dfs1 = rbind(dfs1,dfs2) # dfs1 has age < 30 and age > 30


#looking at distributions
categorical_variable = 'y'  #define which variable to work with
df_cat = dfs1 #define which df you want to use
freqdf = count(df = df_cat, vars = c(categorical_variable)) #temp freqdf for any category under analysis
freqdf$percent = ((freqdf$freq)/nrow(df_cat)) * 100
View(freqdf)

#index of dfs1 as a single 1d array object
c(row(dfs1))

#taking rows between age 30 and 60
anti_join(df, dfs1, by = df.index) # this is not working, idk what an index object is in r hence can't anti join

## Slide 4
dfs3 = subset.data.frame(x = df, (age > 30  & (age < 60))) #subsetting with multiple conditions 
#dfs3 has age 30 - 60

summary(subset(dfs3, y = 'yes'))
summary(subset(dfs3, y = 'no'))

#looking at differences in euribor in the middle age range
means = ddply(df, 'y', summarise, grp.mean = mean(euribor3m))
ggplot(df, aes(x = euribor3m, color = y)) + geom_histogram()

ggdensity(df, x = "euribor3m",
          add = "mean", rug = TRUE,
          color = "y", fill = "y",
          palette = c("#00AFBB", "#E7B800"))

colnames(df) #temp

## Slide 5 Understanding impact of call duration

#adding a column for duration in mins

df$duration_in_mins = df$duration/60

ggdensity(df, x = "duration_in_mins",
          add = "mean", rug = TRUE,
          color = "y", fill = "y", labels = c(0,1,2,4,10,15,20,25,30,35,40,45,50),
          palette = c("#00AFBB", "#E7B800"))

quantile(x = subset(df, y == 'yes')$duration_in_mins)
quantile(x = subset(df, y == 'no')$duration_in_mins)
quantile(x = df$duration_in_mins)

df$duration 
?subset()

#observing conversion in duration above and below 5 minutes
subset(df, duration_in_mins > 5)

categorical_variable = 'y'  #define which variable to work with
df_cat = subset(df, duration_in_mins > 5) #define which df you want to use
freqdf = count(df = df_cat, vars = c(categorical_variable)) #temp freqdf for any category under analysis
freqdf$percent = ((freqdf$freq)/nrow(df_cat)) * 100
freqdf
#View(freqdf)

categorical_variable = 'y'  #define which variable to work with
df_cat = subset(df, duration_in_mins < 5) #define which df you want to use
freqdf = count(df = df_cat, vars = c(categorical_variable)) #temp freqdf for any category under analysis
freqdf$percent = ((freqdf$freq)/nrow(df_cat)) * 100
freqdf
View(freqdf)

## SLIDE 6: Interaction of Duration and Euribor.rate

#scatter plot 
ggplot(df, aes(x = duration_in_mins, y = euribor3m, color = y)) + 
  geom_point(shape = 1, size = 1) + scale_color_manual(values = c("grey66", "royalblue3")) + 
  geom_smooth(method = lm, se = FALSE) + stat_cor(method = 'pearson',label.x = 3)


##Slide 6 multivariate with numeric and categorical

#categorical summaries

# checking if group by can get the job done

df %>% group_by(y) %>% summarise(duration = mean(duration))


#Visualization color scheme, from sadhana's dist plot

#9bbebf (green) #f0bfbc (pink)

grDevices::colors()


#_________________________________________________________________________________
#_________________________________________________________________________________
#                   Multi Bar Plots for distributions

#Exhibit 1: 

ggplot(data = df, aes(x = age ,color = y)) + geom_histogram(binwidth = 2, fill = '#f0bfbc') + 
  geom_vline(aes(xintercept = mean(age))) + scale_color_manual(values=c("#9bbebf", "black")) +
    annotate("text", x = (mean(df$age) + 0), y = 400, label = "Mean") +
      ggtitle(label = 'Exhibit 1: High conversion % in age < 30 and age > 60', subtitle = 'Title: Histograme of Age') 

 
#Exhibit 2: PENDING DEVELOPMENT

ggplot(data = df, aes(x = age ,color = y)) + geom_histogram(binwidth = 2, fill = '#f0bfbc') + 
  geom_vline(aes(xintercept = mean(age))) + scale_color_manual(values=c("#9bbebf", "black")) +
  annotate("text", x = (mean(df$age) + 0), y = 400, label = "Mean") +
  ggtitle(label = 'Exhibit 1: High conversion % in age < 30 and age > 60', subtitle = 'Title: Histograme of Age') 


#Exhibit 3:

column = df$euribor3m
ggplot(data = df, aes(x = column ,color = y)) + geom_histogram(binwidth = 2, fill = '#f0bfbc') + 
  geom_vline(aes(xintercept = mean(column))) + scale_color_manual(values=c("#9bbebf", "black")) +
  annotate("text", x = (mean(column) + 0), y = 400, label = "Mean") + 
  ggtitle(label = 'Exhibit 3: High conversion in lower Euribor Rates', subtitle = 'Title: Histograme of Euribor Rate') +
  xlab(label = 'euribor3m')

#Exhibit 4: PENDING ######################

?qhpvt() #functio under use for generating tables

qhpvt(df, rows = "job", columns = "y", calculations = "n()") #measures count of occurence across rows

#what we want is mean of another variable across rows

qhpvt(df, rows = "job", columns = "y", calculations = mean(euribor3m)) #don't know how to change calculation as required


#attempting pivot with dplyr

skimr::skim(df) #dataframe summary

df %>% 
  group_by(job) %>%
  summarise()

?group_by()


lobsters <- read_csv("/Users/siddhantwade/Documents/Data Science /Content Repository/04 Machine Learning/Machine Learning Master/Datasets Repo/Lobster_Abundance_All_Years.csv")


lobsters %>%
  group_by(YEAR) %>%
  summarize(count_by_year = n())


lobsters %>%
  summarize(count =  n())


#Exhibit 5: Done

ggplot(data = df, mapping = aes(x = age, y= euribor3m, colour = y)) + geom_point(size = 0.8) + 
  ggtitle("Exhibit 5: Euribor varies widely across all age groups", subtitle = 'Title: Scatterplot of Age Vs Euribor3m') +
  geom_smooth(method = 'lm', se = FALSE ) + stat_cor(method = 'pearson', label.y = 3)

#Exhibit 5 b: Full correlation plot #extra

df_numeric <- df[,unlist(lapply(df, is.numeric))]  

corrplot(df_numeric, method="number")


#Exhibit 7: Done

column = df$duration_in_mins
ggplot(data = df, aes(x = column ,color = y)) + geom_histogram(binwidth = 2, fill = '#f0bfbc') + 
  geom_vline(aes(xintercept = mean(column))) + scale_color_manual(values=c("#9bbebf", "black")) +
  annotate("text", x = (mean(column) + 0), y = 400, label = "Mean = 4.3") + 
  ggtitle(label = 'Exhibit 3: Most customers spend less thatn 5 mins on call', subtitle = 'Title: Histograme of Duration in minutes') + 
  xlab(label = 'duration_in_mins')

mean(column)

#Exhibit 9: Done

ggplot(data = df, mapping = aes(x = duration_in_mins, y= euribor3m, colour = y)) + geom_point(size = 0.8) + 
  ggtitle("Exhibit 5: Euribor varies widely across all age groups", subtitle = 'Title: Scatterplot of Age Vs Euribor3m') +
  geom_smooth(method = 'lm', se = FALSE ) 


#Exhibit 10: 

ggplot(data = df, aes(x = column ,color = y)) + geom_histogram(binwidth = 2, fill = '#f0bfbc') +
  ggtitle(label = 'Exhibit 10: Customers with high pre campaign calls are more likely to convert', subtitle = 'Title: Histograme of Calls Before the Campaign') + 
  xlab(label = 'previous')

# Exhibit 11: 






#ntroduction to dataset - summaries

df_numeric <- df[,unlist(lapply(df, is.numeric))]  

skimr::skim(df_numeric)

#selecting multiple columns specifically by name
df_categorical = df %>% select('job','marital','education','default','housing','loan','contact','month','day_of_week','poutcome','y')

skimr :: skim(df_categorical)






