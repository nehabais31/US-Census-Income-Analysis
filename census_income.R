install.packages("ggthemes", repos = "https://cran.rstudio.com") 
install.packages("gridExtra")

###Loading all packages
library(tidyverse)          # Data manupulation
library(tidyr)              # used to tidy up data
library(dplyr)              # used for data manipulation
library("ggthemes")         # for using themes for plots
library(gridExtra)
library(ggplot2)
library(sampling)


############################
#    Preparing data        #             
############################

getwd()
setwd('E:/Neha/Course_Materials/Summer2020/R/Project/Final')          # set to your current working directory

raw_data <- read.csv('adult.csv')

class(raw_data)           # class of entire dataset

colnames(raw_data)
dim(raw_data)             # 32561 obs. of  15 variables

str(raw_data)             # workclas, occupation, native.country columns have some unknown values

table(complete.cases(raw_data))

#########################################
#             Data Cleaning             #
#         Pre-process the data          #
#########################################

# Checking total rows containing missing values
missing_values <- filter(raw_data, raw_data$workclass == '?'| raw_data$occupation == '?'| raw_data$native.country == '?')
nrow(missing_values)               # total 2399 rows containing missing values

head(missing_values)

# Exclude the missing values from our dataset
raw_data <- filter(raw_data, !raw_data$workclass == '?',                #  30162 obs. left
                   !raw_data$occupation == '?',
                   !raw_data$native.country == '?')
raw_data <- droplevels(raw_data)

head(raw_data)
summary(raw_data)
str(raw_data)               # 9 categorical and 6 numerical variables


###############################################
#               Analysing Data                #
###############################################

adult <- raw_data
head(adult)

higher_pay <- subset(adult, adult$income == '>50K')
lower_pay  <- subset(adult, adult$income == '<=50K')

#########################
#    Distibution of Age #
#########################

summary(adult$age) 
mean(adult$age)       # 39 years

summary(higher_pay$age)

summary(lower_pay$age)

age_data <- adult$age

age_graph_distr <- ggplot(data = adult, aes(age_data) )+
                   geom_histogram(binwidth = 5, fill="light green", color="#e9ecef", alpha=0.9) +
                   labs(x = "Age", y = "Count", title = "Histogram for Age")+
                   theme(plot.title = element_text(hjust = 0.5)) 

age_graph_density <- ggplot(adult, aes(x = as.numeric(age_data))) +
                     geom_density() +
                     labs(x = "Age", y = "Density", title ="Density distribution of Age") +
                     theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(age_graph_distr, age_graph_density, nrow = 1)


# Conclusion
# The ages range from 17 to 90 years old with the majority of entries between the ages of 25 and 50 years
# The avergae age of individuals who responded to this survey is 39 years.

###################
# Age vs Income   #
###################

summary(higher_pay$age)
summary(lower_pay$age)

plot1_age <- ggplot(lower_pay, aes(x= income, y = age, color = income)) +
  geom_boxplot(fill = 'light green') +
  labs(x = "Income", y = "Age", title = "Less than 50K", fill = "age")+
  theme(plot.title = element_text(hjust = 0.5)) 

plot2_age <- ggplot(higher_pay, aes(x= income, y = age, color = income)) +
  geom_boxplot(fill = 'light blue') +
  labs(x = "Income", y = "Age", title = "More than 50K", fill = "age")+
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(plot1_age, plot2_age, nrow = 1)

### Conclusion
# Median age of people earning >50K is greater than median age of people earning <50K
# Older people earn more compared to younger ones.


###################################
# Earnings vs time spent on work  #
###################################

summary(higher_pay$hours.per.week)
summary(lower_pay$hours.per.week)

plot1_hr <- ggplot(lower_pay, aes(x= income, y = hours.per.week, fill = income)) +
  geom_boxplot(fill = 'light green') +
  labs(x = "Income", y = "Hours spent per week", title = "Less than 50K")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") 

plot2_hr <- ggplot(higher_pay, aes(x= income, y = hours.per.week, fill = income)) +
  geom_boxplot(fill = 'light blue') +
  labs(x = "Income", y = "Hours spent per week", title = "More than 50K")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") 

grid.arrange(plot1_hr, plot2_hr, nrow = 1)

########### Conclusion
# 1. People who spend more time are likely to earn more.
# The outliers in both the plots represent variations in both groups. Some jobs demand more working hous
# but pay less, while some pay higher even for less working hours.


###########################
#  Capital Gain / Loss
###########################

capital_gain_distr <- ggplot(data = adult, aes(x=capital.gain) )+
  geom_histogram(binwidth = 10000, fill = 'light green' ) +
  labs(x = "Capital Gain", y = "Frequency", title = "Histogram for Capital Gain")+
  theme(plot.title = element_text(hjust = 0.5)) 


capital_loss_distr <- ggplot(data = adult, aes(x=capital.loss) )+
  geom_histogram(binwidth = 500, fill = 'light green' ) +
  labs(x = "Capital Loss", y = "Frequency", title = "Histogram for Capital Loss")+
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(capital_gain_distr, capital_loss_distr, ncol = 2)



###########################
#  Workclass vs Earnings  #
###########################

dim(adult)
colnames(adult)

workclass_data <- adult %>% group_by(workclass, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), 
         order = ifelse(income== '>50K', n/sum(n), 0))

workclass_dist <- ggplot(workclass_data, aes(x = reorder(workclass, order), y = n)) +
  geom_bar(stat = 'identity', fill = 'light green') +
  labs(x = "Count", y = "Workclass", title = "Workclass Distribution")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  coord_flip()

workclass_inc_pct <- ggplot(workclass_data, aes(x = reorder(workclass, order), y = pct, fill = income)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(name = "Workclass") +
  scale_y_continuous(name= "Percentage", labels = scales::percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Greens") +
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income distribution by Workclass") +   
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()

grid.arrange(workclass_dist, workclass_inc_pct, nrow = 1)

### Conclusion:
# People who responded to this survey mostly work for Private sector. Federal govt employees and and people who are self employed mostly earn >50K.


###########################
#    Education vs Income  #
###########################

education_data <- adult %>% group_by(education, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), 
  order = ifelse(income== '>50K', n/sum(n), 0))

education_dist <- ggplot(education_data, aes(x = reorder(education, order), y = n)) +
  geom_bar(stat = 'identity', fill = 'light green') +
  labs(x = "Count", y = "Education", title = "Education Distribution")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  coord_flip()


education_inc_pct <- ggplot(education_data, aes(x = reorder(education, order), y = pct, fill = income)) +
  geom_bar(stat = 'identity') +
  labs(x = "Education", y = "Percentage", title = "Education Distribution", labels = scales::percent)+
  scale_fill_brewer(palette = "Greens") +
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income distribution by Education level") +   
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()

grid.arrange(education_dist, education_inc_pct, nrow = 1)


### Conclusion:
# As per the plot, it is clear that most of the respondents of this survey have completed their high school, gone to some college or have bachelor's degree.
# People of Prof-school, and those who have doctorate and have done masters, more likely to earn > 50K.


##################################
#        Marital Status         #  
##################################

marital_data <- adult %>% group_by(marital.status, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), 
         order = ifelse(income== '>50K', n/sum(n), 0))

marital_dist <- ggplot(marital_data, aes(x = reorder(marital.status, order), y = n)) +
  geom_bar(stat = 'identity', fill = 'light green') +
  labs(x = "Count", y = "Marital Status", title = "Marital status distribution")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  coord_flip()

marital_inc_pct <- ggplot(marital_data, aes(x = reorder(marital.status, order), y = pct, fill = income)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(name = "Marital Status") +
  scale_y_continuous(name= "Percentage", labels = scales::percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Greens") +
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income distribution by Marital status") +   
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()

grid.arrange(marital_dist, marital_inc_pct, nrow = 1)

#### Conclusion
# Most of the respondents are married civilians or Never married.
# As per this survey, married people earn more as compared to not married ones.


#################################
#          OCCUPATION           #
#################################

occupation_data <- adult %>% group_by(occupation, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), 
         order = ifelse(income== '>50K', n/sum(n), 0))

occupation_dist <- ggplot(occupation_data, aes(x = reorder(occupation, order), y = n)) +
  geom_bar(stat = 'identity', fill = 'light green') +
  labs(x = "Count", y = "Occupation", title = "Occupation distribution")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  coord_flip()

occupation_inc_pct <- ggplot(occupation_data, aes(x = reorder(occupation, order), y = pct, fill = income)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(name = "Occupation") +
  scale_y_continuous(name= "Percentage", labels = scales::percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Greens") +
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income distribution by Occupation") +   
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()

grid.arrange(occupation_dist, occupation_inc_pct, nrow = 1)

##### Conclusion
# As per the graph, people at a position of Executive management and professional specialization
# have a higher probaility of earning more than 50K.


###############################
#   Gender vs Income          #
###############################
table(higher_pay$sex)            # 1112 Female |  6396 male
table(lower_pay$sex)             # 8670 Female |  13984 male

higher_pay_male <- higher_pay %>% filter(sex == 'Male')    # 6396 male earning >50K
lower_pay_male  <- lower_pay %>% filter(sex == 'Male')     # 13984 male earning <=50K

higher_pay_female <- higher_pay %>% filter(sex == 'Female')    # 1112 female earning >50K
lower_pay_female  <- lower_pay %>% filter(sex == 'Female')     # 8670 female earning <=50K

male <- c(as.vector(table(lower_pay_male$sex)), as.vector(table(higher_pay_male$sex)))
female <- c(as.vector(table(lower_pay_female$sex)), as.vector(table(higher_pay_female$sex)))

# Contigency table for gender and income
# table(adult$sex, adult$income)

gender_inc_data <- rbind(male, female)
rownames(gender_inc_data) <- c('Male','Female')
colnames(gender_inc_data) <- c('<=50K', '>50K')

pct <- round( prop.table(gender_inc_data, margin = 1)* 100)

par(mfrow=c(1,2))

# Gender distribution
barplot(table(adult$sex), beside = TRUE, las = 1, ylim = c(0,20000),
        legend.text = TRUE, col = c('pink', 'light blue'), args.legend = list(x = 'topleft'),
        main = 'Gender distribution') 

# Income distribution based on Gender
mosaicplot(gender_inc_data, col = c('cyan','light green'), xlab = 'Gender', ylab = 'Income brackets',
           main = 'Gender wise Income distribution' )

par(mfrow=c(1,1))  

#### Conclusion
# Out of total male population - 69% of male earn less than 50K & 31% male earn more than 50K.
# Out of total female population -89% female earn less than 50K & 11% female earn more than 50K.
# Wecan see that there is almost double the sample size of male as compared to female in the dataset.
# % of male who earn more than 50K is much greater than the % of female who earn the same amount.


##################################################################
# Central Limit Theorem and histograms for hours spent per week  #
##################################################################
# 
hours <- adult$hours.per.week
mean(hours)   # 40.93
sd(hours)     # 11.98

# Total samples and sample size
samples <- 10000

sample_size <- c(5, 50, 100, 500)

# Place holder for our samples
hours_sample_5   <- numeric(samples)
hours_sample_50  <- numeric(samples)
hours_sample_100 <- numeric(samples)
hours_sample_500 <- numeric(samples)

for (size in sample_size){
  for (i in 1: samples){
    hours_sample_5[i] <- mean(sample(hours, sample_size[1]))
    hours_sample_50[i] <- mean(sample(hours, sample_size[2]))
    hours_sample_100[i] <- mean(sample(hours, sample_size[3]))
    hours_sample_500[i] <- mean(sample(hours, sample_size[4]))
  }
}

# Plotting histograms for above 4 samples
par(mfrow=c(2,2))

hist(hours_sample_5, xlab = 'Hours spent', ylab = 'Frequency', col = 'pink',
     main = 'Sample size = 5')

hist(hours_sample_50, xlab = 'Hours spent', ylab = 'Frequency', col = 'light green',
     main = 'Sample size = 50')

hist(hours_sample_100, xlab = 'Hours spent', ylab = 'Frequency', col = 'light blue',
     main = 'Sample size = 100')

hist(hours_sample_500, xlab = 'Hours spent', ylab = 'Frequency', col = 'orange', 
     main = 'Sample size = 500')

par(mfrow=c(1,1))

# calculating mean and sd for all samples
sd(hours)/sqrt(c(5,50,100,500))    # sd of samples

sample_5_mean   <- mean(hours_sample_5)
sample_50_mean  <- mean(hours_sample_50)
sample_100_mean <- mean(hours_sample_100)
sample_500_mean <- mean(hours_sample_500)

hours_sample_mean <- c(sample_5_mean, sample_50_mean, sample_100_mean, sample_500_mean)

sample_5_sd   <- sd(hours_sample_5)
sample_50_sd  <- sd(hours_sample_50)
sample_100_sd <- sd(hours_sample_100)
sample_500_sd <- sd(hours_sample_500)


cat("\nPopulation mean: ", round(mean(adult$hours.per.week), digits = 2),
    "  sd: " ,round(sd(adult$hours.per.week),digits = 2),
    "\n\nSample size 5     mean: ", round(mean(hours_sample_5), digits = 2),
    "   sd: " ,round(sd(hours_sample_5),digits = 2), 
    "\nSample size 50    mean: ", round(mean(hours_sample_50), digits = 2),
    "  sd: " ,round(sd(hours_sample_50),digits = 2),
    "\nSample size 100   mean: ", round(mean(hours_sample_100), digits = 2),
    "  sd: " ,round(sd(hours_sample_100),digits = 2), 
    "\nSample size 500   mean: ", round(mean(hours_sample_500), digits = 2),
    "  sd: " ,round(sd(hours_sample_500),digits = 2),sep = '')


####################################################################
#                              Sampling                            #
####################################################################
library(sampling)

par(mfrow=c(2,2))

# Population Frequency proportion
freq_pop <- round( prop.table(table(adult$education.num))*100 , 2)

plot(freq_pop, xlab = 'Education Level', col = 'red', xlim = c(1,16), 
     ylab = 'Frequency', xaxt ='n',
     main = 'Population Freq Distribution', type = 'h')
abline(h=0)
axis(side= 1, at=0:50, labels = TRUE)


#################################################
#  Simple Random Sampling Without Replacement  
#################################################

# Taking a sample of 3000 
s1 <- srswor(3000, nrow(adult)) 

sample_simple <- adult[s1 !=0 , ]

# Sample frequency proportion
freq_sample <- round( prop.table(table(sample_simple$education.num))*100 , 2)

plot(freq_sample, xlab = 'Education Level', col = 'blue', xlim = c(1,16), 
     ylab = 'Frequency', 
     main = 'Simple Random Sampling w/o replacement', type = 'h')
abline(h=0)
axis(side= 1, at=0:50, labels = TRUE)


####################################
#     Systematic Sampling          
####################################

pik <- inclusionprobabilities(adult$education.num, 3000)
length(pik)
sum(pik)

s2 <- UPsystematic(pik)

sample_syst <- adult[s2 != 0, ]

# Sample frequency proportion
freq_syst <- round( prop.table(table(sample_syst$education.num))*100 , 2)

plot(freq_syst, xlab = 'Education Level', col = 'green', xlim = c(1,16), 
     ylab = 'Frequency', 
     main = 'Systematic Sampling - Unequal Prob', type = 'h')
abline(h=0)
axis(side= 1, at=0:50, labels = TRUE)


####################################
#     Stratified Sampling
#####################################

# Order our data based on education.num column
i <- order(adult$education)

data <- adult[i,]

freq <- table(data$education)

st_sizes <- as.vector(round(3000 * freq / sum(freq)))

st <- strata(data, stratanames = c('education'),
             size = st_sizes, method = 'srswor', description = TRUE)

sample_strata <- getdata(data, st)
head(sample_strata)

# Sample frequency proportion
freq_strata <- round( prop.table(table(sample_strata$education.num))*100 , 2)

plot(freq_strata, xlab = 'Education Level', col = 'orange', xlim = c(1,16), 
     ylab = 'Frequency', 
     main = 'Stratified Sampling', type = 'h')
abline(h=0)
axis(side= 1, at=0:50, labels = TRUE)


par(mfrow=c(1,1))


########################
# Sampling Conclusion
########################

table(adult$education.num)              # population distribution
table(sample_simple$education.num)
table(sample_syst$education.num)
table(sample_strata$education.num)

mean(adult$education.num)            # 10.12
mean(sample_simple$education.num)    # 10.10
mean(sample_syst$education.num)      # 10.73
mean(sample_strata$education.num)    # 10.12


sd(adult$education.num)              # 2.549
sd(sample_simple$education.num)      # 2.547
sd(sample_syst$education.num)        # 2.424
sd(sample_strata$education.num)      # 2.548


cat("\nSampling Results:
    \nPopulation               Mean: ", round(mean(adult$education.num), digits = 2),
    "  sd: " ,round(sd(adult$education.num),digits = 2),
    "\nSimple random sampling   Mean: ", round(mean(sample_simple$education.num), digits = 2),
    "  sd: " ,round(sd(sample_simple$education.num) ,digits = 2), 
    "\nSystematic Sampling      Mean: ", round(mean(sample_syst$education.num), digits = 2),
    "  sd: " ,round(sd(sample_syst$education.num),digits = 2),
    "\nStratified Sampling      Mean: ", round(mean(sample_strata$education.num), digits = 2),
    "  sd: " ,round(sd(sample_strata$education.num),2),sep = '')

# Stratified sampling gives a close approximation for our population.
# Hence, if we take a sample of population using stratified sampling, we can get a boundary close to our entire population and can predict population estimates.


#########################################
#           Confidence Interval         #
#########################################
# sample mean, population sd, sample size
# CI = sample_mean +/- (population sd / sqrt(sample size) )
# Calculating 90% and 95% Confidence Interval
# Unknown parameter population mean for hours per week  --> mean(adult$hours.per.week)  # 40.931


sample_size         # 5 50 100 500
hours_sample_mean
population_sd <- sd(adult$hours.per.week) 
population_mean <- mean(adult$hours.per.week)

conf95_lower_limit <- numeric(length(sample_size))
conf95_upper_limit <- numeric(length(sample_size))

# calculating upper & lower limit of confidence interval 90 and 95%
for (i in (1:length(sample_size))){
  conf95_lower_limit[i] <- hours_sample_mean[i] - 1.96 * (population_sd/sqrt(sample_size[i]))
  conf95_upper_limit[i] <- hours_sample_mean[i] + 1.96 * (population_sd/sqrt(sample_size[i]))
}

cat('\nPopulation Mean : ', population_mean, '\n',
    '\nSample size : ' , sample_size[1] , '\n',
    '95% Confidence Interval = ', conf95_lower_limit[1], '~' ,conf95_upper_limit[1],
    '\nSample size : ' , sample_size[2] , '\n',
    '95% Confidence Interval = ', conf95_lower_limit[2], '~' ,conf95_upper_limit[2],
    '\nSample size : ' , sample_size[3] , '\n',
    '95% Confidence Interval = ', conf95_lower_limit[3], '~' ,conf95_upper_limit[3],
    '\nSample size : ' , sample_size[4] , '\n',
    '95% Confidence Interval = ', conf95_lower_limit[4], '~' ,conf95_upper_limit[4])


#plotting Confidence interval
plot(conf95_lower_limit,sample_size,  type = 'l', col = 'black', xlim = c(0, max(conf95_upper_limit)),
     ylab = 'Sample Size', xlab = 'mean sample values',  main = 'Confidence Interval 95%')
lines(conf95_upper_limit, sample_size,  type = 'l', col = 'black')
lines(rep(population_mean, length(sample_size)), sample_size, type = 'l', col = 'red')
legend('topleft', legend = c('CI - upper & lower limits', 'Population mean'), pch = 15, col = c('black', 'red'))

