
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# Your solution code goes here
# Load Libraries
library(ggplot2)
library(dplyr)
summary(ny) #Running a descriptive summary to understanding the data

#The summary showed that both Gender and User Type have blank cells which seem to be missing values.
#Plotting User type by Gender in New York using a side-by-side barchart
ggplot(ny, aes(x= Gender, fill = User.Type))+
  geom_bar(position = "dodge") +
  ylab('Number of riders')+
  ggtitle('Side by Side bar chart showing User type by Gender in NY')


# As the graph confirmed our findings above in regards to variables of interest
#Choice is made to consider the blank cells as NA's.
ny$Gender[ny$Gender==""] <-NA
ny$User.Type[ny$User.Type ==""] <-NA

#Let's plot gain the distribution of User type by gender without missing values (excluding NA's) to get a more clearer picture.
ny %>%
  filter(!is.na(Gender) & !is.na(User.Type)) %>%
  ggplot(aes(x = Gender, fill = User.Type))+
    geom_bar(position = "dodge") + 
    ylab('Number of riders') +
    ggtitle("Distribution of User type by Gender in NY without missing values")


#Running a compute a 2-way frequency table to represent the total counts of customers and subscribers according to their gender.
ny2 <-ny %>%
  filter(!is.na(Gender) & !is.na(User.Type))

table(ny2$Gender, ny2$User.Type)
#Performing a chi-square statistic test to establish whether Gender and user type are associated?
#H0: Gender and user type are independent
#H1: Gender and users type are related
chisq.test(ny2$Gender, ny2$User.Type, correct =FALSE)


# Your solution code goes here
# Let's first plot the Trip.Duration to see how their values are distributed
ggplot(ny,aes(x= Trip.Duration))+
  geom_histogram(binwidth = 300, color = 'black', fill = '#099DD9')+
  ylab('Number of riders')+
  ggtitle("Initial Histogram of Distribution of riders Trip Duration in NY")

#The presence of extreme values in the data is confirmed by the summary of the data. 
ggplot(ny, aes(x= Trip.Duration))+
  geom_histogram(binwidth = 300, color = 'black', fill = '#099DD9')+
  coord_cartesian(xlim=c(61,4000))+
  ylab('Number of riders')+
  ggtitle('Histogram of riders Trip Duration in NY with values between 61 and 4000')

#As extreme values are confirmed in our variable, lets replace them by NA's which be dealt with in later stage.
ny3 <-ny %>%
  mutate(Trip.Duration = ifelse(Trip.Duration > 5000,NA,Trip.Duration))

# To preserve all observations in Trip.Duration variable, let's now replace all NA'S the median value in the column.
median_trip <- median(ny3$Trip.Duration, na.rm = TRUE)
ny3$Trip.Duration[is.na(ny3$Trip.Duration)] <- median_trip

#Plot the Birth.Year variable to check the distribution

ggplot(ny3,aes(x= Birth.Year))+
  geom_histogram(binwidth = 20, color = 'black', fill = '#627F8B')+
  scale_x_continuous(limits = c(1885,2001)) +
  ylab('Number of riders') +
  ggtitle("Histogram of Birth Year of riders in NY")

# In the context of our data, we could assume that user who is more than 55 years is unlikely to ride a bike 
#so lets replace years before by NA's
ny4 <-ny3 %>%
  mutate(Birth.Year = ifelse(Birth.Year < 1962,NA,Birth.Year))

#Replacing the NA's by the median value of in Birth.Year column
median_Birth <- median(ny4$Birth.Year, na.rm = TRUE)
ny4$Birth.Year[is.na(ny4$Birth.Year)] <- median_Birth

qplot(x = Birth.Year, y = Trip.Duration, data = ny4 , size = I(0.5)) +
  geom_smooth( method = "lm", se = FALSE)+
  ggtitle ("Plot of relationship between Birth Year & Trip Duration of riders in NY")

# Your solution code goes here
# initial Plot of the distribution of birth year by gender in New York
qplot(x = Gender, y = Birth.Year, 
  data = subset(ny4, !is.na(Gender)), 
  geom = 'boxplot') + 
  ggtitle('Initial Distribution of Birth Year of riders by Gender in NY')


#Plotting the distribution of birth year by gender by (Birth year between 1962 and 1990)
qplot(x = Gender, y = Birth.Year, 
  data = subset(ny4, !is.na(Gender)), 
  geom = 'boxplot') + 
  coord_cartesian(ylim = c(1962,1990))+
  ggtitle('Distribution of Birth Year of riders by Gender in NY (Birth year between 1962 and 1990)')


#descriptive summary to compare distributions across the two groups
by(ny4$Birth.Year, ny4$Gender, summary)


system('python -m nbconvert Explore_bikeshare_data.ipynb')

