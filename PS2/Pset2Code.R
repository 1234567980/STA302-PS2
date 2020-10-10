##### This file is just r code that was used for dev purposes ######
##### For full report, please see Pset2.Rmd ######

#Load Data
# load cesR package and labelled package
library(labelled)
library(dplyr)
library(ggplot2)
library(readxl)

# Read in Ontario Census Data
OntarioCensus <- read_excel("./OntarioCensus.xlsx")
names(OntarioCensus) <- c("Columns", "Age", "Male", "Female", "NA")

# Rename columns and clean up data to only include data rows that we want
keeps <- c("Columns", "Age", "Male", "Female")
OntarioCensus <- OntarioCensus[keeps]
OntarioCensus <- OntarioCensus[-c(1:26), ]
OntarioCensus <- OntarioCensus[-c(3, 102:105), ]
OntarioCensus <- OntarioCensus[ c(1:7, 8, 14, 20,26,32,38,44,50,56),]
OntarioCensus <- rbind(c("18 to 24 years old",sum(sapply(OntarioCensus[1:7,2], as.numeric)), sum(sapply(OntarioCensus[1:7,3], as.numeric)), sum(sapply(OntarioCensus[1:7,4], as.numeric))), OntarioCensus)
OntarioCensus <- OntarioCensus[-c(2:8), ]

# Calculate the gender percents
total <- sum(sapply(OntarioCensus[1:10,2], as.numeric))
percent_male <- sum(sapply(OntarioCensus[1:10,3], as.numeric)) / total
percent_female <- sum(sapply(OntarioCensus[1:10,4], as.numeric)) / total

# Combine age ranges to suit the answers available in our data
OntarioCensus <- rbind(c("25 to 34 years old",sum(sapply(OntarioCensus[2:3,2], as.numeric)), sum(sapply(OntarioCensus[2:3,3], as.numeric)), sum(sapply(OntarioCensus[2:3,4], as.numeric))), OntarioCensus)
OntarioCensus <- OntarioCensus[-c(3:4), ]
OntarioCensus <- rbind(c("35 to 44 years old",sum(sapply(OntarioCensus[3:4,2], as.numeric)), sum(sapply(OntarioCensus[3:4,3], as.numeric)), sum(sapply(OntarioCensus[3:4,4], as.numeric))), OntarioCensus)
OntarioCensus <- OntarioCensus[-c(4:5), ]
OntarioCensus <- rbind(c("45 to 54 years old",sum(sapply(OntarioCensus[4:5,2], as.numeric)), sum(sapply(OntarioCensus[4:5,3], as.numeric)), sum(sapply(OntarioCensus[4:5,4], as.numeric))), OntarioCensus)
OntarioCensus <- OntarioCensus[-c(5:6), ]
OntarioCensus <- rbind(c("55 years and older",sum(sapply(OntarioCensus[5:7,2], as.numeric)), sum(sapply(OntarioCensus[5:7,3], as.numeric)), sum(sapply(OntarioCensus[5:7,4], as.numeric))), OntarioCensus)
OntarioCensus <- OntarioCensus[-c(6:8), ]

# Add new column for the percentage of each age range
OntarioCensus <- OntarioCensus %>% mutate(Percent = as.numeric(Age) / total)

# Probability weights for survery Q5
# PC, NDP, LIB, GRN
who2018_percentage <- c(0.4063, 0.3369, 0.193, 0.0462, 0.0176)

#TODO: Discuss later? Not sure how to find the appropriate amount of survey takers
survey_size <- 3850

set.seed(123)

# Simulate random sampling from population with our survey (WITHOUT replacement)

my_data <- tibble(
  
  # Probability parameters were calculated above
  gender = sample(c('Male', 'Female'), survey_size, replace = TRUE, prob = c(percent_male, percent_female)),
  age_range = sample(c('18-24','25-34', '35-44' ,'45-54', '55+'),survey_size, replace = TRUE, prob = sapply(OntarioCensus[1:5, 5], as.numeric)),
  
  who2018 = sample(c('PC', 'NDP', 'LIB', 'GRN','Other'),survey_size, replace = TRUE, prob = who2018_percentage),
  
  # Survey Q6 - Sufficient public health response?
  # Source: Angus Reid - Provincial Leadership 
  # Too far: 18% | Not far enough: 30% | Just right: 50% | N/A: 2%
  q6 = sample(c('Too far', 'Not far enough', 'Just right'), survey_size, replace = TRUE, prob = c(0.18, 0.3, 0.5)),
  
  # Survey Q7 - How has covid19 affected your work?
  # Source: Angus Reid - working from home ->QA8 
  # Great: 24% | Okay: 61% | Awful: 12% | N/A: 3%
  q7 = sample(c('Great', 'Okay', 'Awful'), survey_size, replace =TRUE, prob = c(0.24, 0.61, 0.12)),
  
  # Survey Q8 - Household financial needs
  # Source: Angus Reid - Financial Circumstances -> Q9 
  # Great: 12% | Good: 63% | Bad: 19% | N/A: 6% 
  q8 = sample(c('Great', 'Good', 'Bad'), survey_size, replace = TRUE, prob = c(0.12, 0.63, 0.19)),
  
  # Survey Q9 - Mental Health
  # Source: Angus Reid - Covid Concerns + Mental Health -> QA4
  # Great: 9% | Good: 53% | Not Good: 33% | N/A: 0.05%
  q9 = sample(c('Great', 'Good', 'Bad'), survey_size,replace = TRUE, prob = c(0.09, 0.53, 0.33)),
  
  # Survey Q10 - Satisfaction of provincial gov't response?
  # Source: Angus Reid - Provincial Leadership 
  # Good Job: 74% | Bad Job: 23% | N/A: 3%
  q10 = sample(c('Good', 'Bad'), survey_size, replace = TRUE, prob = c(0.74, 0.23)),
  
  # survey Q11 
  # Source: Ontario 2018 election results
  q11 =  sample(c("PC", "NDP", 'LIB', 'GRN', 'Other'), survey_size, replace = TRUE, prob = c(0.45, 0.28, 0.22, 0.04, 0.01)),
  
  # Survey Q12
  # Yes: 20.9% | No: 74.1% | Others: 5%
  q12 = sample(c('Yes', 'No'), survey_size, replace = TRUE, prob = c(0.209, 0.741)),
)

# Preview of data
head(my_data)


# Get a copy the simulate survey results, except re-categorize who they voted for into just PC and not PC
not_PC <- my_data

for (i in 1:dim(not_PC)[1]){
  if (not_PC$who2018[i]!="PC" ){
    not_PC$who2018[i]<- "Not PC"
  }
  
  if (not_PC$q11[i] != "PC"){
    not_PC$q11[i]<- "Not PC"
  }
}

# Calculate the percentage of people who changed from voting for PC to not voting to PC and being negatively affected by Covid19 
# OR from not voting for PC to voting for PC due having a positive experience since during the Covid19 pandemc
num_changed_corona_experience <- 0
for (i in 1:dim(not_PC)[1]){
  if (not_PC$who2018[i]!=not_PC$q11[i]){
    
    if ((not_PC$who2018[i] == 'PC' && (not_PC$q7[i] == 'Awful' || not_PC$q8[i] == 'Bad' || not_PC$q9[i] == 'Bad')) ||
        (not_PC$who2018[i] == 'Not PC' && (not_PC$q7[i] == 'Great' || not_PC$q8[i] == 'Great' || not_PC$q9[i] == 'Great'))
    ){
      num_changed_corona_experience  <- num_changed_corona_experience  + 1;
    }
    
  }
}
# NOTE: This percent was used to simulate survey responses as we added it manually as numbers for the prob parameters
# Since we used my_data to come up with this statistic
percent_changed_corona_experience <- num_changed_corona_experience / nrow(not_PC)

# Get count and percentage of people who voted for PC
voted_PC <- not_PC %>% group_by(who2018) %>% 
  count() %>% mutate(Percent = n / survey_size)

# Get count and percentage of people who did not voted for PC
will_vote_PC <- not_PC %>% group_by(q11) %>% 
  count() %>% mutate(Percent = n / survey_size)

# Combine previous 2 data frames into one for graphing
comparisons_PC <- data.frame("Labels" = c("Voted for PC (2018)", "Will vote for PC"), "Percentage" = c(as.numeric(voted_PC[2,3]), as.numeric(will_vote_PC[2,3])))


###### DIAGRAMS ###########
# Bar plot of the answer distribution of q6
#Scale fill manual not matching the name as we don't want the color to show on the side, but still manually fill the color
ggplot(my_data,aes(x = factor(q6, level = c("Not far enough", "Just right", "Too far", "N/A")), fill=q6, label = scales::percent(prop.table(stat(count))))) +
  geom_bar()+
  scale_x_discrete("" ,labels = c("Just right" = "Appropriate", "Not far enough" = "Insufficient" ,
                                  "Too far"= "Too Extreme", "N/A"="Prefer Not To Say"))+
  scale_y_continuous(limits = c(0,2200))+
  scale_fill_manual("legend",breaks = c("A", "B", "C", "D"), values =
                      c("Just right" = "dodgerblue1", "Not far enough" = "dodgerblue4", "Too far"="skyblue1", "N/A"="grey"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "Opinion on the public health response to \n the current coronavirus pandemic in Ontario", tag = "(1)")+
  theme(plot.title = element_text(lineheight=1, face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  theme(axis.text.x= element_text(colour = "black", size = 12))

# Bar plot of the answer distribution of q7
ggplot(my_data,aes(x = factor(q7, level = c("Awful", "Okay", "Great")), fill=q7, label = scales::percent(prop.table(stat(count))))) +
  geom_bar()+
  scale_x_discrete("" ,labels = c("Awful" = "Impaired My Work", "Okay" = "No Change in My Work" ,
                                  "Great"= "Improved My Work"))+
  scale_y_continuous(limits = c(0,3000))+
  scale_fill_manual(breaks = c("Impaired My Work", "No Change in My Work", "Improved My Work"),values =
                      c("Awful" = "dodgerblue4", "Okay" = "dodgerblue1", "Great"="skyblue1"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "How has the coronavirus pandemic affected your work", tag = "(2)")+
  theme(plot.title = element_text(lineheight=1, face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  theme(axis.text.x= element_text(colour = "black", size = 12))


# Bar plot of the answer distribution of q8
ggplot(my_data,aes(x = factor(q8, level = c("Bad", "Good", "Great")), fill=q8, label = scales::percent(prop.table(stat(count))))) +
  geom_bar()+
  scale_x_discrete("" ,labels = c("Bad" = "More Difficult", "Good" = "Neither More Difficult Nor Easier" ,
                                  "Great"= "Easier"))+
  scale_y_continuous(limits = c(0,2700))+
  scale_fill_manual(breaks = c("More Difficult", "Neither More Difficult Nor Easier", "Easier"),values =
                      c("Bad" = "dodgerblue4", "Good" = "dodgerblue1", "Great"="skyblue1"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "Since the coronavirus pandemic, \n how has it been for you and your household to meet your financial needs", tag = "(3)")+
  theme(plot.title = element_text(lineheight=1, face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  theme(axis.text.x= element_text(colour = "black", size = 12))

# Bar plot of the answer distribution of q9
ggplot(my_data,aes(x = factor(q9, level = c("Bad", "Good", "Great")), fill=q9, label = scales::percent(prop.table(stat(count))))) +
  geom_bar()+
  scale_x_discrete("" ,labels = c("Bad" = "Worse", "Good" = "About the same" ,
                                  "Great"= "Better"))+
  scale_y_continuous(limits = c(0,3500))+
  scale_y_continuous(limits = c(0,2500))+
  scale_fill_manual(breaks = c("Worse", "About the same", "Better"),values =
                      c("Bad" = "dodgerblue4", "Good" = "dodgerblue1", "Great"="skyblue1"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "Compared to before the coronavirus pandemic,\n how would you rate your mental health now", tag = "(4)")+
  theme(plot.title = element_text( face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  theme(axis.text.x= element_text(colour = "black", size = 12))


# Bar plot of the answer distribution of q10
ggplot(my_data,aes(x = factor(q10, level = c("Bad", "Good")), fill=q10, label = scales::percent(prop.table(stat(count))))) +
  geom_bar(width = 0.6)+
  scale_x_discrete("" ,labels = c("Bad" = "Dissatisfied", "Good" = "Satisfied"))+
  scale_y_continuous(limits = c(0,3500))+
  scale_fill_manual(breaks = c("Dissatisfied", "Satisfied"),values =
                      c("Bad" = "dodgerblue4", "Good"="skyblue1"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "How satisfied are you with the provincial government's \n response to the COVID-19 pandemic", tag = "(5)")+
  theme(plot.title = element_text( lineheight=1, face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  theme(axis.text.x= element_text(colour = "black", size = 12))

# Bar plot of the answer distribution of q11
ggplot(my_data,aes(x = factor(q11, level = c("PC", "NDP", "LIB", "GRN", "Other")), fill=q11, label = scales::percent(prop.table(stat(count))))) +
  geom_bar()+
  scale_x_discrete("" ,labels = c("PC" = "Progressive \n Conservative \n Party", "NDP" = "New Democratic \n Party", "LIB"= "Liberal Party", "GRN"="Green Party", "Other"="Other"))+
  scale_y_continuous(limits = c(0,2000))+
  scale_fill_manual(breaks = c("A", "B", "C", "D","E"), values = c("PC" = "dodgerblue1", "NDP" = "orange", "LIB"= "red", "GRN"="limegreen", "Other"="grey"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "Vote Intention in Ontario(Decided Voters)", tag = "(6)")+
  theme(plot.title = element_text(lineheight=1, face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks =element_blank())+
  theme(axis.text.x = element_text(color="black", 
                                   size=12))



# Bar plot of a comparison between the people who voted PC in 2018 and the people who would vote for PC now
ggplot(comparisons_PC, aes(x = Labels, y = Percentage, fill = Labels  , label = paste(round(Percentage*100, 1),"%",sep=""))) + 
  geom_bar(width = 0.6, stat = "identity") +
  geom_text(position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) +
  scale_x_discrete("", labels = c("Voted for Conservatives \n in 2018 Election", "Would vote for Conservatives \n today(Decided voters)")) +
  coord_cartesian(ylim = c(0,0.6))+
  scale_fill_manual(breaks = c("Voted for Conservatives in 2018 Election", "Would vote for Conservatives today(Decided voters"), values = c("Voted for PC (2018)" = "dodgerblue4", "Will vote for PC"="skyblue1"))+
  labs(title = "Shares of Votes in 2018 Provincial Election vs. \n Decided Voters Today", tag = "(7)")+
  theme(plot.title = element_text(hjust = 0.5 , lineheight=1, face="bold", colour = "black"))+
  theme_light()+
  theme(axis.text.x= element_text(colour = "black", size = 12))+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())


# Bar plot of the answer distribution of q12
ggplot(my_data,aes(x = factor(q12, level = c('Yes', 'No')), fill=q12, label = scales::percent(prop.table(stat(count))))) +
  geom_bar(width = 0.6)+
  scale_x_discrete("" ,labels = c('Yes' = "Influenced by \n government response", 'No' = "Not influenced \n by government response"))+
  coord_cartesian(ylim=c(0,3500))+
  scale_fill_manual(breaks = c("Yes", "No"),values =
                      c('Yes' = "dodgerblue1", 'No' = "dodgerblue4"))+ 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4)+
  labs(title = "Is your vote influenced by the government's response \n to the COVID 19 pandemic", tag = "(8)")+
  theme(plot.title = element_text(lineheight=1, face="bold"))+
  theme_light()+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())+
  theme(axis.text.x= element_text(colour = "black", size = 12))

