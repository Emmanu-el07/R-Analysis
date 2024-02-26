# Loading Libraries
library(tidyverse)
library(skimr)
library(lubridate)
library(markdown)
library(plotly)
library(rpart)
library(caTools)
library(randomForest)
library(caret)

# Importing the dataset
recruit <- read_csv("C:/Users/LOLADE/Desktop/Data Analysis Projects/recruitment_data.csv")
View(recruit)
head(recruit)
str(recruit)
table(factor(recruit$Status))
table(recruit$`Education Level`)
table(recruit$`Job Title`)
summary(recruit)
table(recruit$`Years of Experience`)

# Data Cleaning
anyNA(recruit) # There is no missing value in this dataset
skim_without_charts(recruit)
recruit %>% select(c(-1,-7)) -> recruit # Removing Applicant ID and Email
recruit$`Education Level` <- factor(recruit$`Education Level`, levels = c("Bachelor's Degree", "High School","Master's Degree","PhD"))
recruit$Status <- factor(recruit$Status, levels = c( "Applied","In Review","Interviewing","Offered","Rejected" ))

recruit$`Desired Salary` <- as.numeric(recruit$Desired_salary)
  recruit$`Years of Experience` <- as.numeric(recruit$Years_of_experience)
recruit$`Date of Birth` <- dmy(recruit$`Date of Birth`) # Working on the date of birth
names(recruit)[13] <- "Years_of_experience"
names(recruit)[14] <- "Desired_salary"
names(recruit)[12] <- "Education_Level"

# Data Vizualization
recruit %>% 
  filter(Status == "Applied" & `Education Level` == "PhD") %>% 
  plot_ly(x = ~`Education Level` == "Applied", y = ~Status == "PhD") %>% 
  add_bars(name = "Barchart") %>% 
  layout(
    title = "Education Level vs Applied",
    xaxis = list(
      title = "Education Level"
    ),
    yaxis = list(
      title = "Applied Participants"
  )
)

pairs(Status ~ `Years of Experience`+`Education Level`+ `Desired Salary`,data = recruit)

plot(recruit$`Desired Salary`~recruit$`Years of Experience`)

##########################################
################ Questions ###############
##########################################

# Total Number of Applicants by Group

recruit %>% 
  select(Status, Education_Level) %>% 
  group_by(Education_Level) %>% 
  summarize( Abundance = n())

######################
######## Phd #########
######################

#1. How many PhD Applicants do we have
recruit %>% 
  select(`Education Level`) %>% 
  filter(`Education Level` == "PhD") %>% 
  count() # Answer == 741 PhD Applicants

#2. How many PhD were Applied
A <- recruit %>% 
  select(Status,`Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "PhD" & Status == "Applied") %>% 
  count() # Answer == 151

#3. How many PhD were Offered
B <- recruit %>% 
  select(Status,`Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "PhD" & Status == "Offered") %>% 
  count() # Answer == 148

#4. How many PhD was rejected
C <- recruit %>% 
  select(Status,`Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "PhD" & Status == "Rejected") %>% 
  count() # Answer == 146

#5. How manu PhD is undergoing Interview
D <- recruit %>% 
  select(Status,`Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "PhD" & Status == "Interviewing") %>% 
  count() # Answer == 141

#6. How many PhD is under Review
E <- recruit %>% 
  select(Status,`Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "PhD" & Status == "In Review") %>% 
  count() # Answer == 155

############################
##### Barchelor's Degree #####
############################

#1. How Many Bachelor's Degree Applicants do we have
recruit %>% 
  select(`Education Level`) %>% 
  filter(`Education Level` == "Bachelor's Degree") %>% 
  count()   # Answer == 785
#2. How many Barchelor's Degree Applied
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Bachelor's Degree" & Status == "Applied") %>% 
  count() # Answer == 164

#3. How Many Barchelor's Degree has been offered
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Bachelor's Degree" & Status == "Offered") %>% 
  count() # Answer == 157

#4. How many Barchelor's Degree has been rejected
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Bachelor's Degree" & Status == "Rejected") %>% 
  count() # Answer == 159

#5.How many Barchelor's Degree is Undergoing Review
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Bachelor's Degree" & Status == "Interviewing") %>% 
  count() # Answer == 172

#6. How many Barchelor's Degree are still in Review
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Bachelor's Degree" & Status == "In Review") %>% 
  count() # Answer == 133

#######################
##### High School #####
#######################
#1. How Many High School Applicants do we have
recruit %>% 
  select(`Education Level`) %>% 
  filter(`Education Level` == "High School") %>% 
  count()   # Answer ==  738

#2. How many High School Applied
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "High School" & Status == "Applied") %>% 
  count() # Answer == 147

#3. How Many High School has been offered
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "High School" & Status == "Offered") %>% 
  count() # Answer == 141

#4. How many High School has been rejected
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "High School" & Status == "Rejected") %>% 
  count() # Answer == 152

#5.How many High School is Undergoing Review
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "High School" & Status == "Interviewing") %>% 
  count() # Answer == 140

#6. How many High School are still in Review
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "High School" & Status == "In Review") %>% 
  count() # Answer == 158

##########################
##### Master's Degree ####
##########################

#1. How Many Master's Degree Applicants do we have
recruit %>% 
  select(`Education Level`) %>% 
  filter(`Education Level` == "Master's Degree") %>% 
  count()   # Answer == 736

#2. How many Masters's Degree Applied
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Master's Degree" & Status == "Applied") %>% 
  count() # Answer == 149

#3. How Many Master's Degree has been offered
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Master's Degree" & Status == "Offered") %>% 
  count() # Answer == 164

#4. How many Master's Degree has been rejected
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Master's Degree" & Status == "Rejected") %>% 
  count() # Answer == 137

#5.How many Master's Degree is Undergoing Review
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Master's Degree" & Status == "Interviewing") %>% 
  count() # Answer == 137

#6. How many Master's Degree are still in Review
recruit %>% 
  select(Status, `Education Level`) %>% 
  group_by(`Education Level`) %>% 
  filter(`Education Level` == "Master's Degree" & Status == "In Review") %>% 
  count() # Answer == 149


#####################################################################
##### Model on Whether an Applicant would be Employed or not ########
#####################################################################
status_find <- recruit %>% 
  select(Status,Education_Level,Years_of_experience,Desired_salary) %>% 
  filter(Status == c("Offered","Rejected"))
status_find$Status <- factor(status_find$Status, levels = c("Offered","Rejected")) # Use this to change the levels

#Splitting the data
split <- sample(2,nrow(status_find), prob = c(0.75,0.25), replace = TRUE)
train_status <- status_find[split == 1,]
test_status <- status_find[split == 2,]

#Creating a random Forest model
status_model <- randomForest(Status~.,data = train_status,mtry = 2)

#Creating Decision Tree Model 
model_status_tree <- rpart(Status~.,data = train_status)

# Validating the model for RandomForest
bestmty <- tuneRF(train_status,train_status$Status,stepFactor = 1.2,improve = 0.1,trace = T,plot = T)
varImpPlot(status_model)
status_model$importance
varImpPlot(status_model)

# Prediction for the RandomForest model
status_pred <- predict(status_model,test_status,type = "class")
cbind(test_status,status_pred)
confusionMatrix(status_pred,test_status$Status) #Accuracy for RandomForest model = 0.5211 

#Prediction for the Decision Tree Model
status_pred1 <- predict(model_tree,test_status,type = "class")
confusionMatrix(status_pred1,test_status$Status) # Accuracy for Decision Tree model = 0.5141

# From my Prediction I can say the reason why the model accuracy for both RandomForest and Decision Tree,
# is because of the low independent variable and the importance of the independent variables doesn't really,
# add up to affecting the Status except the "Desired Salary" 
