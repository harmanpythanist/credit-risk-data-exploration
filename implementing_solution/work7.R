# DATA EXPLORATORY ANALYSIS

data=read.csv("E:\\data.csv")
data[1, ]

#any missing data
any(is.na(data))

# give column names
colnames(data)

#checking for any null values per each col
colSums(is.na(data)) 

sum(is.na(as.numeric(data$duration)))

#checking if numeric cols contain any non numeric data
is.numeric(data$duration)
is.numeric(data$credit_amount)
is.numeric(data$age)

# Check for unique values in 'own_telephone'
unique(data$own_telephone)

# Check for unique values in 'foreign_worker'
unique(data$foreign_worker)

# Check the unique values in the 'purpose' column
unique(data$purpose)

# Check the unique values in 'savings_status'
unique(data$savings_status)

# summary of data
summary(data)



# Load pander package
library(pander)
library(ggplot2)
library(dplyr)

# Print summary in table format using pander
pander(summary(data))


# Categorical variables distribution
ggplot(data, aes(x = credit_history)) + geom_bar() + theme_minimal()


# Create a pie chart for purpose

purpose_count <- data %>% group_by(purpose) %>% summarise(count = n())

ggplot(purpose_count, aes(x = "", y = count, fill = purpose)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribution of Credit Purpose") +
  theme(legend.position = "right")



# Create a pie chart for personal_status

personal_status_count <- data %>% group_by(personal_status) %>% summarise(count = n())

ggplot(personal_status_count, aes(x = "", y = count, fill = personal_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribution of Personal Status") +
  theme(legend.position = "right")


# bar plot for property_magnitude

property_magnitude_count = data %>% group_by(property_magnitude) %>% summarise(count = n())

ggplot(property_magnitude_count, aes(x = property_magnitude, y = count, fill = property_magnitude)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Property Magnitude", x = "Property Magnitude", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")



# Create a pie chart for foreign_worker

foreign_worker_count = data %>% group_by(foreign_worker) %>% summarise(count = n())

ggplot(foreign_worker_count, aes(x = "", y = count, fill = foreign_worker)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribution of Foreign Workers") +
  theme(legend.position = "right")


# OBJECTIVES
 

#===============================================================================

# objective 1

# 1.1

class_distribution =table(data$class)

# Visualize using bar plot
barplot(class_distribution,
        main = "Distribution of Entries Across Each Class",
        xlab = "Class",
        ylab = "Number of Entries",
        col = "lightblue")


#---------------------
# 1.2


data = read.csv("E://data.csv")


tc_class = table(data$checking_status, data$class)
print(tc_class)


library(ggplot2)

ggplot(data, aes(x=checking_status, fill=class)) +
  geom_bar(position="fill") + 
  labs(title="Effect of Checking Account Status on Credit Class", 
       x="Checking Account Status", 
       y="Proportion of Credit Class") +
  theme_minimal()


#---------------------------
# 1.3

# Recode 'class' to binary (0 = bad, 1 = good)
data$class_binary = ifelse(data$class == "good", 1, 0)
data$class_binary 

# Create age groups for clearer visualization
data$age_group =cut(data$age, breaks=c(0, 30, 40, 50, 60, 100), 
                    labels=c("0-30", "31-40", "41-50", "51-60", "60+"))


ggplot(data, aes(x = age_group, y = duration, fill = class)) +
  geom_boxplot() +
  labs(title = "Loan Duration vs Age Group by Credit Class",
       x = "Age Group",
       y = "Loan Duration (months)") +
  theme_minimal()

library(ggplot2)

# Barplot: count of good/bad credit class by age group
ggplot(data, aes(x = age_group, fill = class)) +
  geom_bar(position = "dodge") + 
  labs(title = "Impact of Age on Credit Class",
       x = "Age Group",
       y = "Count of Customers") +
  theme_minimal()


summary_data <- data %>%
  group_by(housing) %>%
  summarise(mean_credit = mean(credit_amount, na.rm = TRUE))


ggplot(summary_data, aes(x = housing, y = mean_credit)) +
  geom_col(fill = "steelblue") + 
  labs(title = "Effect of Housing on Average Credited Amount",
       x = "Housing Type",
       y = "Average credit_Amount") +
  theme_minimal()




#===============================================================================
#OBJECTIVE 2


# 2.1

job_class_distribution = table(data$job, data$class)
print(job_class_distribution)
# Convert the frequency table to a data frame for easier plotting
job_class_df =as.data.frame(job_class_distribution)

# Create a bar plot for the distribution of jobs by class
library(ggplot2)

ggplot(job_class_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Customers Based on Job Categories",
       x = "Job Categories",
       y = "Number of Customers",
       fill = "Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


#----------------------------
# 2.2

# Load  libraries
library(ggplot2)
library(dplyr)

# Read the CSV file
data = read.csv("E://data.csv")


job_instalment = data %>%
  group_by(job) %>%
  summarise(mean_instalment = mean(installment_commitment))

print(job_instalment)

# Bar plot 
ggplot(job_instalment, aes(x = job, y = mean_instalment, fill = job)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean Installment Commitment by Job Type", 
       x = "Job Type", 
       y = "Mean Installment Commitment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------
# 2.3

library(ggplot2)

# Create a grouped bar plot
ggplot(df, aes(x = personal_status, fill = class)) +
  geom_bar(position = "dodge") +  
  labs(title = "Relationship Between Personal Status and Credit Class",
       x = "Personal Status",
       y = "Count",
       fill = "Credit Class") +
  theme_minimal() + 
  scale_fill_manual(values = c("bad" = "red", "good" = "blue"))  


# Create a grouped bar plot (working type and credit class)
ggplot(df, aes(x = foreign_worker, fill = class)) +
  geom_bar(position = "dodge") +  
  labs(title = "Relationship Between working type and Credit Class",
       x = "Foreign worker",
       y = "Count",
       fill = "Credit Class") +
  theme_minimal() + 
  scale_fill_manual(values = c("bad" = "red", "good" = "blue"))


#----------------------------
# 2.4

# Violin Plot for Num Dependents vs Credit Class

df = read.csv("E://data.csv")

ggplot(df, aes(x = class, y = num_dependents, fill = class)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Dependents by Credit Class", x = "Credit Class", y = "Number of Dependents") +
  theme_minimal()


# boxplot, class and existing credits relation
ggplot(df, aes(x = class, y = existing_credits, fill = class)) +
  geom_boxplot() +
  labs(title = "Boxplot of Existing Credits by Credit Class",
       x = "Credit Class",
       y = "Number of Existing Credits") +
  theme_minimal()



#=================================================================================================
# OBJECTIVE 3

# 3.1

library(dplyr)
library(ggplot2)


data = read.csv("E://data.csv")


# Group by loan purpose and calculate average credit amount
pc_analysis = data %>%
  group_by(purpose) %>%
  summarise(avg_credit_amount = mean(credit_amount, na.rm = TRUE)) %>%
  arrange(desc(avg_credit_amount))


print(pc_analysis)

# Creating a bar plot to visualize the impact of loan purpose on credit amount
ggplot(pc_analysis, aes(x = reorder(purpose, -avg_credit_amount), y = avg_credit_amount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Credit Amount by Loan Purpose",
       x = "Loan Purpose", 
       y = "Average Credit Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#---------------------------------
# 3.2

library(ggplot2)
library(dplyr)

# Load the csv file

data = read.csv("E://data.csv")


# Boxplot for loan duration vs class
ggplot(data, aes(x = class, y = duration, fill = class)) + 
  geom_boxplot() + 
  labs(title = "Loan Duration vs Credit Class", x = "Credit Class", y = "Loan Duration (months)")

# Boxplot for credit amount vs class
ggplot(data, aes(x = class, y = credit_amount, fill = class)) + 
  geom_boxplot() + 
  labs(title = "Credit Amount vs Credit Class", x = "Credit Class", y = "Credit Amount")

# Scatter plot for loan duration, credit amount, and credit history
ggplot(data, aes(x = duration, y = credit_amount, color = credit_history)) + 
  geom_point(alpha = 0.7) + 
  facet_wrap(~class) + 
  labs(title = "Loan Duration vs Credit Amount by Credit History and Class", 
       x = "Loan Duration (months)", y = "Credit Amount")



#---------------------------------------
# 3.3

data = read.csv("E://data.csv")

summary_data = data %>%
  group_by(housing) %>%
  summarise(mean_credit = mean(credit_amount, na.rm = TRUE))


ggplot(summary_data, aes(x = housing, y = mean_credit)) +
  geom_col(fill = "steelblue") + 
  labs(title = "Effect of Housing on Average Credited Amount",
       x = "Housing Type",
       y = "Average credit_Amount") +
  theme_minimal()



#============================================================================================

# OBJECTIVE 4

# 4.1

data = read.csv("E://data.csv")


tc_class = table(data$checking_status, data$class)
print(tc_class)


library(ggplot2)

ggplot(data, aes(x=checking_status, fill=class)) +
  geom_bar(position="fill") + 
  labs(title="Effect of Checking Account Status on Credit Class", 
       x="Checking Account Status", 
       y="Proportion of Credit Class") +
  theme_minimal()


#------------------------------------------------------------------------------------------

# 4.2

library(ggplot2)
library(dplyr)

# libraries
df = read.csv("E://data.csv")


# summarizing age group and class

age_summary = df %>% 
  group_by(class) %>% 
  summarise(mean_age = mean(age),
            median_age = median(age),
            min_age = min(age),
            max_age = max(age))

print(age_summary)



# histogram
ggplot(df, aes(x = age, fill = class)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Credit Class", x = "Age", y = "Count") +
  theme_minimal()


#--------------------------------------------------------------------------------

# 4.3

# Loading the necessary libraries
library(ggplot2)

# Creating a bar plot with 'foreign_worker' and 'class'
ggplot(df, aes(x = foreign_worker, fill = class)) +
  geom_bar(position = "fill") +  # Stacked bar chart with proportions
  labs(title = "Proportion of Good and Bad Credit Class by Foreign Worker Status", 
       x = "Foreign Worker Status", 
       y = "Distribution") +
  scale_fill_manual(values = c("brown", "purple"), 
                    labels = c("Bad Credit", "Good Credit")) +
  theme_minimal()



#=====================================================================================



