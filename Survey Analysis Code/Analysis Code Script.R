# Before label mapping, remeber to calculate the  mean, median, mode, standard deviation, and variance. 



#Label Mapping for Survey Responses
Test_Data$Q1...1 <- factor(Test_Data$Q1...1, levels = c(1, 2, 3, 4, 5, 6, 7),
                           labels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 and above"))

Test_Data$Q2...2 <- factor(Test_Data$Q2...2, levels = c(1, 2, 3, 4),
                           labels = c("Male", "Female", "Non-binary", "Prefer not to say"))

Test_Data$Q3...3 <- factor(Test_Data$Q3...3, levels = c(1, 2, 3, 4, 5, 6, 7),
                           labels = c("No formal education", "Some High School", "High School Graduate", "Some College", "Bachelor's Degree", "Master's Degree", "Doctorate or higher" ))

Test_Data$Q4...4 <- factor(Test_Data$Q4...4, levels = c(1, 2, 3, 4, 5, 6),
                           labels = c("Employed Full-Time", "Employed Part-Time", "Unemployed", "Student", "Retired", "Homemaker" ))

Test_Data$Q5...5 <- factor(Test_Data$Q5...5, levels = c(1, 2, 3, 4, 5),
                           labels = c("Daily", "Weekly", "Monthly", "Rarely", "Never" ))

Test_Data$Q1...6 <- factor(Test_Data$Q1...6, levels = c(1, 2, 3, 4, 5),
                           labels = c("None at all", "A little", "A moderate amount", "A lot", "A great deal" ))

Test_Data$Q2...7 <- factor(Test_Data$Q2...7, levels = c(1, 2, 3, 4, 5),
                           labels = c("Extremely inadequate", "Somewhat inadequate", "Neither adequate nor inadequate", "Somewhat adequate", "Extremely adequate" ))

Test_Data$Q3...8 <- factor(Test_Data$Q3...8, levels = c(1, 2, 3, 4, 5),
                           labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q4...9 <- factor(Test_Data$Q4...9, levels = c(1, 2, 3, 4, 5),
                           labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q5...10 <- factor(Test_Data$Q5...10, levels = c(1, 2, 3, 4, 5),
                            labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q6 <- factor(Test_Data$Q6, levels = c(1, 2, 3, 4, 5),
                       labels = c("Extremely uncomfortable", "Somewhat uncomfortable", "Neither comfortable nor uncomfortable", "Somewhat comfortable", "Extremely comfortable" ))

Test_Data$Q7 <- factor(Test_Data$Q7, levels = c(1, 2, 3, 4, 5),
                       labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q8 <- factor(Test_Data$Q8, levels = c(1, 2, 3, 4, 5),
                       labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q9 <- factor(Test_Data$Q9, levels = c(1, 2, 3, 4, 5),
                       labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q10 <- factor(Test_Data$Q10, levels = c(1, 2, 3, 4, 5),
                        labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q11 <- factor(Test_Data$Q11, levels = c(1, 2, 3, 4, 5),
                        labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q12 <- factor(Test_Data$Q12, levels = c(1, 2, 3, 4, 5),
                        labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

Test_Data$Q13 <- factor(Test_Data$Q13, levels = c(1, 2, 3, 4, 5),
                        labels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree" ))

summary (Test_Data)



#To create frequency tables in R to show the counts (frequencies) and proportions (percentages) for each category of the survey questions.
# First Load necessary library
library(dplyr)

# Function to create frequency table similar to SPSS
create_frequency_table <- function(data, variable_name) {
  data %>%
    select(all_of(variable_name)) %>%
    table() %>%
    as.data.frame() %>%
    mutate(Frequency = Freq,
           Percent = prop.table(Freq) * 100,
           CumulativePercent = cumsum(Percent)) %>%
    select(-Freq)
}

# usage for a variable 'Q1...1'
frequency_table_Q1 <- create_frequency_table(Test_Data, "Q1...1")
print(frequency_table_Q1)

# Section 1: Demographic Question
create_frequency_table(Test_Data, "Q1...1")

create_frequency_table(Test_Data, "Q2...2")

create_frequency_table(Test_Data, "Q3...3")

create_frequency_table(Test_Data, "Q4...4")

create_frequency_table(Test_Data, "Q5...5")

# Section 2: Privacy Concerns
create_frequency_table(Test_Data, "Q1...6")

create_frequency_table(Test_Data, "Q2...7")

create_frequency_table(Test_Data, "Q3...8")

create_frequency_table(Test_Data, "Q4...9")


# Section 3: Trust Towards Online Service Providers
create_frequency_table(Test_Data, "Q5...10")

create_frequency_table(Test_Data, "Q6")

create_frequency_table(Test_Data, "Q7")

# Section 4: Awareness of Ethical Considerations
create_frequency_table(Test_Data, "Q8")

create_frequency_table(Test_Data, "Q9")

create_frequency_table(Test_Data, "Q10")


# Section 5: General Concerns and Perceptions
create_frequency_table(Test_Data, "Q11")

create_frequency_table(Test_Data, "Q12")

create_frequency_table(Test_Data, "Q13")

colnames(Test_Data)



# To Visualize each question first install the ggplot2 package
install.packages("ggplot2")

# Repeat for each question
library(forcats)
library(tidyr) 
library(dplyr)
library(ggplot2)

# A named vector to map column names to more descriptive titles
titles <- c(Q1...1 = "Distribution of Age", 
            Q2...2 = "Distribution of Gender", 
            Q3...3 = "Distribution of Education Level",
            Q4...4 = "Distribution of Occupation",
            Q5...5 = "Frequency of Online Platform Usage",
            Q1...6 = "Concerns about Personal Information Collection",
            Q2...7 = "Trust in Privacy Protection on Online Platforms",
            Q3...8 = "Worries about Personal Data Security",
            Q4...9 = "Understanding of Data Collection via Big Data Analytics",
            Q5...10 = "Trust in Data Handling by Online Service Providers",
            Q6 = "Comfort with Data Use in Big Data Analytics",
            Q7 = "Perception of Transparency in Big Data Analytics Use",
            Q8 = "Awareness of Ethical Issues in Big Data Analytics",
            Q9 = "Informed about Rights Regarding Data Collection",
            Q10 = "Knowledge on Reporting Unethical Use of Big Data Analytics",
            Q11 = "Beliefs about Benefits Justifying Privacy Concerns",
            Q12 = "Opinions on Limiting Big Data Analytics for Privacy Protection",
            Q13 = "Importance of Public Discussions on Big Data Analytics Privacy and Ethics")

# Function to create frequency tables and plot for a given column
create_plot <- function(data, column_name, title, factor_levels) {
  # Convert to factor and set levels to include all possible responses
  data[[column_name]] <- factor(data[[column_name]], levels = factor_levels)
  # Filter out NA values from the specific column in the dataset
  data_clean <- data %>% filter(!is.na(.data[[column_name]]))
  
  # Create a new table with frequencies and percentages for the cleaned data
  frequency_table <- data_clean %>%
    group_by(.data[[column_name]]) %>%
    summarise(Frequency = n()) %>%
    mutate(Percent = (Frequency / sum(Frequency)) * 100) %>%
    ungroup()  # Make sure to ungroup for ggplot
  
  # Plot the chart with ggplot2, including percentages on the bars
  p <- ggplot(frequency_table, aes(x = .data[[column_name]], y = Frequency, label = paste0(round(Percent, 1), "%"))) +
    geom_bar(stat = "identity") +  # Use identity to use the Frequency values directly
    geom_text(vjust = -0.5, size = 3.5) +  # Add text labels above the bars
    labs(title = title, x = title, y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
  
  # Return the plot
  return(p)
}

# Loop over each column and create a plot
plots <- lapply(names(titles), function(col) {
  create_plot(Test_Data, col, titles[col])
})

# Now you can view each plot individually
# For example, to view the first plot:
print(plots[[1]])

# To save the first plot to a file
ggsave("plot1.png", plots[[1]], width = 10, height = 8, dpi = 300)








