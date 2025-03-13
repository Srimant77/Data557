# Generate suitable descriptive statistics of the 
# 1. Distribution of academic field, 
# - Shown by generating a bar chart of sex vs academic field (Shows absolute number)
# - Pie chart shows percentage of men / women who choose a particular field

# 2. Year in which they attained their highest degree, 
# - Histogram shows this for absolute values
# - Density plot shows this for genders (as a percentage of that gender)

# 3. Year they were hired at the university, 
# - Time series chart shows how many men / women were hired at Uni each year (absolute values)
# - Density plot shows the total percentage of women and men hired in a given year

# 4. Academic rank in 1995 
# - Bar chart showing the total counts
# - Percentage of total women and total men in 1995 who hold a particular rank

# 5. monthly salary in 1995 by sex.
# - Boxplot showing the mean and the variance of the salaries by sex
# Time series chart showing how salary of men and women have changed over time

# Write a paragraph commenting on any differences you observe between men and women faculty in these measurements. 


# Load necessary libraries
library(dplyr)    # for data manipulation
library(ggplot2)  # for plotting

# Read in the dataset
salary_data <- read.table(file = "salary.txt", header = TRUE)

# Step 1: Arrange the data by 'id' and 'year'
# This ensures that for each individual (id), the records are in chronological order.
salary_data <- salary_data %>%
  arrange(id, year)

# Step 2: Group by 'id' and calculate the salary increment
# The 'mutate' function creates a new column 'salary_increment'
# which computes the difference between the current salary and the previous year's salary within each id group.
salary_data <- salary_data %>%
  group_by(id) %>%
  mutate(salary_increment = salary - lag(salary)) %>%
  ungroup()  # It's a good practice to ungroup after performing grouped operations

write.table(salary_data, file = "updated_salary.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# ---- The rest of your plotting code remains unchanged ----

print("=============================================================")
degree_counts_by_gender <- table(salary_data$sex, salary_data$deg)
print("Highest degree attained vs Sex")
print(degree_counts_by_gender)
print("=============================================================")
fields_of_work_and_gender <- table(salary_data$sex, salary_data$field)
print("Fields of Work vs Sex")
print(fields_of_work_and_gender)
print("=============================================================")
rank_and_gender <- table(salary_data$sex, salary_data$rank)
print("Rank attained vs Sex")
print(rank_and_gender)
print("=============================================================")
summary_of_year_of_highest_degree_by_sex <- aggregate(yrdeg ~ sex, data = salary_data, FUN = summary)
print("Summary of 'Year in Which highest degree was attained' vs Sex")
print(summary_of_year_of_highest_degree_by_sex)
print("=============================================================")
summary_of_year_of_hire_by_sex <- aggregate(yrdeg ~ sex, data = salary_data, FUN = summary)
print("Summary of Year of hire vs Sex")
print(summary_of_year_of_hire_by_sex)
print("=============================================================")
summary_of_salary_by_sex <- aggregate(yrdeg ~ sex, data = salary_data, FUN = summary)
print("Summary of Salary vs Sex")
print(summary_of_salary_by_sex)
print("=============================================================")

# Answering Part 1 --> Giving distribution of academic field by sex
# Also trying to see the degree attainments of people in different fields

p1 <- ggplot(salary_data, aes(x = deg, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Men and Women by Degree", 
       x = "Degree", 
       y = "Count") +
  theme_minimal()
print(p1)

p6 <- ggplot(salary_data, aes(x = deg, fill = field)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparing fields of work vs Degree of people in that field", 
       x = "Degree", 
       y = "Count") +
  theme_minimal()
print(p6)

p2 <- ggplot(salary_data, aes(x = field, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Fields of Work by Sex", 
       x = "Field", 
       y = "Count") +
  theme_minimal()
print(p2)

p3 <- ggplot(salary_data, aes(x = rank, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Rank Attained vs Sex", 
       x = "Rank", 
       y = "Count") +
  theme_minimal()
print(p3)

p4 <- ggplot(salary_data, aes(x = sex, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Sex", 
       x = "Sex", 
       y = "Salary") +
  theme_minimal()
print(p4)

p5 <- ggplot(salary_data, aes(x = yrdeg, fill = sex)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(title = "Distribution of Year of Highest Degree Attained by Sex", 
       x = "Year of Highest Degree", 
       y = "Count") +
  theme_minimal()
print(p5)

p7 <- ggplot(salary_data, aes(yrdeg))
plot_of_sex_vs_yrdeg <- p7 + geom_density(aes(fill = factor(sex)), alpha = 0.8) + 
  labs(title = "Sex vs Year when Highest Degree was attained (Density Plot)", 
       x = "yrdeg",
       fill = "sex")
print(plot_of_sex_vs_yrdeg)

p8 <- ggplot(salary_data, aes(startyr))
plot_of_sex_vs_joiningyear <- p8 + geom_density(aes(fill = factor(sex)), alpha = 0.8) + 
  labs(title = "Year of Joining University vs Sex", 
       x = "startyr",
       fill = "sex")
print(plot_of_sex_vs_joiningyear)
