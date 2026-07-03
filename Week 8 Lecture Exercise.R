#=========================================================
# Week 8 - Data Manipulation & Descriptive Statistics
# Name : Khairunnisa' Binti Khairulamri
# ID   : 22008631
#=========================================================

#=========================================================
# Install & Load Packages
#=========================================================

install.packages("dplyr")
install.packages("readxl")

library(dplyr)
library(readxl)

#=========================================================
# Read Excel File
#=========================================================

student_data <- read_excel("C:/Users/USER/Downloads/student_data.xlsx")

View(student_data)

#=========================================================
# Data Manipulation
#=========================================================

# Students who failed (Final Exam < 40)
Student_fail <- student_data[student_data$final_exam_mark < 40, ]
View(Student_fail)

# Using dplyr - Descending order
mydata1 <- student_data %>%
  filter(final_exam_mark < 40) %>%
  arrange(desc(final_exam_mark))

View(mydata1)

# Using dplyr - Ascending order
mydata2 <- student_data %>%
  filter(final_exam_mark < 40) %>%
  arrange(final_exam_mark)

View(mydata2)

# Select specific columns
mydata3 <- student_data %>%
  select(student_id, coursework_mark, final_exam_mark)

View(mydata3)

# Create Total Mark column
mydata4 <- student_data %>%
  mutate(Total_Mark = coursework_mark + (final_exam_mark / 200 * 100))

View(mydata4)

# Display structure
glimpse(mydata4)

#=========================================================
# Descriptive Statistics
#=========================================================

data(iris)

View(iris)

head(iris)
tail(iris)

# Minimum value
min(iris$Sepal.Length)

# Maximum value
max(iris$Sepal.Length)

# Range
range(iris$Sepal.Length)

# Standard Deviation
sd(iris$Sepal.Length)

# Summary Statistics
summary(iris)

#=========================================================
# Quartile Example
#=========================================================

A <- c(
  170.2, 181.5, 188.9,
  163.9, 166.4, 163.7,
  160.4, 175.8, 181.5
)

# Quartiles
quantile(A)

# First Quartile (Q1)
quantile(A, 0.25)

# Third Quartile (Q3)
quantile(A, 0.75)

#=========================================================
# Count Rows, Columns & Missing Values
#=========================================================

# Number of rows
nrow(iris)

# Number of columns
ncol(iris)

# Count missing values
sum(is.na(iris$Sepal.Length))

#=========================================================
# Histogram
#=========================================================

hist(
  iris$Sepal.Length,
  main = "Histogram of Sepal Length",
  xlab = "Sepal Length (cm)",
  ylab = "Frequency",
  col = "lightblue",
  border = "black"
)

#=========================================================
# Boxplot & Outlier Detection
#=========================================================

data <- c(
  30, 24, 26, 28, 29, 28, 27, 26,
  32, 34, 13, 15, 14, 31, 29, 28,
  24, 25, 30, 34, 35, 27, 30, 34,
  44, 48
)

boxplot(
  data,
  main = "Boxplot",
  col = "lightgreen"
)

#=========================================================
# Quartiles & IQR
#=========================================================

first_q <- quantile(data, 0.25)
median_q <- quantile(data, 0.50)
third_q <- quantile(data, 0.75)

iqr <- IQR(data)

# Lower & Upper Limits
lower_limit <- first_q - (1.5 * iqr)
upper_limit <- third_q + (1.5 * iqr)

lower_limit
upper_limit

#=========================================================
# Remove Outliers
#=========================================================

data_remove <- data
data_remove <- data_remove[data_remove >= lower_limit]
data_remove <- data_remove[data_remove <= upper_limit]

data_remove

#=========================================================
# Replace Outliers with Mean
#=========================================================

data_mean <- data

avg <- round(mean(data_mean))

data_mean[data_mean < lower_limit] <- avg
data_mean[data_mean > upper_limit] <- avg

data_mean

#=========================================================
# Replace Outliers with Boundary Values
#=========================================================

data_boundary <- data

data_boundary[data_boundary < lower_limit] <- lower_limit
data_boundary[data_boundary > upper_limit] <- upper_limit

data_boundary

