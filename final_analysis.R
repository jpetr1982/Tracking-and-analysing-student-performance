# List the names of the packages you want to install
package_names <- c("caTools", "naivebayes", "gridExtra", "ggplot2", "caret", "e1071", "dbscan", "factoextra", "readxl","cluster", "dplyr", "ggplot2", "tidyr", "readr", "stringr")

# Install the packages
install.packages(package_names)
#Load the needed libraries
library(readxl)
library(dplyr)
library(cluster)    
library(factoextra) 
library(dbscan)     
library(e1071)      
library(caret)      
library(ggplot2)
library(gridExtra)  
library(naivebayes)
library(caTools)


#Open the excel database, change the type of some variables and skip the blank columns
file.choose()
grades <- read_excel("C:/Users/jpetr/Desktop/DAMA/homeworks/hw dama51/Dama51 extra hw/Tracking and analysing student progress/grades.xlsx", 
                     col_types = c("numeric", "numeric", "skip", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "skip", "skip", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "skip", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric"))
str(grades)

#Remove the 1st column and change the name of the columns that consist of the final grade
grades_2 <- grades[-1,]
names(grades_2)[1]  <- "Exams Final"
names(grades_2)[2]  <- "Exams Repeat"

#Create a dataframe that consists the students that did not participated in the final exams
cols_to_check <- c("Exams Final", "Exams Repeat")
students_with_no_final_grade <- grades_2[apply(grades_2[cols_to_check], 1, function(row) all(row %in% c(-1, NA))), ]

#compute the mean of assignments and activities and plot the histograms to investigate their anual participation in the lesson
students_with_no_final_grade <- students_with_no_final_grade %>%
  mutate(across(everything(), ~ ifelse(. == -1 | is.na(.), 0, .)))
students_with_no_final_grade <- students_with_no_final_grade %>%
  mutate(mean_hw_as = round(rowMeans(select(., 3:6), na.rm = TRUE),3),
         mean_comp_act = round(rowMeans(select(., 7:14), na.rm = TRUE),3),
         mean_opt_act = round(rowMeans(select(., 15:24), na.rm = TRUE),3)
  )
new_no_final_grade <- students_with_no_final_grade[,-c(3:24)]
View(new_no_final_grade)
hist(new_no_final_grade$mean_hw_as,
     main = "Histogram of Mean Homework Assignment",
     xlab = "Mean Homework Scores",
     ylab = "Frequency",
     col = "blue")
hist(new_no_final_grade$mean_comp_act,
     main = "Histogram of Mean compulsory activities",
     xlab = "Mean Homework Scores",
     ylab = "Frequency",
     col = "green")
hist(new_no_final_grade$mean_opt_act,
     main = "Histogram of Mean optional activities",
     xlab = "Mean Homework Scores",
     ylab = "Frequency",
     col = "red")


#Remove the previous students and start dealing with those who participated in the final exams
grades_participation <- grades_2[!( (grades_2[,1] == -1 | is.na(grades_2[,1])) & (grades_2[,2] == -1 | is.na(grades_2[,2])) ), ]

#Compute the means of assignments and activities
grades_participation <- grades_participation %>%
  mutate(mean_hw_as = round(rowMeans(select(., 3:6), na.rm = TRUE),3),
         mean_comp_act = round(rowMeans(select(., 7:14), na.rm = TRUE),3),
         mean_opt_act = round(rowMeans(select(., 15:24), na.rm = TRUE),3)
  )
new_grades <- grades_participation[,-c(3:24)]

#Find and remove the outliers 
outliers <- new_grades %>%
  summarise(across(everything(), ~ list(boxplot.stats(.)$out)))
print(outliers)
boxplot(new_grades, main = "Boxplot of Data with Outliers", col = "lightblue")
is_outlier <- function(x) {
  outliers <- boxplot.stats(x)$out
  x %in% outliers
}
clean_new_grades <- new_grades %>%
  filter(if_all(everything(), ~ !is_outlier(.)))
clean_new_grades$final_grade <- pmax(clean_new_grades$`Exams Final`,
                                     clean_new_grades$`Exams Repeat`, na.rm = TRUE)
clean_new_grades <- clean_new_grades %>%
  mutate(across(everything(), ~ ifelse(is.nan(.)| is.na(.), 0, .)))
clean_new_grades <- clean_new_grades[, -c(1,2)]
View(clean_new_grades)

#Created a label column for the final result
clean_new_grades_final <- clean_new_grades %>%
  mutate(exams_result = ifelse(final_grade >= 5, "Pass", "Fail"))
View(clean_new_grades_final)

#Created a pie to see the pass percentage
result_counts <- clean_new_grades_final %>%
  count(exams_result)%>%
  mutate(percentage = round(n / sum(n) * 100, 1))  


ggplot(result_counts, aes(x = "", y = n, fill = exams_result)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +  
  labs(title = "Pass/Fail Distribution") +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +  
  scale_fill_manual(values = c("Pass" = "green", "Fail" = "red"))

ggplot(clean_new_grades, aes(x = final_grade)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Exam Grades",
       x = "Grades",
       y = "Frequency") +
  theme_minimal()

#Created two new label columns for the final grade and the mean of assignments
clean_new_grades_final_cat <- clean_new_grades_final %>%
  mutate(grade_category = case_when(
    final_grade < 5 ~ "Fail",
    final_grade >= 5 & final_grade < 6.5 ~ "Good",
    final_grade >= 6.5 & final_grade < 8.5 ~ "Very Good",
    final_grade >= 8.5 & final_grade <= 10 ~ "Excellent"
  ))

clean_new_grades_final_cat <- clean_new_grades_final_cat %>%
  mutate(hw_as_category = case_when(
    mean_hw_as < 5 ~ "Low",
    mean_hw_as >= 5 & mean_hw_as < 6.5 ~ "Medium",
    mean_hw_as >= 6.5 & mean_hw_as < 8.5 ~ "High",
    mean_hw_as >= 8.5 & mean_hw_as <= 10 ~ "Top"
  ))

#compute the covariance between the variables
scaled_new_grades <- scale(new_grades)
mean<- round(apply(scaled_new_grades, 2, mean, na.rm = TRUE),3)
s_d<- round(apply(scaled_new_grades, 2, sd, na.rm = TRUE),3)
cov_matrix <- round(cov(scaled_new_grades, use = "complete.obs"),3)
print(cov_matrix)

scaled_clean_new_grades <- scale(clean_new_grades)
mean2<- round(apply(scaled_clean_new_grades, 2, mean, na.rm = TRUE),3)
s_d2<- round(apply(scaled_clean_new_grades, 2, sd, na.rm = TRUE),3)
cov_matrix2 <- round(cov(scaled_clean_new_grades, use = "complete.obs"),3)
print(cov_matrix2)

#conduct a PCA analysis
ev <- eigen(cov_matrix)
round(ev$values,3)
round(ev$vectors,3)
new_grades_scales <- scale(new_grades)
new_grades_scales[is.na(new_grades_scales)] <- 0
pca_result <- prcomp(new_grades_scales)
variance_explained <- round((pca_result$sdev^2 / sum(pca_result$sdev^2)),3)
print(pca_result)
print(variance_explained)
cumulative_variance <- cumsum(variance_explained)
print(cumulative_variance)

plot(variance_explained, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b", pch = 19, main = "Scree Plot")
plot(cumulative_variance, xlab = "Principal Component", ylab = "Proportion of Cumulative Variance Explained", type = "b", pch = 19, main = "Cumulative Plot")

warnings()

#Create a Linear model
Final_Exams1 <- new_grades$'Exams Final'
Homework_assignments1 <- new_grades$'mean_hw_as'
# Build the linear model
l_m <- lm(Homework_assignments1 ~ Final_Exams1)

# Extract slope and intercept
slope <- coef(l_m)[2]
intercept <- coef(l_m)[1]

# Predict values
predicted <- predict(l_m, data.frame(Homework_assignments1 = Homework_assignments1))

# Calculate Mean Squared Error (MSE)
mse <- mean((Final_Exams1 - predicted)^2)

# Print the results
cat("Slope:", round(slope, 3), "\n")
cat("Intercept:", round(intercept, 3), "\n")
cat("Mean Squared Error (MSE):", round(mse, 3), "\n")

# Plot the data and the linear model
plot(new_grades$'Exams Final', new_grades$'mean_hw_as', 
     main = "Grade",
     xlab = "Exams Final", ylab = "Homework_assignments",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lty = 2)

clean_Final_Exams <- clean_new_grades$'final_grade'
clean_Homework_assignments <- clean_new_grades$'mean_hw_as'
# Build the linear model
l_m <- lm(clean_Homework_assignments ~ clean_Final_Exams)

# Extract slope and intercept
slope <- coef(l_m)[2]
intercept <- coef(l_m)[1]

# Predict values
predicted <- predict(l_m, data.frame(clean_Homework_assignments = clean_Homework_assignments))

# Calculate Mean Squared Error (MSE)
mse <- mean((clean_Final_Exams - predicted)^2)

# Print the results
cat("Slope:", round(slope, 3), "\n")
cat("Intercept:", round(intercept, 3), "\n")
cat("Mean Squared Error (MSE):", round(mse, 3), "\n")

# Plot the data and the linear model
plot(clean_new_grades$'final_grade', clean_new_grades$'mean_hw_as', 
     main = "Grade",
     xlab = "Exams Final", ylab = "Homework_assignments",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lty = 2)

#Conduct hypothesis test for the exams final and the rest of the variables(assignments and activities)
x1 <- new_grades$'Exams Final'
x2 <- new_grades$'mean_hw_as'

n1 <- length(x1)  # Sample size of Column1
n2 <- length(x2)  # Sample size of Column2

mean1 <- mean(x1)  # Mean of Column1
mean2 <- mean(x2)  # Mean of Column2
sd1 <- sd(x1)
sd2 <- sd(x2)

z_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
correlation_test_result <- cor.test(new_grades$'Exams Final', new_grades$'mean_hw_as', method = "pearson")
p_value <- correlation_test_result$p.value
cat("Z-Statistic:", z_stat, "\n")
cat("P-Value:", p_value, "\n")
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis: Significant difference between means.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant difference between means.\n")
}


filtered_x3 <- is.na(new_grades$'mean_comp_act')
n3 <- length(filtered_x3)  # Sample size of Column3
mean3 <- mean(filtered_x3)  # Mean of Column3
sd3 <- sd(filtered_x3)
z_stat_2 <- (mean1 - mean3) / sqrt((sd1^2 / n1) + (sd3^2 / n3))
correlation_test_result2 <- cor.test(new_grades$'Exams Final', new_grades$'mean_comp_act', method = "pearson")
p_value_2 <- correlation_test_result2$p.value
cat("Z-Statistic:", z_stat_2, "\n")
cat("P-Value:", p_value_2, "\n")
alpha <- 0.05
if (p_value_2 < alpha) {
  cat("Reject the null hypothesis: Significant difference between means.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant difference between means.\n")
}

filtered_x4 <- is.na(new_grades$'mean_opt_act')
n4 <- length(filtered_x4)  # Sample size of Column4
mean4 <- mean(filtered_x4)
sd4 <- sd(filtered_x4)
z_stat_3 <- (mean1 - mean4) / sqrt((sd1^2 / n1) + (sd4^2 / n4))
correlation_test_result3 <- cor.test(new_grades$'Exams Final', new_grades$'mean_opt_act', method = "pearson")
p_value_3 <- correlation_test_result3$p.value
cat("Z-Statistic:", z_stat_3, "\n")
cat("P-Value:", p_value_3, "\n")
alpha <- 0.05
if (p_value_3 < alpha) {
  cat("Reject the null hypothesis: Significant difference between means.\n")
} else {
  cat("Fail to reject the null hypothesis: No significant difference between means.\n")
}

#Find the correlation among the variables
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

scaled_data <- as.data.frame(lapply(clean_new_grades, min_max_normalize))
cor_matrix <- cor(scaled_data)
View(cor_matrix)
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        scale = "none",    
        margins = c(5, 5),
        main = "Correlation Heatmap") 
legend("topright", 
       legend = c("-1", "0", "1"),
       fill = colorRampPalette(c("blue", "white", "red"))(3),
       title = "Correlation",
       cex = 0.8)

#Conduct 2 clustering methods
set.seed(123)  # For reproducibility

# Perform K-Means clustering
kmeans_result <- kmeans(clean_new_grades, centers = 3, nstart = 25)
fviz_cluster(kmeans_result, data = clean_new_grades, 
             geom = "point",
             ellipse.type = "convex", # Convex hull around clusters
             main = "K-Means Clustering",
             palette = "jco", 
             ggtheme = theme_minimal())

dist_matrix <- dist(clean_new_grades, method = "euclidean")
hc_complete <- hclust(dist_matrix, method = "complete")
k <- 2
hc_clusters <- cutree(hc_complete, k = k)
cluster_data <- list(data = clean_new_grades, cluster = hc_clusters)
fviz_cluster(cluster_data, 
             geom = "point", 
             ellipse.type = "convex", 
             main = "Hierarchical Clustering (Complete Linkage)",
             palette = "jco", 
             ggtheme = theme_minimal())

hist(clean_new_grades$final_grade,
    main = "Distribution",
    xlab = "Grades",
    col = "red",
    border = "blue")

#Conduct naive Bayes prediction models
clean_new_grades_final_cat$grade_category <- as.factor(clean_new_grades_final_cat$grade_category)
clean_new_grades_final_cat$hw_as_category <- as.factor(clean_new_grades_final_cat$hw_as_category)
View(clean_new_grades_final_cat)

set.seed(101)

classifier <- naive_bayes(grade_category ~ mean_hw_as, data = clean_new_grades_final_cat, laplace = 1)

test1 <- data.frame(mean_hw_as = seq(5, 10, by = 0.1))
prediction <- predict(classifier, test1, type="prob")
test1$predicted_final_exam <- predict(classifier, newdata = test1)
result <- data.frame(mean_hw_as = test1$mean_hw_as,
                     predicted_final_exam = prediction)
View(round(result,3))

ggplot(test1, aes(x = mean_hw_as, y = predicted_final_exam, color = predicted_final_exam)) +
  geom_point(size = 4) +  # Scatter points
  labs(title = "Predicted Final Exam Results Based on Assignment Grades",
       x = "Assignment Grade",
       y = "Predicted Final Exam Result") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +  # Custom colors
  theme_minimal() +
  scale_y_discrete(drop = FALSE)

classifier2 <- naive_bayes(grade_category ~ hw_as_category, data = clean_new_grades_final_cat, laplace = 1)

test2 <- data.frame(hw_as_category = "Low")
prediction <- predict(classifier2, test2, type="prob")
test2$predicted_final_exam <- predict(classifier2, newdata = test2)
result <- data.frame(hw_as_category = test2$hw_as_category,
                     predicted_final_exam = prediction)
print(result)

test3 <- data.frame(hw_as_category = "Medium")
prediction <- predict(classifier2, test3, type="prob")
result <- data.frame(hw_as_category = test3$hw_as_category,
                     predicted_final_exam = prediction)
print(result)

test4 <- data.frame(hw_as_category = "High")
prediction <- predict(classifier2, test4, type="prob")
result <- data.frame(hw_as_category = test4$hw_as_category,
                     predicted_final_exam = prediction)
print(result)

#multiple linear regression
lm_model <- lm(final_grade ~ mean_hw_as + mean_opt_act + mean_comp_act, data = clean_new_grades_final)
summary(lm_model)

#svm model
df_svm <- clean_new_grades_final[, c("mean_hw_as", "mean_comp_act", "mean_opt_act", "exams_result")]

# Convert 'exams_result' to numeric (1 pass, -1 no pass)
df_svm$exams_result <- ifelse(df_svm$exams_result == "Pass", 1, -1) 
# Normalize features using Z-score
df_svm$mean_hw_as <- scale(df_svm$mean_hw_as)  # Standardize
df_svm$mean_comp_act <- scale(df_svm$mean_comp_act)
df_svm$mean_opt_act <- scale(df_svm$mean_opt_act)

# Set seed for reproducibility
set.seed(123)

# Split the data (70% for training, 30% for testing)
split <- sample.split(df_svm$exams_result, SplitRatio = 0.7)
train_data <- subset(df_svm, split == TRUE)
test_data <- subset(df_svm, split == FALSE)
# Train a linear SVM model
svm_model <- svm(df_svm$exams_result ~ df_svm$mean_hw_as + df_svm$mean_comp_act + df_svm$mean_opt_act, data = df_svm)

# Check the model summary
summary(svm_model)

# Predict the final exam outcome on the test data
predictions <- predict(svm_model, newdata = test_data)

# Display the predictions
print(predictions)


