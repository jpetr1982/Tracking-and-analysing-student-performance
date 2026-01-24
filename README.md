Student Performance Analysis: A Data-Driven Approach

This repository contains a comprehensive data analysis project using R to explore, predict, and cluster student performance based on various academic activities.
📌 Project Overview

The goal of this project is to analyze how different types of academic engagement (Homework, Compulsory Activities, Optional Activities) influence final exam results. By leveraging statistical modeling and machine learning, we aim to identify patterns that lead to student success and provide data-backed recommendations for educational interventions.
📊 Key Features

    Data Cleaning & Preprocessing: Handling missing values (non-participation), outlier removal, and data standardization.

    Exploratory Data Analysis (EDA): Correlation analysis, PCA for variable significance, and distribution visualization.

    Predictive Modeling: * Linear Regression (Single & Multiple) for grade prediction.

        Support Vector Machines (SVM) for classifying pass/fail outcomes.

        Naive Bayes for categorical probability predictions.

    Student Clustering: Implementation of K-means and Hierarchical Clustering to group students into High, Moderate, and Low performers.

🛠️ Tech Stack

    Language: R

    Libraries: dplyr, ggplot2, caret, e1071, factoextra, cluster, naivebayes

📁 Repository Structure

    final_analysis.R: The complete R script containing the data pipeline and models.

    grades.xlsx: The raw dataset (Exams, 4 Homeworks, 8 Compulsory & 10 Optional activities).

    Final Report on Student Performance Analysis.doc: Detailed documentation of the methodology and findings.

    Presentation_student_performance_analysis_final.pptx: A visual summary of the project.

📈 Insights & Results

    Correlation: A positive relationship exists between engagement in optional activities and final grades (r≈0.42).

    Predictability: Assignment performance is a statistically significant predictor of exam success (p<0.05).

    Clustering: Identified distinct groups of students, allowing for targeted support for those in the "Low Performer" cluster.

🚀 How to Run

    Clone the repository.

    Ensure you have R and the required libraries installed.

    Run final_analysis.R to reproduce the plots and model results.

Author: Dimitrios Petridis

Data Science & Machine Learning Student
