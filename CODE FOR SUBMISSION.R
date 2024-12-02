rm(list=ls())

install.packages("readxl")  
install.packages("openxlsx")  

library(readxl)

dataset <- read_excel("C:/Users/molin/Downloads/FINAL 209 20.xlsx")

mean(dataset$`IV (Judicial Reforms)`, na.rm = TRUE)
median(dataset$`IV (Judicial Reforms)`, na.rm = TRUE)
get_mode(dataset$`IV (Judicial Reforms)`)

mean(dataset$`DV (Judicial Integrity)`, na.rm = TRUE)
median(dataset$`DV (Judicial Integrity)`, na.rm = TRUE)
get_mode(dataset$`DV (Judicial Integrity)`)

mean(dataset$`C1 (Judicial Purges)`, na.rm = TRUE)
median(dataset$`C1 (Judicial Purges)`, na.rm = TRUE)
get_mode(dataset$`C1 (Judicial Purges)`)

mean(dataset$`C2 (Hight Court Ind.`, na.rm = TRUE)
median(dataset$`C2 (Hight Court Ind.`, na.rm = TRUE)
get_mode(dataset$`C2 (Hight Court Ind.`)

var(dataset$`IV (Judicial Reforms)`, na.rm = TRUE)
sd(dataset$`IV (Judicial Reforms)`, na.rm = TRUE)
range(dataset$`IV (Judicial Reforms)`, na.rm = TRUE)
IQR(dataset$`IV (Judicial Reforms)`, na.rm = TRUE)

var(dataset$`DV (Judicial Integrity)`, na.rm = TRUE)
sd(dataset$`DV (Judicial Integrity)`, na.rm = TRUE)
range(dataset$`DV (Judicial Integrity)`, na.rm = TRUE)
IQR(dataset$`DV (Judicial Integrity)`, na.rm = TRUE)

var(dataset$`C1 (Judicial Purges)`, na.rm = TRUE)
sd(dataset$`C1 (Judicial Purges)`, na.rm = TRUE)
range(dataset$`C1 (Judicial Purges)`, na.rm = TRUE)
IQR(dataset$`C1 (Judicial Purges)`, na.rm = TRUE)

var(dataset$`C2 (Hight Court Ind.`, na.rm = TRUE)
sd(dataset$`C2 (Hight Court Ind.`, na.rm = TRUE)
range(dataset$`C2 (Hight Court Ind.`, na.rm = TRUE)
IQR(dataset$`C2 (Hight Court Ind.`, na.rm = TRUE)

table(dataset$`IV (Judicial Reforms)`, dataset$`DV (Judicial Integrity)`)

aov_model <- aov(dataset$`DV (Judicial Integrity)` ~ dataset$`IV (Judicial Reforms)`, data = dataset)
summary(aov_model)

install.packages("effectsize") 
library(effectsize)

eta_squared(aov_model)

library(ggplot2)
dataset_clean <- dataset[!is.na(dataset$`IV (Judicial Reforms)`) & !is.na(dataset$`DV (Judicial Integrity)`), ]

ggplot(dataset_clean, aes(x = as.factor(`IV (Judicial Reforms)`), y = `DV (Judicial Integrity)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Judicial Integrity Across Reform Categories",
    x = "Judicial Reforms",
    y = "Judicial Integrity"
  )

dataset_clean$Judicial_Reforms <- as.factor(dataset_clean$`IV (Judicial Reforms)`)

length(unique(dataset_clean$`IV (Judicial Reforms)`))

library(dplyr)

reform_counts <- dataset_clean %>%
  count(`IV (Judicial Reforms)`, sort = TRUE)

dataset_clean$Reforms_Group <- ifelse(dataset_clean$`IV (Judicial Reforms)` %in% reform_counts$`IV (Judicial Reforms)`[1:10],
                                      dataset_clean$`IV (Judicial Reforms)`, "Other")
dataset_clean$Reforms_Group <- as.factor(dataset_clean$Reforms_Group)

aov_model <- aov(`DV (Judicial Integrity)` ~ Reforms_Group, data = dataset_clean)
summary(aov_model)

TukeyHSD(aov_model)

ggplot(dataset_clean, aes(x = Reforms_Group, y = `DV (Judicial Integrity)`)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Judicial Integrity Across Reform Categories",
       x = "Judicial Reforms",
       y = "Judicial Integrity")

ggplot(dataset_clean, aes(x = Reforms_Group, y = `DV (Judicial Integrity)`)) +
  geom_jitter(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatterplot of Judicial Integrity and Reform Categories",
    x = "Judicial Reforms",
    y = "Judicial Integrity"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor.test(dataset_clean$`DV (Judicial Integrity)`, dataset_clean$`IV (Judicial Reforms)`, method = "pearson", use = "complete.obs")

library(ggplot2)

bivariate_model <- lm(`DV (Judicial Integrity)` ~ `IV (Judicial Reforms)`, data = dataset_clean)
summary(bivariate_model)

ggplot(dataset_clean, aes(x = `IV (Judicial Reforms)`, y = `DV (Judicial Integrity)`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Bivariate Relationship Between Judicial Reforms and Integrity",
       x = "Judicial Reforms",
       y = "Judicial Integrity")

multivariate_model <- lm(dataset_clean$`DV (Judicial Integrity)` ~ dataset_clean$`IV (Judicial Reforms)` + dataset_clean$`C1 (Judicial Purges)` + dataset_clean$`C2 (Hight Court Ind.`, data = dataset_clean)
summary(multivariate_model)

library(stargazer)
stargazer(multivariate_model, type = "text", title = "Regression Results", digits = 2)

install.packages("jtools")
library(jtools)

plot_summs(multivariate_model, scale = TRUE, robust = TRUE, inner_ci_level = 0.95) +
  labs(
    title = "Regression Coefficients for Predicting Judicial Integrity",
    x = "Standardized Coefficients",
    y = "Predictors"
  )

bivariate_model <- lm(dataset_clean$`DV (Judicial Integrity)` ~ dataset_clean$`IV (Judicial Reforms)`, data = dataset_clean)
summary(bivariate_model)

