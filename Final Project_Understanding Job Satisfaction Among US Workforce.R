library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("/Users/misbaharshad/Downloads/")
job_sat <- read.csv("highered_00002.csv")

#Group 22

# Data cleaning
# Create dataframes for the major and job mappings
major_mapping <- tibble(
  id = c(198895, 226395, 298895, 318730, 338785, 398895, 419295, 429295, 438995, 449995, 
         459395, 527250, 537260, 547280, 567350, 587995, 611995, 699995, 719995, 799995),
  major = c("Computer and mathematical sciences", "Biological sciences", 
            "Other biological, agricultural, environmental life sciences", "Chemistry, except biochemistry", 
            "Physics and astronomy", "Other physical and related sciences", "Economics", 
            "Political and related sciences", "Psychology", "Sociology and anthropology", 
            "Other social sciences", "Chemical engineering", "Civil engineering", 
            "Electrical, electronics and communications engineering", "Mechanical engineering", 
            "Other engineering", "Health-related fields", "Other science and engineering-related", 
            "Management and administration", "Other non-science and engineering")
)

job_mapping <- tibble(
  id = c(182965, 192895, 222205, 282885, 293995, 311930, 333305, 382995, 393995, 412320,
         432360, 482995, 483995, 505005, 520850, 530860, 540890, 560940, 582800, 611995,
         621995, 631995, 651995, 711410, 711995, 735995, 799995),
  job = c("Postsecondary teachers-Computer and math sciences", "Computer scientists and mathematicians",
          "Biological and medical scientists", "Postsecondary teachers-Life related sciences",
          "Other life and related scientists", "Chemists, except biochemists", "Physicists and astronomers",
          "Postsecondary teachers-Physical and related sciences", "Other physical and related scientists",
          "Economists", "Psychologists", "Postsecondary teachers-Social and related sciences",
          "Other social scientists", "Other engineers", "Chemical engineers", "Civil engineers",
          "Electrical or computer hardware engineers", "Mechanical engineers",
          "Postsecondary teachers - engineering", "Health-related occupations",
          "Science and engineering managers", "Science and engineering pre-college teachers",
          "Science and engineering pre-college teachers", "Top and mid-level managers, executives, administrators",
          "Other management related occupations", "Non-science and engineering pre-college and post-secondary teachers",
          "Other Non-science and engineering occupations")
)

# Functions
replace_codes <- function(col, major_mapping, job_mapping) {
  if (is.numeric(col)) {
    col <- ifelse(col %in% major_mapping$id, major_mapping$major[match(col, major_mapping$id)], col)
    col <- ifelse(col %in% job_mapping$id, job_mapping$job[match(col, job_mapping$id)], col)
  }
  return(col)
}


TEST_job_cleaned <- job_sat |>
  select(!c(PERSONID, PTWTFT, NRREA, WTREASN, FSDED, FSDK, FSDOD, FSDOE, FSHHS, FSNIH, FSNSF, FSOT)) |>
  filter(SALARY != 9999998) |>
  mutate(GENDER = if_else(GENDER == 2, "Male", "Female"),
         RACETH = if_else(RACETH == 2, "White", "Non-White"),
         JOBSATIS = if_else(JOBSATIS %in% c(1, 2), 1, 0)) %>%
  mutate(across(everything(), ~ replace_codes(., major_mapping, job_mapping)))

check <- TEST_job_cleaned |>
  summarize(across(everything(), ~ sum(is.na(.))))

write_csv(TEST_job_cleaned, "TEST_cleaned_job_sat.csv")

TEST_job_cleaned <- read.csv("TEST_cleaned_job_sat.csv")


## Logistic regression

### Create data frame for independent variables
independent_vars <- TEST_job_cleaned %>% select(-JOBSATIS)
x_cat<-sparse.model.matrix(~., data=independent_vars)[,-1]
Y<-as.factor(TEST_job_cleaned$JOBSATIS)

formula <- as.formula(paste("JOBSATIS ~", paste(names(independent_vars), collapse = " + ")))
logit_model <- glm(formula, data = TEST_job_cleaned, family = "binomial")
summary(logit_model)


### Export to CSV
tidy_results <- tidy(logit_model)
write.csv(tidy_results, "logit_model_results.csv", row.names = FALSE)

## Do lasso
lasso1<- gamlr(x_cat, y=Y,family="binomial", lambda.min.ratio=1e-3)
plot(lasso1)

dev1 <- lasso1$deviance[which.min(AICc(lasso1))] 
dev1_0 <- lasso1$deviance[1]
1-dev1/dev1_0

which.min(AICc(lasso1))
summary(lasso1)[56,]

### Get csv table
best_model_index <- which.min(AICc(lasso1))
best_model_coefficients <- coef(lasso1, s = best_model_index)

coeff_df <- as.data.frame(as.matrix(best_model_coefficients))
coeff_df <- cbind(Variable = rownames(coeff_df), Coefficient = coeff_df)
colnames(coeff_df) <- c("Variable", "Coefficient")

coeff_df <- coeff_df[coeff_df$Coefficient != 0,]

write.csv(coeff_df, "lasso_model_coefficients.csv", row.names = FALSE)


# Loading package
want <- c("tidyverse", "dplyr","ggthemes", "readxl", "stringr", "lubridate", 
          "readr", "purrr", "tidytext", "textdata")
need <- want[!(want %in% installed.packages()[,"Package"])]
if(length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only = TRUE))
rm(want, need)

# Reading Dataset
TEST_job_cleaned <- read.csv("TEST_cleaned_job_sat.csv")

### ************************************************************** ###
## Visualizing demographic profile
### ************************************************************** ###

# Function for developing graph
visualizebar <- function(dataset, xvar, fillvar = NULL){
  # Transforming variables
  x_sym <- rlang::sym(xvar)
  fill_sym <- if (!is.null(fillvar)) rlang::sym(fillvar) else NULL
  
  # Order x
  dataset <- dataset %>%
    dplyr::mutate(!!x_sym := forcats::fct_rev(forcats::fct_infreq(!!x_sym)))
  
  # Developing plot
  if(!is.null(fillvar)){
    ggplot(data = dataset, aes(x = !!sym(x_sym))) +
      geom_bar(aes(fill = !!sym(fill_sym)), position = "dodge") +
      theme_classic() + 
      theme(plot.title = element_text(family = "sans"),
            axis.title.x = element_text(family = "sans"),
            axis.title.y = element_text(family = "sans"),
            axis.text.x = element_text(family = "sans"),
            axis.text.y = element_text(family = "sans"))
  } else{
    ggplot(data = dataset, aes(x = !!sym(x_sym))) +
      geom_bar() +
      theme_classic() +
      theme(plot.title = element_text(family = "sans"),
            axis.title.x = element_text(family = "sans"),
            axis.title.y = element_text(family = "sans"),
            axis.text.x = element_text(family = "sans"),
            axis.text.y = element_text(family = "sans"))
  }
}

### *************** Race and Sex *************** ###
racsex_graph <- visualizebar(data = TEST_job_cleaned, 
                             xvar = "GENDER", fillvar = "RACETH") +
  labs(title = "Number of observation based on Gender and Race",
       x = "Gender",
       y = "Number of Observation",
       fill = "Race Categories") +
  scale_y_continuous(breaks = seq(0, 50000, by = 5000))
racsex_graph
ggsave("3. Outputs/Number of Observation based on Gender and Race.png", 
       racsex_graph)

### *************** Number of Observation Based  *************** ###
edmajor_graph <- visualizebar(data = TEST_job_cleaned, xvar = "NDGMED") +
  labs(title = "Major of Highest Education",
       x = "Major",
       y = "Number of Observation") + coord_flip() +
  geom_bar(stat = "count", fill = "#3399CC")
print(edmajor_graph)
ggsave("3. Outputs/Major of Highest Education.png",
       edmajor_graph)

### *************** Age of Respondents *************** ###
age_graph <- TEST_job_cleaned |>
  ggplot(aes(x = AGE)) + geom_boxplot(aes(color = "salmon")) + theme_classic() +
  labs(title = "Age distribution",
       x = "Age")

age_graph2 <- TEST_job_cleaned |>
  ggplot(aes(x = AGE)) + geom_bar() + theme_classic() +
  labs(title = "Age distribution by Gender",
       x = "Age of Respondents",
       y = "Total Observation") +
  geom_vline(xintercept = median(TEST_job_cleaned$AGE))

print(age_graph)
print(age_graph2)
ggsave("3. Outputs/Number of Observation based on Age of Respondents.png",
       age_graph2)

# Facet with sex
agesex_graph <- age_graph2 + 
  facet_grid(rows = vars(GENDER)) + geom_bar(aes(fill = GENDER)) +
  labs(fill = "Gender")

print(agesex_graph)
ggsave("3. Outputs/Age by Sex.png", agesex_graph)

### *************** Salary Distribution *************** ###
# Mutating degree
TEST_job_cleaned <- TEST_job_cleaned |>
  mutate(
    DGRDG = case_when(
      DGRDG == 1 ~ "Bachelor",
      DGRDG == 2 ~ "Master",
      DGRDG == 3 ~ "Doctorate",
      DGRDG == 4 ~ "Professional"),
    DGRDG = factor(DGRDG, 
                   levels = c("Bachelor", "Master", "Doctorate", "Professional"))
  )

### Overall salary graph ***** ###
salary_graph <- TEST_job_cleaned |>
  ggplot(aes(x = SALARY)) + geom_boxplot() + theme_classic() +
  labs(title = "Overall Salary Distribution",
       x = "Salary") +
  geom_boxplot(fill = "#3399CC") + 
  theme(plot.title = element_text(family = "sans"),
        axis.title.x = element_text(family = "sans"),
        axis.title.y = element_text(family = "sans"),
        axis.text.x = element_text(family = "sans"),
        axis.text.y = element_text(family = "sans")) +
  coord_flip()
print(salary_graph)
ggsave("3. Outputs/Salary graph.png", salary_graph)

### Salary Graph ***** ### 
salary_gender_graph <- salary_graph + facet_grid(cols = vars(GENDER)) +
  labs(title = "Salary Distribution by Gender")
salary_gender_graph
ggsave("3. Outputs/Salary by gender.png", salary_gender_graph)

### Salary by Race ***** ###
salary_race_graph <- salary_graph + facet_grid(cols = vars(RACETH)) + 
  labs(title = "Salary Distribution by Race",
       fill = "Race Category")
print(salary_race_graph)
ggsave("3. Outputs/Salary by race.png", salary_race_graph)

### Salary by Degree ***** ###
# Graphing boxlot
salary_degree_graph <- salary_graph + facet_grid(cols = vars(DGRDG)) +
  labs(title = "Salary Distribution by Degree of Education",
       x = "Salary") +
  coord_flip()
print(salary_degree_graph)
ggsave("3. Outputs/Salary by degree.png", salary_degree_graph)

### Salary by Principal Job ***** ###
salary_principal_job <- TEST_job_cleaned |>
  group_by(NOCPR) |>
  summarise(
    average_salary = mean(SALARY),
    max_salary = max(SALARY),
    quantile_75 = quantile(SALARY, 0.75),
    median_salary = median(SALARY),
    quantile_25 = quantile(SALARY, 0.25),
    minimum_salary = min(SALARY)
  )
salary_principal_job

### Salary by  ***** ###

### *************** JOB Satisfaction *************** ###
### Overall Job Satisfaction ***** ###
job_satisfaction <- TEST_job_cleaned |>
  mutate(JOBSATIS = as.factor(JOBSATIS)) |>
  ggplot(aes(x = JOBSATIS)) +
  geom_bar() + theme_classic() +
  labs(title = "Overall Job Satisfaction",
       x = "Job Stasifaction (1 = Satisfied, 0 = Non Satisfied)")
job_satisfaction
ggsave("3. Outputs/Job Satisfaction.png", job_satisfaction)

### Job Satisfaction by Occupation ***** ###
satisfaction_principal <- TEST_job_cleaned |>
  group_by(NOCPR) |> summarize(percentage = mean(JOBSATIS)) |>
  ggplot(aes(x = NOCPR, y = percentage)) +
  geom_col() +
  ylim(0,1) + coord_flip() +
  geom_text(aes(label = NOCPR),
            hjust = 1.1, 
            color = "white", 
            size = 3.5) +
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            hjust = -0.1, 
            color = "black", 
            size = 3.5) +
  labs(x = "Principal Category", y = "Percentage", 
       title = "Job Satisfaction by Principal Job Category") +
  theme_classic() +
  theme(axis.text.y = element_blank())
print(satisfaction_principal)
ggsave("3. Outputs/Job Satisfaction by Peincipal Job.png", satisfaction_principal)


# Tree
library(tree)
library(randomForest)
library(MASS)

# Create tree
job_tree <- tree(JOBSATIS ~., data=TEST_job_cleaned)

par(mfrow=c(1,1))
plot(job_tree, col=8)
text(job_tree, digits=3, cex=0.6)

# Prune the tree using cross validation
cv_tree <- cv.tree(job_tree, K=90)

cv_tree$size
cv_tree$dev
best_nodes <- cv_tree$size[which.min(cv_tree$dev)]

plot(cv_tree, pch=21, bg=8, type="p")

job_pruned <- prune.tree(job_tree, best=best_nodes)
plot(job_pruned, col=8)
text(job_pruned, digits=3, cex=1)

#Clustering -----

table(TEST_job_cleaned[, 22])

TEST_job_cleaned <- TEST_job_cleaned[TEST_job_cleaned$SALARY != 9999998, ]

X_subset <- TEST_job_cleaned %>%
  select(c(NOCPR, SALARY, DGRDG, JOBSATIS))

unique(X_subset$SALARY)

jobsat_scale <- scale(X_subset[,2:3])

apply(jobsat_scale,2,sd) # sd=1

apply(jobsat_scale,2,mean) # mean=0

seven <- kmeans(jobsat_scale,7,nstart=10)

seven$centers 

table_seven <- table(seven$cluster,TEST_job_cleaned$NOCPR) #what is the distribution of professions in each cluster

table_seven

cluster_color<-seven$cluster

i <- sample(1:nrow(jobsat_scale))  

plot(TEST_job_cleaned$SALARY[i], TEST_job_cleaned$DGRDG[i],
     pch=21, cex=.75, bty="n",
     xlab="salary",
     ylab="degree",
     bg=c("maroon","gold", "green", "blue", "pink", "orange", "purple")[cluster_color[i]],
     col=c("maroon","gold", "green", "blue", "pink", "orange", "purple")[TEST_job_cleaned$NOCPR[i]])

kfit <- lapply(1:200, function(k) kmeans(jobsat_scale,k))

source("kIC.R") ## utility script

# you give it kmeans fit, 
# then "A" for AICc (default) or "B" for BIC

kaicc <- sapply(kfit,kIC)

kbic <- sapply(kfit,kIC,"B")

plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), # get them on same page
     bty="n", type="l", lwd=2)

abline(v=which.min(kaicc))

lines(kbic, col=4, lwd=2)

abline(v=which.min(kbic),col=4)

k=7

# IS R^2 of 86.3% (for explaining deviance of x)

1 - sum(kfit[[k]]$tot.withinss)/kfit[[k]]$totss

# clearly still splitting on color

t<-table(TEST_job_cleaned$NOCPR,kfit[[k]]$cluster)

## look also at job vacation average by cluster

#plot(tapply(highered$JOBVAC,kfit[[k]]$cluster,mean))

## look also at job satisfaction average by cluster

plot(tapply(TEST_job_cleaned$JOBSATIS,kfit[[k]]$cluster,mean))

library(gamlr)
library(Matrix)

xclust <- sparse.model.matrix(~factor(kfit[[k]]$cluster)+TEST_job_cleaned$NOCPR) # cluster membership matrix

jobsatregclust <- cv.gamlr(xclust,TEST_job_cleaned$JOBSATIS,lambda.min.ratio=1e-5) # 

plot(jobsatregclust)

max(1-jobsatregclust$cvm/jobsatregclust$cvm[1]) # OOS R2 around 0.03

library(gamlr)  

# create matrix of al three way interactions

x <- model.matrix(NOCPR~.^3, data=highered)[,-1]

jobsatreg <- cv.gamlr(x,highered$NOCPR,lambda.min.ratio=1e-5)

plot(jobsatreg)

max(1-jobsatreg$cvm/jobsatreg$cvm[1]) # max OOS R2 on y of about 1/3


