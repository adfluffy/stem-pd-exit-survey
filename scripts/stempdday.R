#Load necessary libraries ------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(modelsummary)
library(gt)

# File Reading and Directory ---------------------------------------------------
#Set working directory to proper project folder
setwd("C:/Users/Work/Documents/R Projects/031425 STEM PD Day Exit Tickets")
# Sets the files for reading later in the script
exit_ticket_file <-"data/STEM PD Retreat Feedback Survey Data.csv"
question_key_file <- "data/STEM PD Retreat Feedback Survey Questions.csv"

# Data Frame work --------------------------------------------------------------
# Likert and Likeliness definitions
likert_labels <- c(
  "1" = "Strongly Disagree",
  "2" = "Disagree",
  "3" = "Neither Agree Nor Disagree",
  "4" = "Agree",
  "5" = "Strongly Agree"
)
likely_labels <- c(
  "0" = "Not Likely",
  "1" = "Somewhat Likely",
  "2" = "Very Likely",
  "3" = "Extremely Likely"
)
# Creates data frame from CSV file of exit ticket responses
pd_exit <- read.csv(exit_ticket_file)
# Imports the Question Information
question_key <- read.csv(question_key_file)

# Adds Source columns for later analysis
pd_exit <- pd_exit %>%
  mutate(Source = if_else(Q2 == 0, "New", "Prior"))

# Creates data frame for combined likert responses (Strongly Disagree - Strongly Agree)
likert_responses <- pd_exit %>%
  pivot_longer(cols = c(Q3a:Q3d, Q6a:Q6f), 
               names_to = "Question", 
               values_to = "Likert_value") %>%
  select(Source, Likert_value) %>% 
  mutate(Likert_value = factor(Likert_value, 
                               levels = names(likert_labels), 
                               labels = likert_labels)) %>%
  drop_na()

# Creates data frame for likeliness responses (Not Likely - Extremely Likely)
likely_responses <- pd_exit %>%
  pivot_longer(cols = Q4:Q5, 
               names_to = "Question", 
               values_to = "Likely_value") %>%
  select(Source, Likely_value) %>% 
  mutate(Likely_value = factor(Likely_value,
                               levels=names(likely_labels),
                               labels=likely_labels)) %>% 
  drop_na()

# Creates data frame for aggregated data on perceived quality of PD sessions
pd_sessions <- pd_exit %>%
  pivot_longer(cols = Q3a:Q3d, 
               names_to = "Question", 
               values_to = "Likert_value") %>%
  select(Source, Likert_value) %>% 
  mutate(Likert_value = factor(Likert_value, 
                               levels = names(likert_labels), 
                               labels = likert_labels)) %>%
  drop_na()

# Creates data frame for aggregated data on perceived quality of PD event management
pd_event <- pd_exit %>%
  pivot_longer(cols = Q6a:Q6f, 
               names_to = "Question", 
               values_to = "Likert_value") %>%
  select(Source, Likert_value) %>% 
  mutate(Likert_value = factor(Likert_value, 
                               levels = names(likert_labels), 
                               labels = likert_labels)) %>%
  drop_na()



# Histograms -------------------------------------------------------------------
# Q3a-Q3d & Q6a-Q6f Histograms by Q2 -------------------------------------------
for (i in 1:12){
  # Loops through:
  # Q3a - 1
  # Q3b - 2
  # Q3b - 3
  # Q3c - 4
  # Q4  - 5
  # Q5  - 6
  # Q6a - 7
  # Q6b - 8
  # Q6c - 9
  # Q6d - 10
  # Q6e - 11
  # Q6f - 12
  # Ignores Q6g, as this sub-question is open-ended
  
  # Define locations of relevant data and question information
  data_col <- i + 8 #data starts in column 9
  ques_row <- i + 12 # data starts in row 13
  
  # Store the values from the question_key data frame for use in labels
  question_num <- question_key[ques_row,1]
  question_text <- question_key[ques_row,3]
  alt_quest_text <- question_key[ques_row,2]
  
  # Dynamic Label assignment
  if(i==6|i==5){
    # Q4 and Q5 use the alt_quest_text
    title_val <- paste(question_num,"Histogram:",alt_quest_text)
  } else{
    # All other questions use standard question text
    title_val <- paste(question_num,"Histogram:",question_text)
  }
  # X label creation
  x_lab_val <- paste(question_num,"Value")
  # Determines the list to use for x axis labels
  if (question_num %in% c("Q4", "Q5")) {
    x_labels <- likely_labels
    x_breaks <- as.numeric(names(likely_labels))
  } else {
    x_labels <- likert_labels
    x_breaks <- as.numeric(names(likert_labels))
  }
  
  
  # Fixes wrapping issue with dynamic titles
  title_val <- str_wrap(title_val,width = 80)
  
  # File name definition
  file_name <- paste("images/",question_num,"_response_histogram.png",sep="")
  
  
  temp_data <- pd_exit %>% 
    select(Source,Likert_value=all_of(question_num)) %>% 
    mutate(Likert_value = factor(Likert_value,levels=x_breaks,labels=x_labels))
  
  temp_data <- temp_data %>% drop_na()
  
  # Create and store the ggplot for the dual seriesz histogram
  p <- ggplot(temp_data, aes(x = Likert_value, fill = Source)) +
    geom_bar(position = "dodge", color = "black") +
    scale_fill_manual(values = c(
      "New" = "#1ABC9C",
      "Prior" = "#E67E22"
    )) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = title_val,
      x = x_lab_val,
      y = "Frequency",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Save the plot to a PNG file
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300, bg="white")
}



# All Likert Response Histogram by Q2 ------------------------------------------
# Sets value of histogram title
title_val <- "Q3a-Q3d & Q6a-Q6f Histogram: PD Session Quality and PD Event Management Quality"
title_val <- str_wrap(title_val,width = 80)


# Create and store the ggplot for the dual series histogram
p <- ggplot(likert_responses, aes(x = Likert_value, fill = Source)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "New" = "#1ABC9C",
    "Prior" = "#E67E22"
  )) +
  scale_x_discrete(drop = FALSE) +
  labs(title = title_val,
       x = "Response",
       y = "Frequency",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot to a PNG file
ggsave("images/all_likert.png", plot = p, width = 8, height = 6, dpi = 300, bg="white")



# All Likely Response Histogram by Q2 ------------------------------------------
# Sets value of histogram title
title_val <- "Q4 & Q5 Histogram: Attend another MMSA PD and Attend this PD next year"
title_val <- str_wrap(title_val,width = 80)


# Create and store the ggplot for the dual seriesz histogram
p <- ggplot(likely_responses, aes(x = Likely_value, fill = Source)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "New" = "#1ABC9C",
    "Prior" = "#E67E22"
  )) +
  scale_x_discrete(drop = FALSE) +
  labs(title = title_val,
       x = "Response",
       y = "Frequency",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save the plot to a PNG file
ggsave("images/all_likely.png", plot = p, width = 8, height = 6, dpi = 300, bg="white")



# Perceived Session Quality Responses Histogram by Q2 --------------------------
# Sets value of histogram title
title_val <- "Q3a-Q3d Histogram: Perceived PD Session Quality"
title_val <- str_wrap(title_val,width = 80)


# Create and store the ggplot for the dual series histogram
p <- ggplot(pd_sessions, aes(x = Likert_value, fill = Source)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "New" = "#1ABC9C",
    "Prior" = "#E67E22"
  )) +
  scale_x_discrete(drop = FALSE) +
  labs(title = title_val,
       x = "Response",
       y = "Frequency",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot to a PNG file
ggsave("images/session_qual.png", plot = p, width = 8, height = 6, dpi = 300, bg="white")



# Perceived Event Quality Responses Histogram by Q2 ----------------------------
# Sets value of histogram title
title_val <- "Q6a-Q6f Histogram: Perceived PD Event Management Quality"
title_val <- str_wrap(title_val,width = 80)


# Create and store the ggplot for the dual series histogram
p <- ggplot(pd_event, aes(x = Likert_value, fill = Source)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "New" = "#1ABC9C",
    "Prior" = "#E67E22"
  )) +
  scale_x_discrete(drop = FALSE) +
  labs(title = title_val,
       x = "Response",
       y = "Frequency",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot to a PNG file
ggsave("images/event_qual.png", plot = p, width = 8, height = 6, dpi = 300, bg="white")



# Linear Regression Models -----------------------------------------------------
# Most interested in patterns emerging from whether or not respondents attended
#   previous MMSA PD events
#
# Performs linear regressions under a few different criteria for comparison



# Q4 Models --------------------------------------------------------------------
# Predicts Q4 based only on prior PD attendance
model_Q4_prior <- lm(Q4~Q2, data=pd_exit)
summary(model_Q4_prior)
# Predicts Q4 based on prior PD attendance controlling for perceived quality
#   of PD sessions and perceived quality of PD event execution
model_Q4_all <- lm(Q4~Q2+Q3a+Q3b+Q3c+Q3d+Q6a+Q6b+Q6c+Q6d+Q6e+Q6f, data=pd_exit)
summary(model_Q4_all)
# Predicts Q4 based on prior PD attendance controlling for perceived quality of
#   PD sessions
model_Q4_qual <- lm(Q4~Q2+Q3a+Q3b+Q3c+Q3d, data=pd_exit)
summary(model_Q4_qual)
# Predicts Q4 based on prior PD attendance controlling for perceived quality of
#   PD event execution
model_Q4_venue <- lm(Q4~Q2+Q6a+Q6b+Q6c+Q6d+Q6e+Q6f, data=pd_exit)
summary(model_Q4_venue)



# Q5 Models --------------------------------------------------------------------
# Predicts Q5 based only on prior PD attendance
model_Q5_prior <- lm(Q5~Q2, data=pd_exit)
summary(model_Q5_prior)
# Predicts Q5 based on prior PD attendance controlling for perceived quality
#   of PD sessions and perceived quality of PD event execution
model_Q5_all <- lm(Q5~Q2+Q3a+Q3b+Q3c+Q3d+Q6a+Q6b+Q6c+Q6d+Q6e+Q6f, data=pd_exit)
summary(model_Q5_all)
# Predicts Q5 based on prior PD attendance controlling for perceived quality of
#   PD sessions
model_Q5_qual <- lm(Q5~Q2+Q3a+Q3b+Q3c+Q3d, data=pd_exit)
summary(model_Q5_qual)
# Predicts Q5 based on prior PD attendance controlling for perceived quality of
#   PD event execution
model_Q5_venue <- lm(Q5~Q2+Q6a+Q6b+Q6c+Q6d+Q6e+Q6f, data=pd_exit)
summary(model_Q5_venue)



# Regression tables and visualizations -----------------------------------------
#creates model list for regression plotting
model_list <- list(
  Q4_prior = model_Q4_prior,
  Q4_all = model_Q4_all,
  Q4_qual = model_Q4_qual,
  Q4_venue = model_Q4_venue,
  Q5_prior = model_Q5_prior,
  Q5_all = model_Q5_all,
  Q5_qual = model_Q5_qual,
  Q5_venue = model_Q5_venue
)

#Creates diagnostic plots for all regression models
for (name in names(model_list)) {
  model <- model_list[[name]]
  png(filename = paste0("images/model_plot_", name, ".png"), width = 1000, height = 1000)
  par(mfrow = c(2, 2))  # 2x2 diagnostic plot layout
  plot(model, main = paste("Diagnostics:", name))
  dev.off()
}

# Named model list for Q4 associated table output
Q4model_named_list <- list(
  "Q4 - Prior Only"=model_Q4_prior,
  "Q4 – All Predictors"= model_Q4_all,
  "Q4 – PD Quality Only"= model_Q4_qual,
  "Q4 – Venue Questions Only"= model_Q4_venue)
# Named model list for Q5 associated table output
Q5model_named_list <- list(
  "Q5 - Prior Only"=model_Q5_prior,
  "Q5 – All Predictors"= model_Q5_all,
  "Q5 – PD Quality Only"= model_Q5_qual,
  "Q5 – Venue Questions Only"= model_Q5_venue
)

#Outputs models tables and saves them as .png files
modelsummary(Q4model_named_list,output="images/Q4_models_table.png",stars=T)
modelsummary(Q5model_named_list,output="images/Q5_models_table.png",stars=T)
