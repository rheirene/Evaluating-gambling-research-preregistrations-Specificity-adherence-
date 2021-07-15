# Script for Krippendorf's Alpha analysis
# interrater reliability

# install.packages("irr")

library(irr)
library(tidyverse)

rhdata <- read.csv("Data/Scoring_guides_after_pilot_coding_only/Scoring_framework_RH_coding_pilot_only.csv", 
                     header=TRUE)
bkdata <- read.csv("Data/Scoring_guides_after_pilot_coding_only/Scoring_framework_BK_coding_pilot_only.csv", 
                   header=TRUE)

head(rhdata) # Look at the setup of the data
head(bkdata) # Look at the setup of the data

# consistency check of ***specificity** scoring__________________________________________________________
# Filter to select the registrations coded so far (pilot: n = 4) and then select all relevant columns
# CODER 1
specificity.scores.temp.rh<- rhdata %>%
  # rename(No.hypotheses = No..of.hypotheses) %>%
  filter(Preregistration_number == "11"| 
           Preregistration_number == "12"| 
           Preregistration_number == "20"|
           Preregistration_number == "23") %>%
  select(Preregistration_number, 18:43) %>%
  as_tibble() %>%
  print()

# CODER 2
specificity.scores.temp.bk<- bkdata %>%
  # rename(No.hypotheses = Nr_hyp_C_1) %>%
  filter(Preregistration_number == "11"| 
           Preregistration_number == "12"| 
           Preregistration_number == "20"|
           Preregistration_number == "23") %>%
  select(Preregistration_number, 18:43) %>%
  mutate_if(is.factor, ~ as.numeric(as.character(.x))) %>%
  as_tibble() %>%
  print()

# Wrangle the data into the correct format for the analysis by first creating...
# separate datasets for each row and then add each set of rows side by side using bind_cols
# CODER 1
n1rh<-specificity.scores.temp.rh %>% 
  filter(Preregistration_number == "11") %>%
  select(-Preregistration_number)
n2rh<-specificity.scores.temp.rh %>% 
  filter(Preregistration_number == "12") %>%
  select(-Preregistration_number)
n3rh<-specificity.scores.temp.rh %>% 
  filter(Preregistration_number == "20") %>%
  select(-Preregistration_number)
n4rh<-specificity.scores.temp.rh %>% 
  filter(Preregistration_number == "23") %>%
  select(-Preregistration_number)

specificity.scores.rh<- n1rh %>% 
  bind_cols(n2rh, n3rh, n4rh) %>%
  print()

# CODER 2
n1bk<-specificity.scores.temp.bk %>% 
  filter(Preregistration_number == "11") %>%
  select(-Preregistration_number)
n2bk<-specificity.scores.temp.bk %>% 
  filter(Preregistration_number == "12") %>%
  select(-Preregistration_number)
n3bk<-specificity.scores.temp.bk %>% 
  filter(Preregistration_number == "20") %>%
  select(-Preregistration_number)
n4bk<-specificity.scores.temp.bk %>% 
  filter(Preregistration_number == "23") %>%
  select(-Preregistration_number)

specificity.scores.bk<- n1bk %>% 
  bind_cols(n2bk, n3bk, n4bk) %>%
  print()



# Convert values from character to numeric to allow join:
ncol(specificity.scores.bk) # How many columns are there to change?
specificity.scores.bk[1:104] <- lapply(specificity.scores.bk[1:104], as.numeric) 

# BIND TOGETHER specificity scores
specificity.scores<- specificity.scores.rh %>%
  bind_rows(specificity.scores.bk)
# check for any null/NA values
sum(is.null(specificity.scores))
sum(is.na(specificity.scores))

specificity.scores.1<- as.matrix(specificity.scores) #Convert to matrix before running analysis

# Run Krippendorff's Alpha
kripp.alpha(specificity.scores.1, method = c("ordinal"))

# Transpose table to allow Fleiss' Kappa analysis
specificity.scores.transposed <- t(specificity.scores)
View(specificity.scores.transposed)

# Run Fleiss' Kappa
kappam.fleiss(specificity.scores.transposed, exact = FALSE, detail = FALSE)



# consistency check of ***specificty** scoring__________________________________________________________
# Filter to select the registrations coded so far (pilot: n = 4) and then select all relevant columns...
# for the consistency check of ***adherence** scoring
# CODER 1
adherence.scores.temp.rh<- rhdata %>%
  # rename(No.hypotheses = No..of.hypotheses) %>%
  filter(Preregistration_number == "11"| 
           Preregistration_number == "12"| 
           Preregistration_number == "20"|
           Preregistration_number == "23") %>%
  select(Preregistration_number, 44:86) %>%
  select(-Notes, -Notes.1, -Notes.2, -Notes.3) %>%
  mutate_if(is.factor, ~ as.character(.x)) %>% # convert to character now to make replacing character strings (e.g., "U(P)" easier later on
  mutate_if(is.integer, ~ as.character(.x)) %>%  
  as_tibble() %>%
  print()

# CODER 2
adherence.scores.temp.bk<- bkdata %>%
  # rename(No.hypotheses = No..of.hypotheses) %>%
  filter(Preregistration_number == "11"| 
           Preregistration_number == "12"| 
           Preregistration_number == "20"|
           Preregistration_number == "23") %>%
  select(Preregistration_number, 44:85) %>%
  select(-Notes, -Notes.1, -Notes.2) %>%
  mutate_if(is.factor, ~ as.character(.x)) %>% # convert to character now to make replacing character strings (e.g., "U(P)" easier later on
  mutate_if(is.integer, ~ as.character(.x)) %>%
  rename(R6 = R6_dev_score) %>%
  as_tibble() %>%
  print()


# Wrangle the data into the correct format for the analysis by first creating...
# seperate datasets for each row and then add each set of rows side by side using bind_cols
# CODER 1
n1rh<-adherence.scores.temp.rh %>% 
  filter(Preregistration_number == "11") %>%
  select(-Preregistration_number)
n2rh<-adherence.scores.temp.rh %>% 
  filter(Preregistration_number == "12") %>%
  select(-Preregistration_number)
n3rh<-adherence.scores.temp.rh %>% 
  filter(Preregistration_number == "20") %>%
  select(-Preregistration_number)
n4rh<-adherence.scores.temp.rh %>% 
  filter(Preregistration_number == "23") %>%
  select(-Preregistration_number)

adherence.scores.rh<- n1rh %>% 
  bind_cols(n2rh, n3rh, n4rh) %>%
  print()

# CODER 2
n1bk<-adherence.scores.temp.bk %>% 
  filter(Preregistration_number == "11") %>%
  select(-Preregistration_number)
n2bk<-adherence.scores.temp.bk %>% 
  filter(Preregistration_number == "12") %>%
  select(-Preregistration_number)
n3bk<-adherence.scores.temp.bk %>% 
  filter(Preregistration_number == "20") %>%
  select(-Preregistration_number)
n4bk<-adherence.scores.temp.bk %>% 
  filter(Preregistration_number == "23") %>%
  select(-Preregistration_number)

adherence.scores.bk<- n1bk %>% 
  bind_cols(n2bk, n3bk, n4bk) %>%
  print()

# BIND TOGETHER specificity scores
adherence.scores<- adherence.scores.rh %>%
  bind_rows(adherence.scores.bk) %>%
  print()

# check for any null/NA values
sum(is.null(adherence.scores))
sum(is.na(adherence.scores))

adherence.scores[adherence.scores == "U(P)"]<- "96"
adherence.scores[adherence.scores == "U(A)"]<- "97"
adherence.scores[adherence.scores == "U(B)"]<- "98"

adherence.scores.1<- as.matrix(adherence.scores) #Convertto matrix before running analysis

# Run Krippendorff's Alpha
kripp.alpha(adherence.scores.1, method = c("ordinal"))

# Transpose table to allow Fleiss' Kappa analysis
adherence.scores.transposed <- t(adherence.scores)
View(adherence.scores.transposed)

# Run Fleiss' Kappa
kappam.fleiss(adherence.scores.transposed, exact = FALSE, detail = FALSE)

