# tidyverse doesn't want to work...
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(foreach)
library(doMC)

loan_data <- read_csv("kiva-master/kiva_loans.csv")
region_data <- read_csv("kiva-master/kiva_mpi_region_locations.csv")
loan_theme <- read_csv("kiva-master/loan_theme_ids.csv")
loan_theme_by_region <- read_csv("kiva-master/loan_themes_by_region.csv")

######################### How many get funded ####################################

loan_data$loan_differential <- loan_data$funded_amount/loan_data$loan_amount
ggplot(loan_data, aes(x=funded_amount, y=loan_amount)) + geom_point()
ggplot(filter(loan_data, loan_differential != 1), aes(x=loan_differential)) + geom_histogram()
loan_data %>% 
  mutate(loan_funding_buckets = ifelse(loan_differential == 0, "nothing", 
                                ifelse(loan_differential== 1, "everything", "something"))) %>%
  group_by(loan_funding_buckets) %>% 
  summarise(count = n()/length(loan_data$loan_differential))
# 93% get everthing, 6.5 get something (approx normal dist), 0.5 get nothing

########################## Men Vs Women ########################################

### Clean Gender Column
loan_data <- mutate(loan_data,
                    count_female = str_count(borrower_genders, "female"),
                    count_male = str_count(borrower_genders, "\\bmale"),
                    count_borrowers = count_male + count_female)

ggplot(loan_data, aes(x=count_male, y=loan_amount, col=funded_amount)) + geom_point()
# More people, les likely to receive 0.

mutate(loan_data, more_female_ind = factor(ifelse(count_female > count_male, 1, 0))) %>%
    group_by(more_female_ind) %>% 
    summarise(mean_loan_amount = mean(loan_amount),
              sd_loan_amount = sd(loan_amount),
              mean_funded_amount = mean(funded_amount),
              sd_funded_amount = sd(funded_amount))
# If there are more women than men on the loan, they ask for less, but receive a higher proportion of what they are asking for than men

ggplot(loan_data, aes(x=count_borrowers,y=loan_amount)) + geom_point()

####################### activity ##########################################
loan_data %>% 
    group_by(activity) %>% 
    summarise(mean_loan_amount = mean(loan_amount)) %>%
    arrange(desc(mean_loan_amount))


