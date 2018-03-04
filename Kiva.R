# tidyverse doesn't want to work...
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(foreach)
library(doMC)

loan_data <- read_csv("data-science-for-good-kiva-crowdfunding/kiva_loans.csv")
region_data <- read_csv("data-science-for-good-kiva-crowdfunding/kiva_mpi_region_locations.csv")
loan_theme <- read_csv("data-science-for-good-kiva-crowdfunding/loan_theme_ids.csv")
loan_theme_by_region <- read_csv("data-science-for-good-kiva-crowdfunding/loan_themes_by_region.csv")

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
count_male_female <- function(a_string){
  # return 2 col df with count male & female
  data_frame(
    male = str_match_all(a_string, "\\bmale")[[1]] %>% length(),
    female = str_match_all(a_string, "female")[[1]] %>% length(),
    borrower_count = male + female
  )
}
registerDoMC(cores=3)
male_female_df <- as.list(rep(NA, nrow(loan_data)))
i <- 1
male_female_df <- foreach(i = 1:nrow(loan_data)) %dopar%{
  count_male_female(loan_data$borrower_genders)
}


men_vs_women <- loan_data %>% 
  group_by(borrower_genders) %>% 
  summarise(loan_amount = sum(loan_amount),
            loan_count = n(),
            loan_amount_total = sum(loan_data$loan_amount),
            loan_count_toal = length(loan_data$loan_amount))
table(loan_data$borrower_genders)
