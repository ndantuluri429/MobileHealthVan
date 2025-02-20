# Goal of this code is to understand the nature of MHM dataset

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

# accessing and viewing file in R
MHM_Data <- read_excel("~/Downloads/MHM-Data.xlsx")
View(MHM_Data) 

# dimensions
ncol(MHM_Data)
nrow(MHM_Data)
dim(MHM_Data)

# names of all variables 
names(MHM_Data)

# occurrence of unique instances for each variable
sapply(MHM_Data, function(x) length(unique(x)))

# overview of variable types
glimpse(MHM_Data)

# analyzing 1 variable in particular
MHM_Data$country

# create table showing occurance of each variable
MHM_Data %>%
  select(country) %>%
  count(country) %>%
  arrange(desc(country)) %>%
  view()

# barplot showing above
MHM_Data %>% 
  drop_na(country) %>% 
  ggplot(aes(x= country))+
  geom_bar(fill = "97B3C6")+
  theme_bw()+
  labs(x = "country",
       y = NULL,
       title = "Number of MHV-Related Clinics per Country")

# same bar plot as above but omitting USA 
MHM_Data %>% 
  drop_na(country) %>% 
  filter(country != "United States") %>%
  ggplot(aes(x= country))+
  geom_bar(fill = "97B3C6")+
  theme_bw()+
  labs(x = "country",
       y = NULL,
       title = "Number of MHV-Related Clinics per Country (Exclusing USA)")

# show data with NA (for website & country variable)
MHM_Data %>%
  select(website, country) %>%
  filter(!complete.cases(.))

# omit columns with NA (for website & country variable) 
MHM_Data %>%
  select(website, country) %>%
  na.omit() 


# Note: also include the meaning of each variable
# Note: figure out how to remove NA in the dataset




