

##### COMMUNITY TYPE VARIABLE #####

# unique variables for community_type
unique(mhm24_v1$community_type) # Note: There are no NAs

# Define the unique categories for community_type I want to create
unique_community_types <- c("Rural", "Urban", "Suburban", "Frontier", "Other")

# Count the number of clinics serving each community type
community_rural <- mhm24_v1 %>% filter(grepl("Rural", community_type, ignore.case = TRUE)) %>% select(title)
community_urban <- mhm24_v1 %>% filter(grepl("Urban", community_type, ignore.case = TRUE)) %>% select(title)
community_suburban <- mhm24_v1 %>% filter(grepl("Suburban", community_type, ignore.case = TRUE)) %>% select(title)
community_frontier <- mhm24_v1 %>% filter(grepl("Frontier", community_type, ignore.case = TRUE)) %>% select(title)
community_other <- mhm24_v1 %>% filter(grepl("Other", community_type, ignore.case = TRUE)) %>% select(title)

# Extract the number of rows (i.e., clinic counts) for each community type
counts <- c(nrow(community_rural), nrow(community_urban), nrow(community_suburban), nrow(community_frontier), nrow(community_other))

# Create a data frame with the counts
community_counts_df <- data.frame(
  community_type = unique_community_types,
  count = counts
)

# View the resulting data frame
print(community_counts_df)


# simple barplot of communities & counts
ggplot(community_counts_df, aes(x = community_type, y = count, fill = community_type)) +
  geom_bar(stat = "identity") +  # Each bar will be filled with a different color based on community_type
  labs(title = "Different Communities Served in MHM", x = "Community Type", y = "Clinics") +
  theme_minimal()




##### CARE DELIVERED VARIABLE #####
unique(mhm24_v1$care_delivered) # Note: There are no NAs

# Define the unique categories for care delivered I want to create
unique_care_types <- c("Asthma", "Behavioral Health", "Dental", "Mammography", "Maternal and Infant Health", "Pediatrics", "Preventive", "Primary Care", "Sexual and Reproductive Health", "Vision", "Other Specialties")


# Count the number of clinics serving each care type
care_Asthma <- mhm24_v1 %>% filter(grepl("Asthma", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Behavioral <- mhm24_v1 %>% filter(grepl("Behavioral Health", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Dental <- mhm24_v1 %>% filter(grepl("Dental", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Mammography <- mhm24_v1 %>% filter(grepl("Mammography", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Mom_Infant <- mhm24_v1 %>% filter(grepl("Maternal and Infant Health", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Pediatrics <- mhm24_v1 %>% filter(grepl("Pediatrics", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Preventive <- mhm24_v1 %>% filter(grepl("Preventive", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Primary <- mhm24_v1 %>% filter(grepl("Primary Care", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Sex_Repro_Health <- mhm24_v1 %>% filter(grepl("Sexual and Reproductive Health", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Vision <- mhm24_v1 %>% filter(grepl("Vision", care_delivered, ignore.case = TRUE)) %>% select(title)
care_Other <- mhm24_v1 %>% filter(grepl("Other Specialties", care_delivered, ignore.case = TRUE)) %>% select(title)


# Extract the number of rows (i.e., clinic counts) for each community type
counts_care <- c(
  nrow(care_Asthma),
  nrow(care_Behavioral),
  nrow(care_Dental),
  nrow(care_Mammography),
  nrow(care_Mom_Infant),
  nrow(care_Pediatrics),
  nrow(care_Preventive),
  nrow(care_Primary),
  nrow(care_Sex_Repro_Health),
  nrow(care_Vision),
  nrow(care_Other)
)


# Create a data frame with the counts
counts_care_df <- data.frame(
  community_care_type = unique_care_types,
  count = counts_care
)

# View the resulting data frame
print(counts_care_df)


# simple barplot 
ggplot(counts_care_df, aes(x = community_care_type, y = count, fill = community_care_type)) +
  geom_bar(stat = "identity") +  # Each bar will be filled with a different color based on community_type
  labs(title = "Types Of Care Delivered in MHM", x = "Type of Care", y = "Clinics") +
  theme_minimal()



##### POPULATION SERVED VARIABLE #####
unique(mhm24_v1$population_served) # No NAs in this!


# Define the unique population_types I want to create
unique_population_types <- c("Agricultural Workers", "Homeless/Unhoused", "Immigrants", "LGBTQ+", "Low-income", "Students", "Uninsured", "Veterans", "Other")


# Count the number of clinics serving each population type
care_Agricultural_Workers <- mhm24_v1 %>% filter(grepl("Agricultural Workers", population_served, ignore.case = TRUE)) %>% select(title)
care_Homeless_Unhoused <- mhm24_v1 %>% filter(grepl("Homeless/Unhoused", population_served, ignore.case = TRUE)) %>% select(title)
care_Immigrants <- mhm24_v1 %>% filter(grepl("Immigrants", population_served, ignore.case = TRUE)) %>% select(title)
care_LGBTQ <- mhm24_v1 %>% filter(grepl("LGBTQ+", population_served, ignore.case = TRUE)) %>% select(title)
care_Low_income <- mhm24_v1 %>% filter(grepl("Low-income", population_served, ignore.case = TRUE)) %>% select(title)
care_Students <- mhm24_v1 %>% filter(grepl("Students", population_served, ignore.case = TRUE)) %>% select(title)
care_Uninsured <- mhm24_v1 %>% filter(grepl("Uninsured", population_served, ignore.case = TRUE)) %>% select(title)
care_Veterans <- mhm24_v1 %>% filter(grepl("Veterans", population_served, ignore.case = TRUE)) %>% select(title)
care_Other <- mhm24_v1 %>% filter(grepl("Other", population_served, ignore.case = TRUE)) %>% select(title)


# Extract the number of rows for each population type
counts_population <- c(
  nrow(care_Agricultural_Workers),
  nrow(care_Homeless_Unhoused),
  nrow(care_Immigrants),
  nrow(care_LGBTQ),
  nrow(care_Low_income),
  nrow(care_Students),
  nrow(care_Uninsured),
  nrow(care_Veterans),
  nrow(care_Other)
)

# Create a data frame with the counts
counts_population_df <- data.frame(
  population_type = unique_population_types,
  count = counts_population
)

# View the resulting data frame
print(counts_population_df)


# simple barplot 
ggplot(counts_population_df, aes(x = population_type, y = count, fill = population_type)) +
  geom_bar(stat = "identity") +  # Each bar will be filled with a different color based on community_type
  labs(title = "Populations Served in MHM", x = "Population Type", y = "Clinics") +
  theme_minimal()



##### SOURCE OF FUNDING VARIABLE #####
unique(mhm24_v1$source_of_funding) # has NAs (775)
dim(mhm24_v1)

unique_funding_source <- c("Federal Government", "Insurance", "Parent Organization", "Patients Self-Pay", "Philanthropy", "State Government", "Other")


source_Federal_Government <- mhm24_v1 %>% filter(grepl("Federal Government", source_of_funding, ignore.case = TRUE)) %>% select(title)
source_Insurance <- mhm24_v1 %>% filter(grepl("Insurance", source_of_funding, ignore.case = TRUE)) %>% select(title)
source_Parent_Organization <- mhm24_v1 %>% filter(grepl("Parent Organization", source_of_funding, ignore.case = TRUE)) %>% select(title)
source_Patients_Self_Pay <- mhm24_v1 %>% filter(grepl("Patients Self-Pay", source_of_funding, ignore.case = TRUE)) %>% select(title)
source_Philanthropy <- mhm24_v1 %>% filter(grepl("Philanthropy", source_of_funding, ignore.case = TRUE)) %>% select(title)
source_State_Government <- mhm24_v1 %>% filter(grepl("State Government", source_of_funding, ignore.case = TRUE)) %>% select(title)
source_Other <- mhm24_v1 %>% filter(grepl("Other", source_of_funding, ignore.case = TRUE)) %>% select(title)


counts_funding_source <- c(
  nrow(source_Federal_Government),
  nrow(source_Insurance),
  nrow(source_Parent_Organization),
  nrow(source_Patients_Self_Pay),
  nrow(source_Philanthropy),
  nrow(source_State_Government),
  nrow(source_Other)
)


# Create a data frame with the counts
counts_funding_source_df <- data.frame(
  source_of_funding = unique_funding_source,
  count = counts_funding_source
)

# View the resulting data frame
print(counts_funding_source_df)


ggplot(counts_funding_source_df, aes(x = source_of_funding, y = count, fill = source_of_funding)) +
  geom_bar(stat = "identity") +  # Each bar will be filled with a different color based on community_type
  labs(title = "MHM Clinic Funding Sources", x = "Funding Source", y = "Clinics") +
  theme_minimal()



##### ORGANIZATION TYPE VARIABLE #####
unique(mhm24_v1$organization_type) # Note: There are no NAs
mhm24_v1[["organization_type"]]

unique_funding_source <- c("Federal Government", "Insurance", "Parent Organization", "Patients Self-Pay", "Philanthropy", "State Government", "Other")



# what even is the new_patients\"\":\"\"\"\" column
# just change it to "Other"


##### ORGANIZATION TYPE VARIABLE (IF OTHER WAS SELECTED) #####


##### ORGANIZATION COLLABS? ##### im too lazy to do this tbh.

