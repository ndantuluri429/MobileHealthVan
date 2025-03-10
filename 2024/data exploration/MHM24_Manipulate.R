##### Manipulate DATASET!! #####


# Goal 1: ensure consistency in variable types

# look at variable types of each
glimpse(mhm_24_raw_data_clean) # need to switch some column values from dbl to num for consistency

# change to numeric =  id, clinic_id, contact_phone_number, clinic_zip, clinic_geolocation_lat, clinic_geolocation_lng  
mhm_24_raw_data_clean$id <- as.numeric(mhm_24_raw_data_clean$id)
mhm_24_raw_data_clean$clinic_id <- as.numeric(mhm_24_raw_data_clean$clinic_id)
mhm_24_raw_data_clean$contact_phone_number <- as.numeric(mhm_24_raw_data_clean$contact_phone_number)
mhm_24_raw_data_clean$clinic_geolocation_lat <- as.numeric(mhm_24_raw_data_clean$clinic_geolocation_lat)
mhm_24_raw_data_clean$clinic_geolocation_lng <- as.numeric(mhm_24_raw_data_clean$clinic_geolocation_lng)


# check if the columns have been successfully switched to numeric
sapply(mhm_24_raw_data_clean[, c("id", "clinic_id", "contact_phone_number", 
                 "clinic_geolocation_lat", "clinic_geolocation_lng")], class) # successful



# Goal 2: only include "publish" status clinics

# unique values for "publish"
unique(mhm_24_raw_data_clean$status) # are publish, draft, 0

# remove all data points that are "draft"
mhm_24_raw_data_clean_publish_only <- mhm_24_raw_data_clean %>%
  filter(status != "draft" & status != "0" & title != "Perky Paws Pet Hospital") 

unique(mhm_24_raw_data_clean_publish_only$status) # only publish are present
View(mhm_24_raw_data_clean_publish_only) # perky paws is removed!

length(mhm_24_raw_data_clean_publish_only$title) # 1346 clinics w/ title




# goal 3: only include US clinics

# unique countries represented
unique(mhm_24_raw_data_clean_publish_only$clinic_country) # 27 countries + NA (28 total)


# FILTER TIME!

# Step 1: Select clinics with "United States" or NA in clinic_country
select_data1 <- mhm_24_raw_data_clean_publish_only %>% 
  filter(clinic_country %in% c("United States", NA))
dim(select_data1) # 1292 total rows
View(select_data1)

# Step 2: Extract clinics with "United States" in clinic_country
select_data2 <- select_data1 %>% 
  filter(clinic_country == "United States")
dim(select_data2) # 1257 total rows

# Step 3: Remaining data points with NA in clinic_country
na_clinics1 <- select_data1 %>% 
  filter(is.na(clinic_country))
dim(na_clinics1) # 35 total rows

# Step 4: Further filter NA clinics with non-empty address fields
filtered_na_clinics <- na_clinics1 %>% 
  filter(!is.na(clinic_street_address) | 
           !is.na(clinic_address_line_2) | 
           !is.na(clinic_city) | 
           !is.na(clinic_state) | 
           !is.na(clinic_zip) | 
           !is.na(clinic_geolocation_address))
dim(filtered_na_clinics) # 9 total rows
View(filtered_na_clinics)

# Step 5: combine results to get the final filtered dataset

mhm24_v1 <- bind_rows(select_data2, filtered_na_clinics)
dim(mhm24_v1) # 1266 total rows 





# goal 4: are there any duplicates?

# duplicates = more accurate to go for the id bc it is specific to each clinic

# duplicate clinic names?
View(get_dupes(mhm24_v1, title)) # yes but its bc 1 org can have multiple clincics in different areas

# duplicate clinic addresses?
View(get_dupes(mhm24_v1, clinic_street_address))


csa_dupe_table <- mhm24_v1 %>% 
  filter(!is.na(clinic_street_address)) %>%  # Ensure you're not including NAs
  group_by(clinic_street_address) %>%  # Group by the column of interest
  filter(n() > 1) %>%  # Keep only groups with more than one occurrence
  summarise(count = n()) %>%  # Count the number of duplicates
  arrange(desc(count))  # Arrange in descending order of counts

view(csa_dupe_table)  # Display the results in a viewer
print(csa_dupe_table) 

# compare address to the clinic names
mhm24_v1 %>% 
  inner_join(csa_dupe_table, by = "clinic_street_address") %>% 
  select(title, clinic_street_address, count) %>% 
  View() # mitgate by ensuring 1 of each address type

# what about non-address duplicates?

# Filter the rows with clinic_street_address equal to "4"
rows_with_4_address <- mhm24_v1 %>% 
  filter(clinic_street_address == "4")

# View the result
View(rows_with_4_address) # have a geolocation address but it would be more accurate to look @ lat+lng


# select duplicate values for clinic_geolocation_lat & clinic_geolocation_lng 
duplicate_geolocation <- mhm24_v1 %>%
  group_by(clinic_geolocation_lat, clinic_geolocation_lng) %>%
  filter(n() > 1) %>%  # Keep only groups with more than one occurrence
  arrange(clinic_geolocation_lat, clinic_geolocation_lng) %>%  # Optional: Arrange by lat and lng
  ungroup()  # Remove grouping
View(duplicate_geolocation) # too many, count via n > 1


# Count each unique combination of clinic_geolocation_lat and clinic_geolocation_lng
geolocation_count_table <- mhm24_v1 %>%
  group_by(clinic_geolocation_lat, clinic_geolocation_lng) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%  # Keep only combinations with more than one occurrence
  arrange(desc(count)) %>%
  ungroup()
View(geolocation_count_table) # 0.0000, NA, misc. 
# next step: duplicate lat+lng can be removed to only include 1


### Deal with duplicates that are 0 or NA 

# Filter data points with latitude and longitude both equal to 0.00000
zero_geolocation_data <- mhm24_v1 %>%
  filter(clinic_geolocation_lat == 0.00000 & clinic_geolocation_lng == 0.00000)
View(zero_geolocation_data) # 21 are 0,0 coordinates


# Find duplicates based on clinic_geolocation_address in zero_geolocation_data
duplicate_geolocation_address <- zero_geolocation_data %>%
  group_by(clinic_geolocation_address) %>% # though this is no lat&lng, there is an address
  filter(n() > 1) %>% 
  arrange(clinic_geolocation_address) %>% 
  ungroup() 
View(duplicate_geolocation_address) # clinic w/ id 11576 & 11576 are same thing


# Filter data points with latitude and longitude NA
na_geolocation_data <- mhm24_v1 %>%
  filter(is.na(clinic_geolocation_lat) & is.na(clinic_geolocation_lng))
View(na_geolocation_data) # 9 are NA

# mhm24_v1 is perf for visuals!
# mhm24_v2 can be dedicated to dashbord only

# Note: remove clinics w/ invalid coordinates for dashboard


View(mhm24_v1)

