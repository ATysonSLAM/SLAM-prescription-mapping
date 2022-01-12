# Adele Tyson, SLaM, Adele.Tyson@slam.nhs.uk
# 02/11/2021
# Displays cost of mental health drugs by GP practice and finds cost per
# practice for given year

################################################################################

# Load data
prescriptions <- read_excel(path = "Data_outputs/GP-drugs.xlsx")

################################################################################

# Annual cost for one practice

# Extract one GP practice and find annual cost of each drug
cost_annual_practice <- prescriptions %>%
  filter(row_name == "QUEENS ROAD SURGERY") %>%
  group_by(year, row_name, drug_full, drug_class_full) %>%
  summarise(annual_cost = sum(actual_cost))

# Plot annual cost of each drug for one practice
# NB: using log base 10 of y axis to aid visual interpretation
ggplot(cost_annual_practice, aes(x = year, 
                                 y = annual_cost, 
                                 group = drug_full, 
                                 colour = drug_full)) +
                                 #colour = drug_class_full)) + #Alt grouping
  geom_line() +
  geom_point() +
  labs(title = paste0("Cost of MH drugs for ", cost_annual_practice$row_name),
       x = "Year",
       y = "Annual cost (?)")

################################################################################

# Annual cost across all practices

# Find annual cost of each drug
cost_annual_all <- prescriptions %>%
  group_by(year, drug_full, drug_class_full) %>%
  summarise(annual_cost = sum(actual_cost))

# Plot annual cost of each drug for one practice
# NB: using log base 10 of y axis to aid visual interpretation
ggplot(cost_annual_all, aes(x = year, 
                               y = annual_cost, 
                               group = drug_full, 
                               colour = drug_full)) +
                               #colour = drug_class_full)) + #Alt grouping
  geom_line() +
  geom_point() +
  labs(title = paste0("Cost of MH drugs for all practices"),
       x = "Year",
       y = "Annual cost (?)")

################################################################################

# Cost per practice
# For now do 2019 but may want to add functionality for choice of year or
# all years later
# For now do sum across all drugs but may want to add functionality there too


cost_prac <- prescriptions %>%
  filter(year == "2019") %>%
  group_by(year, row_name) %>% #group_by(year, row_name, drug_full, drug_class_full)
  summarise(annual_cost = sum(actual_cost))


################################################################################
################################################################################
