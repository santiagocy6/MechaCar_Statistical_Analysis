##Deliverable 1

library(dplyr)

#Read csv into DataFrame
library(tidyverse) 
mpg_table <- read.csv(file = 'MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

#Perform Linear regression using the lm() function
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mpg_table)
#Use summary() to determine the p-value and the r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mpg_table))

##Deliverable 2
#Read csv into DataFrame
suspension_table <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

#Create summary DataFrame with mean, median, variance and standard deviation of PSI
total_summary <- suspension_table %>% summarize(mean_psi = mean(PSI),
                                                median_psi = median(PSI),
                                                variance_psi = var(PSI),
                                                standard_Dev_psi = sd(PSI))

#Create a lot summary dataframe using group_by and summarize functions
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(mean_psi = mean(PSI),
                                                                              median_psi = median(PSI),
                                                                              variance_psi = var(PSI),
                                                                              standard_Dev_psi = sd(PSI))
##Deliverabl 3
#determine if psi accross manufacturing lots is different from population mean of 1500 psi using t.test()
t.test(suspension_table$PSI, mu=1500)

#three more tests using subset argument to check if different manufacturing lots have statistically different psi than 1500

