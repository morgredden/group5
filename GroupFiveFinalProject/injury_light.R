#### SETUP ####
setwd("/Users/Morgan/Documents/group5")

install.packages("tidyverse")
library(tidyverse)


#### DATA ####
# load data
data <- read.csv("Crash_Data.csv")

# convert to tibble
data_tbl <- as_tibble(data)

# filter data
data1 <- data_tbl %>%
  filter(Light == "DAYLIGHT" | Light == "DARK LIGHTS ON" | Light == "DARK NO LIGHTS" | Light == "DAWN" | Light == "DUSK")
  #filter(ACRS.Report.Type == "Injury Crash" | ACRS.Report.Type == "Property Damage Crash")

# # counting variable in column
# count_data <- data %>%
#   count(Light)
# print(count_data)


#### DATA VISUALIZATION ####

# calculate overall injury crash percentage
total_injury_crashes_overall <- sum(data1$ACRS.Report.Type == "Injury Crash")
total_crashes_overall <- sum(data1$ACRS.Report.Type == "Injury Crash" | data1$ACRS.Report.Type == "Property Damage Crash" | data1$ACRS.Report.Type == "Fatal Crash")
overall_injury_percent <- total_injury_crashes_overall / total_crashes_overall * 100

# calculate injury crash percentage for each light
light_injury_percent <- data1 %>%
  group_by(Light) %>%
  summarise(
    total_injury = sum(ACRS.Report.Type == "Injury Crash"),
    total_crashes = n()
  ) %>%
  mutate(percent = total_injury / total_crashes * 100)

# calculate difference from overall
light_injury_percent <- light_injury_percent %>%
  mutate(difference = percent - overall_injury_percent)

# order the light condition bar graph
light_injury_percent$Light <- factor(light_injury_percent$Light, levels = c("DAYLIGHT", "DUSK", "DAWN", "DARK LIGHTS ON", "DARK NO LIGHTS"))
levels(light_injury_percent$Light) <- c("Daylight", "Dawn", "Dusk", "Dark (Lights)", "Dark (No Lights)")

# plot
my_plt <- ggplot(light_injury_percent, aes(x = Light, y = difference, fill = Light)) +
  geom_bar(stat = "identity") +
  labs(title = "DIFFERENCE IN INJURY CRASH PERCENTAGE BY LIGHT",
       x = "Light Condition",
       y = "Difference from Overall Percentage (%)") +
  theme_minimal()

#saving
ggsave("plot/injury_light.png", plot = my_plt, width = 18, height = 14, units = "cm")
print(my_plt)
