library(ggplot2)
library(dplyr)


raw_dt <- read.csv("Crash_Reporting.csv") %>%
  select(Vehicle.Movement, Injury.Severity)


# function to categorize movement type
movement_type_func <- function(movement) {
  if (movement %in% c("PARKED", "PARKING", "STOPPED IN TRAFFIC LANE", "SLOWING OR STOPPING", "STARTING FROM PARKED")) return("Stopping/Parking") #nolint
  if (movement %in% c("ACCELERATING", "BACKING", "MOVING CONSTANT SPEED", "NEGOTIATING A CURVE", "PASSING", "SKIDDING", "STARTING FROM LANE")) return("Driving") #nolint
  if (movement %in% c("MAKING LEFT TURN", "MAKING RIGHT TURN", "MAKING U TURN", "RIGHT TURN ON RED")) return("Making Turn") #nolint
  if (movement %in% c("CHANGING LANES", "ENTERING TRAFFIC LANE", "LEAVING TRAFFIC LANE")) return("Changing Lanes") #nolint
  else return("Other")
}


# data cleaning
dt <- raw_dt %>%
  group_by(Vehicle.Movement, Injury.Severity) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(movement_type = sapply(Vehicle.Movement, movement_type_func)) # summarize count and add movement type #nolint

# Plot 1 - count
dt1 <- dt %>%
  group_by(movement_type) %>%
  summarise(type_count = sum(count))

plt1 <- ggplot(dt1) +
  geom_col(aes(x = movement_type, y = type_count)) +
  labs(title = "Vehicle Movement Type vs. Injury Severity", x = "Movement Type", y = "Count") + #nolint
    theme(plot.title = element_text(size=16)) #nolint

ggsave("plot/movement_count.png", plot = plt1, width = 18, height = 14, units = "cm") # nolint
print(plt1)


# Plot 2 - drving count

dt2 <- dt %>%
  filter(movement_type == "Driving") %>% # filter type "driving"
  group_by(Vehicle.Movement) %>%
  summarise(count = sum(count))

plt2 <- ggplot(dt2) +
  geom_col(aes(x = sub(" ", "\n", Vehicle.Movement), y = count)) + #nolint
  labs(title = "Vehicle Movement vs. Injury Severity (While Driving)", x = "Vehicle Movement", y = "Count") + #nolint
  theme(plot.title = element_text(size=16), axis.text.x = element_text(angle=60, hjust=1)) #nolint

ggsave("plot/movement_driving_count.png", plot = plt2, width = 18, height = 14, units = "cm") # nolint
print(plt2)


# Plot 3 - drving proportion

dt3 <- dt %>%
  filter(movement_type == "Driving") %>% # filter type "driving"
  group_by(Vehicle.Movement) %>%
  mutate(freq = count / sum(count)) %>% # calculate frequency
  filter(Injury.Severity != "NO APPARENT INJURY") # filter out no apparent injury #nolint

plt3 <- ggplot(dt3) +
  geom_col(aes(x = sub(" ", "\n", Vehicle.Movement), y = freq, fill = dt3$Injury.Severity)) + #nolint
  labs(title = "Vehicle Movement vs. Injury Severity (While Driving)", x = "Vehicle Movement", y = "Proportions", fill = "Injury Severity") + #nolint
  theme(plot.title = element_text(size=16), legend.text=element_text(size = 7), axis.text.x = element_text(angle=60, hjust=1)) #nolint

ggsave("plot/movement_driving_prop.png", plot = plt3, width = 18, height = 14, units = "cm") # nolint
print(plt3)

print("Done")
