library(ggplot2)
library(dplyr)


raw_dt <- read.csv("Crash_Reporting.csv") %>%
  select(Vehicle.Movement, Injury.Severity)


# function to categorize movement type
movement_type_func <- function(movement) {
  if (movement %in% c("PARKED", "PARKING", "STOPPED IN TRAFFIC LANE", "SLOWING OR STOPPING", "STARTING FROM PARKED")) return("Stopping/Parking") #nolint
  if (movement %in% c("ACCELERATING", "BACKING", "DRIVERLESS MOVING VEH.", "MOVING CONSTANT SPEED", "NEGOTIATING A CURVE", "PASSING", "SKIDDING", "STARTING FROM LANE")) return("Driving") #nolint
  if (movement %in% c("MAKING LEFT TURN", "MAKING RIGHT TURN", "MAKING U TURN", "RIGHT TURN ON RED")) return("Making Turn") #nolint
  if (movement %in% c("CHANGING LANES", "ENTERING TRAFFIC LANE", "LEAVING TRAFFIC LANE")) return("Changing Lanes") #nolint
  else return("Other")
}


# data cleaning
dt <- raw_dt %>%
  mutate(movement_type = sapply(Vehicle.Movement, movement_type_func)) %>%
  group_by(movement_type, Injury.Severity) %>%
  summarise(count = n(), .groups = "drop") %>% # summarise
  group_by(movement_type) %>%
  mutate(freq = count / sum(count)) %>% # calculate frequency
  filter(Injury.Severity != "NO APPARENT INJURY") # filter out no apparent injury #nolint


#plotting
plt <- ggplot(dt) +
  geom_col(aes(x = dt$movement_type, y = dt$freq, fill = dt$Injury.Severity)) + #nolint
  labs(title = "Vehicle Movement vs. Injury Severity", x = "Vehicle Movement", y = "Frequency", fill = "Injury Severity") + #nolint
  theme(plot.title = element_text(size=16), legend.text=element_text(size = 8)) #nolint

ggsave("plot/movement.png", plot = plt, width = 18, height = 14, units = "cm")
print(plt)
print("Done")