library(ggplot2)
library(dplyr)
library(patchwork)

# Focusing data only on speed limit and injury severity: 
speedLimitData <- read.csv("Crash_Reporting.csv") %>%
  select(Speed.Limit, Injury.Severity)

# Preprocessing: 
dt <- speedLimitData %>%
  # Filtering out accidents that caused no injury: 
  filter(Injury.Severity != "NO APPARENT INJURY") %>%
  # Calculating the frequencies of each accident at 
  # a specified speed limit: 
  group_by(Speed.Limit, Injury.Severity) %>%
  summarise(count = n(), .groups = "drop") %>% 
  group_by(Speed.Limit) %>%
  mutate(freq = count / sum(count))

plt <- ggplot(data = dt) +
  geom_col(mapping=aes(x = dt$Speed.Limit, y = dt$freq, fill = dt$Injury.Severity)) +
  labs(title = "Speed Limit vs Injury Severity", x = "Speed Limit", y = "Frequency of Injuries") +
  theme(plot.title = element_text(size = 16),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
plt

fatalInjuriesPlt <- ggplot(data = filter(dt, Injury.Severity == "FATAL INJURY")) +
  geom_col(mapping = aes(x = Speed.Limit, y = freq, fill = Injury.Severity)) +
  scale_fill_manual(values = c("FATAL INJURY" = "red")) +
  labs(title = paste("Speed Limit vs Frequency of Fatal Injuries"),
       x = "Speed Limit", y = "Frequency") +
  theme(plot.title = element_text(size = 16),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
fatalInjuriesPlt

possibleInjuriesPlt <- ggplot(data = filter(dt, Injury.Severity == "POSSIBLE INJURY")) +
  geom_col(mapping = aes(x = Speed.Limit, y = freq, fill = Injury.Severity)) +
  scale_fill_manual(values = c("POSSIBLE INJURY" = "orange")) +
  labs(title = paste("Speed Limit vs Frequency of Possible Injuries"),
       x = "Speed Limit", y = "Frequency") +
  theme(plot.title = element_text(size = 16),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
possibleInjuriesPlt

suspectedMinorInjuriesPlt <- ggplot(data = filter(dt, Injury.Severity == "SUSPECTED MINOR INJURY")) +
  geom_col(mapping = aes(x = Speed.Limit, y = freq, fill = Injury.Severity)) +
  scale_fill_manual(values = c("SUSPECTED MINOR INJURY" = "blue")) +
  labs(title = paste("Speed Limit vs Frequency of Suspected Minor Injuries"),
       x = "Speed Limit", y = "Frequency") +
  theme(plot.title = element_text(size = 16),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
suspectedMinorInjuriesPlt

suspectedSeriousInjuriesPlt <- ggplot(data = filter(dt, Injury.Severity == "SUSPECTED SERIOUS INJURY")) +
  geom_col(mapping = aes(x = Speed.Limit, y = freq, fill = Injury.Severity)) +
  scale_fill_manual(values = c("SUSPECTED SERIOUS INJURY" = "purple")) +
  labs(title = paste("Speed Limit vs Frequency of Suspected Serious Injuries"),
       x = "Speed Limit", y = "Frequency") +
  theme(plot.title = element_text(size = 16),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
suspectedSeriousInjuriesPlt

# Grid of graphs for all four different injury types: 
plots <- list(fatalInjuriesPlt, possibleInjuriesPlt, suspectedMinorInjuriesPlt, suspectedSeriousInjuriesPlt)
grid <- wrap_plots(plots, ncol = 2)

ggsave("plot/speedGrid.png", plot = grid, width = 18, height = 14, units = "cm")
