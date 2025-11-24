# ANALYZE with the updated regions - SHOW ACTUAL AREA for 1990
forests_1990_area <- forests %>% 
  filter(Year == 1990) %>%
  group_by(Region) %>%
  summarize(Total_Area = sum(Value)) %>%
  arrange(desc(Total_Area))

print(forests_1990_area)

# Add this after your 1990 analysis
forests_2019_area <- forests %>% 
  filter(Year == 2019) %>%
  group_by(Region) %>%
  summarize(Total_Area = sum(Value)) %>%
  arrange(desc(Total_Area))

print("2019 Forest Area:")
print(forests_2019_area)

# Create the visualization with actual area numbers for 1990
library(maps)
library(ggplot2)

world_map <- map_data("world")
romania <- world_map %>% filter(region == "Romania")

ggplot() +
  geom_polygon(data = romania, aes(x = long, y = lat, group = group), 
               fill = "beige", color = "white") +
  # Add your area labels - UPDATED with actual numbers
  annotate("text", x = 24.6, y = 46.5, label = "Transilvania", 
           size = 4, fontface = "bold", color = "darkred") +
  annotate("text", x = 24.6, y = 46.2, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Transilvania"], 0), "k ha"), 
           size = 4, color = "darkgreen") +
  
  annotate("text", x = 23.5, y = 47.7, label = "Maramureș", 
           size = 3.5, color = "darkgreen", fontface = "bold") +
  annotate("text", x = 23.5, y = 47.4, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Maramureș"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 27.0, y = 47.0, label = "Moldova", 
           size = 3.5, fontface = "bold", color = "darkgreen") +
  annotate("text", x = 27.0, y = 46.7, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Moldova"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 25.8, y = 47.7, label = "Bucovina", 
           size = 3.5, fontface = "bold", color = "darkgreen") +
  annotate("text", x = 25.8, y = 47.4, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Bucovina"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 26.2, y = 44.9, label = "Muntenia", 
           size = 3.5, fontface = "bold", color = "darkred") +
  annotate("text", x = 26.2, y = 44.6, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Muntenia"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 23.6, y = 44.7, label = "Oltenia", 
           size = 3.5, fontface = "bold", color = "darkred") +
  annotate("text", x = 23.7, y = 44.4, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Oltenia"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 21.5, y = 45.7, label = "Banat", 
           size = 3.5, fontface = "bold", color = "darkgreen") +
  annotate("text", x = 21.6, y = 45.4, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Banat"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 28.5, y = 45.1, label = "Dobrogea", 
           size = 3.5, fontface = "bold", color = "darkgreen") +
  annotate("text", x = 28.5, y = 44.8, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Dobrogea"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  annotate("text", x = 22.3, y = 46.8, label = "Crișana", 
           size = 3.5, fontface = "bold", color = "darkgreen") +
  annotate("text", x = 22.3, y = 46.5, label = paste0(round(forests_1990_area$Total_Area[forests_1990_area$Region == "Crișana"], 0), "k ha"), 
           size = 3.5, color = "darkgreen") +
  
  coord_fixed(1.3) +
  labs(title = "Forest Area by Historical Region in Romania (1990)",
       subtitle = "Area in thousands of hectares (k ha)") +
  theme_void()

# Calculate key growth metrics
growth_summary <- growth_comparison %>%
  summarize(
    National_Avg_Growth = mean(Annual_Growth_Rate),
    Highest_Growth = max(Annual_Growth_Rate),
    Lowest_Growth = min(Annual_Growth_Rate),
    Most_Stable = Region[which.min(abs(Annual_Growth_Rate - mean(Annual_Growth_Rate)))],
    Most_Volatile = Region[which.max(abs(Annual_Growth_Rate - mean(Annual_Growth_Rate)))]
  )

print("Growth Summary Statistics:")
print(growth_summary)

# Simple exponential growth projections (using your CAGR)
future_predictions_simple <- growth_comparison %>%
  mutate(
    Projected_2026 = `2019` * (1 + Annual_Growth_Rate/100)^7,   # 2019-2026 = 7 years
    Projected_2027 = `2019` * (1 + Annual_Growth_Rate/100)^8,   # 2019-2027 = 8 years
    Projected_2028 = `2019` * (1 + Annual_Growth_Rate/100)^9,   # 2019-2028 = 9 years
    Projected_2029 = `2019` * (1 + Annual_Growth_Rate/100)^10,  # 2019-2029 = 10 years
    Projected_2030 = `2019` * (1 + Annual_Growth_Rate/100)^11   # 2019-2030 = 11 years
  ) %>%
  select(Region, `2019`, starts_with("Projected"), Annual_Growth_Rate) %>%
  arrange(desc(Annual_Growth_Rate))

print("Simple Exponential Growth Predictions (2026-2030):")
print(future_predictions_simple)

# Reshape for plotting
prediction_long <- future_predictions_simple %>%
  select(Region, `2019`, starts_with("Projected")) %>%
  pivot_longer(cols = -Region, names_to = "Year", values_to = "Area") %>%
  mutate(Year = gsub("Projected_", "", Year),
         Year = gsub("2019", "2019", Year),
         Year = as.numeric(Year))

ggplot(prediction_long, aes(x = as.factor(Year), y = Area, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Area, 0)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 2.5) +
  labs(title = "Forest Area Predictions by Region (2019-2030)",
       subtitle = "Based on historical growth rates",
       x = "Year",
       y = "Forest Area (thousands hectares)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proper ANOVA test - we need the full dataset, not just summary statistics
forests_annual_growth <- forests %>%
  group_by(Region, Year) %>%
  summarize(Total_Area = sum(Value)) %>%
  group_by(Region) %>%
  mutate(Annual_Growth = (Total_Area / lag(Total_Area) - 1) * 100) %>%
  filter(!is.na(Annual_Growth))

# Now run ANOVA on the full dataset
anova_test <- aov(Annual_Growth ~ Region, data = forests_annual_growth)
print("ANOVA Test - Regional Growth Differences:")
print(summary(anova_test))



