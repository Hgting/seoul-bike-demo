install.packages("readxl")
library(readxl)

bike_raw <- read_xlsx("data/IDS data.xlsx")


bike_sel <- bike_raw[, c("Rented Bike Count", "Hour", "Temperature",
                         "Rainfall(mm)", "Snowfall (cm)", "Seasons")]


bike_clean <- bike_sel[complete.cases(bike_sel), ]


bike_clean <- bike_clean[bike_clean$`Rented Bike Count` >= 0, ]
bike_clean <- bike_clean[bike_clean$Temperature >= -30 & bike_clean$Temperature <= 50, ]

if(any(bike_clean$`Rainfall(mm)` >= 0)){
  bike_clean <- bike_clean[bike_clean$`Rainfall(mm)` >= 0, ]
}
if(any(bike_clean$`Snowfall (cm)` >= 0)){
  bike_clean <- bike_clean[bike_clean$`Snowfall (cm)` >= 0, ]
}

str(bike_clean)
head(bike_clean, 10)

# 按季节汇总总租赁量
season_sum <- aggregate(`Rented Bike Count` ~ Seasons, data = bike_clean, sum)

# 按季节逻辑排序
season_sum$Seasons <- factor(season_sum$Seasons,
                             levels = c("Spring", "Summer", "Autumn", "Winter"))

season_sum <- season_sum[order(season_sum$Seasons), ]

barplot(season_sum$`Rented Bike Count`,
        names.arg = season_sum$Seasons,
        col = c("lightgreen", "gold", "orange", "lightblue"),
        main = "Total Rented Bike Count by Season",
        ylab = "Total Rented Bikes",
        las = 2)

bike_clean$Weather <- ifelse(
  bike_clean$`Rainfall(mm)` > 0 | bike_clean$`Snowfall (cm)` > 0,
  "Bad",
  "Good"
)

bike_clean$Weather <- factor(bike_clean$Weather, levels = c("Good", "Bad"))

plot(bike_clean$Hour, bike_clean$`Rented Bike Count`,
     main = "Hour vs Rented Bike Count",
     xlab = "Hour of Day",
     ylab = "Rented Bike Count",
     pch = 16,
     col = rgb(0, 0, 0, 0.15))

plot(bike_clean$Temperature, bike_clean$`Rented Bike Count`,
     main = "Temperature vs Rented Bike Count",
     xlab = "Temperature (°C)",
     ylab = "Rented Bike Count",
     pch = 16,
     col = rgb(0, 0, 0, 0.15))

plot(bike_clean$Weather, bike_clean$`Rented Bike Count`,
     main = "Weather Condition vs Rented Bike Count",
     xlab = "Weather Condition",
     ylab = "Rented Bike Count",
     col = c("green", "red"))

aov_hour     <- aov(`Rented Bike Count` ~ factor(Hour), data = bike_clean)
aov_temp     <- aov(`Rented Bike Count` ~ Temperature, data = bike_clean)
aov_weather  <- aov(`Rented Bike Count` ~ Weather, data = bike_clean)

p_hour    <- summary(aov_hour)[[1]][["Pr(>F)"]][1]
p_temp    <- summary(aov_temp)[[1]][["Pr(>F)"]][1]
p_weather <- summary(aov_weather)[[1]][["Pr(>F)"]][1]

significance <- data.frame(
  Variable = c("Hour", "Temperature", "Weather"),
  P_Value  = c(p_hour, p_temp, p_weather)
)

significance <- significance[order(significance$P_Value), ]

print(significance)

model <- lm(`Rented Bike Count` ~ factor(Hour) + Temperature + Weather,
            data = bike_clean)

summary(model)

