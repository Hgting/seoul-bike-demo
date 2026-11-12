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


# ============================================================
# 可视化 + 显著性分析 + 回归模型（基于已清洗的 bike_clean）
# ============================================================

# ---------- 1. 条形图：各季节总租车量 ----------
season_sum <- aggregate(`Rented Bike Count` ~ Seasons, data = bike_clean, sum)
season_sum$Seasons <- factor(season_sum$Seasons,
                             levels = c("Spring", "Summer", "Autumn", "Winter"))
season_sum <- season_sum[order(season_sum$Seasons), ]

barplot(season_sum$`Rented Bike Count`,
        names.arg = season_sum$Seasons,
        col = c("lightgreen", "gold", "orange", "lightblue"),
        main = "Total Rented Bike Count by Season",
        ylab = "Total Rented Bikes",
        las = 2)

# ---------- 2. 创建天气新变量 ----------
bike_clean$Weather <- ifelse(
  bike_clean$`Rainfall(mm)` > 0 | bike_clean$`Snowfall (cm)` > 0,
  "Bad",
  "Good"
)
bike_clean$Weather <- factor(bike_clean$Weather, levels = c("Good", "Bad"))

# 确保 Hour 为因子
bike_clean$Hour <- factor(bike_clean$Hour)

# ---------- 3. 三个散点图 ----------

# 3a. Hour vs Rented Bike Count
plot(bike_clean$Hour, bike_clean$`Rented Bike Count`,
     main = "Hour of Day vs Rented Bike Count",
     xlab = "Hour of Day",
     ylab = "Rented Bike Count",
     pch = 16,
     col = rgb(0, 0, 0, 0.15))

# 3b. Temperature vs Rented Bike Count
plot(bike_clean$Temperature, bike_clean$`Rented Bike Count`,
     main = "Temperature vs Rented Bike Count",
     xlab = "Temperature (°C)",
     ylab = "Rented Bike Count",
     pch = 16,
     col = rgb(0, 0, 0, 0.15))

# 3c. Weather vs Rented Bike Count
plot(bike_clean$Weather, bike_clean$`Rented Bike Count`,
     main = "Weather Condition vs Rented Bike Count",
     xlab = "Weather (Good = No Rain/Snow, Bad = Rain or Snow)",
     ylab = "Rented Bike Count",
     col = c("green", "red"))

# ---------- 4. 显著性排序（ANOVA） ----------
aov_hour    <- aov(`Rented Bike Count` ~ Hour, data = bike_clean)
aov_temp    <- aov(`Rented Bike Count` ~ Temperature, data = bike_clean)
aov_weather <- aov(`Rented Bike Count` ~ Weather, data = bike_clean)

p_hour    <- summary(aov_hour)[[1]]["Pr(>F)"][1, ]
p_temp    <- summary(aov_temp)[[1]]["Pr(>F)"][1, ]
p_weather <- summary(aov_weather)[[1]]["Pr(>F)"][1, ]

sig_table <- data.frame(
  Variable = c("Hour", "Temperature", "Weather"),
  P_Value  = c(p_hour, p_temp, p_weather)
)
sig_table <- sig_table[order(sig_table$P_Value), ]

cat("\n========== 变量显著性排序（P值越小越显著）==========\n")
print(sig_table)

# ---------- 5. 多元线性回归模型 ----------
model <- lm(`Rented Bike Count` ~ Hour + Temperature + Weather, data = bike_clean)
cat("\n========== 回归模型结果 ==========\n")
summary(model)
