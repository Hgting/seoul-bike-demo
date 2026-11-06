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