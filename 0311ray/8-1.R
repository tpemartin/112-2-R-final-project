install.packages("tidyverse")
library(tidyverse)
data <- read_csv("2013-2022各年度我國學生赴主要留學國家留學簽證人數統計表.csv")
print(data)

total_visas <- data %>%
  summarise(
    美國總和 = sum(美國, na.rm = TRUE),
    加拿大總和 = sum(加拿大, na.rm = TRUE),
    英國總和 = sum(英國, na.rm = TRUE),
    法國總和 = sum(法國, na.rm = TRUE),
    德國總和 = sum(德國, na.rm = TRUE),
    日本總和 = sum(日本, na.rm = TRUE),
    韓國總和 = sum(韓國, na.rm = TRUE),
    澳大利亞總和 = sum(澳大利亞, na.rm = TRUE),
    其他國家總和 = sum(其他國家, na.rm = TRUE)
  )

# 顯示總和
print(total_visas)

total_visas <- data %>%
  summarise(
    美國與加拿大總和 = sum(美國, na.rm = TRUE) + sum(加拿大, na.rm = TRUE),
    英國法國德國總和 = sum(英國, na.rm = TRUE) + sum(法國, na.rm = TRUE) + sum(德國, na.rm = TRUE),
    日本與韓國總和 = sum(日本, na.rm = TRUE) + sum(韓國, na.rm = TRUE),
    澳大利亞總和 = sum(澳大利亞, na.rm = TRUE),
    其他國家總和 = sum(其他國家, na.rm = TRUE)
  )

# 顯示總和
print(total_visas)

# 資料描述 -----
library(tidyverse)

# 資料描述函數
describe_data <- function(df) {
  description <- list()
  
  # 整體描述
  description$overview <- list(
    total_columns = ncol(df),
    total_rows = nrow(df)
  )
  
  # 逐欄描述
  for (col_name in names(df)) {
    column <- df[[col_name]]
    num_missing <- sum(is.na(column))
    missing_ratio <- num_missing / length(column)
    
    if (is.numeric(column)) {
      description[[col_name]] <- list(
        num_missing = num_missing,
        missing_ratio = missing_ratio,
        range = range(column, na.rm = TRUE),
        mean = mean(column, na.rm = TRUE),
        max = max(column, na.rm = TRUE),
        min = min(column, na.rm = TRUE),
        quantiles = quantile(column, na.rm = TRUE),
        median = median(column, na.rm = TRUE)
      )
    } else {
      unique_vals <- unique(column)
      if (length(unique_vals) < 10) {
        value_counts <- table(column)
        value_ratios <- prop.table(value_counts)
        description[[col_name]] <- list(
          num_missing = num_missing,
          missing_ratio = missing_ratio,
          value_counts = value_counts,
          value_ratios = value_ratios
        )
      } else {
        description[[col_name]] <- list(
          num_missing = num_missing,
          missing_ratio = missing_ratio,
          num_unique_values = length(unique_vals)
        )
      }
    }
  }
  
  return(description)
}

# 假設data是已存在的資料框
# 執行描述函數
data_description <- describe_data(data)

# 顯示描述結果
str(data_description)
