library(dplyr)
library(tidyr)

# 整體描述資料 -----
summarize_data <- function(df) {
  # 總覽 -----
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  
  # 欄位描述函數 -----
  describe_column <- function(col) {
    missing_count <- sum(is.na(col))
    missing_prop <- missing_count / length(col)
    
    if (is.numeric(col)) {
      summary_stats <- list(
        min = min(col, na.rm = TRUE),
        max = max(col, na.rm = TRUE),
        mean = mean(col, na.rm = TRUE),
        median = median(col, na.rm = TRUE),
        q1 = quantile(col, 0.25, na.rm = TRUE),
        q3 = quantile(col, 0.75, na.rm = TRUE),
        range = range(col, na.rm = TRUE)
      )
      list(
        type = "continuous",
        missing_count = missing_count,
        missing_prop = missing_prop,
        summary_stats = summary_stats
      )
    } else if (is.factor(col) || is.character(col)) {
      col <- factor(col)
      levels_count <- length(levels(col))
      level_summary <- if (levels_count < 10) {
        levels_prop <- prop.table(table(col, useNA = "ifany"))
        list(counts = table(col, useNA = "ifany"), proportions = levels_prop)
      } else {
        list(counts = table(col, useNA = "ifany"))
      }
      list(
        type = "categorical",
        missing_count = missing_count,
        missing_prop = missing_prop,
        level_summary = level_summary
      )
    }
  }
  
  # 對每個欄位進行描述 -----
  col_descriptions <- lapply(df, describe_column)
  names(col_descriptions) <- colnames(df)
  
  # 結果存於list中 -----
  list(
    overview = list(rows = num_rows, columns = num_cols),
    columns = col_descriptions
  )
}

# 範例測試 -----
# df <- data.frame(
#   x = c(1, 2, 3, 4, 5, NA),
#   y = c("a", "b", "a", "a", "b", "c"),
#   z = c(NA, NA, 3, 4, 5, 6)
# )
# summarize_data(df)
library(dplyr)

# 整體描述資料 -----
summarize_data <- function(df) {
  # 總覽 -----
  num_rows <- nrow(df)
  num_cols <- ncol(df)
  
  # 欄位描述函數 -----
  describe_column <- function(col) {
    missing_count <- sum(is.na(col))
    missing_prop <- missing_count / length(col)
    
    if (is.numeric(col)) {
      summary_stats <- list(
        min = min(col, na.rm = TRUE),
        max = max(col, na.rm = TRUE),
        mean = mean(col, na.rm = TRUE),
        median = median(col, na.rm = TRUE),
        q1 = quantile(col, 0.25, na.rm = TRUE),
        q3 = quantile(col, 0.75, na.rm = TRUE),
        range = range(col, na.rm = TRUE)
      )
      list(
        type = "continuous",
        missing_count = missing_count,
        missing_prop = missing_prop,
        summary_stats = summary_stats
      )
    } else if (is.factor(col) || is.character(col)) {
      col <- factor(col)
      levels_count <- length(levels(col))
      level_summary <- if (levels_count < 10) {
        levels_prop <- prop.table(table(col, useNA = "ifany"))
        list(counts = table(col, useNA = "ifany"), proportions = levels_prop)
      } else {
        list(counts = table(col, useNA = "ifany"))
      }
      list(
        type = "categorical",
        missing_count = missing_count,
        missing_prop = missing_prop,
        level_summary = level_summary
      )
    }
  }
  
  # 對每個欄位進行描述 -----
  col_descriptions <- lapply(df, describe_column)
  names(col_descriptions) <- colnames(df)
  
  # 結果存於list中 -----
  list(
    overview = list(rows = num_rows, columns = num_cols),
    columns = col_descriptions
  )
}

# 範例測試 -----
# df <- data.frame(
#   x = c(1, 2, 3, 4, 5, NA),
#   y = c("a", "b", "a", "a", "b", "c"),
#   z = c(NA, NA, 3, 4, 5, 6)
# )
# summarize_data(df)

describe_data(mf00044yac)
