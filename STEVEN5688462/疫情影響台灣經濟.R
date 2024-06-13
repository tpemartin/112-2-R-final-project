疫情影響台灣經濟
因為使用chatgpt用英文比較方便，所以先請chatgpt 翻譯

# 讀取CSV檔案
data <- read.csv("台灣歷年經濟狀況.csv")

# 定義中文到英文的翻譯字典
translation_dict <- c("年度" = "year", 
                      "經濟成長率" = "economic_growth", 
                      "平均每人國民所得毛額（美元）" = "average_gdp_per_capita_usd",
                      "儲蓄率" = "savings_rate",
                      "失業率（百分比）" = "unemployment_rate",
                      "產業結構（按各產業GDP比重）-農業" = "industry_structure_agriculture",
                      "產業結構（按各產業GDP比重）-工業" = "industry_structure_industry",
                      "產業結構（按各產業GDP比重）-服務業" = "industry_structure_service",
                      "產業結構（按各產業GDP比重）-服務業-金融中介業" = "industry_structure_service_finance",
                      "產業結構（按各產業GDP比重）-服務業-保險業" = "industry_structure_service_insurance",
                      "產業結構（按各產業GDP比重）-服務業-證券期貨及其它金融業" = "industry_structure_service_securities_and_other_financial",
                      "生產者物價-指數" = "producer_price_index",
                      "生產者物價-年增率" = "producer_price_index_annual_change",
                      "消費者物價-指數" = "consumer_price_index",
                      "消費者物價-年增率" = "consumer_price_index_annual_change",
                      "基本工資（金額）-月薪" = "minimum_wage_monthly_amount",
                      "基本工資（金額）-時薪" = "minimum_wage_hourly_amount",
                      "工業及服務業平均月薪資（元）" = "average_monthly_wage_industry_and_service",
                      "製造業平均月薪資（元）" = "average_monthly_wage_manufacturing",
                      "工業及服務業平均月工時（小時）" = "average_monthly_working_hours_industry_and_service")

# 取得目前資料框架的欄位名稱
current_columns <- names(taiwan_econ)

# 使用字典進行轉換
english_names <- sapply(current_columns, function(cn) {
  if (cn %in% names(translation_dict)) {
    return(translation_dict[cn])
  } else {
    return(cn)  # 若無對應的英文翻譯，則保留原名稱
  }
})

# 將欄位名稱轉換為英文
names(taiwan_econ) <- english_names

# 檢視轉換後的欄位名稱
names(taiwan_econ)


1.台灣首例covid-19確診案例於2020出現，所以將資料分為2020前3年與2020後3年並互相比較
這邊先比較經濟成長率與失業率

# 計算2020年前後三年 economic_growth 和 unemployment_rate 的平均值 -----
library(dplyr)

# 分割資料
taiwan_econ_3years_before_2020 <- taiwan_econ %>% filter(year >= 2017 & year <= 2019)
taiwan_econ_3years_after_2020 <- taiwan_econ %>% filter(year >= 2021 & year <= 2023)

# 計算平均值
avg_values <- list(
  before_2020 = taiwan_econ_3years_before_2020 %>% 
    summarise(
      avg_economic_growth = mean(economic_growth, na.rm = TRUE),
      avg_unemployment_rate = mean(unemployment_rate, na.rm = TRUE)
    ),
  after_2020 = taiwan_econ_3years_after_2020 %>% 
    summarise(
      avg_economic_growth = mean(economic_growth, na.rm = TRUE),
      avg_unemployment_rate = mean(unemployment_rate, na.rm = TRUE)
    )
)

# 顯示平均值
avg_values

# 標題 -----
# 計算2020年前後三年 economic_growth 和 unemployment_rate 的平均值 -----

可看出經濟成長率小幅上漲0.5%，失業率小幅下降0.03%，感覺疫情對此兩項資料沒什麼影響。



2.比較前後之gdp_per_capita_usd與savings_rate

# 標題 -----
# 比較2020前3年與2020後3年的 gdp_per_capita_usd 與 savings_rate -----

# 假設 taiwan_econ 已經被載入並有 "year" 欄位

# 選出2020前3年的資料
taiwan_econ_3years_before_2020 <- taiwan_econ %>% filter(year >= 2017 & year < 2020)

# 選出2020後3年的資料
taiwan_econ_3years_after_2020 <- taiwan_econ %>% filter(year >= 2020 & year <= 2023)

# 比較兩個時期的 gdp_per_capita_usd 與 savings_rate 的平均值
comparison <- tibble(
  indicator = c("gdp_per_capita_usd", "savings_rate"),
  avg_2017_2019 = c(
    mean(taiwan_econ_3years_before_2020$average_gdp_per_capita_usd, na.rm = TRUE),
    mean(as.numeric(taiwan_econ_3years_before_2020$savings_rate), na.rm = TRUE)
  ),
  avg_2020_2023 = c(
    mean(taiwan_econ_3years_after_2020$average_gdp_per_capita_usd, na.rm = TRUE),
    mean(as.numeric(taiwan_econ_3years_after_2020$savings_rate), na.rm = TRUE)
  )
)

# 檢視比較結果
print(comparison)

這個警告訊息的意思是，在將 savings_rate 欄位轉換為數字時，有些值無法被成功轉換，因而
被轉換為 NA。這通常是因為 savings_rate 欄位中包含非數字的字符（例如 "…" 或其它無效的
字符）。

我們可以使用 readr 包中的 parse_number 函數來更精確地將這些字符轉換為數字，並將無法
轉換的字符自動移除。

以下是修改後的程式碼：
# 標題 -----
# 比較2020前3年與2020後3年的 gdp_per_capita_usd 與 savings_rate -----

# 假設 taiwan_econ 已經被載入並有 "year" 欄位

# 選出2020前3年的資料
taiwan_econ_3years_before_2020 <- taiwan_econ %>% filter(year >= 2017 & year < 2020)

# 選出2020後3年的資料
taiwan_econ_3years_after_2020 <- taiwan_econ %>% filter(year >= 2020 & year <= 2023)

# 將 savings_rate 欄位轉換為數字
taiwan_econ_3years_before_2020 <- taiwan_econ_3years_before_2020 %>%
  mutate(savings_rate = parse_number(savings_rate))

taiwan_econ_3years_after_2020 <- taiwan_econ_3years_after_2020 %>%
  mutate(savings_rate = parse_number(savings_rate))

# 比較兩個時期的 gdp_per_capita_usd 與 savings_rate 的平均值
comparison <- tibble(
  indicator = c("gdp_per_capita_usd", "savings_rate"),
  avg_2017_2019 = c(
    mean(taiwan_econ_3years_before_2020$average_gdp_per_capita_usd, na.rm = TRUE),
    mean(taiwan_econ_3years_before_2020$savings_rate, na.rm = TRUE)
  ),
  avg_2020_2023 = c(
    mean(taiwan_econ_3years_after_2020$average_gdp_per_capita_usd, na.rm = TRUE),
    mean(taiwan_econ_3years_after_2020$savings_rate, na.rm = TRUE)
  )
)

# 檢視比較結果
print(comparison)

可看出疫情導致人均國內生產毛額上漲許多，可能是因為疫情導致國際經濟活動停滯，
轉而使國內生產較多物品。
亦能看出國民儲蓄率上升，可能是因為疫情導致通貨膨脹使民眾不想花錢

3.將農業、工業、服務業三項佔台灣產業佔有比率之2020前三年與後三年拿出來比較
# 標題 -----
# 比較2020前3年與2020後3年的產業結構平均值 -----

# 假設 taiwan_econ 已經被載入並有 "year" 欄位

# 選出2020前3年的資料
taiwan_econ_3years_before_2020 <- taiwan_econ %>% filter(year >= 2017 & year < 2020)

# 選出2020後3年的資料
taiwan_econ_3years_after_2020 <- taiwan_econ %>% filter(year >= 2020 & year <= 2023)

# 將 industry_structure 欄位轉換為數字
taiwan_econ_3years_before_2020 <- taiwan_econ_3years_before_2020 %>%
  mutate(
    industry_structure_agriculture = parse_number(industry_structure_agriculture),
    industry_structure_industry = parse_number(industry_structure_industry),
    industry_structure_service = parse_number(industry_structure_service)
  )

taiwan_econ_3years_after_2020 <- taiwan_econ_3years_after_2020 %>%
  mutate(
    industry_structure_agriculture = parse_number(industry_structure_agriculture),
    industry_structure_industry = parse_number(industry_structure_industry),
    industry_structure_service = parse_number(industry_structure_service)
  )

# 計算平均值
comparison <- tibble(
  indicator = c("industry_structure_agriculture", "industry_structure_industry", "industry_structure_service"),
  avg_2017_2019 = c(
    mean(taiwan_econ_3years_before_2020$industry_structure_agriculture, na.rm = TRUE),
    mean(taiwan_econ_3years_before_2020$industry_structure_industry, na.rm = TRUE),
    mean(taiwan_econ_3years_before_2020$industry_structure_service, na.rm = TRUE)
  ),
  avg_2020_2023 = c(
    mean(taiwan_econ_3years_after_2020$industry_structure_agriculture, na.rm = TRUE),
    mean(taiwan_econ_3years_after_2020$industry_structure_industry, na.rm = TRUE),
    mean(taiwan_econ_3years_after_2020$industry_structure_service, na.rm = TRUE)
  )
)

# 檢視比較結果
print(comparison)

從結果可看出疫情對台灣三項產業比率並沒有非常明顯的影響。比較能看出來的是服務業佔
有比率下降約1.2%，
推測可能是因為疫情導致實體商店無法正常營業。


4.
producer price index為是一個用來衡量製造商出廠價的平均變化的指數，它是統計部門收集
和整理的若干個物價指數中的一個。如果生產物價指數比預期數值高時，表明有通貨膨脹的風
險。如果生產物價指數比預期數值低時，則表明有通貨緊縮的風險。

consumer price index在經濟學上，是反映與居民生活有關的產品及勞務價格統計出來的物價
變動指標，以百分比變化為表達形式。它是衡量通貨膨脹的主要指標之一。

將兩項資料之2020前後三年之平均值透過chatgpt得出:
  # 計算2020年前後三年 producer_price_index 和 consumer_price_index 的平均值 -----
library(dplyr)

# 轉換列為數值型
taiwan_econ <- taiwan_econ %>%
  mutate(
    producer_price_index = as.numeric(producer_price_index),
    consumer_price_index = as.numeric(consumer_price_index)
  )

# 分割資料
taiwan_econ_3years_before_2020 <- taiwan_econ %>% filter(year >= 2017 & year <= 2019)
taiwan_econ_3years_after_2020 <- taiwan_econ %>% filter(year >= 2021 & year <= 2023)

# 計算平均值
avg_values <- list(
  before_2020 = taiwan_econ_3years_before_2020 %>% 
    summarise(
      avg_producer_price_index = mean(producer_price_index, na.rm = TRUE),
      avg_consumer_price_index = mean(consumer_price_index, na.rm = TRUE)
    ),
  after_2020 = taiwan_econ_3years_after_2020 %>% 
    summarise(
      avg_producer_price_index = mean(producer_price_index, na.rm = TRUE),
      avg_consumer_price_index = mean(consumer_price_index, na.rm = TRUE)
    )
)

# 顯示平均值
avg_values

由於我所找到的資料其在2020年前缺少producer price index 的資料，所以無法比較該項，
但是可以從另一項名為consumer price  index又稱消費者物價指數的欄位，因為其可看出通
貨膨脹或是通貨緊縮，所以由此項之2020前後三年的資料可看出物價提高約6%，而一般定義上
漲超3%即為通貨膨脹，依此得知疫情使台灣通貨膨脹，這也間接證明了我於第一點所提出的猜測。

5.將資料中的average_monthly_working_hours_indusrty_and_service(平均二、三級產業月工時)
分為2020前後三年並做比較
# 計算2020年前後三年 average_monthly_working_hours_industry_and_service 的平均值 -----
library(dplyr)

# 分割資料
taiwan_econ_3years_before_2020 <- taiwan_econ %>% filter(year >= 2017 & year <= 2019)
taiwan_econ_3years_after_2020 <- taiwan_econ %>% filter(year >= 2021 & year <= 2023)

# 計算平均值
avg_working_hours <- list(
  before_2020 = taiwan_econ_3years_before_2020 %>% 
    summarise(
      avg_monthly_working_hours = mean(average_monthly_working_hours_industry_and_service, na.rm = TRUE)
    ),
  after_2020 = taiwan_econ_3years_after_2020 %>% 
    summarise(
      avg_monthly_working_hours = mean(average_monthly_working_hours_industry_and_service, na.rm = TRUE)
    )
)

# 顯示平均值
avg_working_hours

可看出avg_working_hours也就是平均月工時下降了兩小時，可能是因為疫情導致服務業店面線上
化進而導致工時減少。



總結:經過以上資料分析可總結出台灣在2020年出現covid-19的首例後對經濟成長率與失業率沒
什麼影響。但導致人均國內生產毛額增加，儲蓄率上升。對服務業佔三項產業占比下降。導致台
灣通貨膨脹。月工時下降。

經過這次的資料分析，我學會了如何與chatgpt溝通並取的我需要的code，很高興能夠上道這
麼符合學生需求的一門課。





