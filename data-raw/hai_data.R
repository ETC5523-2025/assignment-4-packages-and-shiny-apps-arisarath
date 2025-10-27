## code to prepare `hai_data` dataset goes here

library(dplyr)
library(tibble)

hai_data <- tribble(~hai_type, ~hai_name, ~sample,
                    ~cases, ~cases_lower, ~cases_upper,
                    ~deaths, ~deaths_lower, ~deaths_upper,
                    ~dalys, ~dalys_lower, ~dalys_upper,
                    ~yll, ~yll_lower, ~yll_upper,
                    ~yld, ~yld_lower,~yld_upper,

                    "HAP", "Pneumonia", "PPS",
                    106586, 83618, 137476,
                    3968, 1107, 8164,
                    69508, 34042, 117232,
                    41306, 11475, 84483,
                    27539, 16528, 42824,

                    "SSI", "Surgical Site Infection", "PPS",
                    93222, 75369, 114241,
                    2328, 1888, 2882,
                    28842, 23313, 35303,
                    28376, 22983, 34714,
                    452, 352, 580,

                    "BSI", "Bloodstream Infection", "PPS",
                    26976, 16520, 42252,
                    3905, 2004, 6987,
                    58350, 30940, 104227,
                    49578, 25499, 90816,
                    8787, 4463, 16609,

                    "UTI", "Urinary Tract Infection", "PPS",
                    214150, 175086, 253524,
                    3664, 1462, 7533,
                    66701, 27890, 128543,
                    44871, 18043, 92915,
                    20243, 8095, 40522,

                    "CDI", "Clostridioides difficile", "PPS",
                    36002, 25108, 49934,
                    1917, 112, 4547,
                    20890, 2023, 49443,
                    19937, 1166, 47973,
                    977, 172, 2125,

                    "All", "Five HAIs", "PPS",
                    478222, 421350, 537787,
                    16245, 10863, 22756,
                    248920, 178693, 336239,
                    190245, 131301, 264573,
                    59076, 40263, 84578
) |>
  mutate(hai_type = factor(hai_type, levels = c("UTI", "HAP", "SSI", "CDI", "BSI", "All")),
         sample = factor(sample, levels ="PPS"))

hai_comparison <- tribble(
  ~hai_type, ~sample, ~cases_100k, ~cases_lower, ~cases_upper, ~deaths_100k,
  ~deaths_lower, ~deaths_upper, ~dalys_100k, ~dalys_lower, ~dalys_upper,

  "HAP", "German PPS", 132.0, 103.5, 170.2, 4.9, 1.4, 10.1, 86.1, 42.1, 145.1,
  "UTI", "German PPS", 265.1, 216.8, 313.9, 4.5, 1.8, 9.3, 82.6, 34.5, 159.2,
  "BSI", "German PPS", 33.4, 20.5, 52.3, 4.8, 2.5, 8.7, 72.2, 38.3, 129.0,
  "SSI", "German PPS", 115.4, 93.3, 141.4, 2.9, 2.3, 3.6, 35.7, 28.9, 43.7,
  "CDI", "German PPS", 44.6, 31.1, 61.8, 2.4, 0.1, 5.6, 25.9, 2.5, 61.2,
  "All", "German PPS", 592.1, 521.7, 665.8, 20.1, 13.4, 28.2, 308.2, 221.2, 416.3,

  "HAP", "German Convenience", 162.3, 137.5, 190.7, 6.1, 1.4, 11.7, 103.4, 51.5, 166.5,
  "UTI", "German Convenience", 228.7, 200, 260.7, 3.9, 1.6, 8, 69.5, 29.9, 127.7,
  "BSI", "German Convenience", 52.7, 42, 66.9, 7.9, 4.7, 11.8, 113.5, 72.2, 166,
  "SSI", "German Convenience", 146.9, 126.5, 167.8, 3.7, 3.2, 4.2, 45.0, 38.8, 51.3,
  "CDI", "German Convenience", 44.5, 35.6, 55.4, 2.5, 0.1, 5.3, 26.5, 2.5, 55.6,
  "All", "German Convenience", 636.1, 586.7, 689.2, 24.4, 17.2, 32.6, 359.3, 266.6, 461.5,

  "HAP", "ECDC PPS (EU/EEA)", 143.7, 136.9, 150.8, 5.3, 1.3, 10.2, 109.8, 55.3, 170.5,
  "UTI", "ECDC PPS (EU/EEA)", 174.7, 166.3, 182.4, 3.0, 1.2, 5.9, 57.1, 24.3, 102.9,
  "BSI", "ECDC PPS (EU/EEA)", 22.2, 20.0, 25.1, 3.3, 2.1, 4.6, 76.2, 52.6, 104.8,
  "SSI", "ECDC PPS (EU/EEA)", 111.3, 105.4, 116.6, 2.6, 2.4, 2.7, 35.1, 33.3, 36.8,
  "CDI", "ECDC PPS (EU/EEA)", 16.0, 14.2, 18.3, 0.9, 0.0, 1.8, 10.0, 0.9, 19.2,
  "All", "ECDC PPS (EU/EEA)", 467.9, 456.2, 480.2, 15.3, 10.2, 21.2, 290.0, 214.9, 376.9

) |>
  mutate(hai_type = factor(hai_type, levels = c("UTI", "HAP", "SSI", "CDI", "BSI", "All")),
         sample = factor(sample, levels = c("German PPS", "German Convenience", "ECDC PPS (EU/EEA)"))
         )

usethis::use_data(hai_data, hai_comparison, overwrite = TRUE)
