#parse_LLMWhisperer_PDF.R
#17-Aug-2025

dir.C <- "C:"
dir.app <- file.path(dir.C,"GoogleDrive_MyDrive","scripts","RProject_Shinyapp_data-in-everyday-lives")
dir.extracted.text <- file.path(dir.app,"electricity-bill","unstract-LLMWhisperer-extracted-text")
dir.urban.utility.extracted.text <- file.path(dir.app,"Urban-utilities-bill","unstract-LLMWhisperer-extracted-text")
setwd(dir.app)

#---------------------------------------------------------------------------------------------
# Tabulate extracted text in a similar structure as the "Balance brought forward" table in PDF
## This example works on single file
#---------------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Load the extracted text file
txt <- readLines(con = file.path(dir.extracted.text,"Alinta-energy-bill_supply-period_20250701-20250731.txt")) # class(txt) [1] "character"

txt_all <- paste(txt, collapse = " ")

# Usage (kWh)
solar_kwh <- str_match(txt_all, "Solar\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
peak_kwh  <- str_match(txt_all, "Peak\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
ctl1_kwh  <- str_match(txt_all, "Controlled load 1\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]

# Amount due
amount_due <- str_match(txt_all, "Amount due\\s+\\$([0-9.]+)")[,2]

supply_period <- str_match(txt_all, "Supply period.*?([0-9]{1,2}\\s\\w+\\s[0-9]{4}).*?([0-9]{1,2}\\s\\w+\\s[0-9]{4})")
supply_start <- supply_period[,2]
supply_end   <- supply_period[,3]

# Save data as a data.frame
bill_df <- data.frame(
  supply_start = supply_start,
  supply_end   = supply_end,
  solar_kwh = as.numeric(solar_kwh),
  peak_kwh  = as.numeric(peak_kwh),
  ctl1_kwh  = as.numeric(ctl1_kwh),
  amount_due = as.numeric(amount_due),
  stringsAsFactors = FALSE )
# supply_start  supply_end solar_kwh peak_kwh ctl1_kwh amount_due
# 1  01 Jul 2025 31 Jul 2025    289.36   522.68   154.37     211.91

#---------------------------------------------------------------------------------------------
# Tabulate extracted text in a similar structure as the "Balance brought forward" table in PDF
## This example works on all txt files
#---------------------------------------------------------------------------------------------
# Get file paths
file.paths.txt.files <- list.files(path = dir.extracted.text, pattern = "\\.txt$", full.names = TRUE) # length(file.paths.txt.files) 12

# Initialize empty list to store results
all_data <- list()

# Loop through each file
for (file in file.paths.txt.files) {
  # file <- file.paths.txt.files[2]
  # Example: read lines from each file
  txt_content <- readLines(file, warn = FALSE)
  txt_all <- paste(txt_content, collapse = " ")
  
  # Extract Supply period
  supply_period <- stringr::str_match(txt_all, "Supply period.*?([0-9]{1,2}\\s\\w+\\s[0-9]{4}).*?([0-9]{1,2}\\s\\w+\\s[0-9]{4})")
  supply_start <- lubridate::dmy(supply_period[,2])
  supply_end   <- lubridate::dmy(supply_period[,3])
  
  # Extract Tariff Solar Usage kWh
  solar_kwh <- stringr::str_match(txt_all, "Solar\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
  
  # Extract Tariff Peak Usage kWh or Time of Use Usage kWh
  #peak_kwh  <- stringr::str_match(txt_all, "Peak\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
  peak_kwh <- stringr::str_match(
    txt_all,
    "(Peak|Time of use)\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)"
  )[,3]
  
  # Extract Tariff Controlled load 1 Usage kWh
  ctl1_kwh  <- stringr::str_match(txt_all, "Controlled load 1\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
  
  # Extract Amount due
  amount_due <- stringr::str_match(txt_all, "Amount due\\s+\\$([0-9.]+)")[,2]
  
  # Save data as a data.frame
  df <- data.frame(
    filename = basename(file)
    ,supply_start = supply_start
    ,supply_end   = supply_end
    ,solar_kwh = as.numeric(solar_kwh)
    ,peak_kwh  = as.numeric(peak_kwh)
    ,ctl1_kwh  = as.numeric(ctl1_kwh)
    ,amount_due = as.numeric(amount_due)
    ,stringsAsFactors = FALSE ) # dim(df) 1 7
  
  # Append to list
  all_data[[length(all_data) + 1]] <- df
}

# Combine all data.frames into one
balance.brought.forward <- do.call(rbind, all_data) # dim(balance.brought.forward) 12 7

# Reshape data for plotting
balance.brought.forward.long <- dplyr::select(balance.brought.forward, supply_start, peak_kwh, ctl1_kwh, solar_kwh) %>%
  tidyr::pivot_longer(
    cols = c(peak_kwh, ctl1_kwh, solar_kwh)
    ,names_to = "type"
    ,values_to = "kwh"
    ) %>%
  dplyr::mutate(
    type = dplyr::recode(type,
                         peak_kwh = "Energy usage - all hours",
                         ctl1_kwh = "Energy usage - hot water systems",
                         solar_kwh = "Solar export"),
    month_year = format(lubridate::ymd(supply_start), "%b %Y"),
    month_year_factor = forcats::fct_reorder(month_year, lubridate::ymd(supply_start))
  ) # dim(balance.brought.forward.long) 36 5

# Prepare data
stacked_data <- balance.brought.forward.long %>%
  dplyr::filter(type %in% c("Energy usage - all hours", "Energy usage - hot water systems")) %>%
  dplyr::mutate(group = "Usage")

solar_data <- balance.brought.forward.long %>%
  dplyr::filter(type == "Solar export") %>%
  dplyr::mutate(group = "Solar")

# Combine and prepare X-axis ordering
plot_data <- dplyr::bind_rows(stacked_data, solar_data) %>%
  dplyr::mutate(
    month_year = format(lubridate::ymd(supply_start), "%b %Y"),
    sort_ym   = lubridate::ymd(paste0(format(lubridate::ymd(supply_start), "%Y-%m"), "-01")),
    plot_fill = type  # this variable will control legend
  ) %>%
  dplyr::arrange(sort_ym)

# Create factor for chronological plotting
plot_data$month_year_factor <- factor(plot_data$month_year, levels = unique(plot_data$month_year))
# dim(plot_data) 36 8

# Export to TSV
readr::write_tsv(balance.brought.forward, "data/alinta_bills_balance_brought_forward.tsv")
readr::write_tsv(plot_data, "data/plot_data_alinta_bills_balance_brought_forward.tsv")

#------------------------------------------------------------------------------------------------------------------
# Tabulate extracted text in a similar structure as the "Usage, supply charges and applicable credits" table in PDF
## This example works on all txt files
#------------------------------------------------------------------------------------------------------------------

# function version 14 (working on file 1 to 12)
extract_usage_supply_charges_and_credits <- function(file) {
  library(stringr)
  
  # 1) Read
  raw <- readLines(file, warn = FALSE)
  if (!length(raw)) return(data.frame())
  
  # 2) Locate table start
  start_ix <- which(str_detect(raw, regex("Usage,\\s*supply charges and applicable credits",
                                          ignore_case = TRUE)))
  if (!length(start_ix)) return(data.frame())
  
  lines <- raw[seq(from = start_ix[1], to = length(raw))]
  
  # 3) Pre-clean: remove header labels
  strip_labels <- function(s) {
    s %>%
      str_replace(regex("Usage,\\s*supply charges and applicable credits", ignore_case=TRUE), "") %>%
      str_replace(regex("\\bQuantity\\b", ignore_case=TRUE), "") %>%
      str_replace(regex("\\bRate\\s*Incl\\.?\\s*GST\\b", ignore_case=TRUE), "") %>%
      str_replace(regex("\\bTotal\\s*Incl\\.?\\s*GST\\b", ignore_case=TRUE), "") %>%
      str_squish()
  }
  lines <- vapply(lines, strip_labels, character(1))
  
  # 4) Drop obvious non-row noise (date range line, blank)
  is_range_line <- function(s) {
    str_detect(
      s,
      regex("\\b\\d{1,2}\\s+[A-Za-z]{3}\\s+\\d{2,4}\\s+to\\s+\\d{1,2}\\s+[A-Za-z]{3}\\s+\\d{2,4}\\b.*\\(\\d+\\s*Days?\\)",
            ignore_case = TRUE)
    )
  }
  lines <- lines[lines != "" & !vapply(lines, is_range_line, logical(1))]
  
  # 5) Patterns for qty / rate / total
  qty_re   <- "(?i)\\d+\\.?\\d*\\s*(?:kW|kWh|days)"
  rate_re  <- "-?\\$\\d+\\.\\d{2,5}"
  total_re <- "\\$-?\\d+\\.\\d{2}(?:\\s*cr)?"
  
  full_re  <- paste0("^(.*?)\\s+(", qty_re, ")\\s+(", rate_re, ")\\s+(", total_re, ")\\s*$")
  tail_re  <- paste0("^(", qty_re, ")\\s+(", rate_re, ")\\s+(", total_re, ")\\s*$")
  
  results <- list()
  buf <- NULL  # holds a pending Item line
  
  # -------------------------------
  # CHANGED: maintain a single vector of recognized item names
  # Add new values like Off Peak, Shoulder, etc.
  item_names <- c(
    "Demand",
    "Controlled Load 1",
    "Daily Charge",
    "Daily Charge - Controlled Load 1",
    "Standard Solar",
    "Peak",
    "Off Peak",
    "Shoulder",
    "Rounding Adjustment",
    "Total",
    "Amount due"
  )
  item_re <- paste0("(", paste(item_names, collapse = "|"), ")")
  # -------------------------------
  
  add_row <- function(item, qty, rate, total) {
    item_norm <- str_replace(item, regex("^\\s*Demand\\b.*", ignore_case = TRUE), "Demand")
    # Skip Rounding Adjustment rows
    if (str_detect(item_norm, regex("^Rounding Adjustment$", ignore_case = TRUE))) return()
    
    results[[length(results) + 1]] <<- data.frame(
      Item            = item_norm,
      Quantity        = qty,
      Rate.Incl.GST   = rate,
      Total.Incl.GST  = total,
      IsCredit        = str_detect(total, regex("\\bcr\\b", ignore_case = TRUE)),
      filename        = basename(file),
      stringsAsFactors = FALSE
    )
  }
  
  for (ln in lines) {
    if (ln == "") next
    
    # Case A: full row
    m <- str_match(ln, full_re)
    if (!is.na(m[1])) {
      item <- str_squish(m[2]); qty <- m[3]; rate <- m[4]; total <- m[5]
      add_row(item, qty, rate, total)
      buf <- NULL
      next
    }
    
    # Case B: tail only
    m2 <- str_match(ln, tail_re)
    if (!is.na(m2[1]) && !is.null(buf)) {
      qty <- m2[2]; rate <- m2[3]; total <- m2[4]
      add_row(buf, qty, rate, total)
      buf <- NULL
      next
    }
    
    # Case C: Item-only line
    if (str_detect(ln, regex(paste0("^", item_re), ignore_case = TRUE)) &&
        !str_detect(ln, "\\$\\d")) {
      buf <- str_squish(ln)
      next
    }
    
    # Case D: salvage messy line with recognizable item
    if (str_detect(ln, "\\$\\d") && str_detect(ln, regex(item_re, ignore_case = TRUE))) {
      monies <- str_extract_all(ln, "\\$-?\\d+\\.\\d{2,5}(?:\\s*cr)?")[[1]]
      if (length(monies) >= 2) {
        total <- monies[length(monies)]
        rate  <- monies[length(monies)-1]
        ln_nom <- str_replace(ln, paste0("\\s*", rate, "\\s*", total, "\\s*$"), "")
        qtys <- str_extract_all(ln_nom, qty_re)[[1]]
        if (length(qtys) >= 1) {
          qty <- qtys[length(qtys)]
          item <- str_squish(str_replace(ln_nom, paste0("\\s*", qty, "\\s*$"), ""))
          add_row(item, qty, rate, total)
          buf <- NULL
          next
        }
      }
    }
  }
  
  if (!length(results)) return(data.frame())
  do.call(rbind, results)
}

# Run the function over txt file 12 to 1
parsed_list <- lapply(file.paths.txt.files[c(12:1)], extract_usage_supply_charges_and_credits)

lapply(parsed_list, dim)   # check rows per file

usage.rates.total.credits <- do.call(rbind, parsed_list) %>%
  dplyr::mutate(
    # extract supply start date from file names
    supply_start = stringr::str_extract(filename, "\\d{8}")
    ,supply_start = lubridate::ymd(supply_start)
    ,qty_num = readr::parse_number(Quantity)
    # Remove "$" and convert to numeric
    ,Rate.Incl.GST_num=as.numeric(gsub("\\$", "", Rate.Incl.GST))
    ,total_num=readr::parse_number(Total.Incl.GST)
    ,rate_clean = gsub("\\$", "", Rate.Incl.GST)
    ,rate_clean = gsub("cr", "", rate_clean, ignore.case = TRUE)
    ,rate_clean = trimws(rate_clean)
    ,rate_num = as.numeric(rate_clean)
  )
# dim(usage.rates.total.credits) 79 12

# Export rate summary wide data to TSV
readr::write_tsv(usage.rates.total.credits, "data/alinta_bills_usage_rates_total_credits.tsv")

#----------------------------------------------------------------------------------------------------------------
# Collapse multiple periods to single period per supply period
## e.g., Alinta-energy-bill_supply-period_20240920-20241130.pdf has two periods
### 20 Sep 24 to 17 Nov 24 (59 Days)
### 18 Nov 24 to 30 Nov 24 (13 Days)
#### each has its own Controlled Load 1, Daily Charge, Daily Charge - Controlled Load 1, Standard Solar, and Peak
#----------------------------------------------------------------------------------------------------------------
fmt_rate <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (x < 0) {
    paste0("-$", formatC(abs(x), format = "f", digits = 5))
  } else {
    paste0("$", formatC(x, format = "f", digits = 5))
  }
}

# Summarise data to one Item per supply period
## There are separate periods in 2 bills
usage.rates.total.credits.summary.by.item <- usage.rates.total.credits %>%
  dplyr::group_by(filename, Item, IsCredit) %>%
  dplyr::summarise(
     Quantity = if (all(grepl("days", Quantity))) {
      paste(sum(qty_num, na.rm = TRUE), "days")
    } else if (all(grepl("kWh", Quantity))) {
      paste(sum(qty_num, na.rm = TRUE), "kWh")
    } else if (all(grepl("KW", Quantity))) {
      paste(sum(qty_num, na.rm = TRUE), "KW")
    } else {
      as.character(sum(qty_num, na.rm = TRUE))
    },
    Rate.Incl.GST = {
      nums <- unique(na.omit(rate_num))
      if (length(nums) == 1) {
        fmt_rate(nums)
      } else if (length(nums) > 1) {
        rate <- sum(rate_num * qty_num, na.rm = TRUE) / sum(qty_num, na.rm = TRUE)
        fmt_rate(rate)
      } else {
        NA_character_
      }
    },
    Total.Incl.GST = {
      is_credit <- any(grepl("cr", Total.Incl.GST, ignore.case = TRUE))
      total <- sum(readr::parse_number(Total.Incl.GST), na.rm = TRUE)
      if (is_credit) {
        paste0("-$", formatC(total, format = "f", digits = 2))
      } else {
        paste0("$", formatC(total, format = "f", digits = 2))
      }
    },
    .groups = "drop"
  ) # dim(usage.rates.total.credits.summary.by.item) 66 6

usage.rates.total.credits.summary.by.item <- usage.rates.total.credits %>%
  dplyr::group_by(filename, Item, IsCredit) %>%
  dplyr::summarise(
    Quantity = if (all(grepl("days", Quantity))) {
      paste(sum(qty_num, na.rm = TRUE), "days")
    } else if (all(grepl("kWh", Quantity))) {
      paste(sum(qty_num, na.rm = TRUE), "kWh")
    } else if (all(grepl("KW", Quantity))) {
      paste(sum(qty_num, na.rm = TRUE), "KW")
    } else {
      as.character(sum(qty_num, na.rm = TRUE))
    },
    Rate.Incl.GST = {
      nums <- unique(na.omit(rate_num))
      if (length(nums) == 1) {
        fmt_rate(nums)
      } else if (length(nums) > 1) {
        rate <- sum(rate_num * qty_num, na.rm = TRUE) / sum(qty_num, na.rm = TRUE)
        fmt_rate(rate)
      } else {
        NA_character_
      }
    },
    Rate.Incl.GST_num = {
      nums <- unique(na.omit(rate_num))
      if (length(nums) == 1) {
        nums
      } else if (length(nums) > 1) {
        sum(rate_num * qty_num, na.rm = TRUE) / sum(qty_num, na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    Total.Incl.GST = {
      is_credit <- any(grepl("cr", Total.Incl.GST, ignore.case = TRUE))
      total <- sum(readr::parse_number(Total.Incl.GST), na.rm = TRUE)
      if (is_credit) {
        paste0("-$", formatC(total, format = "f", digits = 2))
      } else {
        paste0("$", formatC(total, format = "f", digits = 2))
      }
    },
    .groups = "drop"
  )
# dim(usage.rates.total.credits.summary.by.item) 66 7

# Reshape Rate.Incl.GST (character) and Rate.Incl.GST_num (numeric) to wide format
rates.summary.by.item.wide <- usage.rates.total.credits.summary.by.item %>%
  dplyr::mutate(
    Item_clean = stringr::str_replace_all(Item, "[ .-]", "_"),
    supply_start_chr = as.character(supply_start)
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(filename, supply_start_chr),   # <-- include filename
    names_from = Item_clean,
    values_from = c(Rate.Incl.GST, Rate.Incl.GST_num),
    names_glue = "{stringr::str_replace_all(.value, '[ .]', '_')}_{Item_clean}",
    values_fn = dplyr::first
  ) %>%
  dplyr::rename_with(~ stringr::str_replace_all(., "_+", "_")) %>%
  dplyr::mutate(supply_start = as.Date(supply_start_chr)) %>%
  dplyr::select(-supply_start_chr)
# dim(rates.summary.by.item.wide) 11 18

# Export rate summary wide data to TSV
readr::write_tsv(rates.summary.by.item.wide, "data/alinta_bills_rates_over_supply_period.tsv")

#---------------------------------------------------------------------------------------------
# Tabulate extracted text in a similar structure as the "Your meter readings" table in PDF
## Locate table start with text "Your meter readings"
## Locate table end with text "Water Usage"
## Extracted table looks like
### SerialNumber	ReadDate	Reading	Usage	Comment
### ADC1511588	  13/06/2024	1692	NA	  NA
### NA	          10/09/2024	1716	24KL	NA

## This example works on single file
#---------------------------------------------------------------------------------------------
library(stringr)
library(dplyr)
library(tidyr)

# Get file paths
file.paths.txt.files.urban.utilities <- list.files(path = dir.urban.utility.extracted.text, pattern = "\\.txt$", full.names = TRUE) # length(file.paths.txt.files.urban.utilities) 2

# Read text file
txt <- readLines(file.paths.txt.files.urban.utilities[1])

# Locate start ("Serial Number") and end ("Water Usage")
start <- grep("^\\s*Serial Number", txt)
end   <- grep("^\\s*Water Usage", txt)

# Extract block
meter_block <- txt[start:(end-1)] %>%
  str_trim() %>%
  .[. != ""]

# Remove header and irrelevant lines
meter_block_clean <- meter_block[!str_detect(meter_block, regex("Serial Number|Your usage|kilolitres|Water Usage", ignore_case = TRUE))]
meter_block_clean <- str_squish(meter_block_clean)  # remove extra spaces

# Initialize empty tibble
meter_tbl <- tibble(
  Serial.Number = character(),
  Read.Date = as.Date(character()),
  Reading = numeric(),
  Usage = numeric(),
  Comment = character()
)

serial_current <- NA_character_

for (line in meter_block_clean) {
  
  # Split line into fields
  fields <- str_split(line, "\\s+")[[1]]
  
  # If first field looks like a Serial Number (letters/numbers, not a date)
  if (!str_detect(fields[1], "^\\d{2}/\\d{2}/\\d{4}$")) {
    serial_current <- fields[1]
    read_date <- dmy(fields[2])
    reading <- as.numeric(fields[3])
    usage <- ifelse(length(fields) >= 4, as.numeric(str_remove(fields[4], "KL")), NA_real_)
  } else {
    # First field is a date, Serial Number missing → use previous
    read_date <- dmy(fields[1])
    reading <- as.numeric(fields[2])
    usage <- ifelse(length(fields) >= 3, as.numeric(str_remove(fields[3], "KL")), NA_real_)
  }
  
  meter_tbl <- bind_rows(meter_tbl, tibble(
    Serial.Number = serial_current,
    Read.Date = read_date,
    Reading = reading,
    Usage = usage,
    Comment = NA_character_
  ))
}

meter_tbl

#---------------------------------------------------------------------------------------------
# Tabulate extracted text in a similar structure as the "Your meter readings" table in PDF
## Locate table start with text "Your meter readings"
## Locate table end with text "Water Usage"
## Extracted table looks like
### SerialNumber	ReadDate	Reading	Usage	Comment
### ADC1511588	  13/06/2024	1692	NA	  NA
### NA	          10/09/2024	1716	24KL	NA

## This example works on all file
#---------------------------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)

# Get file paths
file.paths.txt.files.urban.utilities <- list.files(path = dir.urban.utility.extracted.text, pattern = "\\.txt$", full.names = TRUE) # length(file.paths.txt.files.urban.utilities) 5

# Function to parse a single urban utility text file
parse_your_meter_readings_table <- function(file_path) {
  # Read file (suppress warning for incomplete final line)
  txt <- readLines(file_path, warn = FALSE)
  
  # Locate start and end of the meter readings block
  start <- grep("^\\s*Serial Number", txt)
  end   <- grep("^\\s*Water Usage", txt)
  
  if (length(start) == 0 | length(end) == 0) return(tibble())  # skip if no data
  
  meter_block <- txt[start:(end-1)] %>%
    str_trim() %>%
    .[. != ""]
  
  # Remove header and irrelevant lines
  meter_block_clean <- meter_block[!str_detect(meter_block, regex("Serial Number|Your usage|kilolitres|Water Usage", ignore_case = TRUE))]
  meter_block_clean <- str_squish(meter_block_clean)
  
  # Initialize empty tibble
  meter_tbl <- tibble(
    Serial.Number = character(),
    Read.Date = as.Date(character()),
    Reading = numeric(),
    Usage = numeric(),
    Comment = character()
  )
  
  serial_current <- NA_character_
  
  for (line in meter_block_clean) {
    fields <- str_split(str_squish(line), "\\s+")[[1]]
    
    if (!str_detect(fields[1], "^\\d{2}/\\d{2}/\\d{4}$")) {
      # First field is Serial Number
      serial_current <- fields[1]
      read_date <- dmy(fields[2])
      reading <- as.numeric(fields[3])
      # Only treat last field as Usage if it contains 'kL' (case-insensitive)
      last_field <- fields[length(fields)]
      usage <- ifelse(str_detect(last_field, regex("kL", ignore_case = TRUE)),
                      as.numeric(str_extract(last_field, "\\d+")),
                      NA_real_)
    } else {
      # First field is date
      read_date <- dmy(fields[1])
      reading <- as.numeric(fields[2])
      last_field <- ifelse(length(fields) >= 3, fields[length(fields)], "")
      usage <- ifelse(str_detect(last_field, regex("kL", ignore_case = TRUE)),
                      as.numeric(str_extract(last_field, "\\d+")),
                      NA_real_)
    }
    
    meter_tbl <- bind_rows(meter_tbl, tibble(
      Serial.Number = serial_current,
      Read.Date = read_date,
      Reading = reading,
      Usage = usage,
      Comment = NA_character_
    ))
  }
  
  return(meter_tbl)
}

# Example: Parse multiple files and stack into a single tibble
all_meter_data <- map_dfr(file.paths.txt.files.urban.utilities, parse_your_meter_readings_table)

# Optional: sort by Serial Number and Read Date
all_meter_data <- all_meter_data %>%
  arrange(Serial.Number, Read.Date)

all_meter_data

# Serial.Number Read.Date  Reading Usage Comment
# 1 ADC1511588    2024-06-13    1692    NA NA
# 2 ADC1511588    2024-09-10    1716    24 NA
# 3 ADC1511588    2024-09-10    1716    NA NA
# 4 ADC1511588    2024-12-10    1727    11 NA
# 5 ADC1511588    2024-12-10    1727    NA NA
# 6 ADC1511588    2025-03-20    1738    11 NA
# 7 ADC1511588    2025-03-20    1738    NA NA
# 8 ADC1511588    2025-06-13    1750    12 NA
# 9 ADC1511588    2025-06-13    1750    NA NA
# 10 ADC1511588    2025-09-19    1765    15 NA
