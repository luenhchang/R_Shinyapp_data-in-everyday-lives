#parse_LLMWhisperer_PDF.R
#17-Aug-2025

dir.C <- "C:"
dir.app <- file.path(dir.C,"GoogleDrive_MyDrive","scripts","RProject_Shinyapp_data-in-everyday-lives")
dir.extracted.text <- file.path(dir.app,"electricity-bill","unstract-LLMWhisperer-extracted-text")
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
file.paths.txt.files <- list.files(path = dir.extracted.text, pattern = "\\.txt$", full.names = TRUE) # length(file.paths.txt.files) 10

# Initialize empty list to store results
all_data <- list()

# Loop through each file
for (file in file.paths.txt.files) {
  # file <- file.paths.txt.files[2]
  # Example: read lines from each file
  txt_content <- readLines(file, warn = FALSE)
  txt_all <- paste(txt_content, collapse = " ")
  
  # Extract Suppy period
  supply_period <- stringr::str_match(txt_all, "Supply period.*?([0-9]{1,2}\\s\\w+\\s[0-9]{4}).*?([0-9]{1,2}\\s\\w+\\s[0-9]{4})")
  supply_start <- lubridate::dmy(supply_period[,2])
  supply_end   <- lubridate::dmy(supply_period[,3])
  
  # Extract Tariff Solar Usage kWh
  solar_kwh <- stringr::str_match(txt_all, "Solar\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
  # Extract Tariff Peak Usage kWh
  peak_kwh  <- stringr::str_match(txt_all, "Peak\\s+[0-9]{2}\\s\\w+\\s[0-9]{4}\\s+Actual\\s+[0-9.]+\\s+[0-9.]+\\s+([0-9.]+)")[,2]
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
balance.brought.forward <- do.call(rbind, all_data) # dim(balance.brought.forward) 10 7

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
  ) # dim(balance.brought.forward.long) 30 5

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

# dim(plot_data) 30 8

# Export to TSV
readr::write_tsv(balance.brought.forward, "data/alinta_bills_balance_brought_forward.tsv")
readr::write_tsv(plot_data, "data/plot_data_alinta_bills_balance_brought_forward.tsv")

#------------------------------------------------------------------------------------------------------------------
# Tabulate extracted text in a similar structure as the "Usage, supply charges and applicable credits" table in PDF
## This example works on all txt files
#------------------------------------------------------------------------------------------------------------------

# function version 13 (version 1 to 12 cannot work on File 6)
parse_bill <- function(file) {
  library(stringr)
  
  # 1) Read
  raw <- readLines(file, warn = FALSE)
  if (!length(raw)) return(data.frame())
  
  # 2) Locate table start (first time the header phrase appears)
  start_ix <- which(str_detect(raw, regex("Usage,\\s*supply charges and applicable credits",
                                          ignore_case = TRUE)))
  if (!length(start_ix)) return(data.frame())
  
  lines <- raw[seq(from = start_ix[1], to = length(raw))]
  
  # 3) Pre-clean: remove header labels wherever they appear on the line
  strip_labels <- function(s) {
    s %>%
      str_replace(regex("Usage,\\s*supply charges and applicable credits", ignore_case=TRUE), "") %>%
      str_replace(regex("\\bQuantity\\b", ignore_case=TRUE), "") %>%
      str_replace(regex("\\bRate\\s*Incl\\.?\\s*GST\\b", ignore_case=TRUE), "") %>%
      str_replace(regex("\\bTotal\\s*Incl\\.?\\s*GST\\b", ignore_case=TRUE), "") %>%
      str_squish()
  }
  lines <- vapply(lines, strip_labels, character(1))
  
  # 4) Drop obvious non-row noise (e.g., date range line under header, blank)
  is_range_line <- function(s) {
    str_detect(
      s,
      regex("\\b\\d{1,2}\\s+[A-Za-z]{3}\\s+\\d{2,4}\\s+to\\s+\\d{1,2}\\s+[A-Za-z]{3}\\s+\\d{2,4}\\b.*\\(\\d+\\s*Days?\\)",
            ignore_case = TRUE)
    )
  }
  lines <- lines[lines != "" & !vapply(lines, is_range_line, logical(1))]
  
  # 5) Patterns
  qty_re   <- "(?i)\\d+\\.?\\d*\\s*(?:kW|kWh|days)"
  rate_re  <- "-?\\$\\d+\\.\\d{2,5}"
  total_re <- "\\$-?\\d+\\.\\d{2}(?:\\s*cr)?"
  
  # Full row: Item + qty + rate + total
  full_re  <- paste0("^(.*?)\\s+(", qty_re, ")\\s+(", rate_re, ")\\s+(", total_re, ")\\s*$")
  # Tail only: qty + rate + total
  tail_re  <- paste0("^(", qty_re, ")\\s+(", rate_re, ")\\s+(", total_re, ")\\s*$")
  
  results <- list()
  buf <- NULL  # holds a pending Item line (e.g., "Demand - March (31 days)")
  
  add_row <- function(item, qty, rate, total) {
    # Normalize Demand to just "Demand"
    item_norm <- str_replace(item, regex("^\\s*Demand\\b.*", ignore_case = TRUE), "Demand")
    # Skip Rounding Adjustment rows entirely
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
    
    # Case A: full row on one line
    m <- str_match(ln, full_re)
    if (!is.na(m[1])) {
      item <- str_squish(m[2]); qty <- m[3]; rate <- m[4]; total <- m[5]
      add_row(item, qty, rate, total)
      buf <- NULL
      next
    }
    
    # Case B: tail only (qty + rate + total); combine with buffered item
    m2 <- str_match(ln, tail_re)
    if (!is.na(m2[1]) && !is.null(buf)) {
      qty <- m2[2]; rate <- m2[3]; total <- m2[4]
      add_row(buf, qty, rate, total)
      buf <- NULL
      next
    }
    
    # Case C: line likely an Item-only line (no $ amounts, may include month/day text)
    if (str_detect(
      ln,
      regex("^(Demand|Controlled Load 1|Daily Charge|Daily Charge - Controlled Load 1|Standard Solar|Peak)\\b",
            ignore_case = TRUE)
    ) && !str_detect(ln, "\\$\\d")
    ) {
      buf <- str_squish(ln)
      next
    }
    
    # Case D: salvage lines that contain a recognizable Item + scattered qty/rate/total but didn’t match due to spacing
    if (str_detect(ln, "\\$\\d") &&
        str_detect(ln, regex("(Demand|Controlled Load 1|Daily Charge|Standard Solar|Peak)", ignore_case = TRUE))) {
      monies <- str_extract_all(ln, "\\$-?\\d+\\.\\d{2,5}(?:\\s*cr)?")[[1]]
      if (length(monies) >= 2) {
        total <- monies[length(monies)]
        rate  <- monies[length(monies)-1]
        # Remove trailing "rate total" to expose the left side (which should contain item + quantity)
        ln_nom <- str_replace(ln, paste0("\\s*", rate, "\\s*", total, "\\s*$"), "")
        # Take the LAST quantity prior to the prices
        qtys <- str_extract_all(ln_nom, qty_re)[[1]]
        if (length(qtys) >= 1) {
          qty <- qtys[length(qtys)]
          # Item is everything before the last quantity
          item <- str_squish(str_replace(ln_nom, paste0("\\s*", qty, "\\s*$"), ""))
          add_row(item, qty, rate, total)
          buf <- NULL
          next
        }
      }
    }
    
    # Otherwise ignore noisy lines
  }
  
  if (!length(results)) return(data.frame())
  do.call(rbind, results)
}

# Run the function over txt file 10 to 1
parsed_list <- lapply(file.paths.txt.files[c(10:1)], parse_bill)

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
# dim(usage.rates.total.credits) 65 12

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
  ) 

# Reshape Rate.Incl.GST (character) and Rate.Incl.GST_num (numeric) to wide format
rates.summary.by.item.wide <- usage.rates.total.credits.summary.by.item %>%
  dplyr::mutate(
    # clean Item names: replace space, dot, dash with underscore
    Item_clean = stringr::str_replace_all(Item, "[ .-]", "_")
  ) %>%
  tidyr::pivot_wider(
    id_cols = supply_start,
    names_from = Item_clean,
    values_from = c(Rate.Incl.GST, Rate.Incl.GST_num),
    names_glue = "{stringr::str_replace_all(.value, '[ .]', '_')}_{Item_clean}"
  ) %>%
  # remove multiple consecutive underscores
  dplyr::rename_with(~ stringr::str_replace_all(., "_+", "_"))
# dim(rates.summary.by.item.wide) 10 13

# Export rate summary wide data to TSV
readr::write_tsv(rates.summary.by.item.wide, "data/alinta_bills_rates_over_supply_period.tsv")

