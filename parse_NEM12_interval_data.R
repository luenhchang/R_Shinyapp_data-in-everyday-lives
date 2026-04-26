# ==============================================================================
# Program:    parse_NEM12_interval_data.R
# Date created: 26-APR-2026
# Purpose:
#   Parse raw National Energy Market (NEM) 12-style interval meter data (exported as CSV) and convert it
#   into clean hourly time series for:
#     - Import from grid (consumption)
#     - Export to grid (solar feed-in)
#
# ------------------------------------------------------------------------------
# DATA STRUCTURE OVERVIEW (NEM12 FORMAT)
#
# The input file is NOT a standard rectangular CSV. It is a line-based format
# defined by the Australian National Electricity Market (NEM).
#
# Each row begins with a "Record Type" identifier:
#   Column A
#--------------------------------------------------
#   100     File header (metadata about the file)
#   200     Meter / register definition
#   300     Interval data (actual energy readings)
#
# ------------------------------------------------------------------------------
# RECORD TYPE DETAILS
#
# 100 RECORD (File Header)
#   - Appears once at top of file
#   - Contains file-level metadata (provider, creation date, etc.)
#   - Not used in this program
#
# 200 RECORD (Meter / Register Context)
# | Position | Value      | Meaning                             |
# | -------- | ---------- | ----------------------------------- |
# | 1        | 200        | Record type                         |
# | 2        | QB08112339 | **NMI** (your property ID)          |
# | 3        | B1E1E2     | **Register suffix list**            |
# | 4        | B1         | **Current register (this section)** |
# | 5        | B1         | Unit of measure register            |
# | 6        | B1         | Time-of-use / register ID           |
# | 7        | 701528247  | Meter serial number                 |
# | 8        | kWh        | Unit                                |
# | 9        | 5          | Interval length (minutes)           |
  
# 300 RECORD (Interval Data)
#   - Each row represents:
#         ONE DAY of data for ONE REGISTER
#   - Structure:
#       Column 1  : "300"
#       Column 2  : Date (YYYYMMDD)
#       Columns 3+: Interval values (kWh)
#       Last col  : Quality flag / checksum (ignored)
#
#   - Interval resolution:
#       * 288 values → 5-minute intervals
#       * 48 values  → 30-minute intervals
#
#   - IMPORTANT:
#       The file does NOT contain explicit timestamps for each interval.
#       Time is inferred by position:
#
#           Index 1   = 00:00–00:05
#           Index 2   = 00:05–00:10
#           ...
#
# ------------------------------------------------------------------------------
# REGISTER INTERPRETATION
#
# Registers represent different energy flows:
#
#   E* (e.g. E1, E2) → Import (energy drawn from grid)
#   B* (e.g. B1)     → Export (solar fed back to grid)
#
# Multiple import registers may exist (e.g. peak/off-peak tariffs).
# These are aggregated in this program.
#
# ------------------------------------------------------------------------------
# PROGRAM LOGIC
#
# 1. Read file line-by-line using readLines()
#    (required because rows have variable column counts)
#
# 2. Track current register context from 200 records:
#       - Store NMI and register (E1, B1, etc.)
#
# 3. For each 300 record:
#       a. Extract date
#       b. Extract interval values
#       c. Detect resolution (5-min vs 30-min)
#       d. Convert intervals → hourly kWh by summing:
#            - 12 intervals per hour (5-min data)
#            - 2 intervals per hour (30-min data)
#       e. Assign timestamps:
#            datetime = date + hour offset (start of interval)
#
# 4. Combine all rows into a single dataset
#
# 5. Classify flow:
#       - Import  : register starts with "E"
#       - Export  : register starts with "B"
#
# 6. Aggregate:
#       - Sum across registers (e.g. E1 + E2)
#       - Produce:
#            * import_hourly
#            * export_hourly
#            * combined dataset (joined on datetime)
#
# ------------------------------------------------------------------------------
# OUTPUT
#
# import_df:
#   datetime | import_kwh
#
# export_df:
#   datetime | export_kwh
#
# combined_df:
#   datetime | import_kwh | export_kwh
#
# These outputs are ready for:
#   - Shiny dashboards
#   - Time series analysis
#   - Energy usage visualisation
#
# ------------------------------------------------------------------------------
# ASSUMPTIONS & LIMITATIONS
#
# - Assumes register naming convention:
#       E* = import, B* = export
# - Skips rows with unexpected interval counts
# - Does not explicitly handle daylight saving anomalies
# - Assumes timestamps represent START of interval
#
# ------------------------------------------------------------------------------
# AUTHOR NOTES
#
# This parser is designed to be:
#   - Retailer-agnostic (works across NEM providers)
#   - Robust to formatting inconsistencies
#   - Suitable for high-resolution energy analytics
#
# ==============================================================================

lines <- base::readLines("data/QB08112339_20250916_20260423_20260425000000_EnergyAustralia_DETAILED.csv") # class(lines)

# For 300 rows:
# Column  
#--------------------------------------------
# A       record type (300)
# B       date
# C to KD interval values (this is the only real data you want)
# KE      blank (separator / padding)
# KF, KG  timestamps (Excel shows them as scientific notation like 2.025E+13) (possibly more columns after KG depending on export settings)
# So your assumption should be: Only C to KD is the interval series
#--------------------------------------------
current_register <- NA_character_
current_nmi <- NA_character_

out <- list()

for (ln in lines) {
  parts <- base::strsplit(ln, ",", fixed = TRUE)[[1]]
  rec <- parts[1]
  
  if (rec == "200") {
    current_nmi <- parts[2]
    current_register <- parts[4]
  }
  
  if (rec == "300") {
    date <- base::as.Date(parts[2], "%Y%m%d")
    
    end_col <- which(parts == " ")[1] - 1  # KE is blank separator
    
    vals <- suppressWarnings(base::as.numeric(parts[3:(end_col)]))
    
    n <- base::length(vals) # 288
    if (!(n %in% c(288, 48))) next
    
    k <- if (n == 288) 12 else 2
    
    # Sum interval values from every 5 minute to every hour
    hourly <- base::tapply(
      vals
      , base::rep(base::seq_len(n / k), each = k)
      , base::sum)
    
    df <- base::data.frame(
      datetime = base::as.POSIXct(date) + (base::seq_along(hourly) - 1) * 3600
      , kwh = base::as.numeric(hourly)
      , register = current_register
      , stringsAsFactors = FALSE
    )
    
    out[[base::length(out) + 1]] <- df
  }
}

all_data <- dplyr::bind_rows(out) 

all_data <- dplyr::mutate(
  all_data
  , flow = dplyr::case_when(
    stringr::str_detect(register, "^E") ~ "import"
    , stringr::str_detect(register, "^B") ~ "export"
    , TRUE ~ NA_character_
  )
)

import_df <- all_data |>
  dplyr::filter(flow == "import") |>
  dplyr::group_by(datetime) |>
  dplyr::summarise(
    import_kwh = base::sum(kwh)
    , .groups = "drop"
  )

export_df <- all_data |>
  dplyr::filter(flow == "export") |>
  dplyr::group_by(datetime) |>
  dplyr::summarise(
    export_kwh = base::sum(kwh)
    , .groups = "drop"
  )

combined_df <- dplyr::full_join(
  import_df
  , export_df
  , by = "datetime"
) |>
  dplyr::arrange(datetime)
