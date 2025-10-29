
# Date created: 

#----------------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives/export-gsheet-data.R
# Date created: 29-OCT-2025
# Author(s): Lun-Hsien Chang
# Modified from: 
# Dependency:
# Input: 
## [barcode-scan.gsheet](https://docs.google.com/spreadsheets/d/1hnIOmXw6s56lX-J7IEY3SUBHM2ySd3Usn82bQNxruto/edit?usp=sharing)
# Output: data/barcode-scan_food.tsv
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2025-10-29 Tested code when there are 1697 rows in input data
##---------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------
# Directory in local PC
## 
## www: Where all the images and other assets needed for the viewer
#------------------------------------------------------------------------
dir.C <- "C:"
dir.app <- file.path(dir.C,"GoogleDrive_MyDrive","scripts","RProject_Shinyapp_data-in-everyday-lives")
dir.data <- file.path(dir.app,"data")
setwd(dir.data)

library(dplyr)
library(readr)
library(digest)

#---------------------------
# Read Google sheet tab=food
#---------------------------
sheet.ID.barcodes <- "https://docs.google.com/spreadsheets/d/1hnIOmXw6s56lX-J7IEY3SUBHM2ySd3Usn82bQNxruto/"
googlesheets4::gs4_deauth()

## Adding a new column add its column type to col_types=
gsheet_barcodescan_food <- googlesheets4::read_sheet(sheet.ID.barcodes
                                          ,sheet = "food"
                                          ,col_types = 'TccnDDDc' # T for Datetime, c for character, n for numeric, D for date
                                          ,na=c("NA"," ")) # dim(gsheet_barcodescan_food) 1697 8 

# Step 1: Define stable content-based unique ID
df_new <- gsheet_barcodescan_food |>
  dplyr::mutate(
    unique_id = digest::digest(paste(
      ifelse(is.na(timestamp), "NA", timestamp),
      ifelse(is.na(barcode), "NA", barcode),
      ifelse(is.na(`product-name`), "NA", `product-name`),
      ifelse(is.na(`date-start-use`), "NA", `date-start-use`),
      ifelse(is.na(`date-end-use`), "NA", `date-end-use`),
      sep = "_"
    ), algo = "crc32")
  ) # dim(df_new) 1697 9

# Step 2 — Backup existing TSV
# Define output TSV path
tsv_path <- "barcode-scan_food.tsv"

if (file.exists(tsv_path)) {
  backup_path <- paste0(tools::file_path_sans_ext(tsv_path)
                        ,"_backup_"
                        ,format(Sys.Date(), "%Y%m%d")
                        ,".tsv")
  file.copy(tsv_path, backup_path, overwrite = TRUE)
  message("Backup created: ", backup_path)
}

# Step 3: Compare with Existing TSV and Append Only New Records
if (base::file.exists(tsv_path)) {
  df_existing <- readr::read_tsv(tsv_path, show_col_types = FALSE)
  
  if (!"unique_id" %in% base::names(df_existing)) {
    base::stop("Existing TSV has no unique_id column — please regenerate once.")
  }
  
  df_append <- dplyr::anti_join(df_new, df_existing, by = "unique_id")
  
  if (base::nrow(df_append) > 0) {
    readr::write_tsv(df_append, tsv_path, append = TRUE)
    base::message("Appended ", base::nrow(df_append), " new records.")
  } else {
    base::message("No new records — TSV is up to date.")
  }
  
} else {
  readr::write_tsv(df_new, tsv_path)
  base::message("New TSV created.")
}
