#----------------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives/global.R
# Date created: 13-Nov-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/global.R
# Dependency:

# Input: 
## [barcode-scan.gsheet](https://docs.google.com/spreadsheets/d/1hnIOmXw6s56lX-J7IEY3SUBHM2ySd3Usn82bQNxruto/edit?usp=sharing)
## [container-collection-refund-responses.gsheet](https://docs.google.com/spreadsheets/d/1Q_oxLE1cl_r9gksXX4R9YGnC2nGZBy3M1hf3-JR2ZIw/edit?usp=sharing)
## C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/data/internet-speed-test-Telstra-5G.csv

# Output: https://luenhchang.shinyapps.io/data-in-everyday-lives/
# References
## [The awesomeness that is the global.R file. Or how to clean up your shiny app](https://mraess.rbind.io/2018/07/the-awesomeness-that-is-the-global-r-file-or-how-to-clean-up-your-shiny-app/)
## [How to separate shiny app to two files: UI and SERVER](https://stackoverflow.com/questions/72762120/how-to-separate-shiny-app-to-two-files-ui-and-server)
## [Shiny app disconnected from the server. No errors](https://stackoverflow.com/questions/64095798/shiny-app-disconnected-from-the-server-no-errors)
## [suppress NAs in paste()](https://stackoverflow.com/questions/13673894/suppress-nas-in-paste)
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2025-02-03 Corrected placement of text label on stacked bars of daily number of collected or refunded containers by plotly. Stacked bar plot by ggplot2 doesn't have this issue.  
## 2025-01-13 Deleted menuItem="JOb"
## 2024-11-13 Fixed error reading sheetname="hygiene-products". Error due to a new column added to the sheet not specified in googlesheets4::read_sheet(col_types = )
## 2024-11-13 Error reading barcode-scan.gsheet sheet="food" fixed. Error due to column name timestamp accidentally edited as e during manual update in the file
## 2024-10-03 deployed app
## 2024-10-03 Updated URL to activities.csv on Google drive.
## 2024-06-28 Deployed app. Table Cycling elevation gain (m) in weekdays error on app but no such an error in app deployed locally
## 2024-06-28 Added note to dataTable under Recycling tab. Collapsed multiple rows of Note to one row using paste(ifelse(is.na(Note),"", Note), collapse="\n")
## 2024-06-23 Deployment error Error: Unhandled Exception: child_task=1429683773 child_task_status=error: Unhandled Exception: Unsupported R version 4.4.1 for operating system jammy.
## 2024-06-02 Added some more categories for food
## 2024-05-20 Deployed app
## 2024-05-20 Included Queensland crime trends in menuItem-About.html
## 2024-05-16 Deployed app. App is launched faster in phone and all valueBoxes and infoBoxes working.
## 2024-05-13 Deployed app
## 2024-05-13 Removed menuItem "Internet" from this app. This part made the app so slow that it couldn't be launched in phone
## 2024-05-11 Deployed app with no error. Uploading a printscreen to test the text extraction. App became disconnected. Reconnected after refresh. New text extraction data seen in the dataTable.
## 2024-05-11 Followed steps from [Trying to deploy shiny app with a google drive connection](https://stackoverflow.com/questions/69397716/trying-to-deploy-shiny-app-with-a-google-drive-connection), ran options(gargle_oauth_email = TRUE  ,gargle_oauth_cache = file.path(getwd(),".secrets")), googledrive::drive_auth(), googlesheets4::gs4_auth(). When the authentication was complete, two MS-DOS Application files created in the .secrets folder. These two files contain credentials per Google account for the authentication.

## 2024-05-11 Exported internet-speed-test.gsheet as C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/data/internet-speed-test-Telstra-5G.csv. This file is to use as input and output for menuItem=Internet. 
## 2024-05-10 Changed internet-speed-test.gsheet to anyone with link can view
## 2024-05-10 Error while deploying app : Error in gs4_get_impl_(as_sheets_id(ss)) : Client error: (403) PERMISSION_DENIED • Client does not have sufficient permission. This can happen because the OAuth token does not have the right scopes, the client doesn't have permission, or the API has not been enabled for the client project. • The caller does not have permission
## 2024-05-10 Error while deploying app : Error in googlesheets4::gs4_auth() : Can't get Google credentials. ℹ Are you running googlesheets4 in a non-interactive session? Consider: • Call `gs4_deauth()` to prevent the attempt to get credentials. Call `gs4_auth()` directly with all necessary specifics. ℹ See gargle's "Non-interactive auth" vignette for more details: <https://gargle.r-lib.org/articles/non-interactive-auth.html>
## 2024-05-10 Error in `map()`: ℹ In index: 1.Caused by error in `.f()`:! Client error: (403) Forbidden Request had insufficient authentication scopes. PERMISSION_DENIED • message: Insufficient Permission • domain: global • reason: insufficientPermissions. Finally solved the error by running googledrive::drive_deauth() and googledrive::drive_auth()
## 2024-05-07 Deployed successfully at 20:35
## 2024-05-07 Error in curl::curl_fetch_memory(url, handle = handle) : Timeout was reached: [shinyapps-upload.s3.amazonaws.com] Resolving timed out after 10006 milliseconds x 5 times
## 2024-05-06 Replaced NA with zero using tidyr::replace_na(x,0) to reset cumulative sum
## 2024-05-04 deployment error DataTables warning: table id=DataTables_Table_5 - Ajax error. For more information about this error, please see http://datatables.net/tn/7
## 2024-05-04 App deployed to shinyapps.io
## 2024-04-22 Added a table with longest ride and best elevation gain in each year
## 2024-04-22 Set global color "orange" to valueBox, "olive" to infoBox
## 2024-04-21 Created C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/menuItem-Fitness.Rmd
## 2024-04-20 Moved code from C:/GoogleDrive/Fitness/scripts/analyse-Strava-activities-CSV-file_shiny-web-app.R to this app
## 2024-04-19 Moved containers-collected-refund_shiny-web-app.R to menuItem Recycling in this app
## 2024-04-18 [2024-04-18 10:43:40.436219] Deployment log finished ✔ Successfully deployed to <https://luenhchang.shinyapps.io/data-on-everyday-lives/> Deployment completed: https://luenhchang.shinyapps.io/data-on-everyday-lives/
## 2024-04-18 Error in `POST()`: ! <https://api.shinyapps.io/v1/applications/> failed with HTTP status 402 You have reached the maximum number of applications allowed for your account.
## 2024-04-18 Split barcode-scanner_shiny-web-app.R into separate files global.R, server.R, ui.R because app.R files are becoming very complex and crowded
##----------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------
# Load R packages
## Required uninstalled packages in local PC will cause errors library(pkg) is not available while deploying app to shinyapps.io
#----------------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)

library(googlesheets4)
library(stringr)
library(httr)
library(curl)
library(lubridate)
library(ggridges)
library(ggplot2)
library(labeling)
library(farver)
library(cowplot)
library(tidyr)
library(png)
library(jpeg)
library(RCurl)
library(grid)
library(DT)
library(treemapify)
library(webr)

library(cachem)
library(bslib)
library(crayon)
library(memoise)
library(tzdb)
library(vroom)
library(ggrepel)
library(readr)
library(forcats)
library(tidyverse)
library(ggbreak)
library(timeDate)
library(tsibble)
library(ggthemes)
library(markdown)
library(xfun)
library(ggtext)
library(pals)

library(utils)
library(scales)
library(gghighlight)

library(knitr)
library(rmarkdown)
library(fresh)
library(slickR)
library(here)

library(hutilscpp)
library(googledrive)
library(tesseract)
library(magick)
library(plotly)
library(ggforce)
library(viridisLite)
library(dplyr)
#------------------------------------------------------------------------
# Directory in local PC
## 
## www: Where all the images and other assets needed for the viewer
#------------------------------------------------------------------------
dir.C <- "C:"
dir.app <- file.path(dir.C,"GoogleDrive_MyDrive","scripts","RProject_Shinyapp_data-in-everyday-lives")
# dir.www <- file.path(dir.app,"www")
# dir.create(path = dir.www)
# dir.img <- file.path(dir.www,"image_data-challenges")
# dir.img.annotated <- file.path(dir.www,"image_data-challenges-annotated") # dir.create(dir.img.annotated)
# dir.data <- file.path(dir.app,"data")
# dir.create(dir.data)

# Get functions here
#setwd(dir.app)
source("functions.R")

#*****************************************
# Read data to use under menuItem "Food" 
## Input file: barcode-scan.gsheet
## sheetname="food"
#*****************************************

# Set Google sheet to Share > Anyone with the link > copy link
# URL.barcodes <- "https://docs.google.com/spreadsheets/d/1hnIOmXw6s56lX-J7IEY3SUBHM2ySd3Usn82bQNxruto/edit?usp=sharing"
sheet.ID.barcodes <- "https://docs.google.com/spreadsheets/d/1hnIOmXw6s56lX-J7IEY3SUBHM2ySd3Usn82bQNxruto/"
googlesheets4::gs4_deauth()

# Read Google sheet tab=food
## Adding a new column add its column type to col_types=
barcode.food <- googlesheets4::read_sheet(sheet.ID.barcodes
                                           ,sheet = "food"
                                           ,col_types = 'TccnDDDc' # T for Datetime, c for character, n for numeric, D for date
                                           ,na=c("NA"," ")) |>
  dplyr::mutate( timestamp.date=as.Date(timestamp)
                 ,timestamp.month.label= lubridate::month(timestamp.date, label=TRUE, abbr=TRUE)
                 ,timestamp.month.num=cut.Date(timestamp.date, breaks = "1 month", labels = FALSE)
                 ,timestamp.year=lubridate::year(timestamp.date)
                 ,product.name=`product-name`
                 # Extract the last part of a string by final space
                 ## e.g. 250g from MasterFoods Dijonnaise Mustard sauce 250g 
                 ## Reference [Split string by final space in R](https://stackoverflow.com/questions/19959697/split-string-by-final-space-in-r)
                 ,quantity=do.call(rbind, strsplit(product.name, ' (?=[^ ]+$)', perl = TRUE))[,2]
                 # Extract a pattern from a a string. Pattern 1: decimal number and number without decimal. Pattern 2: letters. Invert the search of pattern 2 will get pattern 1
                 ## Ref [extract letters from character string [duplicate]](https://stackoverflow.com/questions/77190404/extract-letters-from-character-string)
                 ,measurement=as.numeric(gsub(pattern="[[:alpha:]]", replacement="", x=quantity))
                 # Extract pattern 2 letters from a string 
                 ,unit=tolower(gsub(pattern="[^[:alpha:]]", replacement="", x=quantity))
                 ,date.expiry=lubridate::ymd(`date-expiry`)
                 ,date.start.use=`date-start-use`
                 ,date.end.use=(`date-end-use`)
                 ,days.usage=as.numeric(
                   difftime(time1=date.end.use, time2=date.start.use, units = "days")
                 )
                 # Add date of today
                 ,today=Sys.Date()
                 # Date difference between today and expiry
                 ,days.to.expiry=as.numeric(
                   difftime(time1=date.expiry, time2=today, units = "days")
                 )
                 # Arbitrary base date 0 for plotting date difference as an interval
                 ,dummy.xmin=0) |>
  dplyr::arrange(date.expiry, barcode, product.name) |>
  # Create serial number within same product-name
  dplyr::group_by(barcode) |>
  dplyr::mutate(product.name.serial= dplyr::row_number()
                # Combine product name and their serial numbers. String > n characters put to a new line
                ,product.name.serial.numb.50= stringr::str_wrap(
                  paste0(product.name, " #", product.name.serial), width=50)) # class(barcode.food) [1] "grouped_df" "tbl_df" "tbl" "data.frame" # dim(barcode.food) 621 25

# Calculate unit price
barcode.food.df <- as.data.frame(barcode.food) |>
  dplyr::mutate(
    unit.price.numb=dplyr::case_when(
      !is.na(price)& unit %in% c("g","ml") ~ round(price/measurement*100, digits = 2)
      ,!is.na(price)& unit %in% c("capsules","tablets","kg","l") ~ round(price/measurement, digits = 2)
      )
    ,unit.price.text=dplyr::case_when(
      !is.na(price)& unit %in% c("g","ml") ~ paste0("$AUD ", unit.price.numb, " per 100", unit)
      ,!is.na(price)& unit %in% c("capsules","tablets","kg","l") ~ paste0("$AUD ", round(unit.price.numb, digits = 2), " per ", unit)
    )
    ) # class(barcode.food.df) "data.frame" # dim(barcode.food.df) 255 27

# Categorise food product.name
# [First letter to upper case](https://stackoverflow.com/questions/18509527/first-letter-to-upper-case)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

barcode.food.category <- barcode.food.df |>
  dplyr::mutate(category=dplyr::case_when(
    # bread
    grepl(pattern = "sourdough vienna|pane di casa loaf|toast", x=product.name, ignore.case = TRUE) ~ "bread"
    # dairy products
    ,grepl(pattern = "milk|cheese|yoghurt", x=product.name, ignore.case = TRUE) ~ "dairy"
    # Eggs
    ,grepl(pattern = "eggs", x=product.name, ignore.case = TRUE) ~ "eggs"
    # nuts for almond, cashew, peanut, seasame
    ,grepl(pattern = "almond|cashew|peanut|seasame", x=product.name, ignore.case = TRUE) ~ "nuts"
    # confectionery for chocolate, lozenge. This overwrites some nuts categories above that are actually chocolate
    ,grepl(pattern = "chocolate|lozenge", x=product.name, ignore.case = TRUE) ~ "confectionery"
    # legumes for beans
    ,grepl(pattern = "mung bean", x=product.name, ignore.case = TRUE) ~"legumes"
    # soybean product for tofu, soy long life milk
    ,grepl(pattern = "tofu|soy long life milk", x=product.name, ignore.case = TRUE) ~"soybean product"
    # Snacks
    ,grepl(pattern = "biscuit|cookie|crispbread", x=product.name, ignore.case = TRUE) ~ "snacks"
    # spaghetti for pasta, spaghetti
    ,grepl(pattern= "pasta|spaghetti", x=product.name, ignore.case = TRUE) ~ "spaghetti"
    # fast food for instant noodle
    ,grepl(pattern= "nong shim shin ramyun", x=product.name, ignore.case = TRUE) ~ "fast food"
    # coffee for coffee beans
    ,grepl(pattern= "coffee", x=product.name, ignore.case = TRUE) ~ "coffee"
    # supplement for vitamin, energy tablets, fish oil capsules
    ,grepl(pattern= "vitamin|tablet|fish oil", x=product.name, ignore.case = TRUE) ~ "supplement"
    # sauce for sauce, salad dressing
    ,grepl(pattern="sauce|dressing", x= product.name, ignore.case=TRUE) ~ "sauce"
    # cooking oil for grape seed oil, Olive Oil
    ,grepl(pattern="grape seed oil|olive oil ", x= product.name, ignore.case=TRUE) ~ "cooking oil"
    # meat for bacon, duck, hog jowl, Lamb Shoulder, Marinated spicy pork, chicken, sausage
    ,grepl(pattern="bacon|duck|hog jowl|lamb shoulder|marinated spicy pork|chicken|sausage", x= product.name, ignore.case=TRUE) ~ "meat"
    # fermented food for kimchi, Sauerkraut
    ,grepl(pattern="kimchi|sauerkraut", x= product.name, ignore.case=TRUE) ~ "fermented food"
    # fresh fruit
    ,grepl(pattern="grapes|apples|avocados|banana|pears|oranges", x= product.name, ignore.case=TRUE) ~ "fresh fruit"
    # dried fruit
    ,grepl(pattern="sultana", x= product.name, ignore.case=TRUE) ~ "dried fruit"
    # frozen food for frozen dumplings, bao, rice ball
    ,grepl(pattern="pork dumplings|bao|rice ball", x= product.name, ignore.case=TRUE) ~"frozen food"
    # frozen fruit
    ,grepl(pattern="frozen blueberries|frozen pitted cherries", x= product.name, ignore.case=TRUE) ~ "frozen fruit"
    # ice cream
    ,grepl(pattern="ice cream", x= product.name, ignore.case=TRUE) ~ "ice cream"
    # cooking wine
    ,grepl(pattern="rice wine|cooking wine", x= product.name, ignore.case=TRUE) ~ "cooking wine"
    # Alcoholic beverage for beer
    ,grepl(pattern="beer|asahi|lager", x= product.name, ignore.case=TRUE) ~ "alcoholic beverage"
    # soda for soft drink, carbonated mineral water
    ,grepl(pattern="soft drink|mineral water|brewed lemon lime & bitters", x= product.name, ignore.case=TRUE) ~ "soda"
    # canned food
    ,grepl(pattern="chickpea|chick pea|black olive", x= product.name, ignore.case=TRUE) ~ "canned food"
    # spice for pepper, salt
    ,grepl(pattern="pepper|salt", x= product.name, ignore.case=TRUE) ~ "spice"
    # seafood for prawn, frozen fish fillets,
    ,grepl(pattern="prawn|frozen fish|fish paste|squid rings", x= product.name, ignore.case=TRUE) ~ "seafood"
    # Seaweed
    ,grepl(pattern="seaweed", x= product.name, ignore.case=TRUE) ~ "seaweed"
    # Baking & Food Prep Supplies
    ,grepl(pattern="baking paper|sandwich bags", x= product.name, ignore.case=TRUE) ~ "baking & food prep supplies"
    # Desserts
    ,grepl(pattern="herb jelly|shredded coconut", x= product.name, ignore.case=TRUE) ~ "dessert"
    # Water
    ,grepl(pattern="natural spring water", x= product.name, ignore.case=TRUE) ~ "water"
    ) # Close case_when()
    # Categorise food status 
    ,status.consumed.expired=dplyr::case_when(
      # Food fully consumed (i.e. non-missing date.end.use) before its expiry
      !is.na(date.expiry) & !is.na(date.end.use) & (date.expiry > date.end.use) ~ "consumed.before.expiry"
      # Food fully consumed after its expiry
      ,!is.na(date.expiry) & !is.na(date.end.use) & (date.expiry < date.end.use) ~ "consumed.after.expiry"
      # Unopened food expired
      ,!is.na(date.expiry) & is.na(date.start.use) & (date.expiry < today) ~ "unopened.expired"
      # Unopened food that has not expired
      ,!is.na(date.expiry) & is.na(date.start.use) & (date.expiry > today) ~ "unopened.unexpired"
      # Opened food expired
      ,!is.na(date.expiry) & !is.na(date.start.use) & (date.expiry < today) ~ "opened.expired"
      # Opened food that has not expired
      ,!is.na(date.expiry) & !is.na(date.start.use) & (date.expiry > today) ~ "opened.unexpired"
      # Food package has no expiry date
      ,is.na(date.expiry) ~ "expiry.date.unavailable"
      ) # Close case_when()
    ,category.factor=as.factor(category)
    ,category.firstup=firstup(category)
    ,yn.status.expired=dplyr::case_when(
      status.consumed.expired %in% c("consumed.after.expiry","unopened.expired","opened.expired") ~ "yes"
      ,status.consumed.expired %in% c("consumed.before.expiry","unopened.unexpired","opened.unexpired") ~ "no"
    ) # Close case_when()
  ) # Close mutate() # dim(barcode.food.category) 272 32

# Count food category
barcode.food.category.count <- barcode.food.category |>
  dplyr::group_by(category) |>
  dplyr::summarise(count=dplyr::n()) # dim(barcode.food.category.count) 27 2

# Foods that are not categorised
barcode.food.uncategorised <- barcode.food.category |> 
  dplyr::filter(is.na(category)) |> 
  dplyr::select(product.name) |> 
  dplyr::distinct() # dim(barcode.food.uncategorised) 12 1

# Count food expired
barcode.food.category.expired.count <- barcode.food.category |>
  dplyr::filter(status.consumed.expired != "expiry.date.unavailable") |>
  dplyr::group_by(category, yn.status.expired) |>
  dplyr::summarise(count=dplyr::n()) # dim(barcode.food.category.expired.count) 34 3

#-------------------------
# Process food expiry data
#-------------------------
food.expiring <- barcode.food.df |> 
  dplyr::ungroup() |>
  # Remove food item that has finished use (i.e., they have date-end-use populated)
  dplyr::filter(is.na(`date-end-use`)) # dim(food.expiring) 133 24

# Food expiring in 180 days
food.expiring.365 <- food.expiring |> 
  dplyr::filter(days.to.expiry <=365) |>
  dplyr::mutate(date.expiry.char=as.character(date.expiry)
                ,days.to.expiry.char=as.character(days.to.expiry)
  ) |>
  dplyr::select(product.name.serial.numb.50, date.expiry.char, days.to.expiry) # dim(food.expiring.365) 94 3

# Change column names to carry the names over to column headers
colnames(food.expiring.365) <- c("Product", "Expiry date","Days to expiry")

#------------------------
# Process food usage data
#------------------------
food.consumed <- barcode.food.df |> 
  dplyr::ungroup() |>
  # Remove food item that has finished use (i.e., they have non-missing date-end-use)
  dplyr::filter(!is.na(`date-end-use`)) |>
  dplyr::arrange(date.end.use) |>
  dplyr::mutate(# price.char=as.character(price)
    date.start.use.char=as.character(date.start.use)
    ,date.end.use.char=as.character(date.end.use)
    #,days.usage.char=as.character(days.usage)
  ) |>
  dplyr::select( product.name.serial.numb.50
                 ,price
                 #,price.char
                 #,days.usage.char
                 ,days.usage
                 ,date.start.use.char
                 ,date.end.use.char) # dim(food.consumed) 102 5

# Change column names to carry the names over to column headers
colnames(food.consumed) <- c("Product", "Price", "Used in n days", "Open date", "Finish date")

#------------------------
# Process food price data
#------------------------
food.price <- barcode.food.df |> 
  dplyr::ungroup() |>
  # Remove food item that has no price
  dplyr::filter(!is.na(`price`)) |>
  dplyr::arrange(product.name.serial.numb.50, timestamp.date) |>
  dplyr::mutate(#price.char=as.character(price)
    timestamp.date.char=as.character(timestamp.date)) |>
  dplyr::select( product.name.serial.numb.50
                 ,price
                 ,unit.price.text
                 ,timestamp.date.char) # dim(food.price) 193 3

# Change column names to carry the names over to column headers
colnames(food.price) <- c("Product", "Price", "Unit Price", "Scan barcode date")

#---------------------------------------
# Monthly expenditure on food categories
#---------------------------------------
# Monthly expenditure on food categories, taking monthly total of non-missing prices
expenditure.food.category <- barcode.food.category |> 
  dplyr::ungroup() |>
  # Remove food item that has no price
  dplyr::filter(!is.na(`price`)&!is.na(category.factor)) |>
  dplyr::group_by(category, category.factor,category.firstup, timestamp.month.num, timestamp.month.label, timestamp.year) |>
  dplyr::mutate(price.summed=sum(price, na.rm = TRUE)) |>
  dplyr::select(category, category.factor,category.firstup, timestamp.month.num, timestamp.month.label, timestamp.year, price.summed) |>
  dplyr::arrange(category.factor,timestamp.month.num,timestamp.year) |>
  dplyr::distinct() # dim(expenditure.food.category) 56 7

#----------------------------------------------
# Compute values used in Food valueBox, infoBox
#----------------------------------------------
# Number of unique barcodes scanned
num.unique.food.barcode <- length(unique(barcode.food$barcode)) # 123

# Food that has expired (i.e. expiry date is before today)
food.expired <- food.expiring |> dplyr::filter(days.to.expiry <0)
num.food.expired <- length(unique(food.expired$barcode)) # 2

# Number of food categories tracked
num.food.category <- length(unique(barcode.food.category$category)) # 16

# Number of foods that have no categories
num.food.no.category <- length(barcode.food.uncategorised$product.name) # 7

# Total spent on food
summed.price.food <- format(round(sum(food.price$Price, na.rm = TRUE), digits = 2), big.mark = ",") # class(summed.price.food) "numeric"  #467

food.barcode.earliest.date.record <- format(min(barcode.food.df$timestamp.date, na.rm = TRUE), "%d %B %Y")
food.barcode.latest.date.record <- format(
  max(c(barcode.food.df$timestamp.date,barcode.food.df$date.start.use, barcode.food.df$date.end.use)
      , na.rm = TRUE)
  ,"%d %B %Y")

#*****************************************
# Read data to use under menuItem "Bathroom" 
## Input file: barcode-scan.gsheet
## sheetname="hygiene-products"
#*****************************************

# Read Google sheet tab= hygiene-products
barcode.hygiene <- googlesheets4::read_sheet(ss=sheet.ID.barcodes
                                             ,sheet = "hygiene-products"
                                             ,col_types = "TcccnDDc" # T for Datetime, c for character, n for numeric, D for date 
                                             ,na=c("NA"," ")
                                             ) |>
  # Filter out unwanted rows. filter(x !="value") not working
  dplyr::filter(!(note %in% c("returned"))) |> 
  dplyr::mutate( timestamp.date=as.Date(timestamp)
                 ,barcode=as.character(barcode)
                 ,product.name=`product-name`
                 ,date.start.use=lubridate::ymd(`date-start-use`)
                 ,date.end.use=lubridate::ymd(`date-end-use`)
                 ,days.usage=as.numeric(
                   difftime(time1=date.end.use, time2=date.start.use, units = "days")
                 )) |>
  dplyr::arrange(product.name, timestamp) |>
  # Create serial number within same product-name
  dplyr::group_by(barcode) |>
  dplyr::mutate(product.name.serial= dplyr::row_number()
                # Combine product name and their serial numbers. String > n characters put to a new line
                ,product.name.serial.numb.50= stringr::str_wrap(
                  paste0(product.name, " #", product.name.serial), width=50)) # class(barcode.hygiene) [1] "grouped_df" "tbl_df" "tbl" "data.frame" # dim(barcode.hygiene) 103 15

barcode.hygiene.df <- as.data.frame(barcode.hygiene) # class(barcode.hygiene.df) [1] "data.frame"

#-----------------------------
# Process hygiene product data
#-----------------------------
hygiene.count.unopened <- barcode.hygiene.df |>
  dplyr::ungroup() |>
  # Remove product items that have date start use (i.e., they are new unopened)
  dplyr::filter(is.na(`date-start-use`) & is.na(`date-end-use`)) |>
  dplyr::group_by(product.name) |>
  dplyr::summarise(count=dplyr::n()) # class(hygiene.count.unopened) # dim(hygiene.count.unopened) 10 2

# Change column names to carry the names over to column headers
colnames(hygiene.count.unopened) <- c("Product", "Quantity")

#-----------------------------------
# Process hygiene product price data
#-----------------------------------
# Collapse data to 1 record per unique product
hygiene.price <- barcode.hygiene.df |>
  dplyr::ungroup() |>
  dplyr::group_by(barcode, product.name, price) |>
  dplyr::summarise(.product.name.all.serial=paste0("#",product.name.serial, collapse = " ")) |>
  dplyr::mutate(product.name.all.serial=paste0(product.name,"<br/>", .product.name.all.serial)
                ,price=price) |>
  dplyr::ungroup() |>
  dplyr::select(-barcode, -product.name,-.product.name.all.serial) |>
  dplyr::select(product.name.all.serial, price) # dim(hygiene.price) 14 2

# Change column names to carry the names over to column headers
colnames(hygiene.price) <- c("Product", "Price")

#---------------------------------------------------------------
# Compute values used in personal care product valueBox, infoBox
#---------------------------------------------------------------
# Number of unique barcodes scanned
num.unique.hygiene.barcode <- length(unique(barcode.hygiene.df$barcode)) # 13

# Total spent on personal care products
summed.price.hygiene <- format(round(sum(barcode.hygiene.df$price, na.rm = TRUE), digits = 2), big.mark= ",") # "53.9"

#*************************************************************
# Read data to use under menuItem "Recycling" 
## Input file: container-collection-refund-responses.gsheet
## sheetname="Form Responses 1"
#*************************************************************

# Set Google sheet to Share > Anyone with the link > copy link
#url.container.responses <- "https://docs.google.com/spreadsheets/d/1Q_oxLE1cl_r9gksXX4R9YGnC2nGZBy3M1hf3-JR2ZIw/edit?usp=sharing"
sheet.ID.containers <- "https://docs.google.com/spreadsheets/d/1Q_oxLE1cl_r9gksXX4R9YGnC2nGZBy3M1hf3-JR2ZIw/"

# Data is one row per data entry
containers <- googlesheets4::read_sheet(sheet.ID.containers) |>
  as.data.frame() |>
  dplyr::mutate(activities=Activities
                ,timestamp=as.POSIXct(x=Timestamp, format="%Y-%m-%d %H:%M:%S")
                ,date.of.activity=lubridate::ymd(`Date of activity`)
                ,number.PET=tidyr::replace_na(`Plastic bottles`,0)
                ,number.cans=tidyr::replace_na(Cans,0)
                ,number.glass=tidyr::replace_na(`Glass bottles`,0)
                ,number.carton=tidyr::replace_na(carton,0)
                ) |>
  dplyr::select(activities, timestamp, date.of.activity, tidyselect::starts_with("number."), Note) |>
  # limit data from 2024
  dplyr::filter(date.of.activity >= as.Date("2024-01-01")) |>
  dplyr::arrange(date.of.activity, timestamp) |>
  # Sum up number.PET, number.cans, and number.glass
  dplyr::rowwise() |>
  dplyr::mutate(number.all.types= sum(dplyr::across(number.PET:number.carton), na.rm=TRUE)) |>
  # Compute running total of number of containers collected
  dplyr::ungroup() |>
  dplyr::arrange(date.of.activity, timestamp) |>
  dplyr::mutate(
    # Number of containers stocked in the back yard
    # Accumulate container number from collections. Reset it to zero when the condition of activities=="Refund in cash" is met
    ## Method 1 only works when NA values in a variable to cumsum have been replaced with zero
    logical.cumsum.continue=dplyr::case_when(activities=="Collection" ~ TRUE
                                             ,activities=="Refund in cash" ~ FALSE)
     ,numb.PET.stock=hutilscpp::cumsum_reset(x=logical.cumsum.continue, y=number.PET)
     ,numb.cans.stock=hutilscpp::cumsum_reset(x=logical.cumsum.continue, y=number.cans)
     ,numb.glass.stock=hutilscpp::cumsum_reset(x=logical.cumsum.continue, y=number.glass)
     ,numb.carton.stock=hutilscpp::cumsum_reset(x=logical.cumsum.continue, y=number.carton)
     ,numb.all.containers.stock=hutilscpp::cumsum_reset(x=logical.cumsum.continue, y=number.all.types)
    ## Method 2 works on a variable that has no NA
    ### Reset container number to zero when NA or Refund in cash
    #number.all.types.2= case_when( activities=="Refund in cash"~ 0
    #                               ,activities=="Collection"~ number.all.types)
    #,numb.PET.stock=ave(number.PET, cumsum(number.PET.reset=="yes"), FUN = cumsum)
    #,numb.all.containers.stock=ave(number.all.types.2, cumsum(number.all.types.2==0), FUN=cumsum)
    ) # Close mutate() # class(containers) [1] "data.frame" # dim(containers) 462 15

# Calculate daily number of containers 
## Adding multiple collections to one number per day
containers.daily.wide <- containers |>
  dplyr::group_by(activities, date.of.activity) |>
  dplyr::summarise( number.PET.day=sum(number.PET, na.rm = TRUE)
                   ,number.cans.day=sum(number.cans, na.rm = TRUE)
                   ,number.glass.day=sum(number.glass, na.rm = TRUE)
                   ,number.carton=sum(number.carton, na.rm = TRUE)
                   ,number.all.types.day=sum(number.all.types, na.rm = TRUE)
                   # suppress NAs in paste()
                   ,note=paste(ifelse(is.na(Note),"", Note), collapse = "\n")
                   ) |>
  dplyr::mutate(weeks=as.numeric(difftime(date.of.activity, min(date.of.activity), units = "weeks"))) # dim(containers.daily.wide) 270 9

# Daily collection or refund in long format
containers.daily.long <- tidyr::pivot_longer(
  data=containers.daily.wide
  ,cols = c(number.PET.day, number.cans.day, number.glass.day, number.carton, number.all.types.day)
  ,names_to = "container.type"
  ,values_to ="container.number") |>
  dplyr::mutate(
    container.type=gsub(x=container.type, pattern="number.|.day", replacement="")
    # Set container number to positive if collected, negative if refunded                
    ,container.number.adjusted=dplyr::case_when(
      activities=="Collection" ~ container.number
      ,TRUE ~ container.number*-1)
    # Set 0 to missing
    ,container.number.label=dplyr::case_when(
      container.number.adjusted==0 ~ NA_integer_
      ,TRUE ~ container.number.adjusted)) # dim(containers.daily.long) 1350 8

#--------------------------------------
# Data used to create stacked bar plot
#--------------------------------------
## Data structure: 1 row per activities, date of activity, container type
containers.daily.long.not.all.types <- containers.daily.long |> 
  dplyr::filter(container.type !="all.types") # dim(containers.daily.long.not.all.types) 1080 8

number.containers.daily <- containers.daily.long.not.all.types %>%
  dplyr::mutate(activities=gsub(x=activities, pattern = " ", replacement="_")) %>%
  dplyr::group_by(date.of.activity, activities) %>%
  dplyr::summarize(total = sum(container.number.adjusted), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = activities,
    values_from = total,
    names_prefix = "total_",
    values_fill = list(total = 0)
  ) # dim(number.containers.daily) 257 3

# Create y position for text label to annotate stacked bars
stacked.bar.label.height <- number.containers.daily %>%
  dplyr::mutate(
    # Dynamic offset: Ensures text does not overlap, uses min + scaling factor
    offset = dplyr::case_when(
      # Lower scaling factors to get text label closer to the bars
       total_Collection > 0  ~ max(3, total_Collection * 0.1)  
      ,total_Collection == 0 & total_Refund_in_cash != 0 ~ 3  # Refund-only case: Small offset
      ,TRUE ~ 0)
    # Adjust max_height to ensure text is placed above bars
    ,max_height = dplyr::case_when(
      total_Collection > 0 ~ total_Collection + offset  # Text above highest bar
      ,total_Collection == 0 & total_Refund_in_cash != 0 ~ offset  # If only refund, keep slightly above 0
      ,TRUE ~ NA_real_)
    ) # dim(stacked.bar.label.height) 257 5

containers.daily.stacked.bar.label.data <- number.containers.daily %>%
  dplyr::group_by(date.of.activity) %>%
  dplyr::summarise(text_label = case_when(
    !(total_Collection ==0) & !(total_Refund_in_cash ==0) ~ 
      paste0(total_Collection, "\n", total_Refund_in_cash)
    ,!(total_Collection ==0) & (total_Refund_in_cash ==0) ~ as.character(total_Collection)
    ,(total_Collection ==0) & !(total_Refund_in_cash ==0) ~ as.character(total_Refund_in_cash)
    ,TRUE ~ NA_character_ ) # Close case_when()
    # Create y position for text_label
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::mutate(text_label_y_position = stacked.bar.label.height$max_height) # dim(containers.daily.stacked.bar.label.data) 257 3

#-------------------------------------------------------
# Calculate total containers per day regardless of types
#-------------------------------------------------------
## This is used to place number on top of each stacked bar
totals.all.types <- containers.daily.long.not.all.types |> 
  dplyr::group_by(activities, date.of.activity) |> 
  dplyr::summarise(total=sum(container.number.adjusted)) |>
  dplyr::arrange(date.of.activity, activities) |>
  dplyr::mutate(
    # Set total >=10 or <0 to be printed, total of 0 to 9 to be missing
    total.label=dplyr::case_when(total %in% c(0:9) ~ NA_integer_
                                 ,TRUE ~ total)) # dim(totals.all.types) 270 4

#-----------------------------------------------------
# Create subsets by years
#-----------------------------------------------------

#-----
# 2025
#-----
containers.2025 <- containers %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2025-01-01")
                               ,right=as.Date("2025-12-31"))) # dim(containers.2025) 8 15

## Data used in Recycling stacked bar plots
containers.daily.long.not.all.types.2025 <- containers.daily.long.not.all.types %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2025-01-01")
                               ,right=as.Date("2025-12-31"))) # dim(containers.daily.long.not.all.types.2025) 28 8

# Label text data in Recycling stacked bar plots
containers.daily.stacked.bar.label.data.2025 <- containers.daily.stacked.bar.label.data %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2025-01-01")
                               ,right=as.Date("2025-12-31"))) # dim(containers.daily.stacked.bar.label.data.2025) 6 3

totals.all.types.2025 <- totals.all.types %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2025-01-01")
                               ,right=as.Date("2025-12-31"))) # dim(totals.all.types.2025) 7 4

# Calculate total number of collected or refunded containers
totals.2025 <- containers %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2025-01-01")
                               ,right=as.Date("2025-12-31"))) %>% 
  dplyr::group_by(activities) %>%
  dplyr::summarise(total.PET=sum(number.PET, na.rm = TRUE)
                   ,total.cans=sum(number.cans, na.rm = TRUE)
                   ,total.glass=sum(number.glass, na.rm = TRUE)
                   ,total.carton=sum(number.carton, na.rm = TRUE)
                   ,number.activities= dplyr::n()) %>%
  # Total containers collected or refunded
  dplyr::rowwise() %>%
  dplyr::mutate(total=sum(dplyr::c_across(tidyselect::starts_with("total.")), na.rm = TRUE)) # dim(totals.2025) 2 7

#-----
# 2024
#-----

containers.2024 <- containers %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2024-01-01")
                               ,right=as.Date("2024-12-31"))) # dim(containers.2024) 454 15

## Data used in Recycling stacked bar plots
containers.daily.long.not.all.types.2024 <- containers.daily.long.not.all.types %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2024-01-01")
                               ,right=as.Date("2024-12-31"))) # dim(containers.daily.long.not.all.types.2024) 1052 8

totals.all.types.2024 <- totals.all.types %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2024-01-01")
                               ,right=as.Date("2024-12-31"))) # dim(totals.all.types.2024) 263 4

# Calculate total number of collected or refunded containers
totals.2024 <- containers %>% 
  dplyr::filter(dplyr::between(x=date.of.activity
                               ,left= as.Date("2024-01-01")
                               ,right=as.Date("2024-12-31"))) %>% 
  dplyr::group_by(activities) %>%
  dplyr::summarise(total.PET=sum(number.PET, na.rm = TRUE)
                   ,total.cans=sum(number.cans, na.rm = TRUE)
                   ,total.glass=sum(number.glass, na.rm = TRUE)
                   ,total.carton=sum(number.carton, na.rm = TRUE)
                   ,number.activities= dplyr::n()) %>%
  # Total containers collected or refunded
  dplyr::rowwise() %>%
  dplyr::mutate(total=sum(dplyr::c_across(tidyselect::starts_with("total.")), na.rm = TRUE)) # dim(totals.2024) 2 7

#-----------------------------------------------------
# Compute values to use in Recycling valueBox, infoBox
#-----------------------------------------------------
# Current stock
numb.PET.stock <- tail(containers$numb.PET.stock, n=1)
numb.cans.stock <- tail(containers$numb.cans.stock, n=1) 
numb.glass.stock <- tail(containers$numb.glass.stock, n=1)
numb.carton.stock <- tail(containers$numb.carton.stock, n=1)

numb.all.containers.stock <- tail(containers$numb.all.containers.stock,n=1)

# 2025, 2024
numb.PET.collected.2025 <- function.comma.to.thousands(totals.2025$total.PET[1])
numb.PET.collected.2024 <- function.comma.to.thousands(totals.2024$total.PET[1])

numb.cans.collected.2025 <- function.comma.to.thousands(totals.2025$total.cans[1])
numb.cans.collected.2024 <- function.comma.to.thousands(totals.2024$total.cans[1])

numb.glass.collected.2025 <- function.comma.to.thousands(totals.2025$total.glass[1])
numb.glass.collected.2024 <- function.comma.to.thousands(totals.2024$total.glass[1])

numb.carton.collected.2025 <- function.comma.to.thousands(totals.2025$total.carton[1])
numb.carton.collected.2024 <- function.comma.to.thousands(totals.2024$total.carton[1])

numb.PET.refunded.2025 <-  function.comma.to.thousands(totals.2025$total.PET[2])
numb.PET.refunded.2024 <-  function.comma.to.thousands(totals.2024$total.PET[2])

numb.cans.refunded.2025 <-  function.comma.to.thousands(totals.2025$total.cans[2])
numb.cans.refunded.2024 <-  function.comma.to.thousands(totals.2024$total.cans[2])

numb.glass.refunded.2025 <-  function.comma.to.thousands(totals.2025$total.glass[2])
numb.glass.refunded.2024 <-  function.comma.to.thousands(totals.2024$total.glass[2])

numb.carton.refunded.2025 <-  function.comma.to.thousands(totals.2025$total.carton[2])
numb.carton.refunded.2024 <-  function.comma.to.thousands(totals.2024$total.carton[2])

date.earliest.record.recycling <- format(min(containers$date.of.activity, na.rm = TRUE), "%d %B %Y")
date.latest.record.recycling <- format(max(containers$date.of.activity, na.rm = TRUE),"%d %B %Y")

#*************************************************************
# Read data to use under menuItem "Employment" 
## Input file: job-applications-employment.gsheet
## sheetname="employment"
## sheetname="job_events"
#*************************************************************
# Google Sheets link containing employment data
sheet.ID.job <- "https://docs.google.com/spreadsheets/d/1Oq7Vfm0xemPazUM2GhcqgLF6KVwqqQoBWmcONkSy34E/"

#---------------------------------------------------------------
# Read employment events from Google Sheets and process the data
#---------------------------------------------------------------
employment_data <- googlesheets4::read_sheet(sheet.ID.job, sheet = "employment") %>%
  as.data.frame() %>%
  dplyr::group_by(company, position_title, employee_ID) %>%
  dplyr::mutate(
    start_date = min(event_date)  # Identify earliest event date per position
    ,end_date = case_when(
      # Set end date for current employment using start date or today after the start date
      "employment start" %in% event & !("employment end" %in% event) & lubridate::today() < as.Date("2025-02-10") ~ start_date
      ,"employment start" %in% event & !("employment end" %in% event) & lubridate::today() > as.Date("2025-02-10") ~ today()
      # Set latest event date as end date for past employment
      ,TRUE ~ max(event_date)
    )
    ,total_events = n()            # Count number of events
    ,position_label = paste(company, position_title, sep = " - ") # Create unique label for each position
  ) # dim(employment_data) 13 10

# Prepare data for horizontal bars in the timeline
## Summarise data to one row per company, position_title
employment_summary <- employment_data %>%
  dplyr::group_by(position_label, start_date, end_date) %>%
  dplyr::summarize(event_count = n(), .groups = "drop") %>%
  dplyr::arrange(start_date) %>% # Order by start date (earliest at bottom)
  dplyr::mutate(
    position_order = row_number() # Assign row number to maintain order in plot
    ,position_factor = factor(position_label, levels = position_label[order(position_order)]) # Factor for legend order
  ) # dim(employment_summary) 4 6

# Merge row_number from employment_summary back into employment_data
employment_events <- employment_data %>%
  dplyr::left_join(
    employment_summary %>% select(position_label, position_order, position_factor)
    ,by = "position_label") # dim(employment_events) 13 12

# Generate color-blind-friendly color palette
bar_colors <- viridisLite::viridis(n = length(unique(employment_summary$position_factor)), option = "turbo")

color_map <- setNames(bar_colors, levels(employment_summary$position_factor))

# Prepare event dataset with appropriate symbols and hover text
employment_event_data <- employment_events %>%
  dplyr::mutate(
    symbol = case_when(
      event == "employment start" ~ "triangle-up", 
      event == "employment end" ~ "triangle-down", 
      TRUE ~ "circle"
    ),
    y_adjusted = position_order + 0.05, # Shift symbols slightly above bars
    event_color = color_map[position_factor], # Assign colors
    hover_text = paste("Event: ", event, "<br>Date: ", event_date)
  ) # dim(employment_event_data) 13 16

#----------------------------
# Read Job application events
#----------------------------
# Read job application events
job.data <- googlesheets4::read_sheet(sheet.ID.job
                                      ,sheet = "job_events"
                                      ,na="NA") %>%
  as.data.frame() %>%
  dplyr::group_by(company, position_title, reference_number) %>%
  dplyr::mutate(
    start_date = min(event_date)
    ,end_date = max(event_date)
    ,position_label = case_when(
      # Concatenate company, position_title
      is.na(reference_number) ~ paste(company, position_title, sep = " - ")
      # or company, position_title, reference_number (if available)
      ,TRUE ~ paste0(company, " - ", position_title, "( job ID: ", reference_number," )")
    ) 
  ) # dim(job.data) 149 9

job.summary <- job.data %>%
  dplyr::group_by(position_label, start_date, end_date) %>%
  dplyr::summarize(event_count = n(), .groups = "drop") %>%
  # Order by start date (earliest at bottom)
  dplyr::arrange(start_date) %>% 
  dplyr::mutate(
    position_order = row_number() # Assign row number to maintain order in plot
    ,position_factor = factor(position_label, levels = position_label[order(position_order)]) # Factor for legend order
  ) # dim(job.summary) 52 6

# Merge row_number from job.summary back into job.data
job.events <- job.data %>%
  dplyr::left_join(
    job.summary %>% select(position_label, position_order, position_factor)
    ,by = "position_label") # dim(job.events) 149 11

# Generate color-blind-friendly color palette
job.bar.colors <- viridisLite::viridis(n = length(unique(job.summary$position_factor)), option = "turbo")

job.color.map <- setNames(job.bar.colors, levels(job.summary$position_factor))

# Prepare event dataset with appropriate symbols and hover text
job.events.data <- job.events %>%
  dplyr::mutate(
    symbol = case_when(
      event == "submitted application" ~ "triangle-up"
      ,event == "rejected application" ~ "triangle-down"
      ,TRUE ~ "circle"
    )
    # Shift symbols slightly above bars
    ,y_adjusted = position_order + 0.05
    # Assign colors
    ,event_color = color_map[position_factor]
    ,hover_text = paste(position_label
                        ,"<br>Event: ", event
                        ,"<br>Date: ", event_date)
  ) # dim(job.events.data) 149 15

#*************************************************************
# Read data to use under menuItem "Electricity" 
#*************************************************************
# Read tsv in globa.R
alinta_bills_balance_brought_forward_plot_data <- readr::read_tsv(
   file = file.path("data","plot_data_alinta_bills_balance_brought_forward.tsv")
  ,col_types = readr::cols()
) %>%
  # Re-create factor for chronological X-axis
  dplyr::mutate(
    month_year_factor = factor(
      month_year,
      levels = unique(dplyr::arrange(., sort_ym) %>% dplyr::pull(month_year))
    )
  )
# dim(alinta_bills_balance_brought_forward_plot_data) 36 8

alinta_bills_balance_brought_forward_table_data <- readr::read_tsv(
  file =file.path("data","alinta_bills_balance_brought_forward.tsv")
  ,col_types = readr::cols()
)
# dim(alinta_bills_balance_brought_forward_table_data) 12 7

alinta_bills_usage_rates_total_credits <-readr::read_tsv(
  file = "data/alinta_bills_usage_rates_total_credits.tsv"
  ,col_types = readr::cols()
  )
# dim(alinta_bills_usage_rates_total_credits) 79 12

alinta_bills_rates_over_supply_period <- readr::read_tsv(
  file = file.path("data","alinta_bills_rates_over_supply_period.tsv")
  ,col_types = readr::cols()
) # dim(alinta_bills_rates_over_supply_period) 11 18

#-------------------------------
# valueBox
## Total electricity consumption
## Total solar export
#-------------------------------
electricity_supply_start_first <- min(alinta_bills_balance_brought_forward_table_data$supply_start, na.rm = TRUE)
electricity_supply_end_last  <- max(alinta_bills_balance_brought_forward_table_data$supply_end, na.rm = TRUE)

electricity.consumption.general.usage<- sum(alinta_bills_balance_brought_forward_table_data$peak_kwh, na.rm = TRUE)
electricity.consumption.controlled.load <-sum(alinta_bills_balance_brought_forward_table_data$ctl1_kwh, na.rm = TRUE)
electricity.consumption.total <- sum(electricity.consumption.general.usage, electricity.consumption.controlled.load) 

solar.export.total <- sum(alinta_bills_balance_brought_forward_table_data$solar_kwh, na.rm = TRUE)

#-----------------------------------------------
# valueBox
## Total amount paid for electricity consumption
## Total amount earned from solar export
#-----------------------------------------------
amount.paid.total.by.item <- alinta_bills_usage_rates_total_credits %>% 
  dplyr::group_by(Item) %>% 
  dplyr::summarise(total.paid.or.earned=sum(total_num))
# dim(amount.paid.total.by.item) 8 2

total.amount.paid.controlled.load.1 <- as.numeric(amount.paid.total.by.item[1,2])
total.amount.paid.daily.charge <- as.numeric(amount.paid.total.by.item[2,2])
total.amount.paid.daily.charge.controlled.load.1 <- as.numeric(amount.paid.total.by.item[3,2])
total.amount.paid.peak.hour.surcharge <- as.numeric(amount.paid.total.by.item[4,2])
total.amount.paid.offpeak.usage <- as.numeric(amount.paid.total.by.item[5,2])
total.amount.paid.peak.usage <- as.numeric(amount.paid.total.by.item[6,2])
total.amount.paid.shoulder.usage <- as.numeric(amount.paid.total.by.item[7,2])
total.amount.earned.solar.export <- as.numeric(amount.paid.total.by.item[8,2])

total.amount.all.hours.usage <- sum( total.amount.paid.peak.hour.surcharge
                                    ,total.amount.paid.offpeak.usage
                                    ,total.amount.paid.peak.usage
                                    ,total.amount.paid.shoulder.usage)
#-----------------------------------------------
# valueBox
## Current rates and charges
#-----------------------------------------------
most_recent_Rate_Incl_GST_Controlled_Load_1 <- paste0(tail(alinta_bills_rates_over_supply_period, n=1)[1,2],"/KWh")
most_recent_Rate_Incl_GST_Daily_Charge <- paste0(tail(alinta_bills_rates_over_supply_period, n=1)[1,3],"/Day")
most_recent_Rate_Incl_GST_Daily_Charge_Controlled_Load_1 <-paste0(tail(alinta_bills_rates_over_supply_period, n=1)[1,4],"/Day")
most_recent_Rate_Incl_GST_NonPeak <- paste0(tail(alinta_bills_rates_over_supply_period, n=1)[1,5],"/KWh")
most_recent_Rate_Incl_GST_Standard_Solar <- paste0(tail(alinta_bills_rates_over_supply_period, n=1)[1,6],"/KWh")
most_recent_Rate_Incl_GST_Peak <- paste0("$",tail(alinta_bills_rates_over_supply_period, n=1)[1,11] + 
                                             tail(alinta_bills_rates_over_supply_period, n=1)[1,13]
                                         ,"/KWh")
#---------------------------
# Check shinyapps.io account
#---------------------------
#rsconnect::accountInfo()
# $name
# [1] "luenhchang"
# 
# $server
# [1] "shinyapps.io"
# 
# $accountId
# [1] "981190"
# 
# $token
# [1] "A9C5D75F89CEC1B84E8F1CCFEEBB5CF1"
# 
# $secret
# [1] "W10qQN... (redacted)"
# 
# $username
# [1] "luenhchang"
#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#