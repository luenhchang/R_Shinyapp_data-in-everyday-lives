#-------------------------------------------------------------------------------------------------------------------
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
##------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
# Load R packages
## Required uninstalled packages in local PC will cause errors library(pkg) is not available while deploying app to shinyapps.io
#------------------------------------------------------------------------------------------------------------------
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

#------------------------------------------------------------------------
# Directory in local PC
## 
## www: Where all the images and other assets needed for the viewer
#------------------------------------------------------------------------
# dir.C <- "C:"
# dir.app <- file.path(dir.C, "GoogleDrive","scripts","R-shinyapp_data-in-everyday-lives")
# dir.www <- file.path(dir.app,"www")
# dir.create(path = dir.www)
# dir.img <- file.path(dir.www,"image_data-challenges")
# dir.img.annotated <- file.path(dir.www,"image_data-challenges-annotated") # dir.create(dir.img.annotated)
# dir.data <- file.path(dir.app,"data")
# dir.create(dir.data)

# 
# text.1 <- "Create structured folders and subfolders"
# # Read a jpg file
# img.1 <- magick::image_read(path = file.path(dir.img,"nested-folders-subfolders.jpg")) |>
#   magick::image_annotate(text = text.1, size = 30
#                          ,gravity = "north" # location of annotation (north=top, northeast= top right)
#                          ,color = "blue"
#                          ,font = "Roboto") |>
#   magick::image_write(path = file.path(dir.img.annotated,"nested-folders-subfolders[1]annotated.jpg"))
  # class(img.1) [1] "magick-image"

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
sheet_id <- "https://docs.google.com/spreadsheets/d/1Q_oxLE1cl_r9gksXX4R9YGnC2nGZBy3M1hf3-JR2ZIw/"

# Data is one row per data entry
containers.2024 <- googlesheets4::read_sheet(sheet_id) |>
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
    ) # Close mutate() # class(containers.2024) [1] "data.frame" # dim(containers.2024) 335 15

# Calculate daily number of containers 
## Adding multiple collections to one number per day
containers.daily.wide <- containers.2024 |>
  dplyr::group_by(activities, date.of.activity) |>
  dplyr::summarise( number.PET.day=sum(number.PET, na.rm = TRUE)
                   ,number.cans.day=sum(number.cans, na.rm = TRUE)
                   ,number.glass.day=sum(number.glass, na.rm = TRUE)
                   ,number.carton=sum(number.carton, na.rm = TRUE)
                   ,number.all.types.day=sum(number.all.types, na.rm = TRUE)
                   # suppress NAs in paste()
                   ,note=paste(ifelse(is.na(Note),"", Note), collapse = "\n")
                   ) |>
  dplyr::mutate(weeks=as.numeric(difftime(date.of.activity, min(date.of.activity), units = "weeks"))) # dim(containers.daily.wide) 178 9

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
      ,TRUE ~ container.number.adjusted)) # dim(containers.daily.long) 890 7

# Data used to create stacked bar plot
## Data structure: 1 row per activities, date of activity, container type
containers.daily.long.not.all.types <- containers.daily.long |> 
  dplyr::filter(container.type !="all.types") # dim(containers.daily.long.not.all.types) 712 7

# Calculate total containers per day regardless of types
## This is used to place number on top of each stacked bar
totals.all.types <- containers.daily.long.not.all.types |> 
  dplyr::group_by(activities, date.of.activity) |> 
  dplyr::summarise(total=sum(container.number.adjusted)) |>
  dplyr::arrange(date.of.activity, activities) |>
  dplyr::mutate(
    # Set total >=10 or <0 to be printed, total of 0 to 9 to be missing
    total.label=dplyr::case_when(total %in% c(0:9) ~ NA_integer_
                                 ,TRUE ~ total)) # dim(totals.all.types) 178 4

# Calculate total number of collected or refunded containers
totals <- containers.2024 |> 
  dplyr::group_by(activities) |>
  dplyr::summarise(total.PET=sum(number.PET, na.rm = TRUE)
                   ,total.cans=sum(number.cans, na.rm = TRUE)
                   ,total.glass=sum(number.glass, na.rm = TRUE)
                   ,total.carton=sum(number.carton, na.rm = TRUE)
                   ,number.activities= dplyr::n()) |>
  # Total containers collected or refunded
  dplyr::rowwise() |>
  dplyr::mutate(total=sum(dplyr::c_across(tidyselect::starts_with("total.")), na.rm = TRUE)) # dim(totals) 2 7

#-----------------------------------------------------
# Compute values to use in Recycling valueBox, infoBox
#-----------------------------------------------------
# Function to format numbers
my_comma <- scales::label_comma(accuracy = 1, big.mark = ",", decimal.mark = ".")

numb.PET.stock <- tail(containers.2024$numb.PET.stock, n=1)
numb.cans.stock <- tail(containers.2024$numb.cans.stock, n=1) 
numb.glass.stock <- tail(containers.2024$numb.glass.stock, n=1)
numb.carton.stock <- tail(containers.2024$numb.carton.stock, n=1)
numb.all.containers.stock <- tail(containers.2024$numb.all.containers.stock,n=1)

numb.collections.made <- totals$number.activities[1]

numb.PET.collected <- my_comma(totals$total.PET[1])
numb.cans.collected <- my_comma(totals$total.cans[1])
numb.glass.collected <- my_comma(totals$total.glass[1])
numb.carton.collected <- my_comma(totals$total.carton[1])
numb.all.containers.collected <- my_comma(totals$total[1])

numb.refunds.received <- totals$number.activities[2]

numb.PET.refunded <- my_comma(totals$total.PET[2])
numb.cans.refunded <- my_comma(totals$total.cans[2])
numb.glass.refunded <- my_comma(totals$total.glass[2])
numb.carton.refunded <- my_comma(totals$total.carton[2])
numb.all.containers.refunded <- my_comma(totals$total[2])

date.earliest.record.recycling <- format(min(containers.2024$date.of.activity, na.rm = TRUE), "%d %B %Y")
date.latest.record.recycling <- format(max(containers.2024$date.of.activity, na.rm = TRUE),"%d %B %Y")

#*****************************************
# Read data to use under menuItem "Jobs" 
## Input: text copied from Google keep note
#*****************************************

# Plot event log dates per ID
jobs.current <- "company, position title, reference number, event, start date
Greenlight Clinical, Statistical programmer, NA, employment, 2021-09-27
Queensland Health, Senior Data Scientist, QLD/CPSS547629, submitted application, 2024-03-12
Australian Signals Directorate, Data Analytic Engineer, ASD/01826/24, submitted application, 2024-04-17
CRO through i-Pharm Consulting, Statistical Programmer II, NA, submitted application, 2024-04-23
Green Light Worldwide, Data Analyst (security clearance), BH-18098-3, no longer accepting applications, 2024-04-26
Queensland Police Service, Senior Statistical Analyst, QLD/559041/24,submitted application, 2024-04-30
Logan Together, Data Analyst, NA, application sent through LinkedIn, 2024-05-03
Abano Healthcare Group, Data Analyst, NA, application sent through LinkedIn, 2024-05-04"

jobs.past <- "company, position title, reference number, event, start date, end date
University of Queensland, PhD student, NA, employment, 2016-04-05, 2020-05-11
QIMR Berghofer Medical Research Institute, Research Officer, NA, employment, 2020-04-27, 2021-11-09"

jobs.current.df <- read.table(text = jobs.current, sep = ",", header = TRUE, na.strings = "NA") |>
  dplyr::mutate(  event.start.date=lubridate::ymd(start.date)
                 ,event.end.date=Sys.Date()
                 ,event.duration.days=difftime(time1=event.end.date, time2=event.start.date, units = "days")) |>
  dplyr::select(-start.date) # dim(jobs.current.df) 8 7

jobs.past.df <- read.table(text = jobs.past, sep = ",", header = TRUE, na.strings = "NA") |>
  dplyr::mutate(event.start.date=lubridate::ymd(start.date)
                ,event.end.date=lubridate::ymd(end.date)
                ,event.duration.days=difftime(time1=event.end.date, time2=event.start.date, units = "days")) |>
  dplyr::select(-start.date,-end.date) # dim(jobs.past.df) 2 7

jobs <- dplyr::bind_rows(jobs.current.df, jobs.past.df) |> 
  dplyr::arrange(desc(event.start.date))

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