#----------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives/functions.R
# Date created: 13-JAN-2025
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_Strava-activity-data/functions.R
## Date       Changes:
##---------------------------------------------------------------------------------------------------------
## 2025-01-15 Moved customed functions from server.R, global.R  to this file
##---------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------
# Create functions for output
## [Create Function for output in Shiny](https://stackoverflow.com/questions/53590526/create-function-for-output-in-shiny)
## ["Correct" way to specifiy optional arguments in R functions](https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions)
## [How do you use "<<-" (scoping assignment) in R?](https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r)
#------------------------------------------------------------------------------------------------
# Define function for renderValueBox()
function.renderValueBox <- function(shiny_output
                                    ,output.id
                                    ,argument.value
                                    ,argument.subtitle
                                    ,argument.icon
                                    ,argument.icon.lib
                                    ,argument.color){
  # Write default values to optional arguments
  if(missing(argument.icon)){argument.icon <- "th-list"}
  if(missing(argument.icon.lib)){argument.icon.lib <- "glyphicon"}
  if(missing(argument.color)){argument.color<-"orange"}
  
  shiny_output[[output.id]] <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = argument.value
      ,subtitle = argument.subtitle
      #,icon = icon(argument.icon, lib = argument.icon.lib)
      ,color = argument.color)
  }) # Close renderValueBox()
} # Close function{}


# Define function for renderInfoBox()
## Default icon set to trophy
function.renderInfoBox <- function(shiny_output, output.id, arg.title, arg.value, arg.icon, arg.color, arg.fill){
  # Write default values to optional arguments
  if(missing(arg.icon)){arg.icon<-"trophy"}
  if(missing(arg.color)){arg.color<-"olive"}
  if(missing(arg.fill)){arg.fill <- TRUE}
  
  shiny_output[[output.id]] <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = arg.title
      ,value = arg.value
      ,icon=icon(arg.icon)
      ,color = arg.color
      ,fill = arg.fill)
  }) # Close renderInfoBox()
}# Close function{}


