#---------------------------------------------------------------------------------------------------------
# Program: C:/GoogleDrive_MyDrive/scripts/RProject_Shinyapp_data-in-everyday-lives/server.R
# Date created: 13-Nov-2024
# Author(s): Lun-Hsien Chang
# Modified from: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/server.R
# Dependency:
# Local input: C:/GoogleDrive/scripts/R-shinyapp_data-in-everyday-lives/www/speed-test-printscreen_Telstra5G/*.jpg
## [Tree Maps Understanding and using Tree Maps](https://www.tableau.com/data-insights/reference-library/visual-analytics/charts/treemaps)
## Date       Changes:
##--------------------------------------------------------------------------------------------------------
## 2025-01-13 Deleted code for menuItem "Data Challenges", "Fitness", "Job"
## 2024-06-04 Added plotly bar plot subplots, one per food category. Currently no control on bar color and subplot titles
## 2024-05-16 Commented out shiny::renderUI({rmarkdown::render() shiny::includeHTML('menuItem-Fitness.html') }). Now includeHTML() is used to read a html file, which was knitted as html_fragment in .Rmd files
## 2024-05-07 Added a linebreak in renderDataTable with 2 steps: (1) add <br> as linebreak symbol, (2) set datatable(escape=FALSE)
## 2024-05-06 Used valueBox(subtitle=HTML(paste("<b>","text to bold","</b>")) to bold text in valueBox subtitle.
## 2024-05-06 Used valueBox(subtitle=HTML(paste(br()))) to add a new line in valueBox subtitle. 
## 2024-04-24 Created function.renderValueBox(), function.renderInfoBox() with optional arguments that have default values to generate script for renderValueBox(), renderInfoBox()
## 2024-04-23 Added a image slideshow under menuItem Data 
##--------------------------------------------------------------------------------------------------------------

# Get functions here
#setwd(dir.app)
source("functions.R")

server <- function(input, output, session) {
  
  # Stop running shiny app when closing browser window
  ## [How to stop running shiny app by closing the browser window?](https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window)
  ## This is a bad idea! If multiple users are connected to the app, then one user (or browser tab) exiting will cause everyone to be kicked off! – Joe Cheng Aug 7, 2020 at 19:23
  session$onSessionEnded(function() { stopApp() })
  

  #*****************************************
  # Outputs to use under menuItem "About"
  #*****************************************
  
  #*****************************************
  # Outputs to use under menuItem "Food"
  #*****************************************
  
  #----------------
  # Output valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  function.renderValueBox( shiny_output = output
                          ,output.id="valueBox.num.unique.food.barcodes"
                          ,argument.value=num.unique.food.barcode
                          ,argument.subtitle="Barcodes scanned")
  
  function.renderValueBox( shiny_output = output
                          ,output.id="valueBox.num.food.category"
                          ,argument.value=num.food.category
                          ,argument.subtitle="Food categories")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.num.food.no.category"
                          ,argument.value = num.food.no.category
                          ,argument.subtitle = "Uncategorised foods"
                          ,argument.icon ="thumbs-down" )
  
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.num.food.expired"
                          ,argument.value=num.food.expired
                          ,argument.subtitle="Foods expired"
                          ,argument.icon ="thumbs-down" )
  
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.summed.price.food"
                          ,argument.value=paste0("$AUD ",summed.price.food)
                          ,argument.subtitle="Spent on food"
                          ,argument.icon ="credit-card")

  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  ## Ref [How can I introduce a new line within a column using DTedit and shiny::uiOutput?](https://stackoverflow.com/questions/56590555/how-can-i-introduce-a-new-line-within-a-column-using-dtedit-and-shinyuioutput)
  #----------------------------------------------------------------------------------------
  output$table.food.category.product.name <- DT::renderDataTable({
    table.food.category.product.name <- barcode.food.category |> 
      dplyr::select(category, product.name) |> 
      dplyr::distinct() |> 
      dplyr::arrange(category, product.name) |>
      dplyr::rename(Category=category, Product=product.name) # dim(table.food.category.product.name) 122 2
    
    DT::datatable(table.food.category.product.name
                  ,options = list(autoWidth = FALSE, searching = TRUE))
  })
  output$table.food.expiring.DT <- DT::renderDataTable({
    #print(food.expiring.365)
    DT::datatable(food.expiring.365
                  ,options = list(autoWidth = FALSE, searching = TRUE))
    
  }) # Close the renderDataTable function
  
  output$table.food.consumed.DT <- DT::renderDataTable({
    #print(food.consumed)
    DT::datatable(food.consumed
                  ,options = list(autoWidth=FALSE, searching=TRUE ))
  }) # Close the renderDataTable function
  
  output$table.food.price.DT <- DT::renderDataTable({
    #print(food.price)
    DT::datatable(food.price
                  ,options = list(autoWidth=FALSE, searching=TRUE ))
  }) # Close the renderDataTable function
  
  #-------------
  # Output plots 
  #-------------
  # Treemap for food category frequencies
  output$plot.treemap.food.category <- shiny::renderPlot({
    plot.treemap.food.category <- ggplot2::ggplot(data = barcode.food.category.count
                                                  ,aes(area=count, fill=count, label=category))+
      treemapify::geom_treemap()+
      treemapify::geom_treemap_text(colour="white"
                                    ,place = "centre"
                                    ,size=15
                                    ,grow = TRUE)+
      ggplot2::scale_fill_viridis_c()
    return(plot.treemap.food.category)
  }) # Close renderPlot()
  
  # Facet bar plots for monthly spend on food categories using ggplot2
  ## This plot is not showing up in R Shiny. Warning: Error in graphics::plot.new: figure margins too large
  output$ggplot2.barplot.monthly.spend.food.category <- shiny::renderPlot({
    
    # Monthly total expenditure on food category
    ## [How do I get R to recognize the appropriate order for Month Year combinations without manually specifying the order?](https://stackoverflow.com/questions/71498479/how-do-i-get-r-to-recognize-the-appropriate-order-for-month-year-combinations-wi)
    ## [Removing white spaces between facets when using ggplotly()](https://stackoverflow.com/questions/63152833/removing-white-spaces-between-facets-when-using-ggplotly)
    bar.plot.monthly.spend.on.food.category <- ggplot2::ggplot(
      data = expenditure.food.category
      ,aes(x= reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
           ,y=price.summed
           ,text=paste("$AUD",price.summed,"spent on", category, "in", timestamp.month.label, timestamp.year))) +
      ggplot2::geom_col()+
      ggplot2::facet_wrap(~category, ncol = 4, scales = "free_y",strip.position = "top") +
      ggplot2::scale_x_discrete(expand = c(0,0))+
      ggplot2::labs(x="Month",y="Spend ($AUD)")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 10)
      )
    # Not converting a ggplot2 to a plotly object as white space between sub plots/facetted plots is not able to remove
    #plotly::ggplotly(p=bar.plot.monthly.spend.on.food.category, tooltip = "text")
  })
  
  # Facet bar plots for monthly spend on food categories using plotly
  ## [How to facet a plot_ly() chart?](https://stackoverflow.com/questions/58103196/how-to-facet-a-plot-ly-chart)
  ## ["Warning: Error in : Tibble columns must have compatible sizes." when adding hoverinfo to filtered plot](https://stackoverflow.com/questions/68291943/warning-error-in-tibble-columns-must-have-compatible-sizes-when-adding-hov)
  output$plotly.barplot.monthly.spend.food.category <- plotly::renderPlotly({
    expenditure.food.category |>
      dplyr::group_by(category.factor) |>
      plotly::do(p=plot_ly(.
                           , x=~timestamp.month.label
                           , y=~price.summed
                           , color = ~category.factor
                           , type = "bar"
                           , mode = "markers"
                           , orientation="v"
                           , text= ~price.summed # text placed on top of bars or as hoverinfo
                           , textposition="auto"
                           , hoverinfo="text"
                           , hovertext= ~paste(
                             "Month year :", timestamp.month.label, timestamp.year
                             ,"<br> Category :", category
                             ,"<br> Cost : $AUD", price.summed)
                           ) # Close plot_ly()
                 ) |>
      plotly::subplot(nrows = 4
                      ,shareX = TRUE 
                      ,shareY = FALSE
                      ,margin=0.01
                      ,titleX = FALSE
                      ,titleY = FALSE) 
  })
  
  # Facet lollipop plot using ggplot2
  # Lollipop chart horizontal. Lollipops overlap legend in vertical lollipop plot
  ## This plot is not showing up in R Shiny. Warning: Error in graphics::plot.new: figure margins too large
  ## [Lollipop chart with multiple groupings: how to remove and alter legends?](https://stackoverflow.com/questions/62391894/lollipop-chart-with-multiple-groupings-how-to-remove-and-alter-legends)
  ## [Text labels on condition for lollipop plot in ggplot](https://stackoverflow.com/questions/50135338/text-labels-on-condition-for-lollipop-plot-in-ggplot)
  ## Reference [Custom lollipop chart](https://r-graph-gallery.com/301-custom-lollipop-chart.html)
  output$facet.lollipop.plot.monthly.spend.on.food.categories <- shiny::renderPlot({
    ggplot(data = expenditure.food.category
           ,aes(x=price.summed
                ,y=reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
                ,fill = category.firstup
                ,label=round(price.summed, digits = 0)))+
      geom_segment(aes(y=reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
                       ,yend=reorder(timestamp.month.label, lubridate::my(timestamp.month.label))
                       ,x=0
                       ,xend = price.summed)
                   ,color="skyblue")+
      geom_point(size=7.5, color="blue", alpha=0.6)+
      geom_text(size =3, color="white" #nudge_x = 0.1
      ) +
      facet_wrap(~category.firstup, ncol = 3, scales = "free_x",strip.position = "top")+
      ggplot2::labs( y="Month"
                    ,x="Spend ($AUD)")+
      theme_minimal()+
      # Apply The Economist theme
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 10)
        ,legend.position = "none"
        ,panel.grid.major.y = element_blank()
        ) # Close theme()
    })

  #---------------
  # Output infoBox 
  ##  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #---------------
  function.renderInfoBox(shiny_output = output
                         ,output.id="infoBox.food.barcode.earliest.date.record"
                         ,arg.title="Data collected since"
                         ,arg.value=food.barcode.earliest.date.record)
  
  function.renderInfoBox(shiny_output = output
                         ,output.id="infoBox.food.barcode.latest.date.record"
                         ,arg.title="Latest update on"
                         ,arg.value=food.barcode.latest.date.record)
  
  #*****************************************
  # Outputs to use under menuItem "Bathroom"
  #*****************************************
  
  #----------------
  # Output valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #----------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.num.unique.hygiene.barcode"
                          ,argument.value=num.unique.hygiene.barcode
                          ,argument.subtitle="Personal Care Product Barcodes scanned")
  
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.summed.price.hygiene"
                          ,argument.value=paste0("$AUD ", summed.price.hygiene)
                          ,argument.subtitle="Spent on personal care products")
  
  #----------------------------------------------------------------------------------------
  # Output DT dataTable
  ## Ref [How can I introduce a new line within a column using DTedit and shiny::uiOutput?](https://stackoverflow.com/questions/56590555/how-can-i-introduce-a-new-line-within-a-column-using-dtedit-and-shinyuioutput)
  #----------------------------------------------------------------------------------------
  
  output$table.hygiene.count.unopened.DT <- DT::renderDataTable({
    #print(hygiene.count.unopened)
    DT::datatable(hygiene.count.unopened, options = list(autoWidth=TRUE, searching=TRUE))
  }) # Close the renderDataTable function
  
  output$table.hygiene.price.DT <- DT::renderDataTable({
    #print(hygiene.price)
    DT::datatable(hygiene.price, escape=FALSE, options = list(autoWidth=TRUE, searching=TRUE))
  }
  #, escape=FALSE
  ) # Close the renderDataTable function

  #**********************************************
  # Outputs to use under menuItem "Recycling"
  #**********************************************
  
  #----------------
  # Current stock valueBox
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  ## [How to break line inside a paste0() function in shiny dashboard](https://stackoverflow.com/questions/51079153/how-to-break-line-inside-a-paste0-function-in-shiny-dashboard)
  #----------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.numb.all.containers.stock"
                          ,argument.value=numb.all.containers.stock
                          ,argument.subtitle=HTML(paste0("Containers in stock, including",br()
                                                         ,"<b>",numb.PET.stock," plastic bottles","</b>", br()
                                                         ,"<b>",numb.cans.stock," cans","</b>", br()
                                                         ,"<b>",numb.glass.stock," glass bottles","</b>", br()
                                                         ,"<b>",numb.carton.stock," carton","</b>")
                                                  ) # Close HTML()
                          ,argument.color="black")
  #-----------------------
  # Output plots
  #-----------------------
  
  #----------------
  # 2025 valueBoxes
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  ## [How to break line inside a paste0() function in shiny dashboard](https://stackoverflow.com/questions/51079153/how-to-break-line-inside-a-paste0-function-in-shiny-dashboard)
  #----------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.year.in.container.collection.2025"
                          ,argument.value=unique(lubridate::year(containers.2025$date.of.activity))
                          ,argument.subtitle="Year in collection")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.collections.made.2025"
                          ,argument.value = totals.2025$number.activities[1]
                          ,argument.subtitle = "Collections made")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.containers.collected.2025"
                          ,argument.value = function.comma.to.thousands(totals.2025$total[1]) #numb.all.containers.collected
                          ,argument.subtitle = HTML(
                            paste0("Containers collected, including",br()
                                   ,"<b>", numb.PET.collected.2025," plastic bottles","</b>", br()
                                   ,"<b>", numb.cans.collected.2025," cans","</b>", br()
                                   ,"<b>", numb.glass.collected.2025, " glass bottles","</b>", br()
                                   ,"<b>", numb.carton.collected.2025, " carton","</b>")
                          ) # Close HTML()
  )
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.refunds.received.2025"
                          ,argument.value = totals.2025$number.activities[2] 
                          ,argument.subtitle = "Refunds received")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.containers.refunded.2025"
                          ,argument.value = function.comma.to.thousands(totals.2025$total[2]) #numb.all.containers.refunded
                          ,argument.subtitle = HTML(
                            paste0("Containers refunded, including", br()
                                   ,"<b>", numb.PET.refunded.2025," plastic bottles","</b>", br()
                                   ,"<b>", numb.cans.refunded.2025," cans","</b>", br()
                                   ,"<b>", numb.glass.refunded.2025, " glass bottles","</b>", br()
                                   ,"<b>", numb.carton.refunded.2025, " carton","</b>")
                          ) # Close HTML()
  )
  
  #-----------------------------------
  # 2025 stacked bar plot using plotly
  #-----------------------------------
  output$plot.stacked.bars.containers.2025 <- plotly::renderPlotly({
    #--------------------------------------------------
    # Create stacked bar plot using plotly without add_trace
    #--------------------------------------------------
    
    # Colors for different container types
    colors <- as.vector(pals::glasbey(n = 11))[c(8, 2, 9, 11)]
    
    # Prepare data for plotly
    data <- containers.daily.long.not.all.types.2025 %>%
      dplyr::mutate(container.type = factor(container.type, levels = c("PET", "cans", "glass", "carton")))
    
    # Calculate total of containers collected or refunded per day separately:
    # totals <- containers.daily.long.not.all.types.2025 %>%
    #   dplyr::mutate(activities=gsub(x=activities, pattern = " ", replacement="_")) %>%
    #   dplyr::group_by(date.of.activity, activities) %>%
    #   dplyr::summarize(total = sum(container.number.adjusted), .groups = "drop") %>%
    #   tidyr::pivot_wider(
    #     names_from = activities,
    #     values_from = total,
    #     names_prefix = "total_",
    #     values_fill = list(total = 0)
    #   ) # dim(totals) 6 3
    
    # stacked_heights <- totals %>%
    #   mutate(
    #     # Dynamic offset: Ensures text does not overlap, uses min + scaling factor
    #     offset = case_when(
    #       total_Collection > 0  ~ max(5, total_Collection * 0.15),  # Always at least 5, scales with height
    #       total_Collection == 0 & total_Refund_in_cash != 0 ~ 5,  # Refund-only case: Small offset
    #       TRUE ~ 0
    #     ),
    #     
    #     # Adjust max_height to ensure text is placed above bars
    #     max_height = case_when(
    #       total_Collection > 0 ~ total_Collection + offset,  # Text above highest bar
    #       total_Collection == 0 & total_Refund_in_cash != 0 ~ offset,  # If only refund, keep slightly above 0
    #       TRUE ~ NA_real_
    #     )
    #   )
    
    # data.bar.labels <- totals %>%
    #   dplyr::group_by(date.of.activity) %>%
    #   dplyr::summarise(text_label = case_when(
    #     !(total_Collection ==0) & !(total_Refund_in_cash ==0) ~ 
    #       paste0(total_Collection, "\n", total_Refund_in_cash)
    #     ,!(total_Collection ==0) & (total_Refund_in_cash ==0) ~ as.character(total_Collection)
    #     ,(total_Collection ==0) & !(total_Refund_in_cash ==0) ~ as.character(total_Refund_in_cash)
    #     ,TRUE ~ NA_character_ ) # Close case_when()
    #     # Create y position for text_label
    #     ) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::distinct() %>%
    #   dplyr::mutate(text_label_y_position = stacked_heights$max_height) # dim(data.bar.labels) 6 2
    
    # Create the stacked bar plot
    plot <- plotly::plot_ly(
      data = data
      ,x = ~date.of.activity
      ,y = ~container.number.adjusted
      ,color = ~container.type
      ,colors = colors
      ,type = "bar") %>%
      plotly::layout(
        barmode = "stack"
        ,xaxis = list(
          title = ""
          ,tickformat = "%b-%Y"
          ,dtick = "M1")
        ,yaxis = list(
          title = "Number of containers"
          ,tickvals = seq(-300, 300, 50)  # Ensure enough space for negative values
          )
        ,legend = list(
          title = list(text = "Container type")
          ,orientation = "h"
          ,x = 0
          ,y = 1.1)
        ) %>%
      plotly::add_annotations(  x = containers.daily.stacked.bar.label.data.2025$date.of.activity
                               ,y = containers.daily.stacked.bar.label.data.2025$text_label_y_position
                               ,text = containers.daily.stacked.bar.label.data.2025$text_label
                               ,xref = "x"
                               ,yref = "y"
                               ,showarrow = FALSE)
    
    plot
  })
  
  #----------------
  # 2024 valueBoxes
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  ## [How to break line inside a paste0() function in shiny dashboard](https://stackoverflow.com/questions/51079153/how-to-break-line-inside-a-paste0-function-in-shiny-dashboard)
  #----------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.year.in.container.collection.2024"
                          ,argument.value=unique(lubridate::year(containers.2024$date.of.activity))
                          ,argument.subtitle="Year in collection")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.collections.made.2024"
                          ,argument.value = totals.2024$number.activities[1]
                          ,argument.subtitle = "Collections made")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.containers.collected.2024"
                          ,argument.value = function.comma.to.thousands(totals.2024$total[1]) #numb.all.containers.collected
                          ,argument.subtitle = HTML(
                            paste0("Containers collected, including",br()
                                   ,"<b>", numb.PET.collected.2024," plastic bottles","</b>", br()
                                   ,"<b>", numb.cans.collected.2024," cans","</b>", br()
                                   ,"<b>", numb.glass.collected.2024, " glass bottles","</b>", br()
                                   ,"<b>", numb.carton.collected.2024, " carton","</b>")
                          ) # Close HTML()
  )
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.refunds.received.2024"
                          ,argument.value = totals.2024$number.activities[2]
                          ,argument.subtitle = "Refunds received")
  
  function.renderValueBox(shiny_output = output
                          ,output.id = "valueBox.numb.containers.refunded.2024"
                          ,argument.value = function.comma.to.thousands(totals.2024$total[2]) #numb.all.containers.refunded
                          ,argument.subtitle = HTML(
                            paste0("Containers refunded, including", br()
                                   ,"<b>", numb.PET.refunded.2024," plastic bottles","</b>", br()
                                   ,"<b>", numb.cans.refunded.2024," cans","</b>", br()
                                   ,"<b>", numb.glass.refunded.2024, " glass bottles","</b>", br()
                                   ,"<b>", numb.carton.refunded.2024, " carton","</b>")
                          ) # Close HTML()
  )
  
  #------------------------------------
  # 2024 stacked bar plot using ggplot2
  #------------------------------------
  output$plot.stacked.bars.containers.2024 <- shiny::renderPlot({
    #--------------------------------------------------
    # Create upside down and bottom up stacked bar plot
    #--------------------------------------------------
    
    # bar width, position setttings
    width <- .75
    position <- ggplot2::position_stack(vjust=.5)
    
    # Display color from color code
    ## #8 light blue for PET, #2 red for cans, #9 brown for glass, #11 purple for carton
    colors <- as.vector(pals::glasbey(n=11))[c(8,2,9,11)]
    #scales::show_col(colors)
    
    # Pair legend item and color
    legend.items.color <- c( PET=colors[1], cans=colors[2], glass=colors[3],carton=colors[4])
    plot.legend.label.ordered <- c("Plastic bottles","Cans","Glass bottles","Carton") # length(plot.legend.ordered) 4
    
    # Create a stacked bar plot with total number placed on top over each bar
    ggplot2::ggplot(data=containers.daily.long.not.all.types.2024
                    ,aes( x=date.of.activity
                         ,y=container.number.adjusted
                         ,label=container.number.adjusted)) +
      # geom_col() as a shortcut for geom_bar(stat = "identity")
      ggplot2::geom_col(
        # Stacks of the bar order, from top to bottom, controlled by the levels
        aes(fill=factor(container.type, levels = c("PET","cans","glass","carton")))
        # Control transparency of bar color
        ,alpha=0.6)+
      # Draw sum number above the stacked bars using another dataset totals.all.types
      ggplot2::geom_text(data=totals.all.types.2024
                         ,aes(x=date.of.activity, y=total, label=total.label, fill=NULL)
                         ,vjust=0.5
                         ,size=5
                         ,angle=90
                         ,hjust=0.4)+
      # Months on bottom X axis, weeks on top X axis
      ggplot2::scale_x_date(expand = c(0, 0) # expand = c(0,0) to remove margins
                            ,date_breaks = "1 month"
                            ,date_labels = "%b-%Y" # %b Abbreviated month name in current locale (Aug)
                            ,date_minor_breaks = "1 week"
                            ) +
      # Set breaks on Y axis
      ggplot2::scale_y_continuous(breaks = seq(0, 300, 50))+
      ggplot2::labs(x = ""
                    ,y = "Number of containers"
                    # Change legend title from variable name to text
                    ,fill="Container type ")+
      # Remove default theme background color
      ggplot2::theme_bw()+
      ggplot2::theme_minimal()+
      # Apply The Economist theme
      ggthemes::theme_economist_white()+
      ggplot2::theme(
        # hjust controls horizontal justification and vjust controls vertical justification. Values between 0 and 1
        ## hjust=0 means left-justified, hjust=1 means right-justified
        legend.position = "top"
        ,legend.justification='left'
        ,legend.direction='horizontal'
        ,legend.text = element_text(size = 20)
        ,legend.box = "vertical"
        # Reduce gap between legends
        ,legend.spacing.y = unit(-0.25, "cm")
        ,axis.title = element_text(size = 20)
        ,axis.text = element_text(size = 15)
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(color = "gray", linewidth = 0.5))+
      # Modify legend for aesthetic=fill
      ggplot2::scale_fill_manual(
        # Change legend item color from default to colors
        # Use palette from pals package. as.vector() needed to remove color name
        values=legend.items.color
        ,labels=plot.legend.label.ordered)
  }) # Close renderPlot()
  
  #--------------------
  # Output DT dataTable
  #--------------------
  output$table.containers.daily.DT <- DT::renderDataTable({
    containers.daily.wide <- containers.daily.wide |> 
      dplyr::select(date.of.activity, activities, number.PET.day, number.cans.day, number.glass.day, number.carton, number.all.types.day, note) |>
      dplyr::rename( Date=date.of.activity
                    ,Activity=activities
                    ,Plastic=number.PET.day
                    ,Cans=number.cans.day
                    ,Glass=number.glass.day
                    ,Carton=number.carton
                    ,Total=number.all.types.day
                    ,Note=note) # dim(containers.daily.wide) 265 8
    
    # Left-align character columns, right-align numeric columns
    DT::datatable(containers.daily.wide
                  # Sort by 'Date' (1) descending order
                  ,options = list(order=list( c(1, 'desc'))
                                  )
                  )
  }) # Close the renderDataTable function

  #---------------
  # Output infoBox 
  ##  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #---------------
  function.renderInfoBox(shiny_output = output
                         ,output.id = "infoBox.date.earliest.record.recycling"
                         ,arg.title="Data collected since"
                         ,arg.value=date.earliest.record.recycling
                         ,arg.icon = "list" #icon("list")
                         )
  function.renderInfoBox(shiny_output = output
                         ,output.id = "infoBox.date.latest.record.recycling"
                         ,arg.title="Latest update on"
                         ,arg.value= date.latest.record.recycling
                         ,arg.icon = "list" #icon("list")
                         )
  
  #*****************************************
  # Outputs to use under menuItem "Employment"
  #*****************************************
  #----------------------------------------------------------------
  # Create the employment timeline horizontal bar plot using plotly
  #----------------------------------------------------------------
  output$plot.employment.horizontal.bars <- plotly::renderPlotly({
    fig.employment.horizontal.bars <- plot_ly() %>%
      # Add employment duration bars
      plotly::add_segments(
        data = employment_summary
        ,x = ~start_date, xend = ~end_date
        ,y = ~position_order, yend = ~position_order
        ,line = list(width = 10)
        ,color = ~position_factor
        ,colors = bar_colors
        ,name = ~position_factor) %>%
      # Add event markers with different symbols
      plotly::add_trace(
        data = employment_event_data
        ,x = ~event_date
        ,y = ~y_adjusted
        ,type = "scatter"
        ,mode = "markers"
        ,marker = list(size = 10, symbol = ~symbol, color = ~event_color)
        ,text = ~hover_text
        ,hoverinfo = "text"
        ,showlegend = FALSE) %>%
      # Configure layout
      layout(
        title = "Employment Timeline"
        ,xaxis = list(title = "Date", type = "date")
        ,yaxis = list(title = "", showticklabels = FALSE)
        ,legend = list(
          x = 0, y = 0.5
          ,orientation = "v"
          ,yanchor = "center"
          ,traceorder = "reversed" # Maintain order in legend
        ) # End list()
      ) # End layout()
    
    # Display the plot
    fig.employment.horizontal.bars
  })
  
  #----------------------------------------------------------------
  # Create the job application event horizontal bar plot using plotly
  #----------------------------------------------------------------
  output$plot.job.application.horizontal.bars <- plotly::renderPlotly({
    # Create the job application event timeline plot
    fig.job.application.horizontal.bars <- plot_ly() %>%
      # Add employment duration bars
      plotly::add_segments(
        data = job.summary
        ,x = ~start_date, xend = ~end_date
        ,y = ~position_factor, yend = ~position_factor
        ,line = list(width = 10)
        ,color = ~position_factor
        ,colors = job.bar.colors
        ,name = ~position_factor) %>%
      # Add event markers with different symbols
      plotly::add_trace(
        data = job.events.data
        ,x = ~event_date
        ,y = ~position_factor
        ,type = "scatter"
        ,mode = "markers"
        ,marker = list(size = 9, symbol = ~symbol, color = ~event_color)
        ,text = ~hover_text
        ,hoverinfo = "text"
        ,showlegend = FALSE) %>%
      # Configure layout
      layout(
        title = "Job Application Timeline"
        ,xaxis = list(title = "Date", type = "date")
        ,yaxis = list(title = "", showticklabels = FALSE)
        ,legend = list(
          x = 1.05
          ,xanchor = "left"
          ,y = 1
          ,yanchor = "top"
          ,traceorder = "reversed" # Maintain order in legend
          ,itemsizing = "constant"
          ,orientation = "v"
          ,title = list(text="Positions")
          ,scroll = TRUE # Enable scrollable legend
          ,valign = "top"
        ) # End list()
        ,margin = list(r = 200) # Adjust right margin for legend space
      ) # End layout()
    
    # Display the plot
    fig.job.application.horizontal.bars
  })
  #*****************************************
  # Outputs to use under menuItem "Electricity"
  #*****************************************
  output$plot.energy.usage.solar.export <- shiny::renderPlot({
    plot_data <- alinta_bills_balance_brought_forward_plot_data  
    # dim(plot_data) 30 8
    
    # Stacked usage subset
    usage_data <- plot_data %>%
      dplyr::filter(type %in% c("Energy usage - all hours", "Energy usage - hot water systems"))
    # dim(usage_data) 20 6
    
    # Totals for stacked usage (for top labels)
    usage_totals <- usage_data %>%
      dplyr::group_by(month_year_factor) %>%
      dplyr::summarise(total_kwh = sum(kwh), .groups = "drop")
    # dim(usage_totals) 10 2
    
    # Solar export subset
    solar_data <- plot_data %>% dplyr::filter(type == "Solar export")
    # dim(solar_data) 10 8
    
    # Grouped bars
    ## group 1: Stacked bars of peak_kwh and controlled load 1
    ## group 2: solar export
    plot.electricity.usage.solar.export <-  ggplot() +
      # Stacked usage bars
      geom_bar(
        data = usage_data,
        mapping = aes(
          x = month_year_factor,
          y = kwh,
          fill = plot_fill
        ),
        stat = "identity",
        position = "stack",
        width = 0.4
      ) +
      # Labels inside stacked usage bars
      geom_text(
        data = usage_data,
        aes(
          x = month_year_factor,
          y = kwh,
          label = round(kwh, 1),   # show each bar segment
          group = type
        ),
        position = position_stack(vjust = 0.5), # center inside each stacked segment
        color = "white",
        size = 4
      ) +
      # Totals above stacked bars
      geom_text(
        data = usage_totals,
        aes(
          x = month_year_factor,
          y = total_kwh,
          label = round(total_kwh, 1)
        ),
        vjust = -0.5,
        fontface = "bold",
        size = 5
      ) +
      # Solar export bars
      geom_bar(
        data = solar_data,
        mapping = aes(
          x = month_year_factor,
          y = kwh,
          fill = plot_fill
        ),
        stat = "identity",
        width = 0.4,
        position = position_nudge(x = 0.4)
      ) +
      # Labels above solar export bars
      geom_text(
        data = solar_data,
        aes(
          x = month_year_factor,
          y = kwh,
          label = round(kwh, 1)
        ),
        position = position_nudge(x = 0.4),
        vjust = -0.5,
        size = 4,
        color = "black"
      ) +
      scale_fill_manual(
        values = c(
          "Energy usage - all hours" = "#1f77b4",
          "Energy usage - hot water systems" = "#ff7f0e",
          "Solar export" = "orange"
        )
      ) +
      labs(
        x = "Supply Period",
        y = "kWh",
        fill = "Category",
        title = "Electricity Usage and Solar Export per Supply Period"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold")
      )
    plot.electricity.usage.solar.export
  }) # Close renderPlot()
  
  output$table.electricity.usage.solar.export <- DT::renderDataTable({
    DT::datatable(data= alinta_bills_balance_brought_forward_table_data
                  ,options = list(autoWidth = FALSE, searching = TRUE))
    }) # Close renderDataTable()
  
  output$table.rates.over.supply.period <- DT::renderDataTable({
    table_data <- alinta_bills_rates_over_supply_period %>% 
      dplyr::select( supply_start
                     ,Rate_Incl_GST_Daily_Charge
                     ,Rate_Incl_GST_Peak
                     ,Rate_Incl_GST_Demand
                     ,Rate_Incl_GST_Daily_Charge_Controlled_Load_1
                     ,Rate_Incl_GST_Controlled_Load_1
                     ,Rate_Incl_GST_Standard_Solar) %>%
      dplyr::rename_with(
        .cols = c(
          "Rate_Incl_GST_Daily_Charge",
          "Rate_Incl_GST_Peak",
          "Rate_Incl_GST_Demand",
          "Rate_Incl_GST_Daily_Charge_Controlled_Load_1",
          "Rate_Incl_GST_Controlled_Load_1",
          "Rate_Incl_GST_Standard_Solar"
        ),
        .fn = ~ stringr::str_replace(.x, "Rate_Incl_GST_", "") %>%       # remove prefix
          stringr::str_replace_all("_", " ") %>%                       # replace _ with space
          stringr::str_to_title()                                       # capitalize words
      ) %>% 
      dplyr::rename(`Supply Start`=supply_start
                    ,`Peak hour surcharge (4PM-9PM)`=Demand) %>%
      dplyr::arrange(dplyr::desc(`Supply Start`))
    
    DT::datatable(data = table_data
                  ,options = list(autoWidth = FALSE, searching = TRUE))
  })
  #-----------------------------------------------------------------------------------------------------------
  # valueBox: 
  ## Year to date electricity consumption and solar export
  ## Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  ## [How to break line inside a paste0() function in shiny dashboard](https://stackoverflow.com/questions/51079153/how-to-break-line-inside-a-paste0-function-in-shiny-dashboard)
  #-----------------------------------------------------------------------------------------------------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.electricity.consumption.total"
                          ,argument.value=paste0(electricity.consumption.total, "KWh")
                          ,argument.subtitle=HTML(paste0(
                             "consumed from ", electricity_supply_start_first," to ",electricity_supply_end_last, " including",br()
                            ,"<b>",electricity.consumption.general.usage," KWh"," general usage","</b>", br()
                            ,"<b>",electricity.consumption.controlled.load," KWh"," hot water system","</b>")
                          ) # Close HTML()
                          ,argument.color="black")
  
  #------------------------------------------------------
  # valueBox: 
  ## Year to date amount paid for electricity consumption
  ## Total, Rebate and amount paid copied from bills PDF
  ###-------------------------------
  ### period    Total   Rebate  Paid
  ###-------------------------------
  ### 20240820  37.75   -       37.75
  ### 20240920  $106.67 75      31.67
  ### 20241201  83.54   -       83.54 
  ### 20250101  $89.27  75      14.27
  ### 20250201  $69.46  -       69.46
  ### 20250301  $78.61  75      3.61
  ### 20250401  $66.57  -       66.57
  ### 20250501  79.85   -       79.85
  ### 20250601  $158.17 75      83.17
  ### 20250701  211.91  -       211.91
  ### SUM       981.80  300     681.8
  ###-------------------------------

  #------------------------------------------------------
  AUS.Govt.Energy.Price.Relief.Plan.Rebate <- 75
  times.received <- 4
  AUS.Govt.Energy.Price.Relief.Plan.Rebate.total <- AUS.Govt.Energy.Price.Relief.Plan.Rebate*times.received
  function.renderValueBox(
     shiny_output = output
    ,output.id="valueBox.amount.paid.electricity.consumption.total.breakdown"
    ,argument.value=paste0(
      "$AUD "
      ,sum( total.amount.paid.controlled.load.1
            ,total.amount.paid.daily.charge
            ,total.amount.paid.daily.charge.controlled.load.1
            ,total.amount.paid.general.usage
            ,total.amount.paid.peak.hour.surcharge) - sum(total.amount.earned.solar.export, AUS.Govt.Energy.Price.Relief.Plan.Rebate.total)
      ) # Close paste0()
    ,argument.subtitle=HTML(paste0(
       "Paid for electricity consumption from "
      ,electricity_supply_start_first," to ", electricity_supply_end_last, " including",br()
      ,"<b>", "$AUD ", total.amount.paid.general.usage," for general usage","</b>", br()
      ,"<b>", "$AUD ", total.amount.paid.peak.hour.surcharge, " for peak hour surcharge","</b>", br()
      ,"<b>", "$AUD ", total.amount.paid.daily.charge, " for daily charge","</b>", br()
      ,"<b>", "$AUD ", total.amount.paid.controlled.load.1," for hot water system","</b>", br()
      ,"<b>", "$AUD ", total.amount.paid.daily.charge.controlled.load.1, " for hot water system daily charge","</b>", br()
      ,"<b>", "$AUD ", AUS.Govt.Energy.Price.Relief.Plan.Rebate.total, " earned from AUS Govt Energy Price Relief Plan Rebate","</b>", br()
      ,"<b>", "$AUD ", total.amount.earned.solar.export, " earned from solar export") # Close paste0()
                          ) # Close HTML()
                          ,argument.color="black")
  
  #----------------------------------------------
  # valueBox: 
  ## Year to date amount earned from solar export
  #----------------------------------------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.amount.paid.electricity.consumption.earned.solar.export.total"
                          ,argument.value=paste0(electricity.consumption.total, "KWh")
                          ,argument.subtitle=HTML(paste0(
                            "consumed from ", electricity_supply_start_first," to",electricity_supply_end_last, " including",br()
                            ,"<b>",electricity.consumption.general.usage," KWh"," general usage","</b>", br()
                            ,"<b>",electricity.consumption.controlled.load," KWh"," hot water system","</b>")
                          ) # Close HTML()
                          ,argument.color="black")
  
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.solar.export.total"
                          ,argument.value=paste0(solar.export.total, "KWh")
                          ,argument.subtitle=HTML(paste0("Exported to grid from 4 KWh solar panels since ", electricity_supply_start_first)
                                                  ) # Close HTML()
                          ,argument.color="black")
  
  #-----------------------------------------------
  # valueBox
  ## Current rates and charges
  #-----------------------------------------------
  function.renderValueBox(shiny_output = output
                          ,output.id="valueBox.Alinta.most.recent.rates.charges"
                          ,argument.value=paste0("Current rates & charges")
                          ,argument.subtitle=HTML(paste0(
                             "<b>",most_recent_Rate_Incl_GST_NonPeak, " Non-peak hour usage","</b>", br()
                            ,"<b>",most_recent_Rate_Incl_GST_Peak, " Peak hour (4PM-9PM) usage","</b>", br()
                            ,"<b>",most_recent_Rate_Incl_GST_Controlled_Load_1, ' Controlled load usage',"</b>", br()
                            ,"<b>",most_recent_Rate_Incl_GST_Daily_Charge, " Daily charge","</b>", br()
                            ,"<b>",most_recent_Rate_Incl_GST_Daily_Charge_Controlled_Load_1, " Controlled load daily charge","</b>", br()
                            ,"<b>",most_recent_Rate_Incl_GST_Standard_Solar, " Solar feed-in tariff","</b>") # Close paste0()
                            ) # Close HTML()
                          ,argument.color="black")
} # Close the server function

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#