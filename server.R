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
  
} # Close the server function

#************************************************************************************************#
#---------------------------------This is the end of this file ----------------------------------#
#************************************************************************************************#