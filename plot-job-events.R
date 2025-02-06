library(dplyr)
library(plotly)
library(viridisLite)
library(lubridate)

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

# Create the employment timeline plot
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

