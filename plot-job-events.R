library(dplyr)
library(plotly)
library(viridisLite)

# Google Sheets link containing employment data
sheet_url <- "https://docs.google.com/spreadsheets/d/1Oq7Vfm0xemPazUM2GhcqgLF6KVwqqQoBWmcONkSy34E/"

# Read employment events from Google Sheets and process the data
employment_data <- googlesheets4::read_sheet(sheet_url, sheet = "employment") %>%
  as.data.frame() %>%
  dplyr::group_by(company, position_title, reference_number) %>%
  dplyr::mutate(
    start_date = min(event_date),  # Identify earliest event date per position
    end_date = max(event_date),    # Identify latest event date per position
    total_events = n(),            # Count number of events
    position_label = paste(company, position_title, sep = " - ") # Create unique label for each position
  )

# Prepare data for horizontal bars in the timeline
employment_summary <- employment_data %>%
  dplyr::group_by(position_label, start_date, end_date) %>%
  dplyr::summarize(event_count = n(), .groups = "drop") %>%
  dplyr::arrange(start_date) %>% # Order by start date (earliest at bottom)
  dplyr::mutate(
    position_order = row_number(), # Assign row number to maintain order in plot
    position_factor = factor(position_label, levels = position_label[order(position_order)]) # Factor for legend order
  )

# Merge row_number from employment_summary back into employment_data
employment_events <- employment_data %>%
  dplyr::left_join(
    employment_summary %>% select(position_label, position_order, position_factor),
    by = "position_label"
  )

# Generate color-blind-friendly color palette
bar_colors <- viridisLite::viridis(n = length(unique(employment_summary$position_factor)), option = "turbo")
color_map <- setNames(bar_colors, levels(employment_summary$position_factor))

# Prepare event dataset with appropriate symbols and hover text
event_data <- employment_events %>%
  dplyr::mutate(
    symbol = case_when(
      event == "employment start" ~ "triangle-up", 
      event == "employment end" ~ "triangle-down", 
      TRUE ~ "circle"
    ),
    y_adjusted = position_order + 0.05, # Shift symbols slightly above bars
    event_color = color_map[position_factor], # Assign colors
    hover_text = paste("Event: ", event, "<br>Date: ", event_date)
  )

# Create the employment timeline plot
fig <- plot_ly() %>%
  # Add employment duration bars
  add_segments(
    data = employment_summary,
    x = ~start_date, xend = ~end_date,
    y = ~position_order, yend = ~position_order,
    line = list(width = 10),
    color = ~position_factor,
    colors = bar_colors,
    name = ~position_factor
  ) %>%
  # Add event markers with different symbols
  add_trace(
    data = event_data,
    x = ~event_date,
    y = ~y_adjusted,
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, symbol = ~symbol, color = ~event_color),
    text = ~hover_text,
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
  # Configure layout
  layout(
    title = "Employment Timeline",
    xaxis = list(title = "Date", type = "date"),
    yaxis = list(title = "", showticklabels = FALSE),
    legend = list(
      x = 0, y = 0.5,
      orientation = "v",
      yanchor = "center",
      traceorder = "reversed" # Maintain order in legend
    )
  )

# Display the plot
fig

#-----------------------
# Job application events
#-----------------------
# Read job application events
job <- googlesheets4::read_sheet(sheet.ID.job, sheet = "job_events") |>
  as.data.frame() %>%
  dplyr::group_by(company, position_title, reference_number) %>%
  dplyr::mutate(
    start_date = min(event_date)
    ,end_date = max(event_date)
    ,total_events = n()
    ,label = paste(company, position_title, sep = " - ")  # Concatenate company and position_title
  )

job_applications <- job %>%
  dplyr::group_by(company, position_title, reference_number) %>%
  dplyr::filter(!any(stringr::str_detect(event, paste(key_words, collapse = "|")))) %>%
  dplyr::ungroup() # dim(job_applications) 128 10
