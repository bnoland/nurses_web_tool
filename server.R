# File for defining the server logic and supporting functions.

library(shiny)
library(shinyjs)
library(usmap)
library(ggplot2)
library(dplyr)

# Computes and returns union membership or union contract coverage proportions
# from the given (pre-grouped) data.
with_proportions <- function(data, type) {
  if (type == "membership") {
    data %>% summarize(
      prop = sum(member * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      n = n()
    )
  } else if (type == "coverage") {
    data %>% summarize(
      prop = sum(covered * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      n = n()
    )
  } else {
    stop("Type must be either ``membership'' or ``coverage''.")
  }
}

# Returns union membership or union contract coverage trend data, optionally
# grouped by a given variable.
trend_grouped_data <- function(nurses_subset, group_var = "none", type) {
  grouped_data <- nurses_subset %>% group_by(year)
  
  group_var <- as.symbol(group_var)
  if (group_var != "none") {
    grouped_data <- grouped_data %>%
      group_by(.dots = group_var, add = TRUE)
  }
  
  with_proportions(grouped_data, type)
}

# Returns union membership or union contract coverage trend data for two levels
# of a given variable with a column containing the difference between their two
# proportions.
trend_diff_data <- function(nurses_subset, diff_var, diff_levels, type) {
  diff_data <- nurses_subset %>% group_by(year)
  
  diff_var <- as.symbol(diff_var)
  diff_data <- diff_data %>%
    group_by(.dots = diff_var, add = TRUE)
  
  diff_data <- with_proportions(diff_data, type)
  
  diff_data_level1 <- diff_data %>%
    filter(eval(diff_var) == diff_levels[[1]])
  
  diff_data_level2 <- diff_data %>%
    filter(eval(diff_var) == diff_levels[[2]])
  
  diff_data <- inner_join(diff_data_level1, diff_data_level2,
                          by = "year",
                          suffix = c(".level1", ".level2")
  )
  
  diff_data <- diff_data %>%
    mutate(prop_diff = prop.level1 - prop.level2)
  
  diff_data
}

# Simple wrapper around trend_grouped_data() and trend_diff_data().
trend_data <- function(nurses_subset, plot_type, group_var = "none",
                       diff_var = NULL, diff_levels = NULL, type) {
  if (plot_type == "grouped") {
    trend_grouped_data(nurses_subset, group_var, type)
  } else if (plot_type == "diff") {
    trend_diff_data(nurses_subset, diff_var, diff_levels, type)
  } else {
    stop("Plot type must be either ``grouped'' or ``diff''.")
  }
}

# Plots union membership or union contract coverage over time, optionally
# grouped by a given variable.
trend_grouped_plot <- function(nurses_subset, group_var = "none",
                               use_viridis = FALSE, type) {
  grouped_data <- trend_grouped_data(nurses_subset, group_var, type)
  
  if (group_var == "none") {
    p <- ggplot(grouped_data, aes(year, prop)) + geom_line(size = 1)
    return(p)
  }
  
  legend_name <- switch(group_var,
                        "sex" = "Sex",
                        "age_group" = "Age group",
                        "race" = "Race",
                        "hisp" = "Hispanic status",
                        "educ" = "Level of education",
                        "citizen" = "Citizenship status",
                        "state" = "State"
  )
  
  group_var <- as.symbol(group_var)
  
  p <- ggplot(grouped_data, aes(year, prop, color = !!group_var)) +
    geom_line(size = 1)
  if (use_viridis) {
    p + viridis::scale_color_viridis(name = legend_name, discrete = TRUE)
  } else {
    p + scale_color_discrete(name = legend_name)
  }
}

# Plots the difference between either union membership or union coverage
# proportion for two levels of a given variable.
trend_diff_plot <- function(nurses_subset, diff_var, diff_levels, type) {
  diff_data <- trend_diff_data(nurses_subset, diff_var, diff_levels, type)
  
  ggplot(diff_data, aes(year, prop_diff)) +
    geom_line(size = 1)
}

# Simple wrapper around trend_grouped_plot() and trend_diff_plot().
trend_plot <- function(nurses_subset, plot_type, group_var = "none",
                       diff_var = NULL, diff_levels = NULL, fixed_axis = FALSE,
                       use_viridis = FALSE, type) {
  if (plot_type == "grouped") {
    p <- trend_grouped_plot(nurses_subset, group_var, use_viridis, type)
  } else if (plot_type == "diff") {
    p <- trend_diff_plot(nurses_subset, diff_var, diff_levels, type)
  } else {
    stop("Plot type must be either ``grouped'' or ``diff''.")
  }
  
  p <- p + labs(x = "Year", y = "Proportion")
  
  # Ensure that every year in the selected data is marked on the horizontal
  # axis. Only do this if the data selection is non-empty -- otherwise min() and
  # max() will return Inf and -Inf, respectively.
  if (length(nurses_subset$year) > 0) {
    start_year <- min(nurses_subset$year)
    end_year <- max(nurses_subset$year)
    p <- p + scale_x_continuous(breaks = start_year:end_year)
  }
  
  if (fixed_axis) {
    p + coord_cartesian(ylim = c(0, 1))
  } else {
    p + expand_limits(y = 0)
  }
}

# Returns state-level union membership or union contract coverage.
state_data <- function(nurses_subset, type) {
  state_data <- nurses_subset %>% group_by(state)
  
  with_proportions(state_data, type)
}

# Plots a chloropleth map showing state-level union membership or union contract
# coverage.
state_map <- function(nurses_subset, selected_states_only = FALSE,
                      fixed_scale = FALSE, type) {
  state_data <- state_data(nurses_subset, type)
  
  states <- NULL
  if (selected_states_only) {
    states <- state_data$state
  }
  
  scale_max <- NA
  if (fixed_scale) {
    scale_max <- 1
  }
  
  # When no states are selected, ggplot can't infer the scale limits from the
  # data, so we have to set it to some number.
  if (length(state_data$state) == 0) {
    scale_max <- 1
  }
  
  plot_usmap(data = state_data, value = "prop", include = states) +
    scale_fill_gradient(
      low = "#56B1F7", high = "#132B43",
      na.value = "gray",
      name = "Proportion",
      limits = c(0, scale_max),
      label = scales::percent
    ) +
    theme(legend.position = "right")
}

server <- function(input, output, session) {
  # Returns the subset of the nurses data selected by the user.
  nurses_subset_selected <- reactive({
    nurses %>% filter(
      year >= input$year_range[1], year <= input$year_range[2],
      sex %in% input$sex_selection,
      age_group %in% input$age_selection,
      race %in% input$race_selection,
      hisp %in% input$hisp_status_selection,
      educ %in% input$educ_selection,
      citizen %in% input$citizen_selection,
      state %in% input$state_selection
    )
  })
  
  # The trend data is needed for downloading as well as rendering to a data
  # table, among other things. Hence the need for the next two reactive
  # functions to avoid gross code duplication.
  
  # Returns the data table showing the union membership trend data.
  membership_trend_data <- reactive({
    trend_data(
      nurses_subset = nurses_subset_selected(),
      plot_type = input$trend_plot_type,
      group_var = input$trends_group_var,
      diff_var = input$trends_diff_var,
      diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
      type = "membership"
    )
  })
  
  # Returns the data table showing the union contract coverage trend data.
  coverage_trend_data <- reactive({
    trend_data(
      nurses_subset = nurses_subset_selected(),
      plot_type = input$trend_plot_type,
      group_var = input$trends_group_var,
      diff_var = input$trends_diff_var,
      diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
      type = "coverage"
    )
  })
  
  # Render the trend data tables.
  output$membership_trend_data <- renderDataTable(membership_trend_data())
  output$coverage_trend_data <- renderDataTable(coverage_trend_data())
  
  # Render the trend plot for union membership.
  output$membership_trend_plot <- renderPlot({
    trend_plot(
      nurses_subset = nurses_subset_selected(),
      plot_type = input$trend_plot_type,
      group_var = input$trends_group_var,
      diff_var = input$trends_diff_var,
      diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
      fixed_axis = input$trends_fixed_axis,
      use_viridis = input$trends_use_viridis,
      type = "membership"
    )
  })
  
  # Render the trend plot for union contract coverage.
  output$coverage_trend_plot <- renderPlot({
    trend_plot(
      nurses_subset = nurses_subset_selected(),
      plot_type = input$trend_plot_type,
      group_var = input$trends_group_var,
      diff_var = input$trends_diff_var,
      diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
      fixed_axis = input$trends_fixed_axis,
      use_viridis = input$trends_use_viridis,
      type = "coverage"
    )
  })
  
  # Download the union membership trend data.
  output$membership_trend_download <- downloadHandler(
    filename = "membership_trend.csv",
    content = function(file) {
      data <- membership_trend_data()
      write_csv(data, file)
    }
  )
  
  # Download the union contract coverage data.
  output$coverage_trend_download <- downloadHandler(
    filename = "coverage_trend.csv",
    content = function(file) {
      data <- coverage_trend_data()
      write_csv(data, file)
    }
  )
  
  # If difference plots are enabled, disable the grouping controls, and vice
  # versa.
  observe({
    plot_type <- input$trend_plot_type
    if (plot_type == "grouped") {
      enable("trends_group_var")
      disable("trends_diff_var")
      disable("trends_diff_level1")
      disable("trends_diff_level2")
    } else if (plot_type == "diff") {
      enable("trends_diff_var")
      enable("trends_diff_level1")
      enable("trends_diff_level2")
      disable("trends_group_var")
    } else {
      stop("Plot type must be either ``grouped'' or ``diff''.")
    }
  })
  
  # Ensure that the variable level selections in the trend difference controls
  # correspond to the variable selected.
  observe({
    diff_var <- input$trends_diff_var
    diff_var <- eval(as.symbol(diff_var), envir = nurses)
    updateSelectInput(session, "trends_diff_level1", choices = levels(diff_var))
    updateSelectInput(session, "trends_diff_level2", choices = levels(diff_var))
  })
  
  # Render the map showing union membership at the state-level.
  output$membership_state_map <- renderPlot({
    state_map(
      nurses_subset = nurses_subset_selected(),
      selected_states_only = input$selected_states_only,
      fixed_scale = input$maps_fixed_scale,
      type = "membership"
    )
  })
  
  # Render the map showing union contract coverage at the state-level.
  output$coverage_state_map <- renderPlot({
    state_map(
      nurses_subset = nurses_subset_selected(),
      selected_states_only = input$selected_states_only,
      fixed_scale = input$maps_fixed_scale,
      type = "coverage"
    )
  })
  
  # Render the data table showing the state-level union membership data.
  output$membership_state_data <- renderDataTable({
    state_data(
      nurses_subset = nurses_subset_selected(),
      type = "membership"
    )
  })
  
  # Render the data table showing the state-level union contract coverage data.
  output$coverage_state_data <- renderDataTable({
    state_data(
      nurses_subset = nurses_subset_selected(),
      type = "coverage"
    )
  })
  
  # Download the state-level union membership data.
  output$membership_state_download <- downloadHandler(
    filename = "membership_state.csv",
    content = function(file) {
      data <- state_data(
        nurses_subset = nurses_subset_selected(),
        type = "membership"
      )
      
      write_csv(data, file)
    }
  )
  
  # Download the state-level union contract coverage data.
  output$coverage_state_download <- downloadHandler(
    filename = "coverage_state.csv",
    content = function(file) {
      data <- state_data(
        nurses_subset = nurses_subset_selected(),
        type = "coverage"
      )
      
      write_csv(data, file)
    }
  )
  
  # Render the data table showing the subset of the nurses data selected by the
  # user.
  output$nurses_subset_table <- renderDataTable(nurses_subset_selected())
  
  # Download the subset of the nurses data selected by the user.
  output$nurses_subset_download <- downloadHandler(
    filename = "nurses_selected.csv",
    content = function(file) {
      data <- nurses_subset_selected()
      write_csv(data, file)
    }
  )
  
  # Export data table hashes for testing.
  exportTestValues(nurses_subset = {
    nurses_subset <- nurses_subset_selected()
    digest::digest(nurses_subset, algo = "md5")
  })
}
