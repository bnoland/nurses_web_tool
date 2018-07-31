# File for defining the server logic, as well as supporting functions.

library(shiny)
library(usmap)
library(ggplot2)
library(dplyr)

source("factor_levels.R")

# Helper functions ----------------------------------------------------------------------------

# Return union membership or union contract coverage trend data.
#   nurses_subset
#       Subset of the data selected by the user.
#   group_var
#       Variable to group by as a string. ``none'' if no grouping.
#   type
#       A string, either ``membership'' (for union membership rate) or ``coverage'' (for union
#       contract coverage rate).
trend_data <- function(nurses_subset, group_var, type) {
    trend_data <- nurses_subset %>% group_by(year)
    
    group_var <- as.symbol(group_var)
    if (group_var != "none") {
        trend_data <- trend_data %>%
            group_by(.dots = group_var, add = TRUE)
    }
    
    if (type == "membership") {
        trend_data <- trend_data %>%
            summarize(
                prop = mean(member, na.rm = TRUE),
                n = n()
            )
    } else if (type == "coverage") {
        trend_data <- trend_data %>%
            summarize(
                prop = mean(covered, na.rm = TRUE),
                n = n()
            )
    } else {
        stop("Type must be either ``membership'' or ``coverage''.")
    }
    
    trend_data
}

# Plot union membership or union contract coverage over time.
#   nurses_subset
#       Subset of the data selected by the user.
#   group_var
#       Variable to group by as a string. ``none'' if no grouping.
#   fixed_axis
#       Fix the vertical axis to be from 0 to 1, inclusive.
#   type
#       A string, either ``membership'' (for union membership rate) or ``coverage'' (for union
#       contract coverage rate).
# TODO: Plot axis labels, etc. + plot styling.
trend_plot <- function(nurses_subset, group_var, fixed_axis, type) {
    trend_data <- trend_data(nurses_subset, group_var, type)
    
    group_var <- as.symbol(group_var)
    # TODO: aes() vs aes_()?
    if (group_var != "none") {
        p <- ggplot(trend_data,
                    aes_(quote(year), quote(prop), color = group_var))
    } else {
        p <- ggplot(trend_data, aes(year, prop))
    }
    
    if (fixed_axis) {
        p + geom_line() + coord_cartesian(ylim = c(0, 1))
    } else {
        p + geom_line() + expand_limits(y = 0)
    }
}

# Return state-level union membership or union contract coverage.
#   nurses_subset
#       Subset of the data selected by the user.
#   type
#       A string, either ``membership'' (for union membership rate) or ``coverage'' (for union
#       contract coverage rate).
state_data <- function(nurses_subset, type) {
    state_data <- nurses_subset %>% group_by(state)
    
    if (type == "membership") {
        state_data <- state_data %>%
            summarize(
                prop = mean(member, na.rm = TRUE),
                n = n()
            )
    } else if (type == "coverage") {
        state_data <- state_data %>%
            summarize(
                prop = mean(covered, na.rm = TRUE),
                n = n()
            )
    } else {
        stop("Type must be either ``membership'' or ``coverage''.")
    }
    
    state_data
}

# Plot a chloropleth map showing state-level union membership or union contract coverage.
#   nurses_subset
#       Subset of the data selected by the user.
#   selected_states_only
#       If TRUE, display only the states selected by the user on the map; otherwise, show all
#       states.
#   type
#       A string, either ``membership'' (for union membership rate) or ``coverage'' (for union
#       contract coverage rate).
# TODO: Map styling, etc.
state_map <- function(nurses_subset, selected_states_only, type) {
    state_data <- state_data(nurses_subset, type)
    
    if (type == "membership") {
        legend_name <- "Proportion members"
    } else if (type == "coverage") {
        legend_name <- "Proportion covered"
    } else {
        stop("Type must be either ``membership'' or ``coverage''.")
    }
    
    # TODO: Am I doing this right?
    states <- NULL
    if (selected_states_only)
        states <- state_data$state
    
    # TODO: Using fixed scales for now, but this can make it hard to detect subtle differences.
    # Should this be an option?
    # TODO: What colors to use for the scale?
    plot_usmap(data = state_data, value = "prop", include = states) +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = "gray",
                            name = legend_name, limits = c(0, 1), label = scales::comma) +
        theme(legend.position = "right")
}

# Server logic --------------------------------------------------------------------------------

# TODO: The server() function is too long -- may want to modularize in some way.
server <- function(input, output) {
    # Returns the subset of the nurses data selected by the user.
    nurses_subset_selected <- reactive({
        nurses %>%
            filter(
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
    
    # Renders the trend plot for union membership.
    output$members_trend_plot <- renderPlot({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        fixed_axis <- input$trends_fixed_axis
        trend_plot(nurses_subset, group_var, fixed_axis, type = "membership")
    })
    
    # Renders the trend plot for union contract coverage.
    output$coverage_trend_plot <- renderPlot({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        fixed_axis <- input$trends_fixed_axis
        trend_plot(nurses_subset, group_var, fixed_axis, type = "coverage")
    })
    
    # Renders the data table showing the union membership trend data.
    output$membership_trend_data <- renderDataTable({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        trend_data(nurses_subset, group_var, type = "membership")
    })
    
    # Renders the data table showing the union contract coverage trend data.
    output$coverage_trend_data <- renderDataTable({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        trend_data(nurses_subset, group_var, type = "coverage")
    })
    
    # Renders the map showing union membership at the state-level.
    output$membership_state_map <- renderPlot({
        nurses_subset <- nurses_subset_selected()
        selected_states_only <- input$selected_states_only
        state_map(nurses_subset, selected_states_only, type = "membership")
    })
    
    # Renders the map showing union contract coverage at the state-level.
    output$coverage_state_map <- renderPlot({
        nurses_subset <- nurses_subset_selected()
        selected_states_only <- input$selected_states_only
        state_map(nurses_subset, selected_states_only, type = "coverage")
    })
    
    # Renders the data table showing the state-level union membership data.
    output$membership_state_data <- renderDataTable({
        nurses_subset <- nurses_subset_selected()
        state_data(nurses_subset, type = "membership")
    })
    
    # Renders the data table showing the state-level union contract coverage data.
    output$coverage_state_data <- renderDataTable({
        nurses_subset <- nurses_subset_selected()
        state_data(nurses_subset, type = "coverage")
    })
    
    # Renders the data table showing the subset of the nurses data selected by the user.
    output$nurses_subset_table <- renderDataTable(nurses_subset_selected())
}
