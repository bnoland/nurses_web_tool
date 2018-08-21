# File for defining the server logic and supporting functions.

library(shiny)
library(shinyjs)
library(usmap)
library(ggplot2)
library(dplyr)

# Helper functions for trend data and plots ---------------------------------------------------

# Returns union membership or union contract coverage trend data, optionally grouped by a given
# variable.
trend_grouped_data <- function(nurses_subset, group_var, type) {
    grouped_data <- nurses_subset %>% group_by(year)
    
    group_var <- as.symbol(group_var)
    if (group_var != "none") {
        grouped_data <- grouped_data %>%
            group_by(.dots = group_var, add = TRUE)
    }
    
    if (type == "membership") {
        grouped_data <- grouped_data %>%
            summarize(
                prop = mean(member, na.rm = TRUE),
                n = n()
            )
    } else if (type == "coverage") {
        grouped_data <- grouped_data %>%
            summarize(
                prop = mean(covered, na.rm = TRUE),
                n = n()
            )
    } else {
        stop("Type must be either ``membership'' or ``coverage''.")
    }
    
    grouped_data
}

# Returns union membership or union contract coverage trend data for two levels of a given variable
# with a column containing the difference between their two proportions.
trend_diff_data <- function(nurses_subset, diff_var, diff_levels, type) {
    diff_data <- nurses_subset %>% group_by(year)
    
    diff_var <- as.symbol(diff_var)
    diff_data <- diff_data %>%
        group_by(.dots = diff_var, add = TRUE)
    
    if (type == "membership") {
        diff_data <- diff_data %>%
            summarize(
                prop = mean(member, na.rm = TRUE),
                n = n()
            )
    } else if (type == "coverage") {
        diff_data <- diff_data %>%
            summarize(
                prop = mean(covered, na.rm = TRUE),
                n = n()
            )
    } else {
        stop("Type must be either ``membership'' or ``coverage''.")
    }
    
    diff_data_level1 <- diff_data %>%
        filter(eval(diff_var) == diff_levels[[1]])
    
    diff_data_level2 <- diff_data %>%
        filter(eval(diff_var) == diff_levels[[2]])
    
    diff_data <- inner_join(diff_data_level1, diff_data_level2, by = "year",
                            suffix = c(".level1", ".level2"))
    
    diff_data <- diff_data %>%
        mutate(prop_diff = prop.level1 - prop.level2)
    
    diff_data
}

# Simple wrapper around trend_grouped_data() and trend_diff_data().
trend_data <- function(nurses_subset, plot_diff, group_var, diff_var, diff_levels, type) {
    if (plot_diff) {
        trend_diff_data(nurses_subset, diff_var, diff_levels, type)
    } else {
        trend_grouped_data(nurses_subset, group_var, type)
    }
}

# Plots union membership or union contract coverage over time, optionally grouped by a given
# variable.
# TODO: Plot axis labels, etc. + plot styling.
trend_grouped_plot <- function(nurses_subset, group_var, fixed_axis, type) {
    grouped_data <- trend_grouped_data(nurses_subset, group_var, type)
    
    group_var <- as.symbol(group_var)
    # TODO: aes() vs aes_()?
    if (group_var != "none") {
        p <- ggplot(grouped_data,
                    aes_(quote(year), quote(prop), color = group_var))
    } else {
        p <- ggplot(grouped_data, aes(year, prop))
    }
    
    if (fixed_axis) {
        p + geom_line() + coord_cartesian(ylim = c(0, 1))
    } else {
        p + geom_line() + expand_limits(y = 0)
    }
}

# Plots the difference between either union membership or union coverage proportion for two levels
# of a given variable.
trend_diff_plot <- function(nurses_subset, diff_var, diff_levels, fixed_axis, type) {
    diff_data <- trend_diff_data(nurses_subset, diff_var, diff_levels, type)
    
    p <- ggplot(diff_data, aes(year, prop_diff))
    
    if (fixed_axis) {
        p + geom_line() + coord_cartesian(ylim = c(0, 1))
    } else {
        p + geom_line() + expand_limits(y = 0)
    }
}

# Simple wrapper around trend_grouped_plot() and trend_diff_plot().
trend_plot <- function(nurses_subset, plot_diff, group_var, diff_var, diff_levels, fixed_axis,
                       type) {
    if (plot_diff) {
        trend_diff_plot(nurses_subset, diff_var, diff_levels, fixed_axis, type)
    } else {
        trend_grouped_plot(nurses_subset, group_var, fixed_axis, type)
    }
}

# Helper functions for state data and plots ---------------------------------------------------

# Returns state-level union membership or union contract coverage.
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

# Plots a chloropleth map showing state-level union membership or union contract coverage.
# TODO: Map styling, etc.
state_map <- function(nurses_subset, selected_states_only, fixed_scale, type) {
    state_data <- state_data(nurses_subset, type)
    
    if (type == "membership") {
        legend_name <- "Proportion members"
    } else if (type == "coverage") {
        legend_name <- "Proportion covered"
    } else {
        stop("Type must be either ``membership'' or ``coverage''.")
    }
    
    states <- NULL
    if (selected_states_only)
        states <- state_data$state
    
    scale_max <- NA
    if (fixed_scale)
        scale_max <- 1
    
    # When no states are selected, ggplot can't infer the scale limits from the data, so we have
    # to set it to some number.
    if (length(state_data$state) == 0)
        scale_max <- 1
    
    # TODO: What colors to use for the scale?
    plot_usmap(data = state_data, value = "prop", include = states) +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43", na.value = "gray",
                            name = legend_name, limits = c(0, scale_max), label = scales::percent) +
        theme(legend.position = "right")
}

# Server logic --------------------------------------------------------------------------------

# TODO: The server() function is too long -- may want to modularize in some way.
# TODO: Irritating amount of code duplication. May be able to improve performance by using some
# reactive variables.
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
    
    # Renders the trend plot for union membership.
    output$members_trend_plot <- renderPlot({
        trend_plot(
            nurses_subset = nurses_subset_selected(),
            plot_diff = input$trends_plot_diff,
            group_var = input$trends_group_var,
            diff_var = input$trends_diff_var,
            diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
            fixed_axis = input$trends_fixed_axis,
            type = "membership"
        )
    })
    
    # Renders the trend plot for union contract coverage.
    output$coverage_trend_plot <- renderPlot({
        trend_plot(
            nurses_subset = nurses_subset_selected(),
            plot_diff = input$trends_plot_diff,
            group_var = input$trends_group_var,
            diff_var = input$trends_diff_var,
            diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
            fixed_axis = input$trends_fixed_axis,
            type = "coverage"
        )
    })
    
    # Renders the data table showing the union membership trend data.
    output$membership_trend_data <- renderDataTable({
        trend_data(
            nurses_subset = nurses_subset_selected(),
            plot_diff = input$trends_plot_diff,
            group_var = input$trends_group_var,
            diff_var = input$trends_diff_var,
            diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
            type = "membership"
        )
    })
    
    # Renders the data table showing the union contract coverage trend data.
    output$coverage_trend_data <- renderDataTable({
        trend_data(
            nurses_subset = nurses_subset_selected(),
            plot_diff = input$trends_plot_diff,
            group_var = input$trends_group_var,
            diff_var = input$trends_diff_var,
            diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
            type = "coverage"
        )
    })
    
    # Download the union membership trend data.
    output$membership_trend_download <- downloadHandler(
        filename = "membership_trend.csv",
        content = function(file) {
            data <- trend_data(
                nurses_subset = nurses_subset_selected(),
                plot_diff = input$trends_plot_diff,
                group_var = input$trends_group_var,
                diff_var = input$trends_diff_var,
                diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
                type = "membership"
            )
            
            write.csv(data, file)
        }
    )
    
    # Download the union contract coverage data.
    output$coverage_trend_download <- downloadHandler(
        filename = "coverage_trend.csv",
        content = function(file) {
            data <- trend_data(
                nurses_subset = nurses_subset_selected(),
                plot_diff = input$trends_plot_diff,
                group_var = input$trends_group_var,
                diff_var = input$trends_diff_var,
                diff_levels = c(input$trends_diff_level1, input$trends_diff_level2),
                type = "coverage"
            )
            
            write.csv(data, file)
        }
    )
    
    # If difference plots are enabled, disable the grouping controls, and vice versa.
    observe({
        toggleState("trends_group_var", !input$trends_plot_diff)
        
        toggleState("trends_diff_var", input$trends_plot_diff)
        toggleState("trends_diff_level1", input$trends_plot_diff)
        toggleState("trends_diff_level2", input$trends_plot_diff)
    })
    
    # Ensure that the variable level selections in the trend difference controls correspond to the
    # variable selected.
    observe({
        diff_var <- input$trends_diff_var
        diff_var <- eval(as.symbol(diff_var), envir = nurses)
        updateSelectInput(session, "trends_diff_level1", choices = levels(diff_var))
        updateSelectInput(session, "trends_diff_level2", choices = levels(diff_var))
    })
    
    # Renders the map showing union membership at the state-level.
    output$membership_state_map <- renderPlot({
        state_map(
            nurses_subset = nurses_subset_selected(),
            selected_states_only = input$selected_states_only,
            fixed_scale = input$maps_fixed_scale,
            type = "membership"
        )
    })
    
    # Renders the map showing union contract coverage at the state-level.
    output$coverage_state_map <- renderPlot({
        state_map(
            nurses_subset = nurses_subset_selected(),
            selected_states_only = input$selected_states_only,
            fixed_scale = input$maps_fixed_scale,
            type = "coverage"
        )
    })
    
    # Renders the data table showing the state-level union membership data.
    output$membership_state_data <- renderDataTable({
        state_data(
            nurses_subset = nurses_subset_selected(),
            type = "membership"
        )
    })
    
    # Renders the data table showing the state-level union contract coverage data.
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
            
            write.csv(data, file)
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
            
            write.csv(data, file)
        }
    )
    
    # Renders the data table showing the subset of the nurses data selected by the user.
    output$nurses_subset_table <- renderDataTable(nurses_subset_selected())
    
    # Download the subset of the nurses data selected by the user.
    output$nurses_subset_download <- downloadHandler(
        filename = "nurses_selected.csv",
        content = function(file) {
            data <- nurses_subset_selected()
            write.csv(data, file)
        }
    )
}
