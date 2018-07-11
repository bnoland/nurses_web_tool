library(shiny)
library(shinyBS)
library(shinyWidgets)
# TODO: Import entire tidyverse?
library(tidyverse)

source("factor_levels.R")

# Load the data -------------------------------------------------------------------------------

# TODO: Should I have bothered coding all these variables as factors?

nurses <- read_csv("nurses_preprocessed.csv",
    col_types = cols(
        year = col_integer(),
        sex = col_factor(levels = sex_factor_levels()),
        member = col_logical(),
        covered = col_logical(),
        age = col_integer(),
        age_group = col_factor(levels = age_group_factor_levels()),
        race = col_factor(race_factor_levels()),
        hisp = col_logical(),
        educ = col_factor(levels = education_factor_levels()),
        citizen = col_factor(levels = citizenship_factor_levels()),
        state = col_factor(levels = state_factor_levels())
    )
)

# TODO: Should this code go elsewhere?
min_year <- as.double(min(nurses$year, na.rm = TRUE))
max_year <- as.double(max(nurses$year, na.rm = TRUE))

# User interface ------------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Nurses web tool"),
    
    sidebarLayout(
        sidebarPanel(
            # Hack to force the slider input widget to only put tick marks at integer values.
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            
            bsCollapse(
                # Year range selection.
                bsCollapsePanel(title = "Year range",
                    sliderInput(inputId = "year_range", label = NULL,
                        # TODO: Why do min/max need to be doubles for this to work correctly?
                        min = min_year, max = max_year,
                        value = c(min_year, max_year),
                        sep = ""
                    )
                ),
                
                # Sex selection.
                bsCollapsePanel(title = "Sex",
                    checkboxGroupInput(inputId = "sex_selection", label = NULL,
                                       choices = sex_factor_levels(),
                                       selected = sex_factor_levels())
                ),
                
                # Age group selection.
                bsCollapsePanel(title = "Age group",
                    checkboxGroupInput(inputId = "age_selection", label = NULL,
                                       choices = age_group_factor_levels(),
                                       selected = age_group_factor_levels())
                ),
                
                # Race selection.
                bsCollapsePanel(title = "Race",
                    checkboxGroupInput(inputId = "race_selection", label = NULL,
                                       choices = race_factor_levels(),
                                       selected = race_factor_levels())
                ),
                
                # Hispanic status selection.
                bsCollapsePanel(title = "Hispanic status",
                    checkboxGroupInput(inputId = "hisp_status_selection", label = NULL,
                                       choices = c("Hispanic" = TRUE, "Non-Hispanic" = FALSE),
                                       selected = c(TRUE, FALSE))
                ),
                
                # Education level selection.
                bsCollapsePanel(title = "Level of education",
                    checkboxGroupInput(inputId = "educ_selection", label = NULL,
                                       choices = education_factor_levels(),
                                       selected = education_factor_levels())
                ),
                
                # Citizenship status selection.
                bsCollapsePanel(title = "Citizenship status",
                    checkboxGroupInput(inputId = "citizen_selection", label = NULL,
                                       choices = citizenship_factor_levels(),
                                       selected = citizenship_factor_levels())
                ),
                
                # TODO: Should we include statistical regions other than states?
                # State selection.
                bsCollapsePanel(title = "States",
                    # TODO: Get state selection working.
                    pickerInput(inputId = "state_selection", label = NULL,
                                choices = levels(nurses$state),
                                selected = levels(nurses$state),
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE))
                )
            )
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                
                # Trends panel.
                tabPanel(title = "Trends",
                    
                    tabsetPanel(type = "tabs",
                        
                        # Panel for viewing trend plots.
                        tabPanel(title = "Plots",
                            # TODO: Make the plots a fixed size?
                            fluidRow(
                                column(width = 12,
                                    h3("Membership proportion"),
                                    plotOutput("members_trend_plot")
                                )
                            ),
                            fluidRow(
                                column(width = 12,
                                    h3("Coverage proportion"),
                                    plotOutput("coverage_trend_plot")
                                )
                            )
                        ),
                        
                        # Panel for viewing the data used for generating the trend plots.
                        tabPanel(title = "Data",
                            fluidRow(width = 12,
                                h3("Membership proportion"),
                                dataTableOutput("membership_trend_data")
                            ),
                            fluidRow(width = 12,
                                h3("Coverage proportion"),
                                dataTableOutput("coverage_trend_data")
                            )
                        ),
                        
                        # Panel for setting options related to the trend plots.
                        tabPanel(title = "Options",
                            br(),
                            fluidRow(
                                column(width = 12,
                                    selectInput(inputId = "trends_group_var", label = "Group by:",
                                        choices = list(
                                            "None" = "none",
                                            "Sex" = "sex",
                                            "Age group" = "age_group",
                                            "Race" = "race",
                                            "Hispanic status" = "hisp",
                                            "Level of education" = "educ",
                                            "Citizenship status" = "citizen",
                                            "State" = "state"
                                        )
                                    )
                                )
                            )
                        )
                        
                    )
                ),
                
                # Geography panel.
                tabPanel(title = "Geography",
                    
                    tabsetPanel(type = "tabs",
                        
                        # Panel for viewing chloropleth maps.
                        tabPanel(title = "Maps"),
                        
                        # Panel for viewing the data used to generate the maps.
                        tabPanel(title = "Data"),
                        
                        # Panel for setting options related to the maps.
                        tabPanel(title = "Options")
                    )
                ),
                
                # Data viewer panel.
                tabPanel(title = "Data", dataTableOutput("nurses_subset_table"))
                #tabPanel("Download", "Download the data.")
            )
        )
    )
)

# Server logic --------------------------------------------------------------------------------

# Return union membership or union contract coverage trend data.
trend_data <- function(nurses_subset, group_var, type) {
    nurses_subset_grouped <- nurses_subset %>% group_by(year)
    
    group_var <- as.symbol(group_var)
    if (group_var != "none") {
        nurses_subset_grouped <- nurses_subset_grouped %>%
            group_by(.dots = group_var, add = TRUE)
    }
    
    if (type == "membership") {
        nurses_subset_grouped <- nurses_subset_grouped %>%
            summarize(
                prop = mean(member, na.rm = TRUE),
                n = n()
            )
    } else if (type == "coverage") {
        nurses_subset_grouped <- nurses_subset_grouped %>%
            summarize(
                prop = mean(covered, na.rm = TRUE),
                n = n()
            )
    } else {
        # TODO: Put an assertion here.
    }
    
    nurses_subset_grouped
}

# Plot union membership or union contract coverage over time.
# TODO: Plot axis labels, etc. + plot styling.
trend_plot <- function(nurses_subset, group_var, type) {
    trend_data <- trend_data(nurses_subset, group_var, type)
    
    group_var <- as.symbol(group_var)
    if (group_var != "none") {
        p <- ggplot(trend_data,
                    aes_(quote(year), quote(prop), color = group_var))
    } else {
        p <- ggplot(trend_data, aes(year, prop))
    }
    
    p + geom_line() + expand_limits(y = 0)
}

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
        trend_plot(nurses_subset, group_var, type = "membership")
    })
    
    # Renders the trend plot for union contract coverage.
    output$coverage_trend_plot <- renderPlot({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        trend_plot(nurses_subset, group_var, type = "coverage")
    })
    
    # Renders the data table showing the union membership trend data.
    output$membership_trend_data <- renderDataTable({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        trend_data(nurses_subset, group_var, type = "membership")
    })
    
    # Renders the data table showing the union contract coverage data.
    output$coverage_trend_data <- renderDataTable({
        nurses_subset <- nurses_subset_selected()
        group_var <- input$trends_group_var
        trend_data(nurses_subset, group_var, type = "coverage")
    })
    
    # Renders the data table showing the subset of the nurses data selected by the user.
    output$nurses_subset_table <- renderDataTable(nurses_subset_selected())
}

# Run the app ---------------------------------------------------------------------------------

shinyApp(ui, server)
