library(shiny)
library(shinyBS)
# TODO: Import entire tidyverse?
library(tidyverse)

source("factor_levels.R")

# Load the data -------------------------------------------------------------------------------

# TODO: Should I have bothered coding all these variables as factors?? It adds a lot of additional
# code (for a marginal improvement in readability).

nurses <- read_csv("nurses_preprocessed.csv",
    col_types = cols(
        year = col_integer(),
        sex = col_factor(levels = sex_factor_levels()),
        member = col_logical(),
        covered = col_logical(),
        age = col_integer(),
        age_group = col_factor(age_group_factor_levels()),
        race = col_factor(race_factor_levels()),
        hisp = col_logical(),
        educ = col_factor(education_factor_levels()),
        citizen = col_factor(citizenship_factor_levels())
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
                
                # TODO: How to handle geographic region reasonably?
                bsCollapsePanel(title = "Geography")
            )
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel(title = "Trends",
                    br(),
                    fluidRow(
                        column(width = 12,
                            selectInput(inputId = "trends_group_by", label = "Group by:",
                                choices = list(
                                    "None" = "none",
                                    "Sex" = "sex",
                                    "Age group" = "age_group",
                                    "Race" = "race",
                                    "Hispanic status" = "hisp",
                                    "Level of education" = "educ",
                                    "Citizenship status" = "citizen"
                                )
                            )
                        )
                    ),
                    # TODO: Make the plots a fixed size?
                    fluidRow(
                        column(width = 12,
                            h3("Membership proportion"),
                            plotOutput("members_trend")
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                            h3("Coverage proportion"),
                            plotOutput("coverage_trend")
                        )
                    )
                ),
                tabPanel("Geography", "Geographic plots."),
                tabPanel("Data", dataTableOutput("nurses_subset_table")),
                tabPanel("Download", "Download the data.")
            )
        )
    )
)

# Server logic --------------------------------------------------------------------------------

# Returns the subset of the data set selected by the user (and grouped appropriately).
nurses_subset_selected <- function(input) {
    nurses_subset <- nurses %>%
        filter(
            year >= input$year_range[1], year <= input$year_range[2],
            sex %in% input$sex_selection,
            age_group %in% input$age_selection,
            race %in% input$race_selection,
            hisp %in% input$hisp_status_selection,
            educ %in% input$educ_selection,
            citizen %in% input$citizen_selection
        ) %>%
        group_by(year)
    
    group_var <- as.symbol(input$trends_group_by)
    if (group_var != "none") {
        nurses_subset <- nurses_subset %>%
            group_by(.dots = group_var, add = TRUE)
    }
    
    nurses_subset
}

# Plot union membership or union contract coverage over time.
# TODO: Plot axis labels, etc. + plot styling.
trend_plot <- function(input, type) {
    nurses_subset <- nurses_subset_selected(input)
    
    if (type == "membership") {
        nurses_subset <- nurses_subset %>%
            summarize(
                prop = mean(member, na.rm = TRUE),
                n = n()
            )
    } else if (type == "coverage") {
        nurses_subset <- nurses_subset %>%
            summarize(
                prop = mean(covered, na.rm = TRUE),
                n = n()
            )
    } else {
        # TODO: Put an assertion here.
    }
    
    group_var <- as.symbol(input$trends_group_by)
    if (group_var != "none") {
        p <- ggplot(nurses_subset,
                    aes_(quote(year), quote(prop), color = group_var))
    } else {
        p <- ggplot(nurses_subset, aes(year, prop))
    }
    
    p + geom_line() + expand_limits(y = 0)
}

server <- function(input, output) {
    # TODO: Make subset of nurses a reactive variable to avoid lots of redundant computation.
    
    output$members_trend <- renderPlot({
        trend_plot(input, type = "membership")
    })
    
    output$coverage_trend <- renderPlot({
        trend_plot(input, type = "coverage")
    })
    
    output$nurses_subset_table <- renderDataTable(nurses_subset_selected(input))
}

# Run the app ---------------------------------------------------------------------------------

shinyApp(ui, server)
