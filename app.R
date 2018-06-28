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
                # TODO: Are these the right levels for the race variable?
                bsCollapsePanel(title = "Race",
                    checkboxGroupInput(inputId = "race_selection", label = NULL,
                                       choices = race_factor_levels(),
                                       selected = race_factor_levels())
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
            tabsetPanel(
                type = "tabs",
                tabPanel(title = "Trends",
                    # TODO: Make the plots a fixed size?
                    fluidRow(
                        column(width = 12,
                            selectInput(inputId = "trends_group_by", label = "Group by:",
                                choices = list(
                                    "None" = "none",
                                    "Sex" = "sex",
                                    "Age group" = "age_group",
                                    "Race" = "race",
                                    "Level of education" = "educ",
                                    "Citizenship status" = "citizen"
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                               plotOutput("members_trend")
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                               plotOutput("coverage_trend")
                        )
                    )
                ),
                tabPanel("Geography", "Geographic plots."),
                tabPanel("Download", "Download the data.")
            )
        )
    )
)

# Server logic --------------------------------------------------------------------------------

# Plot union membership or union contract coverage over time.
trend_plot <- function(input, output, type) {
    nurses_subset <- nurses %>%
        filter(
            year >= input$year_range[1], year <= input$year_range[2],
            sex %in% input$sex_selection,
            age_group %in% input$age_selection,
            educ %in% input$educ_selection,
            citizen %in% input$citizen_selection
        ) %>%
        group_by(year)
    
    group_var <- as.symbol(input$trends_group_by)
    if (group_var != "none") {
        nurses_subset <- nurses_subset %>%
            group_by(.dots = group_var, add = TRUE)
    }
    
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
    
    if (group_var != "none") {
        p <- ggplot(nurses_subset,
                    aes_(quote(year), quote(prop), color = group_var))
    } else {
        p <- ggplot(nurses_subset, aes(year, prop))
    }
    
    p + geom_line() + expand_limits(y = 0)
}

server <- function(input, output) {
    output$members_trend <- renderPlot({
        trend_plot(input, output, type = "membership")
    })
    
    output$coverage_trend <- renderPlot({
        trend_plot(input, output, type = "coverage")
    })
}

# Run the app ---------------------------------------------------------------------------------

shinyApp(ui, server)
