library(shiny)
library(shinyBS)
# TODO: Import entire tidyverse?
library(tidyverse)

# Load the data -------------------------------------------------------------------------------

# TODO: Have factor levels returned by functions (to ease maintainability)?
# TODO: Right now the thing infers factor levels from the data. Do we want this?
nurses <- read_csv("nurses_preprocessed.csv",
    col_types = cols(
        year = col_integer(),
        #sex = col_factor(levels = c("Male", "Female")),
        sex = col_factor(NULL),
        member = col_logical(),
        covered = col_logical(),
        age = col_integer(),
        #age_group = col_factor(levels = cut(age, breaks = c(0, 16, 25, 55, Inf), right = FALSE))
        age_group = col_factor(NULL),
        educ = col_factor(NULL),
        citizen = col_factor(NULL)
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
                        choices = list(
                            "Male",
                            "Female"
                        )
                        #selected = 1:2
                    )
                ),
                
                # Age group selection.
                bsCollapsePanel(title = "Age group",
                    checkboxGroupInput(inputId = "age_selection", label = NULL,
                        choices = list(
                            "16-24",
                            "25-54",
                            "55 and over"
                        )
                        #selected = 1:3
                    )
                ),
                
                # Race selection.
                # TODO: Are these the right levels for the race variable?
                bsCollapsePanel(title = "Race",
                    checkboxGroupInput(inputId = "race_selection", label = NULL,
                        choices = list(
                            "White" = 1,
                            "Black" = 2,
                            "Hispanic" = 3,
                            "Asian" = 4,
                            "Native American" = 5,
                            "Hawaiian/Pacific Islander" = 6,
                            "Other" = 7
                        ),
                        selected = 1:7
                    )
                ),
                
                # Education level selection.
                bsCollapsePanel(title = "Level of education",
                    checkboxGroupInput(inputId = "educ_selection", label = NULL,
                        choices = list(
                            "No high school",
                            "Completed high school",
                            "Some college",
                            "Associate degree",
                            "Bachelor's degree",
                            "Graduate degree"
                        )
                        #selected = 1:6
                    )
                ),
                
                # Citizenship status selection.
                bsCollapsePanel(title = "Citizenship status",
                    checkboxGroupInput(inputId = "citizen_selection", label = NULL,
                        choices = list(
                            "US native",
                            "Foreign-born, citizen",
                            "Foreign-born, non-citizen"
                        )
                        #selected = 1:3
                    )
                ),
                
                # TODO: How to handle geographic region reasonably?
                bsCollapsePanel(title = "Geography")
            )
        ),
        
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(title = "Trends",
                    fluidRow(
                        # TODO: Make the plots a fixed size?
                        column(width = 12, plotOutput("members_trend")),
                        column(width = 12, plotOutput("coverage_trend"))
                    )
                ),
                tabPanel("Geography", "Geographic plots."),
                tabPanel("Download", "Download the data.")
            )
        )
    )
)

# Server logic --------------------------------------------------------------------------------

server <- function(input, output) {
    output$members_trend <- renderPlot({
        nurses_subset <- nurses %>%
            filter(
                year >= input$year_range[1], year <= input$year_range[2],
                sex %in% input$sex_selection,
                educ %in% input$educ_selection,
                citizen %in% input$citizen_selection
            ) %>%
            # TODO: Example of grouping (by sex). Want to extend this functionality.
            group_by(year, sex) %>%
            summarize(
                prop_members = mean(member, na.rm = TRUE),
                n = n()
            )
        
        ggplot(nurses_subset, aes(year, prop_members, color = sex)) +
            geom_line() +
            #coord_cartesian(ylim = c(0, NA))
            expand_limits(y = 0)
    })
    
    output$coverage_trend <- renderPlot({
        #plot(1:10, 1:10)
    })
}

# Run the app ---------------------------------------------------------------------------------

shinyApp(ui, server)
