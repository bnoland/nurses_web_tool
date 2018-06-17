library(shiny)
library(shinyBS)
# TODO: Import entire tidyverse?
library(tidyverse)

# Load the data -------------------------------------------------------------------------------

# TODO: Column specifications?
nurses <- read_csv("nurses.csv")

# User interface ------------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Nurses web tool"),
    
    sidebarLayout(
        sidebarPanel(
            # Hack to force the slider input widget to only put tick marks at integer values.
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            
            bsCollapse(
                # Year range selection.
                bsCollapsePanel(
                    title = "Year range",
                    sliderInput(
                        inputId = "year_range",
                        label = NULL,
                        # TODO: Why do min/max need to be doubles for this to work correctly?
                        min = as.double(min(nurses$year, na.rm = TRUE)),
                        max = as.double(max(nurses$year, na.rm = TRUE)),
                        value = c(min, max),
                        sep = ""
                    )
                ),
                
                # Gender selection.
                bsCollapsePanel(
                    title = "Gender",
                    checkboxGroupInput(
                        inputId = "gender_selection",
                        label = NULL,
                        choices = list(
                            "Male" = 1,
                            "Female" = 2
                        ),
                        selected = 1:2
                    )
                ),
                
                # Age group selection.
                bsCollapsePanel(
                    title = "Age group",
                    checkboxGroupInput(
                        inputId = "age_selection",
                        label = NULL,
                        choices = list(
                            "16-24" = 1,
                            "25-54" = 2,
                            "55 and over" = 3
                        ),
                        selected = 1:3
                    )
                ),
                
                # Race selection.
                # TODO: Are these the right levels for the race variable?
                bsCollapsePanel(
                    title = "Race",
                    checkboxGroupInput(
                        inputId = "race_selection",
                        label = NULL,
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
                bsCollapsePanel(
                    title = "Level of education",
                    checkboxGroupInput(
                        inputId = "educ_selection",
                        label = NULL,
                        choices = list(
                            "No high school" = 1,
                            "Completed high school" = 2,
                            "Some college" = 3,
                            "Associate degree" = 4,
                            "Bachelor's degree" = 5,
                            "Graduate degree" = 6
                        ),
                        selected = 1:6
                    )
                ),
                
                # Citizenship status selection.
                bsCollapsePanel(
                    title = "Citizenship status",
                    checkboxGroupInput(
                        inputId = "citizen_selection",
                        label = NULL,
                        choices = list(
                            "US-born" = 1,
                            "Foreign-born, non-citizen" = 2,
                            "Foreign-born, citizen" = 3
                        ),
                        selected = 1:3
                    )
                ),
                
                # TODO: How to handle geographic region reasonably?
                bsCollapsePanel(
                    title = "Geography"
                )
            )
        ),
        
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Trends",
                    fluidRow(
                        # TODO: Make the plots a fixed size?
                        column(
                            width = 12,
                            plotOutput("members_trend")
                        ),
                        column(
                            width = 12,
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

server <- function(input, output) {
    # TODO: Only update plots when the corresponding tab is selected?
    output$members_trend <- renderPlot({
        plot(1:10, 1:10)
    })
    output$coverage_trend <- renderPlot({
        plot(1:10, 1:10)
    })
}

# Run the app ---------------------------------------------------------------------------------

shinyApp(ui, server)
