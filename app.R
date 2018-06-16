library(shiny)
library(shinyBS)

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
                        # TODO: The year range should be extracted from the data.
                        min = 2011, max = 2017,
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
                    title = "Age group"
                ),
                
                # Race selection.
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
                
                # Immigrant status selection.
                # TODO: What are the pertinent CPS variables?
                bsCollapsePanel(
                    title = "Immigrant status"
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
                    "Trend plots."
                    # fluidRow(
                    #     column(12, "Here is a plot"),
                    #     column(12, "Here is more stuff")
                    # )
                ),
                tabPanel("Geography", "Geographic plots."),
                tabPanel("Download", "Download the data.")
            )
        )
    )
)

# Server logic --------------------------------------------------------------------------------

server <- function(input, output) {
}

# Set up and run the app ----------------------------------------------------------------------

shinyApp(ui, server)
