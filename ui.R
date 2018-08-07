# File for defining the user interface.

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)

source("factor_levels.R")

# User interface ------------------------------------------------------------------------------

ui <- fluidPage(
    useShinyjs(),
    
    titlePanel("Nurses web tool"),
    
    sidebarLayout(
        sidebarPanel(
            # Hack to force the slider input widget to only put tick marks at integer values.
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            
            # Hack to ensure the checkboxes in the variable selection panel are properly aligned.
            tags$style(type = "text/css", "#selection-panel .awesome-checkbox label {width: 80%;}"),
            
            # Hack to ensure that checkboxes in options tabs are properly aligned.
            # TODO: Is this the right CSS class?
            tags$style(type = "text/css", ".checkboxbs label {width: 80%;}"),
            
            # TODO: Am I guaranteed to set the CSS id thought the ``id'' attribute?
            bsCollapse(id = "selection-panel",
                # Year range selection.
                bsCollapsePanel(title = "Year range",
                    sliderInput(inputId = "year_range",
                        label = NULL,
                        # TODO: Why do min/max need to be doubles for this to work correctly?
                        min = min_year, max = max_year,
                        value = c(min_year, max_year),
                        sep = ""
                    )
                ),
                
                # Sex selection.
                bsCollapsePanel(title = "Sex",
                    awesomeCheckboxGroup(inputId = "sex_selection",
                        label = NULL,
                        choices = sex_factor_levels(),
                        selected = sex_factor_levels()
                    )
                ),
                
                # Age group selection.
                bsCollapsePanel(title = "Age group",
                    awesomeCheckboxGroup(inputId = "age_selection",
                        label = NULL,
                        choices = age_group_factor_levels(),
                        selected = age_group_factor_levels()
                    )
                ),
                
                # Race selection.
                bsCollapsePanel(title = "Race",
                    awesomeCheckboxGroup(inputId = "race_selection",
                        label = NULL,
                        choices = race_factor_levels(),
                        selected = race_factor_levels()
                    )
                ),
                
                # Hispanic status selection.
                bsCollapsePanel(title = "Hispanic status",
                    awesomeCheckboxGroup(inputId = "hisp_status_selection",
                        label = NULL,
                        #choices = c("Hispanic" = TRUE, "Non-Hispanic" = FALSE),
                        #selected = c(TRUE, FALSE)
                        choices = hispanic_factor_levels(),
                        selected = hispanic_factor_levels()
                    )
                ),
                
                # Education level selection.
                bsCollapsePanel(title = "Level of education",
                    awesomeCheckboxGroup(inputId = "educ_selection",
                        label = NULL,
                        choices = education_factor_levels(),
                        selected = education_factor_levels()
                    )
                ),
                
                # Citizenship status selection.
                bsCollapsePanel(title = "Citizenship status",
                    awesomeCheckboxGroup(inputId = "citizen_selection",
                        label = NULL,
                        choices = citizenship_factor_levels(),
                        selected = citizenship_factor_levels()
                    )
                ),
                
                # State selection.
                bsCollapsePanel(title = "States",
                    pickerInput(inputId = "state_selection",
                        label = NULL,
                        choices = state_factor_levels(),
                        selected = state_factor_levels(),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE)
                    )
                )
            )
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                
                # Trends panel.
                tabPanel(title = "Trends",
                    
                    tabsetPanel(type = "tabs",
                        
                        # Panel for viewing trend plots.
                        # TODO: Make the plots a fixed size?
                        tabPanel(title = "Plots",
                            fluidRow(
                                column(width = 12,
                                    # Trend plot for union membership coverage.
                                    h3("Membership proportion"),
                                    plotOutput("members_trend_plot")
                                )
                            ),
                            fluidRow(
                                column(width = 12,
                                    # Trend plot for union contract coverage.
                                    h3("Coverage proportion"),
                                    plotOutput("coverage_trend_plot")
                                )
                            )
                        ),
                        
                        # Panel for viewing the data used for generating the trend plots.
                        tabPanel(title = "Data",
                            fluidRow(
                                column(width = 12,
                                    # Data table showing the data used to generate the union
                                    # membership trend plot.
                                    h3("Membership proportion"),
                                    dataTableOutput("membership_trend_data")
                                )
                            ),
                            fluidRow(
                                column(width = 12,
                                    # Data table showing the data used to generate the union
                                    # contract coverage trend plot.
                                    h3("Coverage proportion"),
                                    dataTableOutput("coverage_trend_data")
                                )
                            )
                        ),
                        
                        # Panel for setting options related to the trend plots.
                        tabPanel(title = "Options",
                            br(),
                            fluidRow(
                                column(width = 12,
                                    wellPanel(
                                        # Widget for selecting the variable to group by in the trend
                                        # data.
                                        selectInput(inputId = "trends_group_var",
                                            label = "Group by:",
                                            # The value of each choice is a variable name in the data
                                            # set, except for ``none'', which corresponds to no
                                            # grouping.
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
                                    ),
                                    
                                    # If selected, fix the vertical axis on each trend plot to be
                                    # from 0 to 1, inclusive.
                                    awesomeCheckbox(inputId = "trends_fixed_axis",
                                        label = "Fix vertical axes to be from 0 to 1, inclusive"
                                    ),
                                    
                                    awesomeCheckbox(inputId = "trends_plot_diff",
                                        label = "Plot the difference between two levels of a variable"
                                    ),
                                    
                                    wellPanel(
                                        selectInput(inputId = "trends_diff_var",
                                            label = "Variable:",
                                            choices = list(
                                                "Sex" = "sex",
                                                "Age group" = "age_group",
                                                "Race" = "race",
                                                "Hispanic status" = "hisp",
                                                "Level of education" = "educ",
                                                "Citizenship status" = "citizen",
                                                "State" = "state"
                                            )
                                        ),
                                        
                                        selectInput(inputId = "trends_diff_first",
                                            label = "First",
                                            choices = NULL
                                        ),
                                        
                                        selectInput(inputId = "trends_diff_second",
                                            label = "Second",
                                            choices = NULL
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
                        tabPanel(title = "Maps",
                            fluidRow(
                                column(width = 12,
                                    # Map showing union membership per state.
                                    h3("Membership proportion"),
                                    plotOutput("membership_state_map")
                                )
                            ),
                            fluidRow(
                                column(width = 12,
                                    # Map showing union contract coverage per state.
                                    h3("Coverage proportion"),
                                    plotOutput("coverage_state_map")
                                )
                            )
                        ),
                        
                        # Panel for viewing the data used to generate the maps.
                        tabPanel(title = "Data",
                            fluidRow(
                                column(width = 12,
                                    # Data table showing the data used to generate the union
                                    # membership map.
                                    h3("Membership proportion"),
                                    dataTableOutput("membership_state_data")
                                )
                            ),
                            fluidRow(
                                column(width = 12,
                                    # Data table showing the data used to generate the union
                                    # contract coverage map.
                                    h3("Coverage proportion"),
                                    dataTableOutput("coverage_state_data")
                                )
                            )
                        ),
                        
                        # Panel for setting options related to the maps.
                        tabPanel(title = "Options",
                            fluidRow(
                                column(width = 12,
                                    # If selected, show only the states selected by the user in the
                                    # chloropleth maps.
                                    awesomeCheckbox(inputId = "selected_states_only",
                                        label = "Show selected states only"
                                    ),
                                    
                                    # If selected, fix the color scale on the maps to assign colors
                                    # to all proportions from 0 to 1, inclusive.
                                    awesomeCheckbox(inputId = "maps_fixed_scale",
                                        # TODO: Better label.
                                        label = "Fix the color scale on the chloropleth maps"
                                    )
                                )
                            )
                        )
                    )
                ),
                
                # Data viewer panel.
                tabPanel(title = "Data",
                    fluidRow(
                        # Data table showing the subset of the nurses data selected by the user.
                        column(width = 12,
                            dataTableOutput("nurses_subset_table")
                        )
                    )
                )
            )
        )
    )
)
