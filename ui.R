# File for defining the user interface.

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)

source("factor_levels.R")

ui <- fluidPage(
  theme = shinytheme("readable"),

  useShinyjs(),

  titlePanel("Nurses web tool"),

  sidebarLayout(
    sidebarPanel(
      # Hack to force the slider input widget to only put tick marks at integer
      # values.
      tags$style(type = "text/css", ".irs-grid-pol.small { height: 0px; }"),

      bsCollapse(
        id = "selection_panels",
        multiple = TRUE,

        # Year range selection.
        bsCollapsePanel(
          title = "Year range",
          sliderInput(
            inputId = "year_range",
            label = NULL,
            # TODO: Why do min/max need to be doubles for this to work
            # correctly?
            min = min_year, max = max_year,
            value = c(min_year, max_year),
            sep = ""
          )
        ),

        # Sex selection.
        bsCollapsePanel(
          title = "Sex",
          checkboxGroupInput(
            inputId = "sex_selection",
            label = NULL,
            choices = sex_factor_levels(),
            selected = sex_factor_levels()
          )
        ),

        # Age group selection.
        bsCollapsePanel(
          title = "Age group",
          checkboxGroupInput(
            inputId = "age_selection",
            label = NULL,
            choices = age_group_factor_levels(),
            selected = age_group_factor_levels()
          )
        ),

        # Race selection.
        bsCollapsePanel(
          title = "Race",
          checkboxGroupInput(
            inputId = "race_selection",
            label = NULL,
            choices = race_factor_levels(),
            selected = race_factor_levels()
          )
        ),

        # Hispanic status selection.
        bsCollapsePanel(
          title = "Hispanic status",
          checkboxGroupInput(
            inputId = "hisp_status_selection",
            label = NULL,
            choices = hispanic_factor_levels(),
            selected = hispanic_factor_levels()
          )
        ),

        # Education level selection.
        bsCollapsePanel(
          title = "Level of education",
          checkboxGroupInput(
            inputId = "educ_selection",
            label = NULL,
            choices = education_factor_levels(),
            selected = education_factor_levels()
          )
        ),

        # Citizenship status selection.
        bsCollapsePanel(
          title = "Citizenship status",
          checkboxGroupInput(
            inputId = "citizen_selection",
            label = NULL,
            choices = citizenship_factor_levels(),
            selected = citizenship_factor_levels()
          )
        ),

        # State selection.
        bsCollapsePanel(
          title = "States",
          pickerInput(
            inputId = "state_selection",
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
      tabsetPanel(id = "tabs",
        type = "tabs",

        # Trends panel.
        tabPanel(
          title = "Trends",

          tabsetPanel(
            type = "tabs",

            # Panel for viewing trend plots.
            # TODO: Make the plots a fixed size?
            tabPanel(
              title = "Plots",
              fluidRow(
                column(
                  width = 12,
                  # Trend plot for union membership coverage.
                  h3("Union membership (density)"),
                  plotOutput("membership_trend_plot")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  # Trend plot for union contract coverage.
                  h3("Union contract coverage"),
                  plotOutput("coverage_trend_plot")
                )
              )
            ),

            # Panel for viewing the data used for generating the trend plots.
            tabPanel(
              title = "Data",
              fluidRow(
                column(
                  width = 12,
                  # Data table showing the data used to generate the union
                  # membership trend plot.
                  h3("Union membership (density)"),
                  downloadButton("membership_trend_download"),
                  br(), br(), # TODO: Hacky spacing.
                  dataTableOutput("membership_trend_data")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  # Data table showing the data used to generate the union
                  # contract coverage trend plot.
                  h3("Union contract coverage"),
                  downloadButton("coverage_trend_download"),
                  br(), br(), # TODO: Hacky spacing.
                  dataTableOutput("coverage_trend_data")
                )
              )
            ),

            # Panel for setting options related to the trend plots.
            tabPanel(
              title = "Options",
              fluidRow(
                column(
                  width = 12,
                  h3("General options"),

                  # If selected, fix the vertical axis on each trend plot to be
                  # from 0 to 1, inclusive.
                  checkboxInput(
                    inputId = "trends_fixed_axis",
                    label = "Fix vertical axes to be from 0 to 1, inclusive"
                  ),

                  # If checked, use the viridis color palette for grouped plots.
                  checkboxInput(
                    inputId = "trends_use_viridis",
                    label = "Use viridis color palette for grouped plots"
                  ),

                  # Plot type selection.
                  radioButtons(
                    inputId = "trend_plot_type",
                    label = "Plot type",
                    choices = list(
                      "Grouped plot" = "grouped",
                      "Difference plot" = "diff"
                    )
                  ),

                  h3("Grouped plot options"),

                  helpText("The resulting plot will contain a trend line for
                            each level of the variable selected."),

                  # Selection for the variable to group by in the trend data.
                  selectInput(
                    inputId = "trends_group_var",
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
                  ),

                  h3("Difference plot options"),

                  helpText("The resulting plot will consist of a trend line of
                            the difference between the first and second levels
                            (i.e., first minus second) of the selected
                            variable."),

                  # Selection for the variable to use in the difference plot.
                  selectInput(
                    inputId = "trends_diff_var",
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

                  # Selection for first level of selected variable.
                  selectInput(
                    inputId = "trends_diff_level1",
                    label = "First:",
                    choices = NULL
                  ),

                  # Selection for second level of selected variable.
                  selectInput(
                    inputId = "trends_diff_level2",
                    label = "Second:",
                    choices = NULL
                  )
                )
              )
            )
          )
        ),

        # States panel.
        tabPanel(
          title = "States",

          tabsetPanel(
            type = "tabs",

            # Panel for viewing chloropleth maps.
            tabPanel(
              title = "Maps",
              fluidRow(
                column(
                  width = 12,
                  # Map showing union membership per state.
                  h3("Union membership (density)"),
                  plotOutput("membership_state_map")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  # Map showing union contract coverage per state.
                  h3("Union contract coverage"),
                  plotOutput("coverage_state_map")
                )
              )
            ),

            # Panel for viewing the data used to generate the maps.
            tabPanel(
              title = "Data",
              fluidRow(
                column(
                  width = 12,
                  # Data table showing the data used to generate the union
                  # membership map.
                  h3("Union membership (density)"),
                  downloadButton("membership_state_download"),
                  br(), br(), # TODO: Hacky spacing.
                  dataTableOutput("membership_state_data")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  # Data table showing the data used to generate the union
                  # contract coverage map.
                  h3("Union contract coverage"),
                  downloadButton("coverage_state_download"),
                  br(), br(), # TODO: Hacky spacing.
                  dataTableOutput("coverage_state_data")
                )
              )
            ),

            # Panel for setting options related to the maps.
            tabPanel(
              title = "Options",
              fluidRow(
                column(
                  width = 12,
                  h3("General options"),

                  # If selected, show only the states selected by the user in
                  # the chloropleth maps.
                  checkboxInput(
                    inputId = "selected_states_only",
                    label = "Show selected states only"
                  ),

                  # If selected, fix the color scale on the maps to assign
                  # colors to all proportions from 0 to 1, inclusive.
                  checkboxInput(
                    inputId = "maps_fixed_scale",
                    # TODO: Better label.
                    label = "Fix the color scale on the chloropleth maps"
                  )
                )
              )
            )
          )
        ),

        # Data viewer panel.
        tabPanel(
          title = "Data",
          fluidRow(
            column(
              width = 12,
              h3("Selected data"),
              downloadButton("nurses_subset_download"),
              br(), br(), # TODO: Hacky spacing.
              dataTableOutput("nurses_subset_table")
            )
          )
        )
      )
    )
  )
)
