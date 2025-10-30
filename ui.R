# ui.R

# --- bslib Theme Definitions ---
light_theme <- bs_theme(version = 5)
dark_theme <- bs_theme(version = 5, bootswatch = "darkly")

ui <- page_sidebar(
  useShinyjs(),
  title = "OpenStat Web App",
  
  sidebar = sidebar(
    materialSwitch(inputId = "dark_mode_switch", label = "Dark Mode", status = "primary"),
    hr(),
    navset_pill(
      nav_panel("Data Input", icon = icon("table")),
      nav_panel("Descriptive Statistics", icon = icon("chart-bar")),
      nav_panel("Inferential Statistics", icon = icon("flask")),
      nav_panel("Regression & Correlation", icon = icon("chart-line")),
      nav_panel("Probability", icon = icon("dice")),
      id = "main_nav"
    )
  ),
  
  conditionalPanel("input.main_nav == 'Data Input'",
                   h2("Data Input and Editor"),
                   layout_columns(
                     col_widths = c(6, 6),
                     card(
                       card_header("Upload Dataset (.csv, .xlsx)"),
                       fileInput("file_upload", "Choose CSV/Excel File",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx")),
                       helpText("Note: For Excel files, only the first sheet will be read.")
                     ),
                     card(
                       card_header("Manual Data Entry"),
                       helpText("Click the button below to open a spreadsheet editor for manual data input."),
                       actionButton("open_manual_data_modal", "Open Spreadsheet Editor", icon = icon("edit")),
                       actionButton("load_sample_data", "Load Sample Data"),
                       actionButton("clear_manual_data", "Clear All Data")
                     )
                   ),
                   card(
                     card_header("Dataset Preview & Editor"),
                     helpText("The table below shows the currently loaded dataset. You can edit cells directly. Edits are saved automatically to the active dataset."),
                     DTOutput("data_preview_table")
                   )
  ),
  
  # --- START: Definitive Corrected Descriptive Statistics Panel ---
  
  conditionalPanel("input.main_nav == 'Descriptive Statistics'",
                   # Use tagList to group all elements within the conditionalPanel
                   tagList(
                     h2("Descriptive Statistics"),
                     
                     # Layout 1: Variable Selection & Summary
                     layout_columns(
                       col_widths = c(4, 8),
                       card(
                         card_header("Variable Selection"),
                         uiOutput("select_descriptive_variable"),
                         uiOutput("select_group_by_variable"),
                         actionButton("analyze_descriptive", "Analyze")
                       ),
                       card(
                         card_header("Summary Statistics"),
                         DTOutput("summary_stats_output")
                       )
                     ),
                     
                     # Layout 2: Histogram & Box Plot
                     layout_columns(
                       col_widths = c(6, 6),
                       card(
                         card_header("Histogram"),
                         checkboxInput("show_mean_median", "Plot Mean and Median", value = TRUE),
                         numericInput("hist_bins", "Number of Bins", value = 30, min = 5, max = 100),
                         selectInput("hist_yaxis_type", "Y-Axis Represents:",
                                     choices = c("Count (Frequency)" = "count",
                                                 "Percent" = "percent")),
                         plotOutput("histogram_plot")
                       ),
                       card(
                         card_header("Box Plot"),
                         plotOutput("boxplot_plot")
                       )
                     ),
                     
                     # Layout 3: Density Plot & Pie Chart
                     layout_columns(
                       col_widths = c(6, 6),
                       card(
                         card_header("Density Plot"),
                         plotOutput("density_plot")
                       ),
                       card(
                         card_header("Pie Chart"),
                         plotOutput("pie_chart_plot")
                       )
                     ),
                     
                     # Layout 4: The new Bar Chart
                     layout_columns(
                       col_widths = 12,
                       card(
                         card_header("Bar Chart (for Categorical Data)"),
                         selectInput("barchart_yaxis_type", "Y-Axis Represents:",
                                     choices = c("Count (Frequency)" = "count",
                                                 "Proportion (Relative Frequency)" = "proportion")),
                         plotOutput("barchart_plot")
                       )
                     ),
                     
                     # Layout 5: Scatter Plot & Dot Plot
                     layout_columns(
                       col_widths = c(6, 6),
                       card(
                         card_header("Scatter Plot"),
                         uiOutput("select_scatter_x"),
                         uiOutput("select_scatter_y"),
                         actionButton("generate_scatter", "Generate Scatter Plot"),
                         plotOutput("scatter_plot")
                       ),
                       card(
                         card_header("Dot Plot"),
                         uiOutput("select_dot_plot_variable"),
                         actionButton("generate_dot_plot", "Generate Dot Plot"),
                         downloadButton("download_dot_plot", "Download Plot"),
                         plotOutput("dot_plot")
                       )
                     )
                   ) # End of tagList
  ),
  
  # --- END: Definitive Corrected Descriptive Statistics Panel ---,
  
  conditionalPanel("input.main_nav == 'Inferential Statistics'",
                   h2("Inferential Statistics"),
                   inferential_tab_ui 
  ),
  
  
  # --- START: New Regression & Correlation Panel Layout ---
  
  conditionalPanel("input.main_nav == 'Regression & Correlation'",
                   h2("Regression and Correlation"),
                   
                   # Changed from layout_columns to a single-column flow
                   
                   # Card 1: Linear Regression (unchanged content, just in a full-width card)
                   card(
                     card_header("Linear Regression"),
                     layout_columns(
                       col_widths = c(4, 8), # Internal layout for inputs
                       card_body(
                         uiOutput("select_regression_dv"),
                         checkboxInput("log_transform_dv_reg", "Apply Log Transform to Dependent Variable (for skew)"),
                         uiOutput("select_regression_iv"),
                         actionButton("run_regression", "Run Linear Regression")
                       ),
                       card_body(
                         navset_card_tab(
                           id = "regression_output_tabs",
                           nav_panel("Summary", verbatimTextOutput("regression_summary")),
                           nav_panel("Diagnostic Plots", plotOutput("regression_diagnostic_plots")),
                           nav_panel("Assumption Checks", verbatimTextOutput("regression_assumption_checks"))
                         )
                       )
                     )
                   ),
                   
                   # Card 2: THE NEW Logistic Regression Module
                   card(
                     card_header("Logistic Regression"),
                     card_body(
                       with_info_popover(
                         ui_element = p("Use for modeling a binary outcome (e.g., Yes/No, 1/0)."),
                         title = "What is Logistic Regression?",
                         content = "Logistic Regression predicts the probability of an outcome occurring. It's used when your dependent variable has only two categories."
                       )
                     ),
                     layout_columns(
                       col_widths = c(4, 8), # Internal layout for inputs/outputs
                       card_body(
                         uiOutput("select_logistic_dv"),
                         uiOutput("select_logistic_iv"),
                         actionButton("run_logistic", "Run Logistic Regression")
                       ),
                       card_body(
                         verbatimTextOutput("logistic_summary")
                       )
                     )
                   ),
                   
                   # Card 3: Correlation Matrix (unchanged content, just in a full-width card)
                   card(
                     card_header("Correlation Matrix"),
                     uiOutput("select_correlation_vars"),
                     actionButton("run_correlation", "Calculate Correlation"),
                     verbatimTextOutput("correlation_matrix")
                   )
  ),
  
  # --- END: Corrected Regression & Correlation Panel ---
  
  conditionalPanel("input.main_nav == 'Probability'",
                   h2("Probability Distributions and Calculations"),
                   navset_card_tab(
                     # --- START: Refactored Basic Event Probability Panel ---
                     
                     nav_panel("Basic Event Probability",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 
                                 # --- Input Card: Focused on calculation type ---
                                 card(
                                   card_header("Select Rule and Input Parameters"),
                                   
                                   selectInput("prob_calc_type", "Select Calculation Type:",
                                               choices = c(
                                                 "P(A or B) - Additive Rule" = "union",
                                                 "P(A|B) - Conditional Probability" = "conditional",
                                                 "Check for Independence / Mutually Exclusive" = "check_relationship"
                                               )),
                                   
                                   hr(),
                                   
                                   # Dynamic inputs controlled by the selection
                                   uiOutput("prob_required_inputs"),
                                   
                                   actionButton("calculate_basic_probs", "Calculate")
                                 ),
                                 
                                 # --- Results Card ---
                                 card(
                                   card_header("Result and Interpretation"),
                                   verbatimTextOutput("calculated_output_title"),
                                   verbatimTextOutput("calculated_output")
                                 )
                               )
                     ),
                     
                     # --- END: Refactored Basic Event Probability Panel ---
                     nav_panel("Normal Distribution",
                               layout_columns(
                                 col_widths = c(4, 8), # Split the page 1/3 and 2/3
                                 
                                 # --- Column 1: All Inputs ---
                                 card(
                                   card_header("Distribution Parameters"),
                                   numericInput("normal_mean", "Mean (\u03bc):", value = 0),
                                   numericInput("normal_sd", "Standard Deviation (\u03c3):", value = 1, min = 0.01),
                                   
                                   # --- START: New UI Element for Empirical Rule ---
                                   checkboxInput("show_empirical_rule", "Show Empirical Rule (68-95-99.7)", value = FALSE),
                                   # --- END: New UI Element ---
                                   
                                   hr(), # --- Separator ---
                                   
                                   # Single dropdown to control the entire panel
                                   selectInput(
                                     "normal_prob_type", "Select Calculation Type:",
                                     choices = c(
                                       "Find Probability from x (P(X < x))" = "less",
                                       "Find Probability from x (P(X > x))" = "greater",
                                       "Find Probability from range (P(a < X < b))" = "between",
                                       "Find x from Probability (Solve for x)" = "inverse"
                                     )
                                   ),
                                   # Placeholder for the dynamic input box
                                   uiOutput("normal_inputs"),
                                   
                                   # Single button for all actions
                                   actionButton("calc_normal", "Calculate")
                                 ),
                                 
                                 # --- Column 2: All Outputs ---
                                 card(
                                   card_header("Results"),
                                   h4("Calculated Value:"),
                                   verbatimTextOutput("normal_result"),
                                   hr(),
                                   h4("Visual Representation:"),
                                   plotOutput("normal_plot")
                                 )
                               )
                     ),
                     nav_panel("Binomial Distribution",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 card(
                                   card_header("Binomial Distribution Parameters"),
                                   numericInput("binom_size", "Number of Trials (n)", value = 10, min = 1, step = 1),
                                   numericInput("binom_prob", "Probability of Success (p)", value = 0.5, min = 0, max = 1, step = 0.01),
                                   
                                   ## --- ADDED --- ##
                                   hr(),
                                   h4("Distribution Summary"),
                                   verbatimTextOutput("binom_summary_stats"),
                                   ## --- END ADDED --- ##
                                   
                                   hr(),
                                   h4("Calculate P(X) given x"),
                                   numericInput("binom_k", "Number of Successes (x)", value = 5, min = 0, step = 1),
                                   selectInput("binom_type", "Probability Type", choices = c("P(X = x)", "P(X <= x)", "P(X >= x)")),
                                   actionButton("calc_binom_prob", "Calculate Probability"),
                                   verbatimTextOutput("binom_prob_output"),
                                   hr(),
                                   h4("Find x for a given Cumulative Probability P(X \u2264 x)"),
                                   numericInput("binom_p_for_k", "Cumulative Probability (e.g., 0.95):", value = 0.95, min = 0, max = 1, step = 0.01),
                                   actionButton("solve_binom_k", "Solve for x"),
                                   verbatimTextOutput("solve_binom_k_output")
                                 ),
                                 card(
                                   card_header("Binomial Distribution PMF Plot"),
                                   plotOutput("binom_pmf_plot")
                                 )
                               )
                     ),
                     nav_panel("Poisson Distribution",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 card(
                                   card_header("Poisson Distribution Parameters"),
                                   numericInput("pois_lambda", "Lambda (\u03bb - average rate)", value = 3, min = 0.01),
                                   
                                   ## --- ADDED --- ##
                                   hr(),
                                   h4("Distribution Summary"),
                                   verbatimTextOutput("pois_summary_stats"),
                                   ## --- END ADDED --- ##
                                   
                                   hr(),
                                   h4("Calculate P(X = k) or P(X <= k)"),
                                   numericInput("pois_k", "Number of Events (k)", value = 2, min = 0, step = 1),
                                   selectInput("pois_type", "Probability Type", choices = c("P(X = k)", "P(X <= k)", "P(X >= k)")),
                                   actionButton("calc_pois_prob", "Calculate Poisson Probability"),
                                   verbatimTextOutput("pois_prob_output")
                                 ),
                                 card(
                                   card_header("Poisson Distribution PMF Plot"),
                                   plotOutput("pois_pmf_plot")
                                 )
                               )
                     )
                   )
  ),
  theme = light_theme
)