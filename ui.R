# ui.R

# --- bslib Theme Definitions ---
light_theme <- bs_theme(version = 5)
dark_theme <- bs_theme(version = 5, bootswatch = "darkly")

ui <- page_sidebar(
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
  
  conditionalPanel("input.main_nav == 'Descriptive Statistics'",
                   h2("Descriptive Statistics"),
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
                       verbatimTextOutput("summary_stats_output")
                     )
                   ),
                   layout_columns(
                     col_widths = c(6, 6),
                     card(
                       card_header("Histogram"),
                       checkboxInput("show_mean_median", "Plot Mean and Median", value = TRUE),
                       numericInput("hist_bins", "Number of Bins", value = 30, min = 5, max = 100),
                       plotOutput("histogram_plot")
                     ),
                     card(
                       card_header("Box Plot"),
                       plotOutput("boxplot_plot")
                     )
                   ),
                   layout_columns(
                     col_widths = c(6, 6),
                     card(
                       card_header("Density Plot"),
                       plotOutput("density_plot")
                     ),
                     card(
                       card_header("Pie Chart (for Categorical Data)"),
                       plotOutput("pie_chart_plot")
                     )
                   ),
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
  ),
  
  conditionalPanel("input.main_nav == 'Inferential Statistics'",
                   h2("Inferential Statistics"),
                   inferential_tab_ui 
  ),
  
  conditionalPanel("input.main_nav == 'Regression & Correlation'",
                   h2("Regression and Correlation"),
                   layout_columns(
                     col_widths = c(6, 6),
                     card(
                       card_header("Linear Regression"),
                       uiOutput("select_regression_dv"),
                       uiOutput("select_regression_iv"),
                       actionButton("run_regression", "Run Linear Regression"),
                       verbatimTextOutput("regression_summary")
                     ),
                     card(
                       card_header("Correlation Matrix"),
                       uiOutput("select_correlation_vars"),
                       actionButton("run_correlation", "Calculate Correlation"),
                       verbatimTextOutput("correlation_matrix")
                     )
                   )
  ),
  
  conditionalPanel("input.main_nav == 'Probability'",
                   h2("Probability Distributions and Calculations"),
                   navset_card_tab(
                     nav_panel("Basic Event Probability",
                               layout_columns(
                                 col_widths = c(6, 6),
                                 card(
                                   card_header("Input Probabilities"),
                                   numericInput("prob_A", "P(A)", value = 0.5, min = 0, max = 1, step = 0.01),
                                   numericInput("prob_B", "P(B)", value = 0.5, min = 0, max = 1, step = 0.01),
                                   numericInput("prob_A_and_B", "P(A and B) (Intersection)", value = 0.25, min = 0, max = 1, step = 0.01),
                                   actionButton("calculate_probabilities", "Calculate Basic Probabilities")
                                 ),
                                 card(
                                   card_header("Calculated Results"),
                                   h4("Calculated Probabilities:"),
                                   verbatimTextOutput("calculated_probs"),
                                   hr(),
                                   h4("Conditional Probabilities:"),
                                   verbatimTextOutput("conditional_probs"),
                                   hr(),
                                   h4("Event Relationships:"),
                                   verbatimTextOutput("event_relationships")
                                 )
                               )
                     ),
                     nav_panel("Normal Distribution",
                               layout_columns(
                                 col_widths = c(4, 8), # Split the page 1/3 and 2/3
                                 
                                 # --- Column 1: All Inputs ---
                                 card(
                                   card_header("Distribution Parameters"),
                                   numericInput("normal_mean", "Mean (\u03bc):", value = 0),
                                   numericInput("normal_sd", "Standard Deviation (\u03c3):", value = 1, min = 0.01),
                                   
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