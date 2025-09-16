# server.R

server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  observeEvent(input$dark_mode_switch, {
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode_switch)) dark_theme else light_theme
    )
  })
  
  # --- Reactive Values ---
  data_r <- reactiveVal(NULL)
  modal_data_r <- reactiveVal(NULL)
  current_dot_plot <- reactiveVal(NULL)
  current_normal_plot <- reactiveVal(NULL)
  
  
  # --- Data Input and Management Logic ---
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    file_ext <- tools::file_ext(input$file_upload$name)
    df <- NULL
    tryCatch({
      if (file_ext == "csv") {
        df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      } else if (file_ext == "xlsx") {
        df <- readxl::read_excel(input$file_upload$datapath)
      } else {
        showNotification("Unsupported file type. Please upload a .csv or .xlsx file.", type = "error")
      }
      if (!is.null(df)) {
        data_r(df)
        showNotification("Dataset uploaded successfully!", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_sample_data, {
    showModal(modalDialog(
      title = "Select a Sample Dataset",
      selectInput("sample_data_choice", "Choose a dataset:",
                  choices = c("Cars (mtcars)", "Flowers (iris)", "Student Exam Scores")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_selected_sample_data", "Load")
      )
    ))
  })
  
  observeEvent(input$load_selected_sample_data, {
    df_to_load <- switch(input$sample_data_choice,
                         "Cars (mtcars)" = mtcars,
                         "Flowers (iris)" = iris,
                         "Student Exam Scores" = exam_scores)
    data_r(df_to_load)
    removeModal()
    showNotification(paste(input$sample_data_choice, "dataset loaded."), type = "message")
  })
  
  observeEvent(input$clear_manual_data, {
    data_r(NULL)
    showNotification("All data cleared.", type = "message")
  })
  
  observeEvent(input$open_manual_data_modal, {
    if (is.null(data_r())) {
      modal_data_r(data.frame(Variable1 = rep(NA, 5), Variable2 = rep(NA, 5)))
    } else {
      modal_data_r(data_r())
    }
    
    showModal(modalDialog(
      title = "Manual Data Entry",
      size = "l",
      fluidPage(
        h4("Edit your data below:"),
        checkboxInput("mobile_edit_mode", "Enable Mobile Edit Mode", value = FALSE),
        uiOutput("manual_editor_ui"),
        helpText("Use the toggle above for mobile typing mode. Spreadsheet mode allows adding/removing columns and rows.")
      ),
      footer = tagList(
        actionButton("add_row", "Add Row", class = "btn-primary"),
        actionButton("remove_row", "Remove Last Row", class = "btn-danger"),
        actionButton("add_column", "Add Column", class = "btn-primary"),
        actionButton("remove_column", "Remove Last Column", class = "btn-danger"),
        actionButton("modal_save_data", "Save Changes", class = "btn-success"),
        modalButton("Cancel")
      )
    ))
  })
  
  output$manual_editor_ui <- renderUI({
    if (isTRUE(input$mobile_edit_mode)) {
      df <- modal_data_r()
      if (is.null(df)) return(NULL)
      
      tagList(
        lapply(1:nrow(df), function(row_idx) {
          fluidRow(
            column(12, strong(paste("Row", row_idx))),
            lapply(seq_along(df), function(col_idx) {
              column(6,
                     textInput(
                       inputId = paste0("cell_", row_idx, "_", col_idx),
                       label = names(df)[col_idx],
                       value = ifelse(is.na(df[row_idx, col_idx]), "", as.character(df[row_idx, col_idx]))
                     )
              )
            })
          )
        })
      )
    } else {
      rHandsontableOutput("modal_spreadsheet")
    }
  })
  
  output$modal_spreadsheet <- renderRHandsontable({
    req(modal_data_r())
    df <- modal_data_r()
    rh <- rhandsontable(df, readOnly = FALSE, useTypes = FALSE)
    for (col in names(df)) {
      rh <- hot_col(rh, col, type = "text")
    }
    rh %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
  })
  
  observeEvent(input$add_column, {
    df <- modal_data_r()
    existing_names <- names(df)
    new_col_name <- paste0("Column", ncol(df) + 1)
    while (new_col_name %in% existing_names) {
      new_col_name <- paste0(new_col_name, "_new")
    }
    df[[new_col_name]] <- NA
    modal_data_r(df)
  })
  
  observeEvent(input$remove_column, {
    df <- modal_data_r()
    if (ncol(df) > 1) {
      df <- df[, -ncol(df), drop = FALSE]
      modal_data_r(df)
    } else {
      showNotification("At least one column must remain.", type = "warning")
    }
  })
  
  observeEvent(input$add_row, {
    df <- modal_data_r()
    new_row <- as.data.frame(as.list(rep(NA, ncol(df))))
    names(new_row) <- names(df)
    df <- rbind(df, new_row)
    modal_data_r(df)
  })
  
  observeEvent(input$remove_row, {
    df <- modal_data_r()
    if (nrow(df) > 1) {
      df <- df[-nrow(df), , drop = FALSE]
      modal_data_r(df)
    } else {
      showNotification("At least one row must remain.", type = "warning")
    }
  })
  
  observeEvent(input$modal_spreadsheet, {
    if (!is.null(input$modal_spreadsheet)) {
      modal_data_r(hot_to_r(input$modal_spreadsheet))
    }
  })
  
  observeEvent(input$modal_save_data, {
    df_to_save <- NULL
    if (isTRUE(input$mobile_edit_mode)) {
      df_temp <- modal_data_r()
      for (row_idx in 1:nrow(df_temp)) {
        for (col_idx in 1:ncol(df_temp)) {
          value <- input[[paste0("cell_", row_idx, "_", col_idx)]]
          if (!is.null(value)) df_temp[row_idx, col_idx] <- value
        }
      }
      df_to_save <- df_temp
    } else {
      if (!is.null(input$modal_spreadsheet)) {
        df_to_save <- hot_to_r(input$modal_spreadsheet)
      }
    }
    if (!is.null(df_to_save)) {
      df_converted <- df_to_save %>%
        mutate(across(everything(), ~ type.convert(.x, as.is = TRUE)))
      data_r(df_converted)
      removeModal()
      showNotification("Data saved successfully!", type = "message")
    } else {
      removeModal()
      showNotification("No data to save.", type = "warning")
    }
  })
  
  output$data_preview_table <- renderDT({
    df <- data_r()
    if (is.null(df)) {
      return(datatable(data.frame(Message = "No data loaded. Please upload a file or enter data manually."),
                       options = list(dom = 't')))
    }
    datatable(df, editable = TRUE, options = list(pageLength = 10))
  })
  
  observeEvent(input$data_preview_table_cell_edit, {
    info <- input$data_preview_table_cell_edit
    df <- data_r()
    if (!is.null(df)) {
      df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
      data_r(df)
      showNotification(paste("Cell [", info$row, ",", info$col, "] updated."), type = "message")
    }
  })
  
  # --- UI Dropdown Generation (for non-inferential tabs) ---
  observe({
    df <- data_r()
    if (!is.null(df)) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)
      
      # Descriptive
      output$select_descriptive_variable <- renderUI({ selectInput("descriptive_variable", "Select Variable for Descriptive Stats", choices = c("", all_cols)) })
      output$select_group_by_variable <- renderUI({ selectInput("group_by_variable", "Group By (Optional)", choices = c("None", all_cols)) })
      output$select_scatter_x <- renderUI({ selectInput("scatter_x", "Select X-axis Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_scatter_y <- renderUI({ selectInput("scatter_y", "Select Y-axis Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_dot_plot_variable <- renderUI({ selectInput("dot_plot_variable", "Select Variable for Dot Plot", choices = c("", numeric_cols)) })
      
      # Regression & Correlation
      output$select_regression_dv <- renderUI({ selectInput("regression_dv", "Dependent Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_regression_iv <- renderUI({ selectInput("regression_iv", "Independent Variable(s) (Numeric)", choices = numeric_cols, multiple = TRUE) })
      output$select_correlation_vars <- renderUI({ selectInput("correlation_vars", "Select Variables for Correlation (Numeric)", choices = numeric_cols, multiple = TRUE) })
    } else {
      # Clear UI when no data is loaded
      output$select_descriptive_variable <- renderUI({ selectInput("descriptive_variable", "Select Variable", choices = "") })
      output$select_group_by_variable <- renderUI({ selectInput("group_by_variable", "Group By (Optional)", choices = "") })
      output$select_scatter_x <- renderUI({ selectInput("scatter_x", "Select X-axis Variable", choices = "") })
      output$select_scatter_y <- renderUI({ selectInput("scatter_y", "Select Y-axis Variable", choices = "") })
      output$select_dot_plot_variable <- renderUI({ selectInput("dot_plot_variable", "Select Variable", choices = "") })
      output$select_regression_dv <- renderUI({ selectInput("regression_dv", "Dependent Variable", choices = "") })
      output$select_regression_iv <- renderUI({ selectInput("regression_iv", "Independent Variable(s)", choices = "") })
      output$select_correlation_vars <- renderUI({ selectInput("correlation_vars", "Select Variables", choices = "") })
    }
  })
  
  # --- INFERENTIAL UI DROPDOWN GENERATION ---
  observe({
    df <- data_r()
    numeric_cols <- if (is.null(df)) "" else names(df)[sapply(df, is.numeric)]
    char_factor_cols <- if (is.null(df)) "" else names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    
    # Mean Tests Panel
    output$select_ht_variable <- renderUI({ selectInput("ht_variable", "Select Variable for t-test (Numeric)", choices = c("", numeric_cols)) })
    output$select_ht_group_variable <- renderUI({ selectInput("ht_group_variable", "Grouping Variable (for Two-Sample Test)", choices = c("None", char_factor_cols)) })
    output$paired_var1_ui <- renderUI({ selectInput("paired_var1", "Select First Variable (Numeric):", choices = c("", numeric_cols)) })
    output$paired_var2_ui <- renderUI({ selectInput("paired_var2", "Select Second Variable (Numeric):", choices = c("", numeric_cols)) })
    output$select_anova_dv <- renderUI({ selectInput("anova_dv", "Dependent Variable (Numeric)", choices = c("", numeric_cols)) })
    output$select_anova_iv <- renderUI({ selectInput("anova_iv", "Independent Variable (Categorical)", choices = c("", char_factor_cols)) })
    
    # Categorical & Normality Panels
    output$select_chi_x <- renderUI({ selectInput("chi_x", "Row Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_chi_y <- renderUI({ selectInput("chi_y", "Column Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_normality_var <- renderUI({ selectInput("normality_var", "Select Variable for Normality Check (Numeric)", choices = c("", numeric_cols)) })
    
    # Proportion Test Panel
    output$prop_variable_ui <- renderUI({ selectInput("prop_variable", "Select a categorical variable:", choices = c("", char_factor_cols)) })
    output$two_prop_var_ui <- renderUI({ selectInput("prop_var", "Select Proportion Variable:", choices = c("", char_factor_cols)) })
    output$two_prop_group_var_ui <- renderUI({ selectInput("two_prop_group_var", "Select Grouping Variable:", choices = c("", char_factor_cols)) })
  })
  
  # Observer for One-Proportion Test's 'Success Value' dropdown
  observe({
    df <- data_r()
    req(df, input$prop_variable)
    vals <- unique(na.omit(df[[input$prop_variable]]))
    output$success_value_ui <- renderUI({
      selectInput("success_value", "Success Value:", choices = vals, selected = vals[1])
    })
  })
  
  # Observer for Two-Proportion Test's dependent dropdowns
  observe({
    df <- data_r()
    req(df, input$prop_var, input$two_prop_group_var)
    
    success_vals <- unique(na.omit(df[[input$prop_var]]))
    output$two_prop_success_ui <- renderUI({
      selectInput("two_prop_success", "Select Success Value:", choices = success_vals, selected = success_vals[1])
    })
    
    group_vals <- unique(na.omit(df[[input$two_prop_group_var]]))
    output$two_prop_group1_ui <- renderUI({
      selectInput("two_prop_group1", "Value for Group 1:", choices = group_vals, selected = group_vals[1])
    })
    
    output$two_prop_group2_ui <- renderUI({
      req(input$two_prop_group1) 
      remaining_vals <- setdiff(group_vals, input$two_prop_group1)
      selectInput("two_prop_group2", "Value for Group 2:", choices = remaining_vals, selected = if(length(remaining_vals)>0) remaining_vals[1] else NULL)
    })
  })
  
  
  # --- Descriptive Statistics Logic ---
  
  observeEvent(input$analyze_descriptive, {
    df <- data_r()
    
    output$summary_stats_output <- renderPrint({
      req(df, input$descriptive_variable)
      var_name <- input$descriptive_variable
      group_var <- input$group_by_variable
      
      if (!(var_name %in% names(df))) {
        cat("Please select a valid variable for descriptive statistics.\n")
        return()
      }
      
      if (is.numeric(df[[var_name]])) {
        if (group_var != "None" && group_var %in% names(df)) {
          grouped_summary <- df %>%
            group_by(.data[[group_var]]) %>%
            summarise(
              N = sum(!is.na(.data[[var_name]])),
              Mean = mean(.data[[var_name]], na.rm = TRUE),
              Median = median(.data[[var_name]], na.rm = TRUE),
              SD = sd(.data[[var_name]], na.rm = TRUE),
              Min = min(.data[[var_name]], na.rm = TRUE),
              Q1 = quantile(.data[[var_name]], 0.25, na.rm = TRUE),
              Q3 = quantile(.data[[var_name]], 0.75, na.rm = TRUE),
              Max = max(.data[[var_name]], na.rm = TRUE),
              .groups = "drop"
            )
          print(grouped_summary)
        } else {
          summary_df <- data.frame(
            N = length(na.omit(df[[var_name]])),
            Mean = mean(df[[var_name]], na.rm = TRUE),
            Median = median(df[[var_name]], na.rm = TRUE),
            SD = sd(df[[var_name]], na.rm = TRUE),
            Min = min(df[[var_name]], na.rm = TRUE),
            Q1 = quantile(df[[var_name]], 0.25, na.rm = TRUE),
            Q3 = quantile(df[[var_name]], 0.75, na.rm = TRUE),
            Max = max(df[[var_name]], na.rm = TRUE)
          )
          print(summary_df)
        }
      } else {
        if (group_var != "None" && group_var %in% names(df)) {
          grouped_counts <- df %>%
            filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]])) %>%
            mutate(across(all_of(c(var_name, group_var)), as.character)) %>%
            group_by(.data[[group_var]], .data[[var_name]]) %>%
            summarise(Count = n(), .groups = "drop_last") %>%
            mutate(Percentage = round(100 * Count / sum(Count), 2)) %>%
            ungroup()
          print(grouped_counts)
        } else {
          counts <- table(df[[var_name]])
          percentages <- round(100 * prop.table(counts), 2)
          summary_table <- data.frame(
            Category = names(counts),
            Count = as.numeric(counts),
            Percentage = paste0(percentages, "%"),
            stringsAsFactors = FALSE
          )
          print(summary_table)
        }
      }
    })
    
    output$histogram_plot <- renderPlot({
      req(df, input$descriptive_variable)
      var <- input$descriptive_variable
      group_var <- input$group_by_variable
      validate(need(is.numeric(df[[var]]), "Histogram requires a numeric variable."))
      
      gg <- ggplot(df, aes(x = .data[[var]]))
      
      if (group_var != "None" && group_var %in% names(df)) {
        gg <- gg +
          geom_histogram(fill = "steelblue", bins = ifelse(!is.null(input$hist_bins), input$hist_bins, 20)) +
          facet_wrap(vars(.data[[group_var]]), scales = "free_y")
      } else {
        gg <- gg +
          geom_histogram(fill = "steelblue", bins = ifelse(!is.null(input$hist_bins), input$hist_bins, 20))
      }
      
      gg <- gg + labs(title = paste("Histogram of", var), x = var, y = "Count")
      
      if (!is.null(input$show_mean_median) && input$show_mean_median) {
        gg <- gg +
          geom_vline(aes(xintercept = mean(df[[var]], na.rm = TRUE)), color = "red", linetype = "dashed") +
          geom_vline(aes(xintercept = median(df[[var]], na.rm = TRUE)), color = "green", linetype = "dashed")
      }
      gg
    })
    
    output$boxplot_plot <- renderPlot({
      req(df, input$descriptive_variable)
      var_name <- input$descriptive_variable
      group_var <- input$group_by_variable
      if (is.numeric(df[[var_name]])) {
        if (group_var != "None" && group_var %in% names(df)) {
          ggplot(df, aes(x = .data[[group_var]], y = .data[[var_name]], fill = .data[[group_var]])) +
            geom_boxplot() +
            labs(title = paste("Boxplot of", var_name, "by", group_var), x = group_var, y = var_name) +
            theme(legend.position = "none")
        } else {
          ggplot(df, aes(y = .data[[var_name]])) +
            geom_boxplot(fill = "lightgreen") +
            labs(title = paste("Boxplot of", var_name), y = var_name)
        }
      }
    })
    
    output$density_plot <- renderPlot({
      req(df, input$descriptive_variable)
      var <- input$descriptive_variable
      group_var <- input$group_by_variable
      validate(need(is.numeric(df[[var]]), "Density plot requires a numeric variable."))
      
      gg <- ggplot(df, aes(x = .data[[var]]))
      
      if (group_var != "None" && group_var %in% names(df)) {
        gg <- gg +
          geom_density(fill = "blue", alpha = 0.4) +
          facet_wrap(vars(.data[[group_var]]), scales = "free_y")
      } else {
        gg <- gg +
          geom_density(fill = "blue", alpha = 0.4)
      }
      
      gg + labs(title = paste("Density Plot of", var), x = var, y = "Density")
    })
    
    output$pie_chart_plot <- renderPlot({
      req(df, input$descriptive_variable)
      var_name <- input$descriptive_variable
      group_var <- input$group_by_variable
      validate(
        need(var_name %in% names(df), "Selected variable not found."),
        need(!is.numeric(df[[var_name]]), "Pie chart requires a categorical variable.")
      )
      
      if (group_var != "None" && group_var %in% names(df)) {
        df_summary <- df %>%
          filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]])) %>%
          group_by(.data[[group_var]], .data[[var_name]]) %>%
          summarise(Count = n(), .groups = "drop_last") %>%
          group_by(.data[[group_var]]) %>%
          mutate(
            Percentage = Count / sum(Count),
            Label = paste0(round(Percentage * 100, 1), "%")
          )
        ggplot(df_summary, aes(x = "", y = Percentage, fill = .data[[var_name]])) +
          geom_col(width = 1) +
          coord_polar(theta = "y") +
          facet_wrap(vars(.data[[group_var]])) +
          geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
          theme_void() +
          labs(title = paste("Pie Chart of", var_name, "by", group_var))
      } else {
        df_summary <- df %>%
          filter(!is.na(.data[[var_name]])) %>%
          mutate(across(all_of(var_name), as.character)) %>%
          group_by(.data[[var_name]]) %>%
          summarise(Count = n(), .groups = "drop") %>%
          mutate(
            Percentage = Count / sum(Count),
            Label = paste0(round(Percentage * 100, 1), "%")
          )
        ggplot(df_summary, aes(x = "", y = Percentage, fill = .data[[var_name]])) +
          geom_col(width = 1) +
          coord_polar(theta = "y") +
          geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
          theme_void() +
          labs(title = paste("Pie Chart of", var_name))
      }
    })
  })
  
  observeEvent(input$generate_scatter, {
    df <- data_r()
    req(df, input$scatter_x, input$scatter_y)
    
    output$scatter_plot <- renderPlot({
      x_var <- input$scatter_x
      y_var <- input$scatter_y
      group_var <- input$group_by_variable
      
      validate(
        need(x_var %in% names(df) && y_var %in% names(df), "Selected variable(s) not found."),
        need(is.numeric(df[[x_var]]) && is.numeric(df[[y_var]]), "Scatter plots require numeric variables.")
      )
      
      gg <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]]))
      
      if (group_var != "None" && group_var %in% names(df)) {
        gg <- gg + geom_point(aes(color = .data[[group_var]]))
      } else {
        gg <- gg + geom_point(color = "darkblue")
      }
      
      gg + labs(title = paste("Scatter Plot of", y_var, "vs", x_var), x = x_var, y = y_var)
    })
  })
  
  observeEvent(input$generate_dot_plot, {
    df <- data_r()
    req(df, input$dot_plot_variable)
    
    var <- input$dot_plot_variable
    validate(need(is.numeric(df[[var]]), "Dot plot requires a numeric variable."))
    
    # --- START OF DEFINITIVE FIX ---
    
    data_vec <- na.omit(df[[var]])
    
    if (length(data_vec) < 2) {
      showNotification("Not enough data to generate a dot plot.", type = "warning")
      current_dot_plot(NULL) 
      return()
    }
    
    data_range <- max(data_vec) - min(data_vec)
    dynamic_binwidth <- if (data_range == 0) 1 else data_range / 30
    
    # Create the base plot object
    p_base <- ggplot(df, aes(x = .data[[var]])) +
      geom_dotplot(
        binaxis = 'x',
        stackdir = 'up',
        dotsize = 0.8,
        fill = "steelblue",
        binwidth = dynamic_binwidth
      )
    
    # Build the plot to find the max frequency for the y-axis
    built_p <- ggplot_build(p_base)
    max_freq <- max(built_p$data[[1]]$count)
    
    y_breaks <- if (max_freq > 1) {
      seq(from = 1, to = floor(max_freq), by = 1)
    } else {
      1
    }
    
    # Add the final layers for scales, labels, and an EXPLICIT theme override
    p_final <- p_base + 
      scale_y_continuous(
        breaks = y_breaks,
        limits = c(0, NA),
        expand = expansion(mult = c(0.02, 0.05))
      ) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 7)
      ) +
      labs(
        title = paste("Dot Plot of", var),
        x = var,
        y = "Frequency"
      ) +
      theme_light() + 
      
      # THIS IS THE CRUCIAL OVERRIDE
      # We are explicitly forcing the axis text to be drawn with a visible color and size.
      # This will override any settings from thematic_shiny().
      theme(
        axis.text.x = element_text(color = "gray30", size = 10, angle = 0), # Use a dark gray, size 10, no angle
        axis.text.y = element_text(color = "gray30", size = 10), # Also force the y-axis text
        
        # Also ensure axis titles are visible
        axis.title = element_text(color = "gray30", size = 11),
        
        panel.grid.minor = element_blank()
      )
    
    # --- END OF DEFINITIVE FIX ---
    
    
    current_dot_plot(p_final) 
  })
  
  
  output$dot_plot <- renderPlot({
    req(current_dot_plot())
    current_dot_plot()
  })
  
  output$download_dot_plot <- downloadHandler(
    filename = function() {
      paste("dot-plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(current_dot_plot())
      ggsave(
        filename = file,
        plot = current_dot_plot(),
        width = 8,
        height = 6,
        dpi = 300,
        units = "in"
      )
    }
  )
  
  # --- All Inferential and Regression Logic ---
  
  observeEvent(input$run_anova, {
    df <- data_r()
    req(df, input$anova_dv, input$anova_iv)
    dv <- input$anova_dv
    iv <- input$anova_iv
    
    if (!is.numeric(df[[dv]])) {
      showNotification("Dependent variable for ANOVA must be numeric.", type = "warning")
      return(NULL)
    }
    if (!is.character(df[[iv]]) && !is.factor(df[[iv]])) {
      showNotification("Independent variable for ANOVA must be categorical.", type = "warning")
      return(NULL)
    }
    df[[iv]] <- as.factor(df[[iv]])
    
    output$anova_output <- renderPrint({
      formula_str <- paste(dv, "~", iv)
      model <- aov(as.formula(formula_str), data = df)
      cat("ANOVA Summary:\n")
      print(summary(model))
      cat("\nPost-Hoc Test (Tukey HSD) if significant:\n")
      if (summary(model)[[1]]$`Pr(>F)`[1] < 0.05) {
        print(TukeyHSD(model))
      } else {
        cat("No significant differences found, no post-hoc test performed.\n")
      }
    })
  })
  
  observeEvent(input$run_prop_test, {
    # This logic now handles both manual and data-driven modes
    
    if (isTRUE(input$prop_test_manual_mode)) {
      # --- MANUAL MODE ---
      req(input$prop_manual_successes, input$prop_manual_trials, input$prop_null)
      
      successes <- input$prop_manual_successes
      total <- input$prop_manual_trials
      
      # Add validation for manual inputs
      if (successes > total) {
        showNotification("Error: Number of successes cannot be greater than the number of trials.", type = "error")
        return()
      }
      
      test <- prop.test(x = successes, n = total, p = input$prop_null, alternative = input$prop_alternative)
      
      output$prop_test_result <- renderPrint({
        cat("One-Proportion Test (Manual Input)\n\n")
        cat("Successes (x):", successes, "\n")
        cat("Trials (n):", total, "\n\n")
        print(test)
      })
      
    } else {
      # --- DATA-DRIVEN MODE (Original Logic) ---
      df <- data_r()
      req(df, input$prop_variable, input$success_value, input$prop_null)
      
      var <- input$prop_variable
      success_val <- input$success_value
      
      successes <- sum(df[[var]] == success_val, na.rm = TRUE)
      total <- sum(!is.na(df[[var]]))
      
      if (total == 0) {
        output$prop_test_result <- renderPrint({
          cat("Error: No valid data for this variable.")
        })
        return()
      }
      
      test <- prop.test(x = successes, n = total, p = input$prop_null, alternative = input$prop_alternative)
      
      output$prop_test_result <- renderPrint({
        cat("One-Proportion Test (from Dataset)\n")
        cat("Variable:", var, "\n")
        cat("Success Value:", success_val, "\n")
        print(test)
      })
    }
  })
  
  observeEvent(input$run_two_prop_test, {
    df <- data_r()
    req(df, input$prop_var, input$two_prop_group_var, input$two_prop_group1, input$two_prop_group2, input$two_prop_success)
    
    prop_var <- input$prop_var
    group_var <- input$two_prop_group_var
    group1 <- input$two_prop_group1
    group2 <- input$two_prop_group2
    success_val <- input$two_prop_success
    
    if (!is.character(df[[prop_var]]) && !is.factor(df[[prop_var]])) {
      output$two_prop_test_result <- renderPrint({"Proportion variable must be categorical."})
      return(NULL)
    }
    if (!is.character(df[[group_var]]) && !is.factor(df[[group_var]])) {
      output$two_prop_test_result <- renderPrint({"Grouping variable must be categorical."})
      return(NULL)
    }
    
    df[[prop_var]] <- as.factor(df[[prop_var]])
    df[[group_var]] <- as.factor(df[[group_var]])
    
    df_filtered <- df %>% filter(.data[[group_var]] %in% c(group1, group2))
    if (nrow(df_filtered) == 0) {
      output$two_prop_test_result <- renderPrint({"No data found for selected groups."})
      return(NULL)
    }
    
    group1_data <- df_filtered %>% filter(.data[[group_var]] == group1)
    group2_data <- df_filtered %>% filter(.data[[group_var]] == group2)
    
    n1 <- nrow(group1_data)
    n2 <- nrow(group2_data)
    x1 <- sum(group1_data[[prop_var]] == success_val, na.rm = TRUE)
    x2 <- sum(group2_data[[prop_var]] == success_val, na.rm = TRUE)
    
    if (n1 == 0 || n2 == 0) {
      output$two_prop_test_result <- renderPrint({"One or both groups have no data."})
      return(NULL)
    }
    
    output$two_prop_test_result <- renderPrint({
      cat("Two-Proportion Test\n\n")
      cat("Group 1:", group1, "| Successes:", x1, "/", n1, "\n")
      cat("Group 2:", group2, "| Successes:", x2, "/", n2, "\n\n")
      print(prop.test(x = c(x1, x2), n = c(n1, n2), alternative = input$two_prop_alternative))
    })
  })
  
  observeEvent(input$run_ht, {
    req(data_r(), input$ht_variable)
    
    df <- data_r()
    var_name <- input$ht_variable
    group_var <- input$ht_group_variable
    mu <- input$ht_mu
    
    if (!(var_name %in% names(df))) {
      showNotification("Selected variable not found in dataset.", type = "error")
      return(NULL)
    }
    
    if (is.character(df[[var_name]]) || is.logical(df[[var_name]]) || is.factor(df[[var_name]])) {
      vals <- na.omit(df[[var_name]])
      if (length(vals) > 0 && all(vals %in% c("Yes", "No"))) {
        df[[var_name]] <- ifelse(df[[var_name]] == "Yes", 1, 0)
      } else if (length(vals) > 0 && all(vals %in% c("TRUE", "FALSE", TRUE, FALSE))) {
        df[[var_name]] <- ifelse(df[[var_name]] %in% c("TRUE", TRUE), 1, 0)
      }
    }
    
    if (!is.null(group_var) && group_var != "None" && group_var %in% names(df)) {
      if (is.character(df[[group_var]]) || is.logical(df[[group_var]]) || is.factor(df[[group_var]])) {
        vals <- na.omit(df[[group_var]])
        if (length(vals) > 0 && all(vals %in% c("Yes", "No"))) {
          df[[group_var]] <- factor(ifelse(df[[group_var]] == "Yes", "Yes", "No"))
        } else if (length(vals) > 0 && all(vals %in% c("TRUE", "FALSE", TRUE, FALSE))) {
          df[[group_var]] <- factor(ifelse(df[[group_var]] %in% c("TRUE", TRUE), "TRUE", "FALSE"))
        }
      }
    }
    
    if (!is.numeric(df[[var_name]])) {
      showNotification("Variable for t-test must be numeric or convertible (Yes/No, True/False).", type = "warning")
      return(NULL)
    }
    
    output$ht_output <- renderPrint({
      if (!is.null(group_var) && group_var != "None" && group_var %in% names(df)) {
        df_filtered <- df %>% filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]]))
        df_filtered[[group_var]] <- as.factor(df_filtered[[group_var]])
        
        if (nlevels(df_filtered[[group_var]]) != 2) {
          cat("Error: Grouping variable must have exactly two levels for two-sample t-test.\n")
          return()
        }
        
        cat("Two-Sample t-test\n-----------------\n")
        print(t.test(as.formula(paste(var_name, "~", group_var)),
                     data = df_filtered,
                     alternative = input$ht_alternative,
                     var.equal = input$ht_var_equal))
        
      } else {
        cat("One-Sample t-test\n-----------------\n")
        print(t.test(df[[var_name]], mu = mu, alternative = input$ht_alternative))
      }
    })
  })
  
  observeEvent(input$run_paired_ttest, {
    df <- data_r()
    req(df, input$paired_var1, input$paired_var2)
    
    var1 <- input$paired_var1
    var2 <- input$paired_var2
    
    if (var1 == var2) {
      output$paired_ttest_result <- renderPrint({"Error: Please select two different variables."})
      return(NULL)
    }
    
    if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) {
      output$paired_ttest_result <- renderPrint({"Both variables must be numeric."})
      return(NULL)
    }
    
    df_clean <- df %>% select(all_of(c(var1, var2))) %>% na.omit()
    
    if (nrow(df_clean) < 2) {
      output$paired_ttest_result <- renderPrint({"Not enough data for paired t-test."})
      return(NULL)
    }
    
    test <- t.test(df_clean[[var1]], df_clean[[var2]], paired = TRUE, alternative = input$paired_alternative)
    
    output$paired_ttest_result <- renderPrint({
      cat("Paired t-test\n\n")
      print(test)
    })
  })
  
  observeEvent(input$run_chi_sq, {
    df <- data_r()
    req(df, input$chi_x, input$chi_y)
    x_var <- input$chi_x
    y_var <- input$chi_y
    
    if (!is.character(df[[x_var]]) && !is.factor(df[[x_var]])) {
      showNotification("Row variable for Chi-Squared must be categorical.", type = "warning")
      return(NULL)
    }
    if (!is.character(df[[y_var]]) && !is.factor(df[[y_var]])) {
      showNotification("Column variable for Chi-Squared must be categorical.", type = "warning")
      return(NULL)
    }
    
    output$chi_sq_output <- renderPrint({
      df_filtered <- df %>%
        filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
      
      if (nrow(df_filtered) == 0) {
        cat("No valid data pairs found for the selected variables.\n")
        return()
      }
      
      contingency_table <- table(df_filtered[[x_var]], df_filtered[[y_var]])
      
      cat("Contingency Table (Observed Counts):\n")
      print(contingency_table)
      cat("\n----------------------------------------\n")
      
      if (nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
        cat("The contingency table must have at least 2 rows and 2 columns to perform the test.\n")
        return()
      }
      
      if (input$use_fisher_exact) {
        cat("Fisher's Exact Test for Count Data:\n\n")
        tryCatch({
          test_result <- fisher.test(contingency_table)
          print(test_result)
        }, error = function(e) {
          cat("Fisher's Exact Test could not be performed. Error:\n", e$message)
        })
        
      } else {
        test_result <- suppressWarnings(chisq.test(contingency_table))
        
        cat("Chi-Squared Test Result:\n\n")
        print(test_result)
        cat("\nExpected Counts:\n")
        print(round(test_result$expected, 2))
        
        if (any(test_result$expected < 5)) {
          cat("\n----------------------------------------\n")
          cat("WARNING: Chi-squared approximation may be incorrect because some expected counts are less than 5.\n")
          cat("Consider using Fisher's Exact Test by checking the box above.\n")
        }
      }
    })
  })
  
  observeEvent(input$check_normality, {
    df <- data_r()
    req(df, input$normality_var)
    var_name <- input$normality_var
    
    if (!is.numeric(df[[var_name]])) {
      showNotification("Normality check requires a numeric variable.", type = "warning")
      return(NULL)
    }
    
    output$normality_plot <- renderPlot({
      ggplot(df, aes_string(sample = var_name)) +
        stat_qq() +
        stat_qq_line() +
        labs(title = paste("Q-Q Plot of", var_name),
             x = "Theoretical Quantiles", y = "Sample Quantiles")
    })
    
    output$shapiro_wilk_output <- renderPrint({
      data_for_test <- na.omit(df[[var_name]])
      if (length(data_for_test) < 3 || length(data_for_test) > 5000) {
        cat("Shapiro-Wilk test requires between 3 and 5000 data points.\n")
        if (length(data_for_test) < 30) {
          cat("For n < 30, visual inspection of Q-Q plot is more critical.\n")
        }
        return(NULL)
      }
      cat("Shapiro-Wilk Normality Test:\n")
      print(shapiro.test(data_for_test))
    })
  })
  
  observeEvent(input$run_regression, {
    df <- data_r()
    req(df, input$regression_dv, input$regression_iv)
    
    dv <- input$regression_dv
    ivs <- input$regression_iv
    
    if (!is.numeric(df[[dv]])) {
      showNotification("Dependent variable for regression must be numeric.", type = "warning")
      return(NULL)
    }
    if (length(ivs) == 0) {
      showNotification("Please select at least one independent variable for regression.", type = "warning")
      return(NULL)
    }
    if (!all(sapply(df[ivs], is.numeric))) {
      showNotification("All independent variables for regression must be numeric.", type = "warning")
      return(NULL)
    }
    
    output$regression_summary <- renderPrint({
      formula_str <- paste(dv, "~", paste(ivs, collapse = " + "))
      model <- lm(as.formula(formula_str), data = df)
      model_summary <- summary(model)
      
      r_squared <- model_summary$r.squared
      r_value <- sqrt(r_squared)
      
      cat("Linear Regression Summary:\n")
      print(model_summary)
      cat("\n")
      cat("R-squared:", round(r_squared, 4), "\n")
      cat("R (correlation):", round(r_value, 4), "\n")
    })
  })
  
  observeEvent(input$run_correlation, {
    df <- data_r()
    req(input$correlation_vars)
    if (length(input$correlation_vars) < 2) {
      showNotification("Please select at least two variables for correlation.", type = "warning")
      return(NULL)
    }
    corr_data <- df[, input$correlation_vars, drop = FALSE]
    corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")
    output$correlation_matrix <- renderPrint({
      cat("Correlation Matrix:\n")
      print(round(corr_matrix, 4))
    })
  })
  
  # --- PROBABILITY LOGIC ---
  
  observeEvent(input$calculate_probabilities, {
    P_A <- input$prob_A
    P_B <- input$prob_B
    P_A_and_B <- input$prob_A_and_B
    if (P_A < 0 || P_A > 1 || P_B < 0 || P_B > 1 || P_A_and_B < 0 || P_A_and_B > 1) {
      showNotification("Probabilities must be between 0 and 1.", type = "error")
      output$calculated_probs <- renderPrint({ cat("Invalid input. Probabilities must be between 0 and 1.\n") })
      output$conditional_probs <- renderPrint({ cat("Invalid input.\n") })
      output$event_relationships <- renderPrint({ cat("Invalid input.\n") })
      return()
    }
    if (P_A_and_B > P_A || P_A_and_B > P_B) {
      showNotification("P(A and B) cannot be greater than P(A) or P(B).", type = "error")
      output$calculated_probs <- renderPrint({ cat("Invalid input: P(A and B) cannot be greater than P(A) or P(B).\n") })
      output$conditional_probs <- renderPrint({ cat("Invalid input.\n") })
      output$event_relationships <- renderPrint({ cat("Invalid input.\n") })
      return()
    }
    P_A_or_B <- P_A + P_B - P_A_and_B
    P_A_complement <- 1 - P_A
    P_B_complement <- 1 - P_B
    output$calculated_probs <- renderPrint({
      cat(paste0("P(A) = ", round(P_A, 4), "\n"))
      cat(paste0("P(B) = ", round(P_B, 4), "\n"))
      cat(paste0("P(A and B) = ", round(P_A_and_B, 4), "\n"))
      cat(paste0("P(A or B) = ", round(P_A_or_B, 4), "\n"))
      cat(paste0("P(A') = ", round(P_A_complement, 4), "\n"))
      cat(paste0("P(B') = ", round(P_B_complement, 4), "\n"))
    })
    P_A_given_B <- ifelse(P_B > 0, P_A_and_B / P_B, NA)
    P_B_given_A <- ifelse(P_A > 0, P_A_and_B / P_A, NA)
    output$conditional_probs <- renderPrint({
      if (is.na(P_A_given_B)) {
        cat("P(A | B): Cannot be calculated (P(B) is 0).\n")
      } else {
        cat(paste0("P(A | B) = ", round(P_A_given_B, 4), "\n"))
      }
      if (is.na(P_B_given_A)) {
        cat("P(B | A): Cannot be calculated (P(A) is 0).\n")
      } else {
        cat(paste0("P(B | A) = ", round(P_B_given_A, 4), "\n"))
      }
    })
    is_independent <- abs(P_A_and_B - (P_A * P_B)) < 1e-9
    is_mutually_exclusive <- P_A_and_B == 0
    output$event_relationships <- renderPrint({
      if (is_independent) {
        cat("Events A and B are Independent.\n")
      } else {
        cat("Events A and B are Dependent.\n")
      }
      if (is_mutually_exclusive) {
        cat("Events A and B are Mutually Exclusive.\n")
      } else {
        cat("Events A and B are Not Mutually Exclusive.\n")
      }
    })
  })
  
  output$normal_inputs <- renderUI({
    req(input$normal_prob_type)
    prob_type <- input$normal_prob_type
    
    if (prob_type == "inverse") {
      numericInput("normal_p", "Cumulative Probability P(X < x):", value = 0.95, min = 0, max = 1, step = 0.01)
    } else if (prob_type %in% c("less", "greater")) {
      numericInput("normal_x", "X Value:", value = 1.96)
    } else if (prob_type == "between") {
      tagList(
        numericInput("normal_a", "Lower Bound (a):", value = -1.96),
        numericInput("normal_b", "Upper Bound (b):", value = 1.96)
      )
    }
  })
  
  observeEvent(input$calc_normal, {
    req(input$normal_mean, input$normal_sd, input$normal_prob_type)
    
    if (input$normal_sd <= 0) {
      showNotification("Standard deviation must be positive.", type = "error")
      output$normal_result <- renderPrint({ "Invalid input: Standard deviation must be positive." })
      return()
    }
    
    prob_type <- input$normal_prob_type
    
    result_text <- switch(
      prob_type,
      "less" = {
        req(input$normal_x)
        prob <- pnorm(input$normal_x, mean = input$normal_mean, sd = input$normal_sd)
        paste0("P(X < ", input$normal_x, ") = ", round(prob, 4))
      },
      "greater" = {
        req(input$normal_x)
        prob <- 1 - pnorm(input$normal_x, mean = input$normal_mean, sd = input$normal_sd)
        paste0("P(X > ", input$normal_x, ") = ", round(prob, 4))
      },
      "between" = {
        req(input$normal_a, input$normal_b)
        prob <- pnorm(input$normal_b, mean = input$normal_mean, sd = input$normal_sd) -
          pnorm(input$normal_a, mean = input$normal_mean, sd = input$normal_sd)
        paste0("P(", input$normal_a, " < X < ", input$normal_b, ") = ", round(prob, 4))
      },
      "inverse" = {
        req(input$normal_p)
        x_val <- qnorm(input$normal_p, mean = input$normal_mean, sd = input$normal_sd)
        paste0("The x-value for P(X < x) = ", input$normal_p, " is ", round(x_val, 4))
      }
    )
    
    output$normal_result <- renderPrint({ result_text })
    
    output$normal_plot <- renderPlot({
      mean_val <- input$normal_mean
      sd_val <- input$normal_sd
      x_vals <- seq(mean_val - 4 * sd_val, mean_val + 4 * sd_val, length.out = 500)
      df <- data.frame(x = x_vals, y = dnorm(x_vals, mean = mean_val, sd = sd_val))
      
      gg <- ggplot(df, aes(x, y)) +
        geom_line(color = "steelblue", linewidth = 1) +
        labs(
          title = paste("Normal Distribution (\u03bc =", mean_val, ", \u03c3 =", sd_val, ")"),
          x = "X", y = "Density"
        )
      
      if (prob_type == "less") {
        req(input$normal_x)
        gg <- gg + geom_area(data = subset(df, x <= input$normal_x), aes(y = y), fill = "lightblue", alpha = 0.5)
      } else if (prob_type == "greater") {
        req(input$normal_x)
        gg <- gg + geom_area(data = subset(df, x >= input$normal_x), aes(y = y), fill = "lightblue", alpha = 0.5)
      } else if (prob_type == "between") {
        req(input$normal_a, input$normal_b)
        gg <- gg + geom_area(data = subset(df, x >= input$normal_a & x <= input$normal_b), aes(y = y), fill = "lightblue", alpha = 0.5)
      } else if (prob_type == "inverse") {
        req(input$normal_p)
        x_val <- qnorm(input$normal_p, mean = mean_val, sd = sd_val)
        gg <- gg + geom_vline(xintercept = x_val, color = "red", linetype = "dashed", linewidth = 1) +
          geom_area(data = subset(df, x <= x_val), aes(y = y), fill = "lightblue", alpha = 0.5)
      }
      
      gg
    })
  })
  
  ## --- ADDED FOR BINOMIAL SUMMARY --- ##
  output$binom_summary_stats <- renderPrint({
    req(input$binom_size, input$binom_prob)
    n <- input$binom_size
    p <- input$binom_prob
    
    # Validate inputs
    if (n < 1 || p < 0 || p > 1) {
      cat("Invalid parameters: n must be >= 1 and p must be between 0 and 1.")
      return()
    }
    
    expected_value <- n * p
    variance <- n * p * (1 - p)
    std_dev <- sqrt(variance)
    
    cat(
      "Expected Value (E[X] = np): ", round(expected_value, 4), "\n",
      "Variance (\u03c3\u00b2 = np(1-p)): ", round(variance, 4), "\n",
      "Standard Deviation (\u03c3): ", round(std_dev, 4), sep = ""
    )
  })
  ## --- END ADDED --- ##
  
  observeEvent(input$calc_binom_prob, {
    req(input$binom_size, input$binom_prob, input$binom_k)
    if (input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1 || input$binom_k < 0) {
      showNotification("Invalid Binomial parameters.", type = "error")
      output$binom_prob_output <- renderPrint({ cat("Invalid input: Check n, p, and k values.\n") })
      return()
    }
    if (input$binom_k > input$binom_size) {
      showNotification("k cannot be greater than n for Binomial distribution.", type = "error")
      output$binom_prob_output <- renderPrint({ cat("Invalid input: k cannot be greater than n.\n") })
      return()
    }
    
    prob_val <- switch(input$binom_type,
                       "P(X = x)" = dbinom(input$binom_k, size = input$binom_size, prob = input$binom_prob),
                       "P(X <= x)" = pbinom(input$binom_k, size = input$binom_size, prob = input$binom_prob),
                       "P(X >= x)" = 1 - pbinom(input$binom_k - 1, size = input$binom_size, prob = input$binom_prob)
    )
    
    output$binom_prob_output <- renderPrint({
      cat(paste0(input$binom_type, " = ", round(prob_val, 4), "\n"))
    })
  })
  
  observeEvent(input$solve_binom_k, {
    req(input$binom_size, input$binom_prob, input$binom_p_for_k)
    if (input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1 || input$binom_p_for_k < 0 || input$binom_p_for_k > 1) {
      showNotification("Invalid Binomial parameters.", type = "error")
      output$solve_binom_k_output <- renderPrint({ cat("Invalid input: Check n, p, and probability values.\n") })
      return()
    }
    x_val <- qbinom(p = input$binom_p_for_k, size = input$binom_size, prob = input$binom_prob)
    actual_prob <- pbinom(x_val, size = input$binom_size, prob = input$binom_prob)
    output$solve_binom_k_output <- renderPrint({
      cat(paste0("To achieve a cumulative probability of at least ", input$binom_p_for_k, ",\n",
                 "you need ", x_val, " successes (x).\n\n",
                 "The actual probability at this point is P(X <= ", x_val, ") = ", round(actual_prob, 4)))
    })
  })
  
  output$binom_pmf_plot <- renderPlot({
    req(input$binom_size, input$binom_prob)
    if (input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1) return(NULL)
    x_vals <- 0:input$binom_size
    y_vals <- dbinom(x_vals, size = input$binom_size, prob = input$binom_prob)
    df_plot <- data.frame(k = x_vals, probability = y_vals)
    ggplot(df_plot, aes(x = k, y = probability)) +
      geom_bar(stat = "identity", fill = "seagreen") +
      labs(title = paste("Binomial Distribution (n=", input$binom_size, ", p=", input$binom_prob, ")"),
           x = "Number of Successes (x)", y = "Probability") +
      scale_x_continuous(breaks = x_vals)
  })
  ## --- ADDED FOR POISSON SUMMARY --- ##
  output$pois_summary_stats <- renderPrint({
    req(input$pois_lambda)
    lambda <- input$pois_lambda
    
    # Validate input
    if (lambda <= 0) {
      cat("Invalid parameter: Lambda (\u03bb) must be positive.")
      return()
    }
    
    expected_value <- lambda
    variance <- lambda
    std_dev <- sqrt(variance)
    
    cat(
      "Expected Value (E[X] = \u03bb): ", round(expected_value, 4), "\n",
      "Variance (\u03c3\u00b2 = \u03bb): ", round(variance, 4), "\n",
      "Standard Deviation (\u03c3): ", round(std_dev, 4), sep = ""
    )
  })
  ## --- END ADDED --- ##
  
  observeEvent(input$calc_pois_prob, {
    req(input$pois_lambda, input$pois_k)
    if (input$pois_lambda <= 0 || input$pois_k < 0) {
      showNotification("Invalid Poisson parameters. Lambda must be positive, k non-negative.", type = "error")
      return()
    }
    
    prob_val <- switch(input$pois_type,
                       "P(X = k)" = dpois(input$pois_k, lambda = input$pois_lambda),
                       "P(X <= k)" = ppois(input$pois_k, lambda = input$pois_lambda),
                       "P(X >= k)" = 1 - ppois(input$pois_k - 1, lambda = input$pois_lambda)
    )
    output$pois_prob_output <- renderPrint({
      cat(paste0(input$pois_type, " = ", round(prob_val, 4), "\n"))
    })
  })
  
  output$pois_pmf_plot <- renderPlot({
    req(input$pois_lambda)
    if (input$pois_lambda <= 0) return(NULL)
    
    max_k <- qpois(0.999, lambda = input$pois_lambda) + 5
    if (max_k < 10) max_k <- 10
    x_vals <- 0:max_k
    y_vals <- dpois(x_vals, lambda = input$pois_lambda)
    df_plot <- data.frame(k = x_vals, probability = y_vals)
    
    ggplot(df_plot, aes(x = k, y = probability)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      labs(title = paste("Poisson Distribution (\u03bb =", input$pois_lambda, ")"),
           x = "Number of Events (k)", y = "Probability") +
      scale_x_continuous(breaks = x_vals[x_vals %% 1 == 0])
  })
  
}