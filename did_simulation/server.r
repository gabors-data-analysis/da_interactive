# Server logic
function(input, output, session) {

  # Warning for year fixed effects
  observe({
    if (input$year_fe & input$timing == "once" & !input$global_trend & !input$individual_trend) {
      showModal(modalDialog(
        title = "Warning",
        "For uniformly timed single intervention and no global or individual trends, time dummies can not be added.",
        easyClose = TRUE
      ))
    }
  })

  # Validate event time window
  observe({
    if (input$min_event_time >= -1) {
      updateSliderInput(session, "min_event_time", value = -2)
      showModal(modalDialog(
        title = "Invalid Event Window",
        "Minimum event time must be less than -1 (the reference period)",
        easyClose = TRUE
      ))
    }
    if (input$max_event_time <= 1) {
      updateSliderInput(session, "max_event_time", value = 2)
      showModal(modalDialog(
        title = "Invalid Event Window",
        "Maximum event time must be greater than 1",
        easyClose = TRUE
      ))
    }
  })

  # Validate dynamic effect values
  observe({
    if(input$dynamic_effect) {
      dyn_values <- as.numeric(strsplit(input$dynamic_effect_values, ",")[[1]])

      if(length(dyn_values) != 3) {
        showModal(modalDialog(
          title = "Invalid Dynamic Effect Values",
          "Please provide exactly 3 comma-separated percentages for dynamic effect progression.",
          easyClose = TRUE
        ))
        updateTextInput(session, "dynamic_effect_values", value = "50,75,100")
      } else if(any(is.na(dyn_values)) || any(dyn_values < 0) || any(dyn_values > 100)) {
        showModal(modalDialog(
          title = "Invalid Dynamic Effect Values",
          "All percentages must be between 0 and 100.",
          easyClose = TRUE
        ))
        updateTextInput(session, "dynamic_effect_values", value = "50,75,100")
      } else if(dyn_values[1] > dyn_values[2] || dyn_values[2] > dyn_values[3]) {
        showModal(modalDialog(
          title = "Invalid Dynamic Effect Values",
          "Percentages should be in ascending order.",
          easyClose = TRUE
        ))
        updateTextInput(session, "dynamic_effect_values", value = "50,75,100")
      }
    }
  })

  # Validate years to reversal
  observe({
    if(input$reversal) {
      if(input$years_to_reversal < 1) {
        showModal(modalDialog(
          title = "Invalid Reversal Timing",
          "Years until reversal must be at least 1.",
          easyClose = TRUE
        ))
        updateNumericInput(session, "years_to_reversal", value = 3)
      }
    }
  })

  # Generate reactive dataset
  data <- reactive({
    generate_data(input)
  })

  # Reactive models — computed once, used by table + coef plot
  models <- reactive({
    run_models(data(), input)
  })

  # TWFE transformation reactive
  twfe_data <- reactive({
    run_twfe_transform(data())
  })

  # Event study transformation reactive
  event_study_data <- reactive({
    if(input$num_shocks == "1") {
      transform_event_study_data(
        data(),
        input$min_event_time,
        input$max_event_time,
        input$timing
      )
    } else {
      return(NULL)
    }
  })

  # Main plot output
  output$did_plot <- renderPlotly({
    p <- create_did_plot(data())
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # PanelView plot output
  output$panel_view <- renderPlotly({
    p <- create_panel_view(data())
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # Treatment explanation output
  output$treatment_explanation <- renderText({
    if(input$dynamic_effect && input$reversal) {
      paste("Treatment: Dynamic build-up over time, then reversal after",
            input$years_to_reversal, "years.")
    } else if(input$dynamic_effect) {
      dyn_values <- strsplit(input$dynamic_effect_values, ",")[[1]]
      paste("Treatment: Dynamic build-up over time. Year 1:",
            dyn_values[1], "%, Year 2:",
            dyn_values[2], "%, Year 3+:",
            dyn_values[3], "%")
    } else if(input$reversal) {
      paste("Treatment: Full effect immediately, then reversal after",
            input$years_to_reversal, "years.")
    } else {
      "Treatment: Full effect immediately after implementation."
    }
  })

  # ---- Pane 1: Panel models regression table (TWFE / FD) ----
  output$model_results_panel <- renderUI({
    m <- models()

    dictName <- c(
      "treatment" = "Average Effect",
      "treatment_diff" = "Contemporaneous Effect (FD)",
      "treatment_diff_lag3" = "Cumulative Effect (FD)",
      "(Intercept)" = "Constant"
    )

    # Drop the d2_treatment reparameterization nuisance coefficients
    # (they are running cumulative effects at each horizon, not incremental — confusing to display)
    # Use % prefix to match original variable names regardless of dict renaming
    drop_vars <- c("Intercept", "%d2_treatment", "%d2_treatment_lag1", "%d2_treatment_lag2")

    # Coefficient names to bold in the table
    bold_coefs <- c("Average Effect", "Contemporaneous Effect (FD)", "Cumulative Effect (FD)")

    html_table <- tryCatch({
      tbl_df <- etable(m$pols, m$twfe, m$fd, m$fd_cumul,
                       dict = dictName, drop = drop_vars, signif.code = NA, tex = FALSE)
      # Rename model columns (columns 2+ are models) by position
      new_names <- c("(1) POLS", "(2) FE", "(3) FD", "(4) Dyn FD")
      ncols <- ncol(tbl_df)
      if (ncols >= 5) {
        names(tbl_df)[2:5] <- new_names
      }
      # Bold the key coefficient rows (name column + value row)
      name_col <- names(tbl_df)[1]
      model_col_names <- names(tbl_df)[2:min(5, ncols)]
      for (bc in bold_coefs) {
        match_rows <- which(tbl_df[[name_col]] == bc)
        if (length(match_rows) > 0) {
          tbl_df[[name_col]][match_rows] <- paste0("<b>", bc, "</b>")
          for (mc in model_col_names) {
            tbl_df[[mc]][match_rows] <- paste0("<b>", tbl_df[[mc]][match_rows], "</b>")
          }
        }
      }
      knitr::kable(tbl_df, format = "html", table.attr = 'class="etable"', escape = FALSE)
    }, error = function(e) {
      paste0("<p style='color:#856404;'><em>Could not generate table: ", htmltools::htmlEscape(e$message), "</em></p>")
    })

    HTML(html_table)
  })

  output$model_explanation_panel <- renderText({
    paste0(
      "(1) POLS: Pooled OLS, no country FE. ",
      "(2) FE: Levels with country FE (TWFE). ",
      "(3) FD: First-differenced, contemporaneous effect only. ",
      "(4) Dyn FD: Cumulative effect after 3 years via reparameterization trick ",
      "(coefficient has proper SE for the sum of lags 0-3)."
    )
  })

  # ---- Pane 2: Event study regression table ----
  output$model_results_event <- renderUI({
    req(input$num_shocks == "1")
    m <- models()

    dictName <- c(
      "treatment" = "Treatment",
      "treatment_diff" = "Contemporaneous Effect",
      "(Intercept)" = "Constant"
    )

    # Event study + SA in one etable (both are fixest objects)
    html_table <- ""
    has_event <- !is.null(m$event)
    has_sunab <- !is.null(m$sunab)

    html_table <- tryCatch({
      event_names <- c()
      tbl_df <- if (has_event && has_sunab) {
        event_names <- c("Event Study FD", "Sun & Abraham")
        etable(m$event, m$sunab,
               dict = dictName, drop = "Intercept", signif.code = NA, tex = FALSE)
      } else if (has_event) {
        event_names <- c("Event Study FD")
        etable(m$event,
               dict = dictName, drop = "Intercept", signif.code = NA, tex = FALSE)
      } else if (has_sunab) {
        event_names <- c("Sun & Abraham")
        etable(m$sunab,
               dict = dictName, drop = "Intercept", signif.code = NA, tex = FALSE)
      } else {
        NULL
      }
      if (!is.null(tbl_df)) {
        # Rename model columns by position (columns 2+)
        n_models <- length(event_names)
        if (ncol(tbl_df) >= n_models + 1) {
          names(tbl_df)[2:(n_models + 1)] <- event_names
        }
        knitr::kable(tbl_df, format = "html", table.attr = 'class="etable"', escape = FALSE)
      } else ""
    }, error = function(e) {
      paste0("<p style='color:#856404;'><em>Could not generate event study table: ", htmltools::htmlEscape(e$message), "</em></p>")
    })

    HTML(html_table)
  })

  # Estimator warnings
  output$estimator_warnings <- renderText({
    m <- models()
    warnings <- c()
    if (is.null(m$sunab) && input$show_sunab && input$num_shocks == "1") {
      warnings <- c(warnings, "Sun & Abraham: Could not estimate (may need staggered treatment or year FE).")
    }
    paste(warnings, collapse = " | ")
  })

  # TWFE plot output
  output$twfe_plot <- renderPlotly({
    req(twfe_data())

    data_raw <- twfe_data() %>% filter(transformation == "1. Raw Data")
    data_unit <- twfe_data() %>% filter(transformation == "2. Unit FE Removed")
    data_both <- twfe_data() %>% filter(transformation == "3. Unit & Time FE Removed")

    # Plot 1: Raw data (original y limits)
    all_years <- sort(unique(data_raw$year))

    p1 <- ggplot(data_raw, aes(x = year, y = value, color = country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = all_years) +
      ggtitle("1. Raw Data") +
      labs(x = "Year", y = "Value")

    # Plot 2: Unit FE removed — dynamic y-axis
    p2_ylim <- compute_ylim(data_unit$value)
    p2 <- ggplot(data_unit, aes(x = year, y = value, color = country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = all_years) +
      coord_cartesian(ylim = p2_ylim) +
      ggtitle("2. Unit FE Removed") +
      labs(x = "Year", y = "Value")

    # Plot 3: Both FE removed — dynamic y-axis
    p3_ylim <- compute_ylim(data_both$value)
    p3 <- ggplot(data_both, aes(x = year, y = value, color = country)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = all_years) +
      coord_cartesian(ylim = p3_ylim) +
      ggtitle("3. Unit & Time FE Removed") +
      labs(x = "Year", y = "Value")

    # Combine plots with shared legend
    fig <- subplot(
      ggplotly(p1, tooltip = c("country", "year", "value")),
      ggplotly(p2, tooltip = c("country", "year", "value")),
      ggplotly(p3, tooltip = c("country", "year", "value")),
      nrows = 1,
      shareX = TRUE,
      titleX = TRUE,
      titleY = TRUE
    )

    # Robust legend handling via legendgroup
    n_countries <- length(unique(twfe_data()$country))
    for (i in seq_len(length(fig$x$data))) {
      country_idx <- ((i - 1) %% n_countries) + 1
      fig$x$data[[i]]$legendgroup <- paste0("country_", country_idx)
      fig$x$data[[i]]$showlegend <- (i <= n_countries)
    }

    fig <- fig %>% layout(
      title = "TWFE Transformation: Step-by-Step Fixed Effects Removal",
      legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center")
    )

    return(fig)
  })

  # TWFE explanation output
  output$twfe_explanation <- renderText({
    paste(
      "1. Raw Data: Observed sales by country over time.",
      "2. Country FE Removed: Adjusted for country-level differences, showing within-country variation.",
      "3. Country & Year FE Removed: Adjusted for both country and year effects, isolating within-country, within-year variation.",
      sep = "\n"
    )
  })

  # Event Study individual panel outputs
  output$event_plot_a <- renderPlotly({
    req(event_study_data())
    create_event_study_panel(event_study_data(), "A")
  })

  output$event_plot_b <- renderPlotly({
    req(event_study_data())
    create_event_study_panel(event_study_data(), "B")
  })

  output$event_plot_c <- renderPlotly({
    req(event_study_data())
    create_event_study_panel(event_study_data(), "C")
  })

  output$event_plot_d <- renderPlotly({
    req(event_study_data())
    fig <- create_event_study_panel(event_study_data(), "D")
    req(fig)
    fig
  })

  # Treatment effect (levels) coefficient plot
  output$event_level_plot <- renderPlotly({
    req(input$num_shocks == "1")
    m <- models()
    p <- create_event_level_plot(m, input)
    req(p)
    ggplotly(p, tooltip = c("estimator", "event_time", "estimate")) %>%
      layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"))
  })

  # Dynamic effect (FD) coefficient plot
  output$event_coef_plot <- renderPlotly({
    req(input$num_shocks == "1")
    m <- models()
    p <- create_event_coef_plot(m, input)
    req(p)
    ggplotly(p, tooltip = c("estimator", "event_time", "estimate")) %>%
      layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center"))
  })


  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("panel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}
