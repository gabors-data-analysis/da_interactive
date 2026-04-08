# Load required packages
library(shiny)
library(tidyverse)
library(fixest)
library(plotly)
library(did)

# version: 2026-03-14 v0.4.0

#########################################
# Utility functions
#########################################

# Compute symmetric y-axis limits with padding
compute_ylim <- function(values) {
  max_abs <- max(abs(values), na.rm = TRUE)
  padded <- max_abs * 1.15
  c(-padded, padded)
}

#########################################
# Data generation function
#########################################

generate_data <- function(input) {
  years <- 2010:2022
  countries <- LETTERS[1:6]
  base_values <- c(input$base_a, input$base_b, input$base_c,
                   input$base_d, input$base_e, input$base_f)

  if (input$timing == "once") {
    treat_timing <- rep(2015, 4)
  } else {
    treat_timing <- c(2013, 2014, 2016, 2017)
  }
  first_treat <- min(treat_timing)
  second_treat_timing <- treat_timing + 3

  if (input$effect_size == "uniform") {
    effects <- rep(input$uniform_effect, 4)
  } else {
    effects <- as.numeric(strsplit(input$hetero_effects, ",")[[1]])
    if (input$early_smaller) {
      effects <- effects[order(abs(effects))]
    }
  }

  # Parse dynamic effect values if provided
  if (input$dynamic_effect && !is.null(input$dynamic_effect_values)) {
    dynamic_values <- as.numeric(strsplit(input$dynamic_effect_values, ",")[[1]])
    if (length(dynamic_values) != 3) {
      dynamic_values <- c(50, 75, 100)  # Fallback to default if invalid
    }
  } else {
    dynamic_values <- c(100, 100, 100)  # No dynamic effect (immediate full effect)
  }

  # Default years to reversal
  years_to_reversal <- ifelse(!is.null(input$years_to_reversal),
                              input$years_to_reversal, 3)

  data <- expand.grid(year = years, country = countries) %>%
    arrange(country, year) %>%
    mutate(
      country_id = as.integer(factor(country)),
      base_value = rep(base_values, each = length(years)),
      value = base_value,
      treated = country %in% LETTERS[1:4]
    )

  if (input$global_trend) {
    data <- data %>%
      mutate(value = value + input$global_trend_size * (year - min(year)))
  }

  if (input$individual_trend) {
    ind_trends <- as.numeric(strsplit(input$individual_trends, ",")[[1]])
    data <- data %>%
      group_by(country) %>%
      mutate(
        trend_value = ind_trends[match(country, LETTERS[1:6])] * (year - min(year)),
        value = value + trend_value
      ) %>%
      ungroup()
  }

  data <- data %>%
    mutate(
      cohort = case_when(
        country == "A" ~ treat_timing[1],
        country == "B" ~ treat_timing[2],
        country == "C" ~ treat_timing[3],
        country == "D" ~ treat_timing[4],
        TRUE ~ Inf
      ),
      # Numeric cohort for did::att_gt() -- 0 means never-treated
      cohort_numeric = ifelse(is.infinite(cohort), 0, as.numeric(cohort)),
      second_cohort = case_when(
        input$num_shocks == "1" ~ Inf,
        country == "A" ~ second_treat_timing[1],
        country == "B" ~ second_treat_timing[2],
        country == "C" ~ second_treat_timing[3],
        country == "D" ~ second_treat_timing[4],
        TRUE ~ Inf
      ),
      relative_time = year - cohort,
      post = !is.infinite(cohort) & year >= cohort,
      post_second = !is.infinite(second_cohort) & year >= second_cohort,

      treatment = case_when(
        !treated ~ 0,
        treated & !post ~ 0,
        treated & post & (input$num_shocks == "1") ~ 1,
        treated & post & !post_second ~ 1,
        treated & post & post_second ~ 2,
        TRUE ~ 0
      )
    )

  # Add reversal calculation
  if (input$reversal) {
    data <- data %>%
      mutate(
        reversal_year = cohort + years_to_reversal,
        post_reversal = !is.infinite(reversal_year) & year >= reversal_year
      )
  } else {
    data <- data %>%
      mutate(
        reversal_year = Inf,
        post_reversal = FALSE
      )
  }

  # Apply effect considering dynamic effect and reversal
  data <- data %>%
    mutate(
      effect = case_when(
        treatment == 0 ~ 0,
        treatment == 1 & post_reversal & input$reversal ~ 0,
        treatment == 1 & input$dynamic_effect ~ effects[match(country, LETTERS[1:4])] *
          case_when(
            relative_time == 0 ~ dynamic_values[1]/100,
            relative_time == 1 ~ dynamic_values[2]/100,
            relative_time >= 2 ~ dynamic_values[3]/100,
            TRUE ~ 0
          ),
        treatment == 1 ~ effects[match(country, LETTERS[1:4])],
        treatment == 2 & input$num_shocks == "2_same" ~ 2 * effects[match(country, LETTERS[1:4])],
        treatment == 2 & input$num_shocks == "2_varied" ~
          effects[match(country, LETTERS[1:4])] * (1 + input$second_shock_percent/100),
        TRUE ~ 0
      ),
      noise = rnorm(n(), mean = 0, sd = input$noise_sd),
      value = value + effect + noise
    )
  return(data)
}


#########################################
# Enhanced TWFE transformation function
#########################################

run_twfe_transform <- function(data) {
  # Step 1: Raw Data (Baseline)
  raw_data <- data %>%
    mutate(transformation = "1. Raw Data")

  # Step 2: Calculate and Remove Unit (Country) FE
  unit_fe <- data %>%
    group_by(country) %>%
    summarize(
      unit_fe = mean(value),
      pre_treatment_mean = mean(value[treatment == 0]),
      post_treatment_mean = mean(value[treatment > 0]),
      treatment_effect = post_treatment_mean - pre_treatment_mean,
      .groups = 'drop'
    )

  unit_adjusted <- data %>%
    left_join(unit_fe, by = "country") %>%
    mutate(
      value_unit_adjusted = value - unit_fe,
      transformation = "2. Unit FE Removed"
    )

  # Step 3: Calculate and Remove Time FE
  time_fe <- unit_adjusted %>%
    group_by(year) %>%
    summarize(
      time_fe = mean(value_unit_adjusted),
      treated_mean = mean(value_unit_adjusted[treatment > 0]),
      control_mean = mean(value_unit_adjusted[treatment == 0]),
      .groups = 'drop'
    )

  final_data <- unit_adjusted %>%
    left_join(time_fe, by = "year") %>%
    mutate(
      value_final = value_unit_adjusted - time_fe,
      transformation = "3. Unit & Time FE Removed"
    )

  # Step 4: Calculate Treatment Effect Components
  treatment_components <- final_data %>%
    filter(treatment > 0) %>%
    group_by(country) %>%
    summarize(
      avg_treatment_effect = mean(value_final),
      timing_component = mean(value_final) - mean(value_final[year == min(year[treatment > 0])]),
      heterogeneity_component = mean(value_final) - mean(value_final[treatment == 1]),
      .groups = 'drop'
    )

  # Combine all steps with components
  twfe_data <- bind_rows(
    raw_data %>% select(year, country, value, transformation),
    unit_adjusted %>% select(year, country, value = value_unit_adjusted, transformation),
    final_data %>% select(year, country, value = value_final, transformation)
  ) %>%
    left_join(unit_fe, by = "country") %>%
    left_join(time_fe, by = "year") %>%
    left_join(treatment_components, by = "country") %>%
    mutate(
      transformation = factor(transformation,
                              levels = c("1. Raw Data",
                                         "2. Unit FE Removed",
                                         "3. Unit & Time FE Removed"))
    )

  return(twfe_data)
}

# Create base plot
create_did_plot <- function(data) {
  p <- ggplot(data, aes(x = year, y = value, color = country, group = country)) +
    geom_hline(yintercept = seq(0, max(data$value) + 1000, by = 500),
               color = "grey90", linewidth = 0.2) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey85"),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = 2010:2022) +
    scale_y_continuous(breaks = seq(-1000, max(data$value) + 1000, by = 1000),
                       expand = c(0, 0),
                       limits = c(-1000, max(data$value) + 1000)) +
    labs(title = "Treatment Effects Over Time", x = "Year", y = "Sales of Sugary Drinks")
  return(p)
}


#########################################
# Run regression models
#########################################

run_models <- function(data, input) {
  # First, ensure treatment variable correctly accounts for reversal
  model_data <- data

  if(input$reversal) {
    model_data <- model_data %>%
      mutate(
        treatment = case_when(
          post_reversal ~ 0,
          TRUE ~ treatment
        )
      )
  }

  # First difference data preparation
  fd_data <- model_data %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      value_diff = value - lag(value),
      treatment_diff = treatment - lag(treatment),
      treatment_diff_lag1 = lag(treatment_diff, 1),
      treatment_diff_lag2 = lag(treatment_diff, 2),
      treatment_diff_lag3 = lag(treatment_diff, 3),
      d2_treatment = treatment_diff - lag(treatment_diff),
      d2_treatment_lag1 = lag(d2_treatment, 1),
      d2_treatment_lag2 = lag(d2_treatment, 2)
    ) %>%
    filter(!is.na(value_diff))

  # Event study data preparation with dynamic min/max time
  data_event <- model_data %>%
    group_by(country) %>%
    mutate(
      value_diff = value - lag(value),
      rel_year = factor(
        case_when(
          relative_time < input$min_event_time ~ paste("<", input$min_event_time),
          relative_time > input$max_event_time ~ paste(">", input$max_event_time),
          TRUE ~ as.character(relative_time)
        ),
        levels = c(
          paste("<", input$min_event_time),
          as.character(input$min_event_time:input$max_event_time),
          paste(">", input$max_event_time)
        )
      )
    ) %>%
    filter(!is.infinite(cohort) | is.infinite(relative_time)) %>%
    filter(!is.na(value_diff))

  # Pooled OLS (no country FE)
  if(input$year_fe) {
    pols_model <- feols(value ~ treatment | year,
                        data = model_data,
                        cluster = "country")
  } else {
    pols_model <- feols(value ~ treatment,
                        data = model_data,
                        cluster = "country")
  }

  # Model specifications
  if(input$year_fe) {
    twfe_model <- feols(value ~ treatment | country + year,
                        data = model_data,
                        cluster = "country")

    if(input$country_fe_fd) {
      fd_model <- feols(value_diff ~ treatment_diff | country + year,
                        data = fd_data,
                        cluster = "country")
      fd_cumul_model <- feols(value_diff ~ treatment_diff_lag3 +
                                d2_treatment + d2_treatment_lag1 + d2_treatment_lag2 |
                                country + year,
                              data = fd_data,
                              cluster = "country")
    } else {
      fd_model <- feols(value_diff ~ treatment_diff | year,
                        data = fd_data,
                        cluster = "country")
      fd_cumul_model <- feols(value_diff ~ treatment_diff_lag3 +
                                d2_treatment + d2_treatment_lag1 + d2_treatment_lag2 |
                                year,
                              data = fd_data,
                              cluster = "country")
    }
  } else {
    twfe_model <- feols(value ~ treatment | country,
                        data = model_data,
                        cluster = "country")

    if(input$country_fe_fd) {
      fd_model <- feols(value_diff ~ treatment_diff | country,
                        data = fd_data,
                        cluster = "country")
      fd_cumul_model <- feols(value_diff ~ treatment_diff_lag3 +
                                d2_treatment + d2_treatment_lag1 + d2_treatment_lag2 |
                                country,
                              data = fd_data,
                              cluster = "country")
    } else {
      fd_model <- feols(value_diff ~ treatment_diff,
                        data = fd_data,
                        cluster = "country")
      fd_cumul_model <- feols(value_diff ~ treatment_diff_lag3 +
                                d2_treatment + d2_treatment_lag1 + d2_treatment_lag2,
                              data = fd_data,
                              cluster = "country")
    }
  }

  # Event study models — uses user's event window, not hard-coded -3/3
  event_model <- NULL
  if(input$num_shocks == "1") {
    # FD-based event study (dynamic effects = changes)
    event_model <- tryCatch({
      feols(value_diff ~ i(relative_time, ref = -1),
            cluster = "country",
            data = filter(data_event,
                          relative_time >= input$min_event_time &
                          relative_time <= input$max_event_time))
    }, error = function(e) NULL)
  }

  # Sun & Abraham (2021) via fixest::sunab()
  # Works for single shock; most useful with staggered timing
  sunab_model <- NULL
  if (input$num_shocks == "1" && input$show_sunab) {
    sunab_model <- tryCatch({
      feols(value ~ sunab(cohort, year) | country + year,
            data = model_data,
            cluster = "country")
    }, error = function(e) NULL)
  }

  return(list(
    pols = pols_model,
    twfe = twfe_model,
    fd = fd_model,
    fd_cumul = fd_cumul_model,
    event = event_model,
    sunab = sunab_model
  ))
}


####################################################################
# Event Study Transformation Functions
####################################################################

# Transform data for event study visualization
transform_event_study_data <- function(data, min_time, max_time, timing) {
  # Get treatment timing for controls (use first treated unit's timing)
  first_treatment <- min(data$cohort[!is.infinite(data$cohort)])

  # Add basic indicators
  base_data <- data %>%
    mutate(
      treated_group = !is.infinite(cohort),
      relative_time = if_else(is.infinite(cohort),
                              year - first_treatment,
                              year - cohort)
    )

  # Normalize each country's time series by subtracting the value at t-1
  normalized_data <- base_data %>%
    group_by(country) %>%
    mutate(
      t_minus_1_value = value[relative_time == -1],
      normalized_value = value - t_minus_1_value
    ) %>%
    ungroup()

  # 1. Original data panel
  p1_data <- base_data %>%
    mutate(
      panel = "1. Original Time Series",
      vline = if_else(!is.infinite(cohort), cohort, NA_real_)
    )

  # 2. Event time panel
  p2_data <- base_data %>%
    filter(relative_time >= min_time,
           relative_time <= max_time) %>%
    mutate(
      panel = "2. Event Time",
      vline = 0
    )

  # 3. Normalized group averages — improved for staggered treatment
  # First average within cohort, then across cohorts (equal cohort weights)
  if (timing == "staggered") {
    # Cohort-weighted averages for treated
    treated_avg <- normalized_data %>%
      filter(relative_time >= min_time, relative_time <= max_time,
             treated_group) %>%
      group_by(relative_time, cohort) %>%
      summarize(cohort_avg = mean(normalized_value), .groups = 'drop') %>%
      group_by(relative_time) %>%
      summarize(value = mean(cohort_avg), .groups = 'drop') %>%
      mutate(treated_group = TRUE)

    control_avg <- normalized_data %>%
      filter(relative_time >= min_time, relative_time <= max_time,
             !treated_group) %>%
      group_by(relative_time) %>%
      summarize(value = mean(normalized_value), .groups = 'drop') %>%
      mutate(treated_group = FALSE)

    p3_data <- bind_rows(treated_avg, control_avg) %>%
      mutate(
        panel = "3. Normalized Group Averages",
        country = if_else(treated_group, "Treated Average", "Control Average"),
        vline = 0
      )
  } else {
    # Simple average for simultaneous treatment
    p3_data <- normalized_data %>%
      filter(relative_time >= min_time, relative_time <= max_time) %>%
      group_by(relative_time, treated_group) %>%
      summarize(
        value = mean(normalized_value),
        .groups = 'drop'
      ) %>%
      mutate(
        panel = "3. Normalized Group Averages",
        country = if_else(treated_group, "Treated Average", "Control Average"),
        vline = 0
      )
  }

  # 4. Cohort-specific lines (only for staggered)
  p4_data <- NULL
  if (timing == "staggered") {
    p4_treated <- normalized_data %>%
      filter(relative_time >= min_time, relative_time <= max_time,
             treated_group) %>%
      group_by(relative_time, cohort) %>%
      summarize(value = mean(normalized_value), .groups = 'drop') %>%
      mutate(
        country = paste0("Cohort ", cohort),
        treated_group = TRUE
      )

    p4_control <- normalized_data %>%
      filter(relative_time >= min_time, relative_time <= max_time,
             !treated_group) %>%
      group_by(relative_time) %>%
      summarize(value = mean(normalized_value), .groups = 'drop') %>%
      mutate(
        country = "Control Average",
        cohort = Inf,
        treated_group = FALSE
      )

    p4_data <- bind_rows(p4_treated, p4_control) %>%
      mutate(
        panel = "4. Cohort-Specific Effects",
        vline = 0
      )
  }

  # Combine all panels
  panels <- list(p1_data, p2_data, p3_data)
  if (!is.null(p4_data)) panels <- c(panels, list(p4_data))

  all_levels <- c("1. Original Time Series", "2. Event Time",
                  "3. Normalized Group Averages")
  if (!is.null(p4_data)) all_levels <- c(all_levels, "4. Cohort-Specific Effects")

  plot_data <- bind_rows(panels) %>%
    mutate(panel = factor(panel, levels = all_levels))

  return(plot_data)
}

# Create event study plot
create_event_study_panel <- function(data, panel_name) {
  # Create vertical line data
  vlines <- data %>%
    filter(!is.na(vline) & !is.infinite(vline)) %>%
    distinct(panel, vline) %>%
    rename(xint = vline)

  # Get axis breaks
  year_breaks <- sort(unique(filter(data, panel == "1. Original Time Series")$year))
  relative_breaks <- sort(unique(filter(data, panel != "1. Original Time Series")$relative_time))

  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11)
    )

  if (panel_name == "A") {
    p <- ggplot(filter(data, panel == "1. Original Time Series"),
                aes(x = year, y = value, color = country, group = country)) +
      geom_line() + geom_point() +
      geom_vline(data = filter(vlines, panel == "1. Original Time Series"),
                 aes(xintercept = xint), linetype = "dashed") +
      base_theme +
      scale_x_continuous(breaks = year_breaks,
                         labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
      labs(title = "Graph A: Original Time Series", x = "Year", y = "Value")
    return(ggplotly(p, tooltip = c("country", "year", "value")) %>%
             layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")))

  } else if (panel_name == "B") {
    p <- ggplot(filter(data, panel == "2. Event Time"),
                aes(x = relative_time, y = value, color = country, group = country)) +
      geom_line() + geom_point() +
      geom_vline(data = filter(vlines, panel == "2. Event Time"),
                 aes(xintercept = xint), linetype = "dashed") +
      base_theme +
      scale_x_continuous(breaks = relative_breaks) +
      labs(title = "Graph B: Event Time", x = "Relative Time", y = "Value")
    return(ggplotly(p, tooltip = c("country", "relative_time", "value")) %>%
             layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")))

  } else if (panel_name == "C") {
    p_data <- filter(data, panel == "3. Normalized Group Averages")
    p_ylim <- compute_ylim(p_data$value)
    p <- ggplot(p_data,
                aes(x = relative_time, y = value, color = country, group = country)) +
      geom_line(linewidth = 1.5) + geom_point(size = 3) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(data = filter(vlines, panel == "3. Normalized Group Averages"),
                 aes(xintercept = xint), linetype = "dashed") +
      base_theme +
      scale_x_continuous(breaks = relative_breaks) +
      coord_cartesian(ylim = p_ylim) +
      labs(title = "Graph C: Normalized Group Averages", x = "Relative Time", y = "Normalized Value")
    return(ggplotly(p, tooltip = c("country", "relative_time", "value")) %>%
             layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")))

  } else if (panel_name == "D") {
    has_panel4 <- "4. Cohort-Specific Effects" %in% levels(data$panel)
    if (!has_panel4) return(NULL)
    p_data <- filter(data, panel == "4. Cohort-Specific Effects")
    p_ylim <- compute_ylim(p_data$value)
    p <- ggplot(p_data,
                aes(x = relative_time, y = value, color = country, group = country)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(aes(xintercept = 0), linetype = "dashed") +
      base_theme +
      scale_x_continuous(breaks = relative_breaks) +
      coord_cartesian(ylim = p_ylim) +
      labs(title = "Graph D: Cohort-Specific Effects", x = "Relative Time", y = "Normalized Value")
    return(ggplotly(p, tooltip = c("country", "relative_time", "value")) %>%
             layout(legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")))
  }
}


####################################################################
# Event Study Coefficient Plot
####################################################################

create_event_coef_plot <- function(models, input) {
  coef_data <- data.frame()

  # FD-based TWFE event study only (changes, not levels)
  if (!is.null(models$event)) {
    event_ct <- tryCatch({
      ct <- coeftable(models$event)
      rnames <- rownames(ct)
      event_times <- suppressWarnings(as.numeric(gsub(".*::(-?[0-9]+)$", "\\1", rnames)))
      data.frame(
        event_time = event_times,
        estimate = ct[, "Estimate"],
        se = ct[, "Std. Error"],
        estimator = "TWFE Event Study (FD)",
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)

    if (!is.null(event_ct)) coef_data <- bind_rows(coef_data, event_ct)
  }

  if (nrow(coef_data) == 0) return(NULL)

  coef_data <- coef_data %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )

  # Add reference point at t = -1 (normalized to 0)
  ref_rows <- data.frame(
    event_time = -1, estimate = 0, se = 0,
    ci_lower = 0, ci_upper = 0,
    estimator = unique(coef_data$estimator),
    stringsAsFactors = FALSE
  )
  coef_data <- bind_rows(coef_data, ref_rows)

  coef_data <- coef_data %>%
    filter(event_time >= input$min_event_time,
           event_time <= input$max_event_time)

  p <- ggplot(coef_data, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray70") +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    color = "#1f77b4", size = 0.6) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40")
    ) +
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]))) +
    labs(
      title = "Dynamic Effects (First Differences)",
      subtitle = "Period-by-period changes with 95% CIs (reference: t = \u22121)",
      x = "Event Time (periods relative to treatment)",
      y = "Change in Outcome"
    )

  return(p)
}


####################################################################
# Treatment Effect (Levels) Coefficient Plot
####################################################################

create_event_level_plot <- function(models, input) {
  coef_data <- data.frame()

  # Helper: cumulate FD coefficients to get level estimates
  # FD_coef(k) = Level(k) - Level(k-1), so:
  #   Level(k) = Level(k-1) + FD_coef(k)     [forward from ref]
  #   Level(k) = Level(k+1) - FD_coef(k+1)   [backward from ref]
  cumulate_fd <- function(ct_df) {
    df <- ct_df %>% arrange(event_time)
    # Insert reference period t = -1 with 0
    df <- bind_rows(data.frame(event_time = -1, estimate = 0, se = 0), df) %>%
      arrange(event_time)
    ref_idx <- which(df$event_time == -1)

    # Store original FD coefficients (needed for backward pass)
    fd_est <- df$estimate
    fd_se  <- df$se

    # Forward cumulation (t >= 0): Level(k) = Level(k-1) + FD(k)
    if (ref_idx < nrow(df)) {
      for (i in (ref_idx + 1):nrow(df)) {
        df$estimate[i] <- df$estimate[i - 1] + fd_est[i]
        df$se[i] <- sqrt(df$se[i - 1]^2 + fd_se[i]^2)
      }
    }
    # Backward cumulation (t <= -2): Level(k) = Level(k+1) - FD(k+1)
    if (ref_idx > 1) {
      for (i in (ref_idx - 1):1) {
        df$estimate[i] <- df$estimate[i + 1] - fd_est[i + 1]
        df$se[i] <- sqrt(df$se[i + 1]^2 + fd_se[i + 1]^2)
      }
    }
    df %>% filter(event_time != -1)
  }

  # 1. TWFE — cumulate FD event study coefficients to get levels
  # (Direct level model with country+year FE is unreliable with staggered treatment)
  if (!is.null(models$event)) {
    twfe_ct <- tryCatch({
      ct <- coeftable(models$event)
      rnames <- rownames(ct)
      event_times <- suppressWarnings(as.numeric(gsub(".*::(-?[0-9]+)$", "\\1", rnames)))
      fd_df <- data.frame(event_time = event_times,
                          estimate = ct[, "Estimate"],
                          se = ct[, "Std. Error"],
                          stringsAsFactors = FALSE)
      level_df <- cumulate_fd(fd_df)
      level_df$estimator <- "TWFE"
      level_df
    }, error = function(e) NULL)

    if (!is.null(twfe_ct) && nrow(twfe_ct) > 0) coef_data <- bind_rows(coef_data, twfe_ct)
  }

  # 2. Sun & Abraham (already in levels via sunab)
  if (!is.null(models$sunab)) {
    sa_ct <- tryCatch({
      # Try multiple aggregation approaches for robustness across fixest versions
      sa_agg <- tryCatch(
        aggregate(models$sunab, agg = "att"),
        error = function(e) tryCatch(
          aggregate(models$sunab, agg = TRUE),
          error = function(e) aggregate(models$sunab)
        )
      )
      ct <- coeftable(sa_agg)
      rnames <- rownames(ct)
      event_times <- suppressWarnings(as.numeric(gsub(".*::(-?[0-9]+)$", "\\1", rnames)))
      # Drop any NA event times (failed parsing)
      valid <- !is.na(event_times)
      data.frame(
        event_time = event_times[valid],
        estimate = ct[valid, "Estimate"],
        se = ct[valid, "Std. Error"],
        estimator = "Sun & Abraham",
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)

    if (!is.null(sa_ct) && nrow(sa_ct) > 0) coef_data <- bind_rows(coef_data, sa_ct)
  }

  if (nrow(coef_data) == 0) return(NULL)

  coef_data <- coef_data %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )

  # Add reference point at t = -1 (normalized to 0)
  ref_rows <- data.frame(
    event_time = -1,
    estimate = 0,
    se = 0,
    ci_lower = 0,
    ci_upper = 0,
    estimator = unique(coef_data$estimator),
    stringsAsFactors = FALSE
  )
  coef_data <- bind_rows(coef_data, ref_rows)

  coef_data <- coef_data %>%
    filter(event_time >= input$min_event_time,
           event_time <= input$max_event_time)

  dodge_width <- 0.3

  p <- ggplot(coef_data, aes(x = event_time, y = estimate,
                              color = estimator, shape = estimator)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray70") +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(width = dodge_width),
                    size = 0.6) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40")
    ) +
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]))) +
    labs(
      title = "Treatment Effects: Level Estimates",
      subtitle = "Point estimates with 95% CIs (reference: t = -1)",
      x = "Event Time (periods relative to treatment)",
      y = "Treatment Effect (level)",
      color = "Estimator",
      shape = "Estimator"
    )

  return(p)
}


####################################################################
# Format CS output as HTML table
####################################################################


####################################################################
# Create PanelView heatmap function
####################################################################

create_panel_view <- function(data) {
  panel_data <- data %>%
    select(year, country, treatment, post_reversal) %>%
    mutate(
      treatment_status = case_when(
        treatment == 0 ~ "No treatment",
        treatment == 1 & post_reversal ~ "No treatment",
        treatment == 1 ~ "Treated once",
        treatment == 2 ~ "Treated twice",
        TRUE ~ NA_character_
      ),
      treatment_status = factor(treatment_status,
                                levels = c("No treatment", "Treated once", "Treated twice"))
    )

  p <- ggplot(panel_data, aes(x = year, y = country, fill = treatment_status)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_manual(values = c("No treatment" = "#f0f8ff",
                                 "Treated once" = "#6495ed",
                                 "Treated twice" = "#000080")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0),
          legend.position = "bottom") +
    scale_x_continuous(breaks = 2010:2022) +
    labs(title = "Treatment Status by Country and Year",
         x = "Year", y = "Country", fill = "Treatment Status")

  return(p)
}
