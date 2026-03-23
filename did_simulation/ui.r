# This is a shiny app to show how variation in interventions are estimated in two different models: TWFE and DiD.
# Let us have 6 countries, A to F. and 13 time periods 2010 to 2022.
# Outcome (y) is sales of sugary drinks, set at a 1000, b 2000, c 4000, d 5000, e 3000, f 6000. E and F and controls, no intervention.
# When the intervention happens at once, it is 2015. When staggered it is 2013, 2014, 2016 and 2017.
# Uniform intervention effect is -1000. Heterogenous is -500, -1500, -500, -1500.

# Load required packages
library(shiny)
library(plotly)

# UI Definition
ui <- fluidPage(
  # CSS for styled tables and layout
  tags$head(
    tags$style(HTML("
      .etable { border-collapse: collapse; width: 100%; margin: 10px 0; font-size: 0.9em; }
      .etable th, .etable td { border: 1px solid #ddd; padding: 6px 10px; text-align: right; }
      .etable th { background-color: #f5f5f5; font-weight: bold; }
      .etable tr:nth-child(even) { background-color: #fafafa; }
      .cs-table { border-collapse: collapse; width: auto; margin: 10px 0; font-size: 0.9em; }
      .cs-table th, .cs-table td { border: 1px solid #ddd; padding: 5px 10px; text-align: right; }
      .cs-table th { background-color: #f5f5f5; font-weight: bold; }
      .cs-table tr:nth-child(even) { background-color: #fafafa; }
      .warning-text { color: #856404; font-style: italic; margin-top: 5px; }
      .nav-pills > li > a { font-size: 1.1em; padding: 10px 20px; }
      .nav-pills > li.active > a { background-color: #0275d8; }
      .tab-content { padding-top: 15px; }
    "))
  ),

  titlePanel("Gabor's Panel Models Estimation Comparison"),

  # Top description panel
  fluidRow(
    column(12,
           div(
             class = "well",
             style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #0275d8; margin-bottom: 20px;",
             
             p("Welcome to the Panel data and difference in differences simulation. v0.5.1 2026-03-22."),
             p("When you observe many units over time and want understand the effect of some policy change, you consider TWFE or FD panel models or do an event study design for first differences. This is a toy simulation to illustrate some points."),
             p("This is a panel estimation illustration. Imagine we have 6 countries, A to F, and 13 time periods 2010 to 2022."),
             p("Outcome (y) is average per capita sales of sugary drinks (ml/week), set at a 1000, b 2000, c 4000, d 5000, e 3000, f 6000. E and F are controls, no intervention. "),
             p("The intervention is a 1 dollar sales tax. The true effect of the tax cuts consumption by -1000 as default."),
             p("You can set many aspects of the intervention: size, timing, you can even have 2. You can add noise. You can also add dynamics (time to build), and reversal. "),
             p(
               "In development. Suggestions: contact ",
               tags$a(href = "mailto:bekesg@ceu.edu", "Gabor"),
               " / add an issue to ",
               tags$a(
                 href = "https://github.com/gabors-data-analysis/da_interactive/tree/main/did_simulation",
                 target = "_blank",
                 "repo"
               )),
             p("This is part of Gabors Data Analysis ecosystem.", tags$a(
               href = "https://gabors-data-analysis.com/chapters/#chapter-23-methods-for-panel-data",
               target = "_blank",
               "Chapter 23 (panel data)") ,
               tags$a(
                 href = "https://gabors-data-analysis.com/chapters/#chapter-24-appropriate-control-groups-for-panel-data",
                 target = "_blank",
                 "Chapter 24 (event studies)")
             )
           )
    )
  ),

  # Two graphs side by side
  fluidRow(
    column(6, plotlyOutput("panel_view")),
    column(6, plotlyOutput("did_plot"))
  ),

  # Treatment explanation
  fluidRow(
    column(12,
           div(
             style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin-top: 10px; margin-bottom: 20px;",
             textOutput("treatment_explanation")
           )
    )
  ),

  # Settings section
  fluidRow(
    column(12,
           tags$hr(),
           h3("Simulation Settings"),
           div(style = "display: flex; flex-wrap: wrap;")
    )
  ),

  fluidRow(
    column(3,
           # Main controls
           div(class = "well",
               h4("Treatment Settings"),
               radioButtons("timing", "Treatment Timing:",
                            choices = c("Simultaneous" = "once",
                                        "Staggered" = "staggered")),

               radioButtons("effect_size", "Treatment Effect:",
                            choices = c("Uniform" = "uniform",
                                        "Heterogeneous" = "heterogeneous")),

               checkboxInput("early_smaller", "Early interventions are smaller", FALSE),

               checkboxInput("dynamic_effect", "Dynamic effect build-up", FALSE),
               tags$span(
                 style = "font-size: 0.8em; color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;",
                 "When enabled, treatment effect builds up gradually according to percentages."
               ),

               checkboxInput("reversal", "Treatment reversal", FALSE),
               tags$span(
                 style = "font-size: 0.8em; color: #6c757d; display: block; margin-top: -10px; margin-bottom: 10px;",
                 "When enabled, treatment effect disappears after specified years."
               ),
               h4("Other Factors (trends)"),

               checkboxInput("global_trend", "Include global trend", FALSE),
               checkboxInput("individual_trend", "Include individual trends", FALSE)
           )
    ),

    column(3,
           # Treatment complexity settings
           div(class = "well",
               h4("Treatment Complexity"),
               radioButtons("num_shocks", "Number of Shocks:",
                            choices = c("One" = "1",
                                        "Two (Same Effect)" = "2_same",
                                        "Two (Varied Effect)" = "2_varied"),
                            selected = "1"),

               conditionalPanel(
                 condition = "input.num_shocks == '2_varied'",
                 sliderInput("second_shock_percent", "Second Shock Effect (%)",
                             min = -100, max = 200, value = 50, step = 10)
               ),

               sliderInput("noise_sd", "Noise SD:",
                           min = 0, max = 1000, value = 0, step = 50))
    ),

    column(3,
           # Model settings
           div(class = "well",
               h4("Model Settings"),
               checkboxInput("year_fe", "Include Year Fixed Effects", FALSE),
               checkboxInput("country_fe_fd", "Add Country FE to First Difference Model", FALSE),

               # Event Study Controls
               h4("Event Study Settings"),
               sliderInput("min_event_time", "Minimum Event Time:",
                           min = -10, max = -1, value = -3, step = 1),
               sliderInput("max_event_time", "Maximum Event Time:",
                           min = 1, max = 10, value = 3, step = 1),

               # Estimator selection
               h4("Estimator Selection"),
               checkboxInput("show_sunab", "Sun & Abraham (2021)", TRUE),
               tags$span(
                 style = "font-size: 0.8em; color: #6c757d; display: block; margin-top: -5px;",
                 "Heterogeneity-robust estimators shown alongside TWFE for comparison."
               )
           )
    ),

    column(3,
           # Advanced settings
           div(class = "well",
               h4("Advanced Settings"),
               actionButton("show_advanced", "Show/Hide Advanced Settings"),

               # Advanced settings panel
               conditionalPanel(
                 condition = "input.show_advanced % 2 == 1",
                 numericInput("base_a", "Base value Country A:", 2000),
                 numericInput("base_b", "Base value Country B:", 3000),
                 numericInput("base_c", "Base value Country C:", 5000),
                 numericInput("base_d", "Base value Country D:", 6000),
                 numericInput("base_e", "Base value Country E:", 4000),
                 numericInput("base_f", "Base value Country F:", 7000),
                 numericInput("uniform_effect", "Uniform effect size:", -1000),
                 textInput("hetero_effects", "Heterogeneous effects (comma-separated):",
                           "-500,-1500,-500,-1500"),
                 numericInput("global_trend_size", "Global trend increment:", 100),
                 textInput("individual_trends", "Individual trends (comma-separated):",
                           "100,200,300,400,0,0"),

                 # Dynamic and reversal settings
                 textInput("dynamic_effect_values", "Dynamic effect progression (%):",
                           "50,75,100"),
                 numericInput("years_to_reversal", "Years until reversal:", 3, min = 1, max = 10)
               ))
    )
  ),

  # ===================================================================
  # TABBED ANALYSIS SECTION
  # ===================================================================
  fluidRow(
    column(12,
           tags$hr(),
           tabsetPanel(
             id = "analysis_tabs", type = "pills",

             # ----- TAB 1: Panel Models (FE / FD) -----
             tabPanel(
               "Panel Models (FE / FD)",
               div(style = "margin-top: 15px;"),

               # Regression table
               div(
                 style = "background-color: #f8f9fa; padding: 10px; border-left: 4px solid #0275d8; margin-bottom: 10px;",
                 p(style = "margin: 0;",
                   "(1) POLS: Pooled OLS, no country FE. ",
                   "(2) FE: TWFE in levels with country FE. ",
                   "(3) FD: First differences, contemporaneous effect. ",
                   "(4) Dyn FD: Cumulative effect after 3 years (reparameterization). ",
                   "All estimated with fixest, clustered SEs by country."
                 )
               ),
               htmlOutput("model_results_panel"),
               div(
                 style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 10px 0;",
                 textOutput("model_explanation_panel")
               ),

               # TWFE Transformation
               tags$h4("Illustrating TWFE: Removing Fixed Effects"),
               tags$p("This section demonstrates how the TWFE model works step by step by removing country and year fixed effects."),
               plotlyOutput("twfe_plot"),
               textOutput("twfe_explanation")
             ),

             # ----- TAB 2: Event Study -----
             tabPanel(
               "Event Study",
               div(style = "margin-top: 15px;"),

               # Only show event study content for single intervention
               conditionalPanel(
                 condition = "input.num_shocks == '1'",

                 # Summary right after title
                 div(
                   style = "background-color: #f8f9fa; padding: 10px; border-left: 4px solid #28a745; margin-bottom: 10px;",
                   p(style = "margin: 0;",
                     "Event study analysis with two estimators: ",
                     "TWFE event study (standard) and Sun & Abraham (2021) interaction-weighted estimator, ",
                     "which is robust to treatment effect heterogeneity under staggered adoption."
                   )
                 ),

                 # Regression table
                 htmlOutput("model_results_event"),
                 div(class = "warning-text",
                     textOutput("estimator_warnings")
                 ),

                 # Bridging explanation
                 div(
                   style = "background-color: #f8f9fa; padding: 10px; border-left: 4px solid #6c757d; margin: 15px 0;",
                   p(style = "margin: 0;",
                     tags$strong("Two views of event study coefficients: "),
                     "We first show the ", tags$em("treatment effect"), " (level estimates relative to t = \u22121), ",
                     "followed by the ", tags$em("dynamic effect"), " (period-by-period changes, i.e. first differences).")
                 ),

                 # Treatment effect plot (LEVELS) — first
                 div(style = "margin: 15px 0;"),
                 plotlyOutput("event_level_plot", height = "450px"),
                 div(
                   style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 5px 0 20px 0;",
                   p(tags$strong("Treatment Effect (levels):"),
                     " Each coefficient shows the total treatment effect at that event time relative to t = \u22121.",
                     " Pre-treatment coefficients near zero support parallel trends.",
                     " Post-treatment coefficients show the cumulative level effect at each horizon.")
                 ),

                 # Dynamic effect plot (FD) — second
                 plotlyOutput("event_coef_plot", height = "450px"),
                 div(
                   style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 5px 0 20px 0;",
                   p(tags$strong("Dynamic Effect (first differences):"),
                     " Each coefficient shows the period-by-period change in the outcome.",
                     " These are the FD-based event study coefficients from the TWFE regression on differenced data.")
                 ),

                 # Event study data transformation — individual graphs
                 tags$h4("Event Study: Data Transformation"),

                 # Graph A
                 plotlyOutput("event_plot_a", height = "450px"),
                 div(
                   style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 5px 0 20px 0;",
                   p(tags$strong("Graph A: Original Time Series."),
                     " Raw outcome data for all countries over calendar time. Vertical dashed lines mark treatment onset. The level differences across countries reflect country fixed effects.")
                 ),

                 # Graph B
                 plotlyOutput("event_plot_b", height = "450px"),
                 div(
                   style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 5px 0 20px 0;",
                   p(tags$strong("Graph B: Event Time."),
                     " Same data recentered so that time 0 is the treatment date for each unit. This aligns treatment timing across countries, making pre/post comparisons possible.")
                 ),

                 # Graph C
                 plotlyOutput("event_plot_c", height = "450px"),
                 div(
                   style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 5px 0 20px 0;",
                   p(tags$strong("Graph C: Normalized Group Averages."),
                     " Treated vs control group averages, normalized to 0 at t-1. The gap after t=0 is the estimated treatment effect. Parallel pre-trends support the identifying assumption.")
                 ),

                 # Graph D (staggered only)
                 conditionalPanel(
                   condition = "input.timing == 'staggered'",
                   plotlyOutput("event_plot_d", height = "450px"),
                   div(
                     style = "background-color: #f0f9ff; padding: 10px; border-left: 4px solid #17a2b8; margin: 5px 0 20px 0;",
                     p(tags$strong("Graph D: Cohort-Specific Effects."),
                       " Each treatment cohort shown separately to reveal effect heterogeneity across groups. When effects differ by cohort, TWFE averages them with potentially negative weights.")
                   )
                 ),

                 # ── Detailed note: why centering on event time is not enough ──
                 tags$h4("Why Centering on Event Time Is Not Enough"),
                 div(
                   style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #6c757d; margin: 10px 0;",
                   p("When we have staggered and heterogeneous intervention, with early effects smaller, centering at event time is not enough.",
                     " The standard event-time regression centers everything on relative time, estimating a single coefficient ",
                     tags$em("\u03bc"), tags$sub("l"), " for each relative period ", tags$em("l"),
                     ". The issue is that OLS still has to solve a system where different cohorts contribute to each ",
                     "relative-time bin simultaneously. The resulting coefficient is a weighted average of the cohort-specific effects ",
                     tags$em("\u03b4"), tags$sub("e,l"),
                     ", but the weights come from OLS mechanics \u2014 they depend on cohort sizes, the variance structure, and critically on ",
                     "which other cohorts serve as effective controls at each ", tags$em("l"),
                     ". These weights can be negative."),

                   p("In our setup \u2014 early cohorts have small effects, late cohorts have large effects \u2014 a not-yet-treated late cohort ",
                     "serves as a \u201ccontrol\u201d for an early cohort at some relative time ", tags$em("l"),
                     ". But that same late cohort eventually gets treated and contributes its own (larger) effect at the same relative time. ",
                     "OLS blends these together with weights you don\u2019t control. The contamination goes both ways: the estimated coefficient at, say, ",
                     tags$em("l"), " = +3 is not a clean weighted average of \u201cthe +3 effect for each cohort\u201d \u2014 it can pick up effects from ",
                     "other relative periods for other cohorts due to the multicollinearity structure of the stacked relative-time dummies."),

                   tags$h5("Sun and Abraham\u2019s fix"),
                   p("Sun and Abraham\u2019s interaction-weighted estimator breaks this apart. You estimate fully saturated cohort \u00d7 relative-time interactions: ",
                     tags$em("\u03b4"), tags$sub("e,l"), " for each cohort ", tags$em("e"), " and relative time ", tags$em("l"),
                     ". This gives you the building blocks \u2014 the cohort-specific dynamic effects. Then you choose how to aggregate:"),
                   p(style = "text-align: center; font-style: italic; margin: 10px 0;",
                     "\u03bc\u0302", tags$sub("l"), " = \u03a3", tags$sub("e"),
                     " w", tags$sub("e"), " \u00b7 \u03b4\u0302", tags$sub("e,l")),
                   p("using known, non-negative weights (typically cohort shares in the population at relative time ", tags$em("l"), ")."),

                   tags$h5("The practical difference"),
                   p("With small early effects and large late effects, the standard event-time regression can produce pre-trend ",
                     "\u201cviolations\u201d even when none exist (because negative weights on late cohorts\u2019 post-treatment effects leak into ",
                     "early cohorts\u2019 pre-treatment bins), and can distort the post-treatment path in hard-to-predict ways. ",
                     "Sun and Abraham\u2019s decomposition keeps each cohort\u2019s trajectory clean and lets you see that early adopters have ",
                     "small effects and late adopters have large ones \u2014 and then decide how to average them transparently."),

                   div(
                     style = "background-color: #fff3cd; padding: 10px; border-left: 4px solid #ffc107; margin-top: 10px;",
                     p(style = "margin: 0; font-weight: bold;",
                       "One-line summary: centering on event time fixes the indexing problem (you\u2019re looking at the right time axis), ",
                       "but it doesn\u2019t fix the weighting problem (OLS still mixes cohorts with bad weights when effects are heterogeneous). ",
                       "Sun and Abraham fix the weighting. In this toy example, this bias is zero or small, but in real-life settings it could be larger.")
                   )
                 )
               ),

               # Warning message for multiple interventions
               conditionalPanel(
                 condition = "input.num_shocks != '1'",
                 div(
                   style = "background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 20px 0;",
                   p("Event study analysis is only available for single intervention scenarios.")
                 )
               )
             ),

             # ----- TAB 3: What If? -----
             tabPanel(
               "What If?",
               div(style = "margin-top: 15px;"),

               div(
                 style = "background-color: #fff3cd; padding: 12px; border-left: 4px solid #ffc107; margin-bottom: 20px;",
                 p(style = "margin: 0; font-size: 15px;",
                   tags$strong("Instructions: "),
                   "For each scenario, change the settings on the left panel as described, then examine the results in the Panel Regressions and Event Study tabs. ",
                   "Try to predict what will happen ", tags$em("before"), " you look!")
               ),

               # ── Part 1: Panel Models ──
               tags$h4(style = "border-bottom: 2px solid #007bff; padding-bottom: 5px;",
                       "Part 1: Panel Models — POLS vs FE vs FD vs Dyn FD"),

               # Scenario 1
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 1: Different country baselines"),
                 p(tags$strong("Settings: "), "Timing = simultaneous, Effect = homogeneous, Dynamic = instant, Noise = low, Year FE = off, Trends = off"),
                 p(tags$strong("Compare: "), "POLS coefficient vs FE coefficient. Are they the same? Why or why not?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 2
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 2: Dynamic buildup"),
                 p(tags$strong("Settings: "), "Same as Scenario 1, but Dynamic = gradual (50%, 75%, 100%)"),
                 p(tags$strong("Compare: "), "FD contemporaneous vs FE vs Dyn FD cumulative. Which is largest? Which captures the full effect?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 3
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 3: Global trend without year FE"),
                 p(tags$strong("Settings: "), "Same as Scenario 1, but Global trend = on, Year FE = off"),
                 p(tags$strong("Then: "), "Turn Year FE = on. What changes? Why?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 4
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 4: Treatment reversal"),
                 p(tags$strong("Settings: "), "Same as Scenario 1, but Reversal = on"),
                 p(tags$strong("Compare: "), "FE vs FD. Which is more affected by the reversal? Why does FE get diluted?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 5
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 5: High noise"),
                 p(tags$strong("Settings: "), "Same as Scenario 1, but Noise = high. Re-run a few times."),
                 p(tags$strong("Observe: "), "How do the standard errors change? Do the coefficients stay close to the true effect?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               div(style = "margin: 25px 0;"),

               # ── Part 2: Event Study ──
               tags$h4(style = "border-bottom: 2px solid #28a745; padding-bottom: 5px;",
                       "Part 2: Event Study — TWFE vs Sun & Abraham"),

               # Scenario 6
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 6: The clean case"),
                 p(tags$strong("Settings: "), "Timing = simultaneous, Effect = homogeneous, Dynamic = instant"),
                 p(tags$strong("Observe: "), "Do TWFE and SA agree? What do the pre-treatment coefficients look like?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 7
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 7: Staggered timing, homogeneous effects"),
                 p(tags$strong("Settings: "), "Change Timing = staggered, keep Effect = homogeneous"),
                 p(tags$strong("Observe: "), "Does TWFE still agree with SA? Does staggering alone cause bias?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 8
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 8: The classic problem — staggered + heterogeneous"),
                 p(tags$strong("Settings: "), "Timing = staggered, Effect = heterogeneous (early_smaller)"),
                 p(tags$strong("Observe: "), "Now compare TWFE vs SA. Where do they diverge? Look at Graph D (cohort-specific effects) \u2014 what do you see?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 9
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 9: Staggered + heterogeneous + dynamic buildup"),
                 p(tags$strong("Settings: "), "Same as Scenario 8, but Dynamic = gradual"),
                 p(tags$strong("Observe: "), "Does the divergence between TWFE and SA get worse? Why might dynamic effects compound the problem?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               # Scenario 10
               div(style = "background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
                 tags$h5("Scenario 10: Levels vs changes"),
                 p(tags$strong("Settings: "), "Dynamic = gradual, any timing"),
                 p(tags$strong("Compare: "), "The treatment effect plot (levels) vs the dynamic effect plot (changes). How do they relate to each other?"),
                 div(style = "border: 1px dashed #aaa; padding: 10px; min-height: 30px; color: #999; font-style: italic;",
                     "Your answer: ___")
               ),

               div(style = "margin: 25px 0;"),

               # ── Part 3: Key Takeaway ──
               tags$h4(style = "border-bottom: 2px solid #dc3545; padding-bottom: 5px;",
                       "Key Takeaway"),

               div(style = "background: #fff; border: 2px solid #dc3545; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                 p(style = "font-size: 15px; font-weight: bold; color: #dc3545; margin-bottom: 10px;",
                   "Based on all the scenarios above, what is the ONE setting combination that breaks TWFE?"),
                 div(style = "border: 1px dashed #aaa; padding: 15px; min-height: 50px; color: #999; font-style: italic;",
                     "Discuss in class: ___"),
                 p(style = "margin-top: 15px; font-weight: bold; color: #dc3545;",
                   "When does it matter in practice? What should applied researchers do?"),
                 div(style = "border: 1px dashed #aaa; padding: 15px; min-height: 50px; color: #999; font-style: italic;",
                     "Discuss in class: ___")
               )
             )
           )
    )
  ),

  # Footer
  fluidRow(
    column(12,
           tags$hr(),
           downloadButton("downloadData", "Download Data"),
           textOutput("warning_message"),
           tags$div(
             style = "margin-top: 20px; margin-bottom: 30px;",
             tags$p("Full app code available at: ",
                    tags$a(href = "https://github.com/gabors-data-analysis/da_interactive/tree/main/did_simulation",
                           "GitHub Repository",
                           target = "_blank"))
           )
    )
  )
)
