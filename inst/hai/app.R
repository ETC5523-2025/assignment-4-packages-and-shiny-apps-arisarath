library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(shinythemes)
library(plotly)

data("hai_data", package = "hihai")
data("hai_comparison", package = "hihai")

hai_individual <- hai_data |>
  filter(hai_type != "All")
LEVELS <- c("UTI", "HAP", "SSI", "CDI", "BSI", "All")

pal_hai <- c("BSI" = "#DC267F",
             "HAP" = "#FE6100",
             "CDI" = "#FFB000",
             "SSI" = "#009988",
             "UTI" = "#33BBEE")
pal_comparison <- c("German PPS" = "#E8384F",
                    "ECDC PPS (EU/EEA)" = "#4A90E2")

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  navbarPage(
    title = "HiHai explorer",
    theme = shinytheme("flatly"),
    tabPanel("Getting started",
             fluidPage(
               fluidRow(
                 column(12,
                        div(style="text-align:center;padding:30px 0 20px 0;",
                            h1(style="color:#2c3e50;font-weight:700;margin-bottom:10px;", "HiHai Explorer"),
                            h3(style="color:#34495e;font-weight:400;", "Healthcare-Associated Infections in Germany (2011)")
                        ),
                        div(style="background:#ffffff;padding:25px;border-radius:10px;margin:20px auto;max-width:1000px;box-shadow:0 2px 8px rgba(0,0,0,0.1);",
                            h3(style="color:#2c3e50;border-bottom:3px solid #9ad4d6;padding-bottom:10px;", "📚 Welcome"),
                            p(style="font-size:16px;line-height:1.8;",
                              " Approximately ", tags$b("1 in 25 hospitalized patients"),
                              " in Germany are affected by HAIs, causing over ", tags$b("478,000 cases"),
                              " and ", tags$b("16,000 deaths"), " annually."
                            ),
                            p(style="font-size:16px;line-height:1.8;",
                              "This interactive application in", tags$code("hihai"), "package helps you explore the burden of Healthcare-Associated Infections (HAIs) in Germany using data from the ",
                              tags$b("2011 ECDC Point Prevalence Survey."),
                            ),
                            h3(style="color:#2c3e50;border-bottom:3px solid #9ad4d6;padding-bottom:10px;padding-top:10px;", "🦠 What are HAIs?"),
                            p(style="font-size:15px;line-height:1.8;",
                              "Healthcare-Associated Infections (HAIs) are infections that patients acquire while receiving medical treatment in a healthcare facility."),
                            p(style="font-size:15px;line-height:1.8;",
                              tags$b("This app focuses on five main types:"),
                              tags$li("Urinary tract infection (UTI)"),
                              tags$li("Pneumonia (HAP)"),
                              tags$li("Surgical site infection (SSI)"),
                              tags$li("Clostridioides difficile infection (CDI)"),
                              tags$li("Bloodstream infection (BSI)")
                              ),
                            h3(style="color:#2c3e50;border-bottom:3px solid #9ad4d6;padding-bottom:10px;padding-top:10px;", "💡️ Understanding the Metrics"),
                            p(style="font-size:15px;line-height:1.8;",
                              tags$li(tags$b("Cases:"), "Annual number of infections showing frequency"),
                              tags$li(tags$b("Deaths:"), "Deaths caused by HAIs showing mortality impact"),
                              tags$li(tags$b("DALYs:"), "Disability-adjusted life years (YLL + YLD) showing overall burden"),
                              tags$li(tags$b("YLLs:"), "Years of life lost showing mortality"),
                              tags$li(tags$b("YLDs:"), "Years lived with disability showing morbidity"),
                              ),
                            h3(style="color:#2c3e50;border-bottom:3px solid #9ad4d6;padding-bottom:10px;padding-top:10px;", "🔍 App Guide"),
                            p(style="font-size:18px;line-height:1.8;",
                              tags$b("Tab1 : Explore"),
                              tags$li(tags$b("What:"),"Bar chart display frequency of HAI"),
                              tags$li(tags$b("Use:"),"Compare infections by different metrics"),
                              tags$li(tags$b("Learn:"),"Which HAIs are most common and most harmful"),
                              ),
                            p(style="font-size:18px;line-height:1.8;",
                              tags$b("Tab2 : Severity"),
                              tags$li(tags$b("What:"),"Bubble chart show cases, deaths, and burden"),
                              tags$li(tags$b("Use:"),"Visualise frequency vs severity"),
                              tags$li(tags$b("Learn:"),"Which HAIs should be prioritised"),
                              ),
                            p(style="font-size:18px;line-height:1.8;",
                              tags$b("Tab3 : Comparison"),
                              tags$li(tags$b("What:"),"Bar chart demonstrate Germany vs EU/EEA"),
                              tags$li(tags$b("Use:"),"Compare population-level burden "),
                              tags$li(tags$b("Learn:"),"How Germany perform comparing to international average"),
                              ),
                            h3(style="color:#2c3e50;border-bottom:3px solid #9ad4d6;padding-bottom:10px;padding-top:10px;", "🗂 Data source"),
                            p(style="font-size:18px;line-height:1.8;",
                              tags$i("Zacher et al. (2019). Application of a new methodology and R package reveals a high burden of healthcare-associated infections (HAI) in Germany compared to the average in the European Union/European Economic Area, 2011 to 2012. Eurosurveillance, 24(46)."),
                              tags$a("Click",
                                href="https://doi.org/10.2807/1560-7917.ES.2019.24.46.1900135",
                                     target="_blank"
                                )
                            )
                            )
                        )
                 )
               )

    ),

    tabPanel("Explore",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("metric", "Metric",
                             choices = c("Cases"="cases","Deaths"="deaths","DALYs"="dalys",
                                         "YLLs (mortality)"="yll","YLDs (disability)"="yld"),
                             selected = "cases"),
                 checkboxGroupInput("hai_filter", "HAI types",
                                    choices = setNames(hai_individual$hai_type, hai_individual$hai_name),
                                    selected = hai_individual$hai_type),
                 checkboxInput("show_ci", "Show 95% UI (bars)", TRUE),
                 hr(),
                 helpText("Change metric and filter HAIs above.")
               ),
               mainPanel(
                 h3("Frequency of HAIs", style="text-align:center;margin-top:20px;"),
                 br(),
                 plotOutput("bar_plot", height = 420),
                 br(),
                 DTOutput("tbl_explore"),
                 br(),
                 div(style = "background:#ffffff ;padding:12px;border-radius:6px;",
                     h4("How to read"),
                     tags$li(tags$b("X-axis:"), "HAI types"),
                     tags$li(tags$b("Y-axis:"), "Change according to selected metric"),
                     tags$li(tags$b("Order:"), "Ranked from highest to lowest"),
                     tags$li(tags$b("Error bars:"), "Show 95% uncertainty intervals"),
                     tags$li(tags$b("Color:"), "Indicate different HAI types"),
                     tags$li(tags$b("Table:"), "Show actual number. Click column headers to sort"),
                     uiOutput("explore_summary")
                 )
               )
             )
    ),
    tabPanel("Severity",
             div(style="margin-left:100px ; margin-right:100px; padding:20px",
                h3("Severity of HAIs", style="text-align:center;margin-top:20px;"),
                plotlyOutput("bubble_plot", height = 600),
                br(),
                div(style="background:#ffffff;padding:14px;border-radius:6px;margin:20px;",
                    h4("How to read"),
                    tags$li(tags$b("X-axis:"), "Annual number of cases tell how common the infection is"),
                    tags$li(tags$b("Y-axis:"), "Annual attributable deaths tell how severe the infection is"),
                    tags$li(tags$b("Bubble size:"), "Disability-Adjusted Life Years (DALYs) combines mortality and morbidity"),
                    tags$li(tags$b("Color:"), "Indicate different HAI types"),
                    br(),
                    h4("Key findings"),
                    tags$p(style="margin-bottom:10px;line-height:1.6;",
                           "UTIs are very common but have moderate severity, while HAP and BSI are less frequent but cause significantly more deaths and DALYs per case."
                    ),
                    tags$p(style="margin-bottom:10px;line-height:1.6;",
                           tags$b("Example:"), " BSI has only 26,976 cases but 3,905 deaths (14.5% fatality rate), while UTI has 214,150 cases but 3,664 deaths (1.7% fatality rate). Despite having 8 times fewer cases, BSI causes similar total deaths!"
                    )
                ),
             )
    ),
    tabPanel("Comparison",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("cmp_metric",
                             "Metric",
                             choices = c("Cases"="cases",
                                         "Deaths"="deaths",
                                         "DALYs"="dalys"),
                             selected = "cases"),
                 sliderInput("population_scale",
                             "Population scale",
                             min = 1000,
                             max = 100000,
                             value = 100000,
                             step = 1000)
               ),
               mainPanel(
                 h3("GERMANY VS EU/EEA", style="text-align:center;margin-top:20px;"),
                 br(),
                 plotOutput("cmp_plot", height = 520),
                 br(),
                 uiOutput("interpretation_text")
               )
             )
    )
  )
)

server <- function(input, output, session) {

  filtered_hai <- reactive({
    req(input$hai_filter)
    hai_individual |> filter(hai_type %in% input$hai_filter) |>
      mutate(hai_type = factor(hai_type, levels = LEVELS))
  })

  metric_axis_label <- function(m) {
    switch(m,
           "cases"  = "Number of Cases",
           "deaths" = "Attributable Deaths",
           "dalys"  = "DALYs (burden)",
           "yll"    = "YLLs (Years of Life Lost)",
           "yld"    = "YLDs (Years Lived with Disability)"
    )
  }

  output$bar_plot <- renderPlot({
    d  <- filtered_hai()
    met <- input$metric
    lo  <- paste0(met, "_lower")
    hi  <- paste0(met, "_upper")

    y_lab <- metric_axis_label(met)

    p <- ggplot(d, aes(x = reorder(hai_type, -.data[[met]]), y = .data[[met]], fill = hai_type)) +
      geom_col(width = 0.6, alpha = 0.9) +
      scale_fill_manual(values = pal_hai, guide = "none") +
      scale_y_continuous(labels = comma) +
      labs(x = "HAI type", y = y_lab)

    if (isTRUE(input$show_ci)) {
      p <- p + geom_errorbar(aes(ymin = .data[[lo]], ymax = .data[[hi]]),
                             width = 0.2, alpha = 0.7)
    }
    p + theme_minimal(base_size = 13) +
      theme(
        axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
        plot.background = element_rect(color = "#9ad4d6", fill = NA, linewidth = 1)
      )
  })

  output$tbl_explore <- renderDT({
    filtered_hai() |>
      select(hai_type, cases, deaths, dalys, yll, yld) |>
      datatable(options = list(pageLength = 8, dom = 'tip'),
                rownames = FALSE,
                colnames = c("HAI","Cases","Deaths","DALYs","YLLs","YLDs"),
                selection = 'none',) |>
      formatCurrency(columns = 2:6, currency = "", digits = 0, mark = ",")
  })

  output$explore_summary <- renderUI({
    d <- hai_individual
    met <- req(input$metric)
    lab <- metric_text(met)

    total_val <- sum(d[[met]], na.rm = TRUE)
    req(total_val > 0)

    ranked <- d |>
      arrange(desc(.data[[met]])) |>
      mutate(share = 100 * .data[[met]] / total_val)

    lines <- lapply(seq_len(nrow(ranked)), function(i) {
      nm <- ranked$hai_name[i]
      ab <- ranked$hai_type[i]
      v  <- ranked[[met]][i]
      sh <- ranked$share[i]

      tags$p(
        tags$b(sprintf("%s rank", ordinal_en(i))), " is ",
        tags$b(sprintf("%s (%s)", nm, ab)), " with ",
        tags$b(scales::comma(v)), " ", tags$b(lab$noun),
        ", accounting for ", tags$b(sprintf("%.1f%%", sh)),
        " of ", tags$b(lab$total), "."
      )
    })

    div(tags$h4("Key findings"),
        tags$p(
        tags$b(lab$lead),
        sprintf(": total = %s %s.", scales::comma(total_val), lab$noun)
      ),
      lines
    )
  })

  metric_text <- function(m) {
    switch(m,
           "cases"  = list(noun = "cases",  total = "total infections",
                           lead = "When we look at cases"),
           "deaths" = list(noun = "deaths", total = "total mortality",
                           lead = "When we look at deaths"),
           "dalys"  = list(noun = "DALYs",  total = "total disease burden",
                           lead = "When we look at DALYs"),
           "yll"    = list(noun = "YLLs",   total = "total YLLs",
                           lead = "When we look at YLLs"),
           "yld"    = list(noun = "YLDs",   total = "total YLDs",
                           lead = "When we look at YLDs")
    )
  }

  ordinal_en <- function(n) {
    if (n %% 100L %in% 11:13) return(paste0(n, "th"))
    suffix <- c("th","st","nd","rd","th","th","th","th","th","th")[(n %% 10L) + 1L]
    paste0(n, suffix)
  }

  # Severity
  output$bubble_plot <- renderPlotly({
    plot_data <- hai_individual |>
      mutate(
        hai_type = factor(hai_type, levels = LEVELS),
        hover_text = paste0(
          "<b>", hai_name, "</b><br>",
          "Cases: ", format(cases, big.mark=","),
          " (", format(cases_lower, big.mark=","), " - ", format(cases_upper, big.mark=","), ")<br>",
          "Deaths: ", format(deaths, big.mark=","),
          " (", format(deaths_lower, big.mark=","), " - ", format(deaths_upper, big.mark=","), ")<br>",
          "DALYs: ", format(dalys, big.mark=","),
          " (", format(dalys_lower, big.mark=","), " - ", format(dalys_upper, big.mark=","), ")"
        )
      )

    p <- ggplot(plot_data, aes(x = cases, y = deaths, size = dalys, color = hai_type, text = hover_text)) +
      geom_point(alpha = 0.9, stroke = 1, color = "black", aes(fill = hai_type), shape = 21) +
      geom_text(aes(label = hai_type), color = "black", size = 4, fontface = "bold", show.legend = FALSE) +
      scale_size_continuous(range = c(10, 30), labels = comma, guide = "none") +
      scale_fill_manual(values = pal_hai, guide = "none") +
      scale_x_continuous(labels = comma, limits = c(0, 250000), expand = c(0.025, 0)) +
      scale_y_continuous(labels = comma, limits = c(0, 5000), expand = c(0.02, 0)) +
      labs(x = "Annual Number of Cases",
           y = "Annual Attributable Deaths") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"),
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
      )

    ggplotly(p, tooltip = "text") |>
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        xaxis = list(showline = TRUE, linewidth = 1, linecolor = "black", mirror = TRUE),
        yaxis = list(showline = TRUE, linewidth = 1, linecolor = "black", mirror = TRUE)
      ) |>
      config(displayModeBar = FALSE)
  })
  # Comparison
  scaled_comparison <- reactive({
    req(input$population_scale)
    scale_factor <- input$population_scale / 100000
    hai_comparison |>
      filter(hai_type != "All", sample %in% c("German PPS", "ECDC PPS (EU/EEA)")) |>
      mutate(
        cases_scaled = cases_100k * scale_factor,
        cases_lower_scaled = cases_lower * scale_factor,
        cases_upper_scaled = cases_upper * scale_factor,
        deaths_scaled = deaths_100k * scale_factor,
        deaths_lower_scaled = deaths_lower * scale_factor,
        deaths_upper_scaled = deaths_upper * scale_factor,
        dalys_scaled = dalys_100k * scale_factor,
        dalys_lower_scaled = dalys_lower * scale_factor,
        dalys_upper_scaled = dalys_upper * scale_factor
      )
  })

  output$cmp_plot <- renderPlot({
    req(input$cmp_metric)

    met <- paste0(input$cmp_metric, "_scaled")
    lo  <- paste0(input$cmp_metric, "_lower_scaled")
    hi  <- paste0(input$cmp_metric, "_upper_scaled")

    d <- scaled_comparison() |>
      mutate(hai_type = factor(hai_type, levels = LEVELS),
             sample   = factor(sample, levels = c("German PPS", "ECDC PPS (EU/EEA)")))

    ggplot(d, aes(x = hai_type, y = .data[[met]], fill = sample)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.6, alpha = 0.9) +
      geom_errorbar(aes(ymin = .data[[lo]], ymax = .data[[hi]]),
                    position = position_dodge(width = 0.75), width = 0.2, linewidth = 0.4) +
      scale_fill_manual(values = pal_comparison,
                        labels = c("German (PPS)", "EU/EEA Average")) +
      scale_y_continuous(labels = comma) +
      labs(x = "HAI type",
           y = paste(tools::toTitleCase(input$cmp_metric), "per", format(input$population_scale, big.mark=","), "population"),
           fill = "Sample") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 10)),
            axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
            plot.background = element_rect(color = "#9ad4d6", fill = NA, linewidth = 1)
      )
  })

  output$interpretation_text <- renderUI({
    req(input$cmp_metric, input$population_scale)
    d <- scaled_comparison() |>
      select(hai_type, sample, contains("_scaled")) |>
      pivot_wider(
        names_from = sample,
        values_from = contains("_scaled")
      )

    met_german <- paste0(input$cmp_metric, "_scaled_German PPS")
    met_ecdc <- paste0(input$cmp_metric, "_scaled_ECDC PPS (EU/EEA)")

    d <- d |>
      mutate(
        difference = .data[[met_german]] - .data[[met_ecdc]],
        pct_diff = (difference / .data[[met_ecdc]]) * 100
      )

    metric_name <- case_when(
      input$cmp_metric == "cases" ~ "cases",
      input$cmp_metric == "deaths" ~ "deaths",
      input$cmp_metric == "dalys" ~ "DALYs"
    )

    comparisons <- d |>
      mutate(
        comparison_text = case_when(
          abs(pct_diff) < 10 ~ paste0(hai_type, ": similar rates (Germany: ",
                                      round(.data[[met_german]], 1), " vs EU/EEA: ",
                                      round(.data[[met_ecdc]], 1), ")"),
          difference > 0 ~ paste0(hai_type, ": Germany ", round(pct_diff, 0),
                                  "% higher (", round(.data[[met_german]], 1),
                                  " vs ", round(.data[[met_ecdc]], 1), ")"),
          difference < 0 ~ paste0(hai_type, ": Germany ", round(abs(pct_diff), 0),
                                  "% lower (", round(.data[[met_german]], 1),
                                  " vs ", round(.data[[met_ecdc]], 1), ")")
        )
      ) |>
      arrange(desc(abs(pct_diff))) |>
      pull(comparison_text)

    div(
      style="background:#ffffff;padding:14px;border-radius:6px;margin-top:20px;",
      h4(paste0("Key findings (per ", format(input$population_scale, big.mark=","), " population):")),
      tags$p(
        paste0("Comparing German PPS to EU/EEA rates for ", metric_name, ":"),
        style="margin-top:8px;margin-bottom:8px;"
      ),
      tags$ul(
        style="margin-bottom:0;",
        lapply(comparisons, function(x) tags$li(x))
      ),
      tags$p(style="margin:0;",
             tags$small(tags$i("Note: Germany has higher population burden despite better hospital infection control due to high hospitalization rates."))
      )
    )
  })
}

shinyApp(ui, server)
