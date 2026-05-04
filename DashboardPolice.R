library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# =====================================================
# 1) LOAD + CLEAN DATA
# =====================================================
df <- read.csv(
  "uk_violence_data.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

names(df) <- trimws(names(df))

df <- df %>%
  mutate(
    Period = str_squish(str_remove_all(Year, "\\s*\\[note\\s*\\d+\\]")),
    Year_Ending = as.integer(str_extract(Period, "(\\d{4})(?!.*\\d{4})"))
  ) %>%
  mutate(across(
    c(
      Violence_Incidents_1000s,
      `Violence with injury`,
      Wounding,
      `Assault with minor injury`,
      `Violence without injury`
    ),
    ~ as.numeric(gsub(",", "", .))
  )) %>%
  arrange(Year_Ending)

# one row per year
df_year <- df %>%
  group_by(Year_Ending) %>%
  summarise(
    Violence_Total = last(Violence_Incidents_1000s),
    Violence_with_injury = last(`Violence with injury`),
    Wounding = last(Wounding),
    Assault_minor = last(`Assault with minor injury`),
    Violence_without_injury = last(`Violence without injury`),
    .groups = "drop"
  ) %>%
  arrange(Year_Ending)

# long format for component views
df_long <- df_year %>%
  select(
    Year_Ending,
    Violence_Total,
    Violence_with_injury,
    Wounding,
    Assault_minor,
    Violence_without_injury
  ) %>%
  pivot_longer(
    cols = -Year_Ending,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric_Label = recode(
      Metric,
      "Violence_Total" = "Violence with or without injury",
      "Violence_with_injury" = "Violence with injury",
      "Wounding" = "Wounding",
      "Assault_minor" = "Assault with minor injury",
      "Violence_without_injury" = "Violence without injury"
    )
  )

latest_year <- max(df_year$Year_Ending, na.rm = TRUE)

latest_total <- df_year %>%
  filter(Year_Ending == latest_year) %>%
  pull(Violence_Total)

prev_total <- df_year %>%
  filter(Year_Ending == sort(unique(df_year$Year_Ending), decreasing = TRUE)[2]) %>%
  pull(Violence_Total)

latest_yoy <- round((latest_total - prev_total) / prev_total * 100, 0)

peak_total <- max(df_year$Violence_Total, na.rm = TRUE)
reduction_from_peak <- round((1 - latest_total / peak_total) * 100, 0)

latest_structure <- df_year %>%
  filter(Year_Ending == latest_year) %>%
  select(Violence_without_injury, Assault_minor, Wounding) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Category",
    values_to = "Incidents"
  ) %>%
  mutate(
    Category = recode(
      Category,
      "Violence_without_injury" = "Violence without injury",
      "Assault_minor" = "Assault with minor injury",
      "Wounding" = "Wounding"
    )
  ) %>%
  distinct(Category, .keep_all = TRUE) %>%
  mutate(
    Share = Incidents / sum(Incidents)
  ) %>%
  arrange(desc(Incidents))


offence_lookup <- c(
  "Total violence" = "Violence_Total",
  "Violence with injury" = "Violence_with_injury",
  "Violence without injury" = "Violence_without_injury",
  "Assault with minor injury" = "Assault_minor",
  "Wounding" = "Wounding"
)

# =====================================================
# 2) PLOT THEME
# =====================================================
plot_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# =====================================================
# 3) UI
# =====================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style="display:flex; align-items:center;",
      
      tags$img(
        src="logo.png",
        height="32px",
        style="margin-right:12px;"
      ),
      
      tags$span(
        "Dashboard Prototype",
        style="font-size:18px; font-weight:600;"
      )
    ),
    titleWidth = 420
  ),
  
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      
      menuItem("Violence trends", icon = icon("shield-alt"),
               menuSubItem("Long-term trend", tabName = "long_term"),
               menuSubItem("Recent trend", tabName = "recent_trend"),
               menuSubItem("Violence structure", tabName = "structure"),
               menuSubItem("Component trends", tabName = "components")
      ),
      
      menuItem("Data notes", tabName = "notes", icon = icon("info-circle"))
    ),
    
    br(),
    
    selectInput(
      "source_filter",
      "Data source",
      choices = c("CSEW", "Police Recorded Crime"),
      selected = "CSEW"
    ),
    
    selectInput(
      "offence_type",
      "Offence type",
      choices = c(
        "Total violence",
        "Violence with injury",
        "Violence without injury",
        "Assault with minor injury",
        "Wounding"
      ),
      selected = "Total violence"
    ),
    
    sliderInput(
      "year_range",
      "Year range",
      min = min(df_year$Year_Ending, na.rm = TRUE),
      max = max(df_year$Year_Ending, na.rm = TRUE),
      value = c(
        max(df_year$Year_Ending, na.rm = TRUE) - 10,
        max(df_year$Year_Ending, na.rm = TRUE)
      ),
      step = 1,
      sep = ""
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .small-box, .box {
          border-radius: 12px;
        }
        .box-header.with-border {
          border-bottom: 0;
        }
        .skin-blue .main-sidebar {
          background-color: #1f1f1f !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li > a,
        .skin-blue .main-sidebar .treeview-menu > li > a {
          color: #ffffff !important;
        }
        .skin-blue .main-header .logo {
          background-color: #102f59 !important;
          color: #ffffff !important;
        }
        .skin-blue .main-header .navbar {
          background-color: #102f59 !important;
        }
      "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("kpi_total", width = 3),
          valueBoxOutput("kpi_yoy", width = 3),
          valueBoxOutput("kpi_peak", width = 3),
          valueBoxOutput("kpi_latest_year", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Selected offence trend",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotOutput("overview_trend_plot", height = 360))
          ),
          box(
            title = "Selected offence summary",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotOutput("overview_structure_plot", height = 360))
          )
        )
      ),
      
      tabItem(
        tabName = "long_term",
        fluidRow(
          box(
            title = "Long-term trend",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotOutput("long_term_plot", height = 520))
          )
        )
      ),
      
      tabItem(
        tabName = "recent_trend",
        fluidRow(
          box(
            title = "Recent trend",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotOutput("recent_plot", height = 520))
          )
        )
      ),
      
      tabItem(
        tabName = "structure",
        fluidRow(
          box(
            title = "Violence structure",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotOutput("structure_plot", height = 500))
          )
        )
      ),
      
      tabItem(
        tabName = "components",
        fluidRow(
          box(
            title = "Violence component trends",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            withSpinner(plotOutput("component_plot", height = 520))
          )
        )
      ),
      
      tabItem(
        tabName = "notes",
        fluidRow(
          box(
            title = "How to interpret this dashboard",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tags$ul(
              tags$li("This prototype is built directly from the violence dataset provided."),
              tags$li("The Year field contains note markers such as [note 6] and [note 7], which are removed during cleaning before extracting the ending year."),
              tags$li("CSEW measures victimisation and is more suitable for long-term violence trend analysis."),
              tags$li("The dataset includes gaps in survey coverage, including missing pandemic-era periods."),
              tags$li("This dashboard is focused on violence only; a broader crime dashboard would require additional offence series."),
              tags$li("A production version would include a source toggle so users can separate survey-based trend analysis from police recorded operational indicators.")
            )
          )
        )
      )
    )
  )
)

# =====================================================
# 4) SERVER
# =====================================================
server <- function(input, output, session) {
  
  selected_metric <- reactive({
    unname(offence_lookup[input$offence_type])
  })
  
  filtered_series <- reactive({
    req(selected_metric())
    
    df_long %>%
      filter(
        Metric == selected_metric(),
        Year_Ending >= input$year_range[1],
        Year_Ending <= input$year_range[2]
      ) %>%
      arrange(Year_Ending)
  })
  
  filtered_total <- reactive({
    sum(filtered_series()$Value, na.rm = TRUE)
  })
  
  filtered_peak_value <- reactive({
    max(filtered_series()$Value, na.rm = TRUE)
  })
  
  filtered_latest_year <- reactive({
    max(filtered_series()$Year_Ending, na.rm = TRUE)
  })
  
  filtered_first_year <- reactive({
    min(filtered_series()$Year_Ending, na.rm = TRUE)
  })
  
  filtered_yoy <- reactive({
    df_plot <- filtered_series()
    
    if (nrow(df_plot) < 2) return(NA_real_)
    
    current <- df_plot %>% slice_tail(n = 1) %>% pull(Value)
    previous <- df_plot %>% slice(n() - 1) %>% pull(Value)
    
    if (is.na(current) || is.na(previous) || previous == 0) return(NA_real_)
    
    round((current - previous) / previous * 100, 0)
  })
  
  filtered_drop_from_peak <- reactive({
    latest_val <- filtered_series() %>%
      slice_tail(n = 1) %>%
      pull(Value)
    
    peak_val <- filtered_peak_value()
    
    if (length(latest_val) == 0 || is.na(latest_val) || is.na(peak_val) || peak_val == 0) {
      return(NA_real_)
    }
    
    round((1 - latest_val / peak_val) * 100, 0)
  })
  
  output$kpi_total <- renderValueBox({
    valueBox(
      value = paste0(comma(filtered_total()), "k"),
      subtitle = paste(
        "Estimated violence incidents | Year ending",
        max(filtered_series()$Year_Ending, na.rm = TRUE)
      ),
      icon = icon("shield-alt"),
      color = "blue"
    )
  })
  
  output$kpi_yoy <- renderValueBox({
    yoy_val <- filtered_yoy()
    
    valueBox(
      value = ifelse(
        is.na(yoy_val),
        "N/A",
        paste0(ifelse(yoy_val > 0, "+", ""), yoy_val, "%")
      ),
      subtitle = paste("YoY change |", filtered_latest_year()),
      icon = icon("percent"),
      color = "navy"
    )
  })
  
  output$kpi_peak <- renderValueBox({
    peak_drop <- filtered_drop_from_peak()
    
    valueBox(
      value = ifelse(is.na(peak_drop), "N/A", paste0(peak_drop, "%")),
      subtitle = "Drop from peak within selected range",
      icon = icon("arrow-down"),
      color = "purple"
    )
  })
  
  output$kpi_latest_year <- renderValueBox({
    valueBox(
      value = paste0(input$year_range[1], "–", input$year_range[2]),
      subtitle = "Selected year range",
      icon = icon("calendar"),
      color = "teal"
    )
  })
  
  output$overview_trend_plot <- renderPlot({
    req(selected_metric())
    
    plot_df <- df_long %>%
      filter(
        Metric == selected_metric(),
        Year_Ending >= input$year_range[1],
        Year_Ending <= input$year_range[2]
      ) %>%
      select(Year_Ending, Value) %>%
      distinct() %>%
      complete(
        Year_Ending = seq(input$year_range[1], input$year_range[2], by = 1)
      ) %>%
      arrange(Year_Ending)
    
    req(nrow(plot_df) > 0)
    
    ggplot(plot_df, aes(x = Year_Ending, y = Value)) +
      geom_line(linewidth = 1.2, na.rm = FALSE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_x_continuous(
        breaks = seq(input$year_range[1], input$year_range[2], by = 1)
      ) +
      scale_y_continuous(labels = comma) +
      labs(
        title = input$offence_type,
        subtitle = "England & Wales | Crime Survey for England and Wales (CSEW)
Pandemic survey disruption (2021–2022)",
        x = "Period end year",
        y = "Incidents (thousands)"
      ) +
      plot_theme
  })
  
  output$overview_structure_plot <- renderPlot({
    
    if (input$offence_type == "Total violence") {
      
      plot_df <- df_year %>%
        filter(
          Year_Ending >= input$year_range[1],
          Year_Ending <= input$year_range[2]
        ) %>%
        summarise(
          `Violence without injury` = sum(Violence_without_injury, na.rm = TRUE),
          `Assault with minor injury` = sum(Assault_minor, na.rm = TRUE),
          `Wounding` = sum(Wounding, na.rm = TRUE)
        ) %>%
        pivot_longer(
          cols = everything(),
          names_to = "Metric_Label",
          values_to = "Value"
        ) %>%
        arrange(desc(Value))
      
      ggplot(plot_df, aes(x = Value, y = reorder(Metric_Label, Value))) +
        geom_col(width = 0.65) +
        geom_text(
          aes(label = paste0(comma(Value), "k")),
          hjust = -0.08,
          size = 3.7,
          fontface = "bold"
        ) +
        scale_x_continuous(
          labels = function(x) paste0(comma(x), "k"),
          expand = expansion(mult = c(0, 0.18))
        ) +
        labs(
          title = NULL,
          subtitle = paste0(
            "Breakdown of violence incidents by type | Year ending ",
            max(filtered_series()$Year_Ending, na.rm = TRUE),
            " | CSEW"
          ),
          x = "Incidents (thousands)",
          y = NULL
        ) +
        plot_theme +
        theme(
          plot.subtitle = element_text(size = 9),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 9)
        )
    } else {
      
      # ORIGINAL behaviour (bars by year)
      plot_df <- filtered_series() %>%
        arrange(desc(Year_Ending))
      
      ggplot(plot_df, aes(x = Value, y = factor(Year_Ending))) +
        geom_col(width = 0.65) +
        geom_text(
          aes(label = paste0(comma(Value), "k")),
          hjust = -0.08,
          size = 3.7,
          fontface = "bold"
        ) +
        scale_x_continuous(
          labels = function(x) paste0(comma(x), "k"),
          expand = expansion(mult = c(0, 0.18))
        ) +
        labs(
          title = NULL,
          subtitle = paste(
            input$offence_type,
            "|",
            input$year_range[1],
            "to",
            input$year_range[2],
            "| CSEW"
          ),
          x = "Incidents (thousands)",
          y = "Period end year"
        ) +
        plot_theme +
        theme(
          plot.subtitle = element_text(size = 9),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 9)
        )
    }
  })
  
  output$long_term_plot <- renderPlot({
    peak_row <- df_year %>% slice_max(Violence_Total, n = 1)
    latest_row <- df_year %>% slice_max(Year_Ending, n = 1)
    lowest_row <- df_year %>% slice_min(Violence_Total, n = 1)
    
    ggplot(df_year, aes(Year_Ending, Violence_Total)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 2.1) +
      geom_point(data = peak_row, size = 4.5) +
      geom_point(data = latest_row, size = 4.5) +
      geom_point(data = lowest_row, size = 4.5) +
      geom_text(
        data = peak_row,
        aes(label = paste0("Peak: ", comma(Violence_Total))),
        vjust = -1.1,
        fontface = "bold",
        size = 4.5
      ) +
      geom_text(
        data = latest_row,
        aes(label = paste0("Latest: ", comma(Violence_Total))),
        hjust = -0.05,
        fontface = "bold",
        size = 4.2
      ) +
      geom_text(
        data = lowest_row,
        aes(label = paste0("Low: ", comma(Violence_Total))),
        vjust = 1.5,
        fontface = "bold",
        size = 4.2
      ) +
      scale_x_continuous(
        breaks = seq(1985, 2030, by = 5),
        limits = c(min(df_year$Year_Ending, na.rm = TRUE), max(df_year$Year_Ending, na.rm = TRUE) + 5)
      ) +
      scale_y_continuous(labels = comma) +
      coord_cartesian(clip = "off") +
      labs(
        title = "Violence has fallen 74% since the mid-1990s peak",
        subtitle = "England & Wales | Crime Survey for England and Wales (CSEW)",
        x = "Year",
        y = "Incidents (thousands)"
      ) +
      plot_theme +
      theme(plot.margin = margin(10, 90, 10, 10))
  })
  
  output$recent_plot <- renderPlot({
    df_recent <- filtered_series()
    
    req(nrow(df_recent) > 0)
    
    latest_recent <- df_recent %>% slice_max(Year_Ending, n = 1)
    lowest_recent <- df_recent %>% slice_min(Value, n = 1)
    
    p <- ggplot(df_recent, aes(Year_Ending, Value)) +
      geom_point(size = 2.8)
    
    if (nrow(df_recent) > 1) {
      p <- p + geom_line(linewidth = 1.25)
    }
    
    p +
      geom_point(data = latest_recent, size = 4.3) +
      geom_point(data = lowest_recent, size = 4.3) +
      geom_text(
        data = latest_recent,
        aes(label = paste0("Latest: ", comma(Value), "k")),
        hjust = -0.05,
        fontface = "bold",
        size = 4.2
      ) +
      geom_text(
        data = lowest_recent,
        aes(label = paste0("Low: ", comma(Value), "k")),
        vjust = 1.5,
        fontface = "bold",
        size = 4.2
      ) +
      scale_y_continuous(labels = comma) +
      coord_cartesian(clip = "off") +
      labs(
        title = paste(
          input$offence_type,
          "|",
          input$year_range[1],
          "to",
          input$year_range[2]
        ),
        subtitle = "England & Wales | Crime Survey for England and Wales (CSEW)",
        x = "Period end year",
        y = "Incidents (thousands)"
      ) +
      plot_theme +
      theme(plot.margin = margin(10, 80, 10, 10))
  })
  
  
  output$structure_plot <- renderPlot({
    ggplot(latest_structure, aes(reorder(Category, Incidents), Incidents)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = paste0(round(Share * 100), "% (", comma(Incidents), "k)")),
        hjust = -0.05,
        size = 4.8
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = function(x) paste0(comma(x), "k"),
        expand = expansion(mult = c(0, 0.20))
      ) +
      labs(
        title = paste(
          "Violence structure | selected range",
          input$year_range[1],
          "to",
          input$year_range[2]
        ),
        subtitle = "England & Wales | Crime Survey for England and Wales (CSEW)",
        x = NULL,
        y = "Incidents (thousands)"
      ) +
      plot_theme
  })
  
  output$component_plot <- renderPlot({
    component_df <- df_long %>%
      filter(
        Metric != "Violence_Total",
        Year_Ending >= input$year_range[1],
        Year_Ending <= input$year_range[2]
      )
    
    ggplot(component_df, aes(Year_Ending, Value)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      facet_wrap(~Metric_Label, scales = "free_y", ncol = 2) +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste(
          "Violence structure |",
          input$year_range[1],
          "to",
          input$year_range[2]
        ),
        subtitle = "England & Wales | Crime Survey for England and Wales (CSEW)",
        x = "Period end year",
        y = "Incidents (thousands)"
      ) +
      plot_theme
  })
  
}

shinyApp(ui, server)

