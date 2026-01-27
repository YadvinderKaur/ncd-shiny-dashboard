# ============================================================
# NCD Global Health Indicator — Exam-ready Shiny App (FIXED LAYOUT)
# - NO squeezed plots
# - Widgets fill cards properly
# - Premium spacing + readable charts
# ============================================================

library(shiny)
library(bslib)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)
library(tidyr)
library(htmltools)

# -----------------------------
# 1) Read data (ROBUST PATH)
# -----------------------------
csv_candidates <- c("health_data.csv", file.path("..", "health_data.csv"))
csv_file <- csv_candidates[file.exists(csv_candidates)][1]

if (is.na(csv_file) || length(csv_file) == 0) {
  stop("health_data.csv not found. Put it in the same folder as app.R OR one level above.")
}

health <- read_csv(csv_file, show_col_types = FALSE)
names(health) <- tolower(names(health))

required_cols <- c(
  "region", "country",
  "diabetes_rate", "obesity_rate", "hypertension_rate",
  "smoking_rate", "physical_inactivity"
)

missing_cols <- setdiff(required_cols, names(health))
if (length(missing_cols) > 0) {
  stop(paste0("Missing columns in CSV: ", paste(missing_cols, collapse = ", ")))
}

num_cols <- c("diabetes_rate", "obesity_rate", "hypertension_rate", "smoking_rate", "physical_inactivity")

health <- health %>%
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(
    region  = as.character(region),
    country = as.character(country)
  )

# -----------------------------
# 2) Country name fixes for map join
# -----------------------------
name_map <- c(
  "usa" = "United States of America",
  "uk"  = "United Kingdom"
)

health <- health %>%
  mutate(
    country_match = if_else(
      tolower(country) %in% names(name_map),
      unname(name_map[tolower(country)]),
      country
    )
  )

# -----------------------------
# 3) World geometry
# -----------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    name_long = as.character(name_long),
    name = as.character(name)
  )

# -----------------------------
# 4) Pastel palette helpers
# -----------------------------
pastel_regions <- c(
  "#F7CAD0", # pastel pink
  "#BDE0FE", # pastel blue
  "#B7E4C7", # pastel green
  "#CDB4DB", # mauve
  "#FFE5B4", # warm pastel
  "#A8DADC", # aqua pastel
  "#FFC8DD", # soft rose
  "#D0F4DE"  # pale mint
)

make_region_colors <- function(regions) {
  regs <- sort(unique(as.character(regions)))
  setNames(rep(pastel_regions, length.out = length(regs)), regs)
}

# Pastel gradient for map + heat
pastel_gradient <- c("#F7CAD0", "#CDB4DB", "#BDE0FE", "#A8DADC", "#B7E4C7")

plotly_pastel_colorscale <- list(
  list(0.00, "#F7CAD0"),
  list(0.25, "#CDB4DB"),
  list(0.50, "#BDE0FE"),
  list(0.75, "#A8DADC"),
  list(1.00, "#B7E4C7")
)

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = "NCD Global Health Indicator",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  
  header = tagList(
    tags$style(HTML("
      :root{
        --navy:#213547;
        --ink:#0f172a;
        --muted:#64748b;
        --bg:#f6f7fb;
        --border:rgba(15,23,42,0.10);
      }
      body{ background: var(--bg) !important; }

      /* Navbar */
      .navbar{ background: var(--navy) !important; }
      .navbar .navbar-brand, .navbar .nav-link{ color:#fff !important; }
      .navbar .nav-link.active{ font-weight:800; border-bottom:2px solid rgba(255,255,255,0.70); }

      /* Layout spacing */
      .bslib-sidebar-layout{ gap: 22px; align-items: flex-start; }
      .bslib-grid{ gap: 22px !important; }

      /* Sidebar */
      .bslib-sidebar{
        background:#fff;
        border:1px solid var(--border);
        border-radius:18px;
        padding:18px;
        position: sticky;
        top: 12px;
      }

      /* Cards */
      .card{
        border-radius:18px !important;
        border:1px solid var(--border) !important;
        box-shadow: 0 12px 28px rgba(15,23,42,0.07);
        overflow: hidden;
        min-width: 420px; /* prevents ugly squeeze */
      }
      .card-header{
        background:#fff !important;
        border-bottom:1px solid var(--border) !important;
        font-weight:800;
        color:var(--ink);
        padding: 14px 16px !important;
      }
      .card-body{
        padding: 14px 16px !important;
      }

      /* Snapshot area - prevent scrollbars */
      .snapshot-wrap{ overflow: hidden !important; }

      /* Metric blocks */
      .metric-grid{ display:grid; grid-template-columns: 1fr; gap: 12px; }
      .metric-card{
        border:1px solid var(--border);
        border-radius:16px;
        background: linear-gradient(180deg, #ffffff, #fbfbfe);
        padding: 14px;
      }
      .metric-label{ color:var(--muted); font-size:12px; font-weight:800; text-transform: uppercase; letter-spacing:.06em; }
      .metric-value{ font-size:30px; font-weight:900; color:var(--ink); letter-spacing:-0.02em; margin-top: 2px; }

      /* Sidebar headings */
      .sidebar-step{
        color: var(--muted);
        font-weight: 900;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: .08em;
        margin-top: 6px;
      }
      .sidebar-title{
        font-size: 22px;
        font-weight: 900;
        color: var(--ink);
        margin: 6px 0 12px 0;
      }
      .small-note{ color: var(--muted); font-size: 12px; }

      /* IMPORTANT: force widgets to fill their parent */
      .viz-fill{
        height: 100%;
        width: 100%;
      }
      .viz-fill .html-widget,
      .viz-fill .plotly,
      .viz-fill .plotly html-widget,
      .viz-fill .leaflet{
        height: 100% !important;
        width: 100% !important;
      }
      .leaflet-container{ border-radius: 14px; }

      /* Make plotly text readable */
      .main-container{ padding-bottom: 28px; }
    "))
  ),
  
  sidebar = sidebar(
    width = 380,
    open = "desktop",
    
    div(class = "sidebar-step", "Step 1"),
    div(class = "sidebar-title", "Choose an indicator"),
    selectInput(
      "metric",
      label = NULL,
      choices = c(
        "Diabetes rate (%)" = "diabetes_rate",
        "Obesity rate (%)" = "obesity_rate",
        "Hypertension rate (%)" = "hypertension_rate",
        "Smoking rate (%)" = "smoking_rate",
        "Physical inactivity (%)" = "physical_inactivity"
      ),
      selected = "hypertension_rate"
    ),
    
    hr(),
    
    div(class = "sidebar-step", "Step 2"),
    div(class = "sidebar-title", "Filter"),
    selectInput("region", "Region", choices = c("All", sort(unique(health$region))), selected = "All"),
    checkboxInput("show_points", "Show country points (centroids)", value = FALSE),
    
    hr(),
    
    div(class = "sidebar-step", "Heat plot"),
    sliderInput("top_n", "Show top countries", min = 8, max = 60, value = 25, step = 1),
    
    hr(),
    div(class = "small-note", paste("Loaded:", csv_file))
  ),
  
  # -------------------------
  # Dashboard
  # -------------------------
  nav_panel(
    "Dashboard",
    
    # Row 1
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        height = "520px",
        card_header("Global snapshot"),
        card_body(
          div(class = "snapshot-wrap",
              uiOutput("snapshot_cards")
          ),
          br(),
          div(class = "small-note", "Updates based on region + indicator selection.")
        )
      ),
      
      card(
        height = "520px",
        full_screen = TRUE,
        card_header("Map"),
        card_body(
          div(class = "viz-fill",
              leafletOutput("map", height = "100%")
          )
        )
      )
    ),
    
    br(),
    
    # Row 2
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        height = "560px",
        full_screen = TRUE,
        card_header("Heat plot: Countries × indicators"),
        card_body(
          div(class = "viz-fill",
              plotlyOutput("heat", height = "100%")
          )
        )
      ),
      
      card(
        height = "560px",
        full_screen = TRUE,
        card_header("Scatter (2D): Obesity vs selected indicator"),
        card_body(
          div(class = "viz-fill",
              plotlyOutput("scatter2d", height = "100%")
          )
        )
      )
    ),
    
    br(),
    
    # Row 3
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        height = "560px",
        full_screen = TRUE,
        card_header("Scatter (3D): Obesity, Diabetes, Hypertension"),
        card_body(
          div(class = "viz-fill",
              plotlyOutput("scatter3d", height = "100%")
          )
        )
      ),
      
      card(
        height = "560px",
        full_screen = TRUE,
        card_header("Boxplot: Selected indicator by region"),
        card_body(
          div(class = "viz-fill",
              plotlyOutput("boxplot", height = "100%")
          )
        )
      )
    )
  ),
  
  # -------------------------
  # Country profile
  # -------------------------
  nav_panel(
    "Country profile",
    
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        height = "240px",
        card_header("Select country"),
        card_body(
          selectInput(
            "country_pick", "Country",
            choices = sort(unique(health$country)),
            selected = sort(unique(health$country))[1]
          ),
          div(class = "small-note", "Pick a country to view its indicator profile.")
        )
      ),
      
      card(
        height = "240px",
        card_header("Country snapshot"),
        card_body(uiOutput("country_cards"))
      )
    ),
    
    br(),
    
    layout_columns(
      col_widths = c(7, 5),
      
      card(
        height = "560px",
        full_screen = TRUE,
        card_header("Indicator profile (bar)"),
        card_body(
          div(class = "viz-fill",
              plotlyOutput("country_bar", height = "100%")
          )
        )
      ),
      
      card(
        height = "560px",
        full_screen = TRUE,
        card_header("Country data (table)"),
        card_body(DTOutput("country_table"))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  health_f <- reactive({
    df <- health
    if (!is.null(input$region) && input$region != "All") df <- df %>% filter(region == input$region)
    df
  })
  
  observeEvent(input$region, {
    df <- health_f()
    choices <- sort(unique(df$country))
    if (length(choices) == 0) choices <- sort(unique(health$country))
    updateSelectInput(session, "country_pick", choices = choices, selected = choices[1])
  }, ignoreInit = TRUE)
  
  world_joined_f <- reactive({
    df <- health_f()
    w <- world %>% left_join(df, by = c("name_long" = "country_match"))
    if (sum(!is.na(w$country)) == 0) w <- world %>% left_join(df, by = c("name" = "country_match"))
    w
  })
  
  # Snapshot cards
  output$snapshot_cards <- renderUI({
    req(input$metric)
    df <- health_f() %>% filter(!is.na(.data[[input$metric]]))
    mean_v <- if (nrow(df) > 0) mean(df[[input$metric]], na.rm = TRUE) else NA_real_
    med_v  <- if (nrow(df) > 0) median(df[[input$metric]], na.rm = TRUE) else NA_real_
    n_v    <- nrow(df)
    
    tagList(
      div(class="metric-grid",
          div(class="metric-card",
              div(class="metric-label", "Mean"),
              div(class="metric-value", ifelse(is.na(mean_v), "NA", sprintf("%.1f%%", mean_v)))
          ),
          div(class="metric-card",
              div(class="metric-label", "Median"),
              div(class="metric-value", ifelse(is.na(med_v), "NA", sprintf("%.1f%%", med_v)))
          ),
          div(class="metric-card",
              div(class="metric-label", "Countries with data"),
              div(class="metric-value", n_v)
          )
      )
    )
  })
  
  # Map
  output$map <- renderLeaflet({
    req(input$metric)
    w <- world_joined_f()
    metric <- input$metric
    vals <- w[[metric]]
    
    pal <- colorNumeric(palette = pastel_gradient, domain = vals, na.color = "transparent")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      w$name_long,
      gsub("_", " ", metric),
      ifelse(is.na(vals), "no data", sprintf("%.1f%%", vals))
    ) %>% lapply(HTML)
    
    m <- leaflet(w) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, options = leafletOptions(opacity = 0.30)) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor = ~pal(vals),
        fillOpacity = 0.90,
        weight = 0.7,
        color = "#9aa4b2",
        label = labels,
        highlightOptions = highlightOptions(weight = 2, color = "#334155", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = vals,
        title = gsub("_", " ", metric),
        position = "bottomright",
        opacity = 0.9
      )
    
    if (isTRUE(input$show_points)) {
      cent <- st_centroid(st_geometry(w))
      coords <- st_coordinates(cent)
      keep <- !is.na(vals)
      if (any(keep)) {
        m <- m %>%
          addCircleMarkers(
            lng = coords[keep, 1],
            lat = coords[keep, 2],
            radius = 4,
            stroke = FALSE,
            fillOpacity = 0.80,
            color = "#CDB4DB",
            label = labels[keep]
          )
      }
    }
    m
  })
  
  # Heat plot
  output$heat <- renderPlotly({
    req(input$metric, input$top_n)
    df <- health_f() %>%
      filter(!is.na(.data[[input$metric]])) %>%
      arrange(desc(.data[[input$metric]])) %>%
      slice_head(n = input$top_n) %>%
      select(country, all_of(num_cols))
    
    if (nrow(df) == 0) return(plotly_empty())
    
    mat <- df %>%
      pivot_longer(cols = all_of(num_cols), names_to = "indicator", values_to = "value") %>%
      mutate(
        indicator = gsub("_", " ", indicator),
        country = factor(country, levels = rev(unique(df$country)))
      )
    
    plot_ly(
      data = mat,
      x = ~indicator,
      y = ~country,
      z = ~value,
      type = "heatmap",
      colorscale = plotly_pastel_colorscale,
      hovertemplate = "<b>%{y}</b><br>%{x}: %{z:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -15, automargin = TRUE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 150, r = 20, t = 10, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Scatter 2D
  output$scatter2d <- renderPlotly({
    req(input$metric)
    df <- health_f() %>%
      filter(!is.na(obesity_rate), !is.na(.data[[input$metric]]))
    
    if (nrow(df) == 0) return(plotly_empty())
    
    cols <- make_region_colors(df$region)
    
    p <- ggplot(
      df,
      aes(
        x = obesity_rate,
        y = .data[[input$metric]],
        color = region,
        text = paste0(
          "<b>", country, "</b>",
          "<br>Region: ", region,
          "<br>Obesity: ", obesity_rate,
          "<br>", gsub("_"," ", input$metric), ": ", .data[[input$metric]]
        )
      )
    ) +
      geom_point(size = 4.2, alpha = 0.85) +
      scale_color_manual(values = cols) +
      theme_minimal(base_size = 14) +
      labs(x = "Obesity rate (%)", y = paste0(gsub("_"," ", input$metric), " (%)")) +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(title = list(text = "<b>Region</b>")),
        margin = list(l = 80, r = 20, t = 10, b = 70)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Scatter 3D
  output$scatter3d <- renderPlotly({
    df <- health_f() %>%
      filter(!is.na(obesity_rate), !is.na(diabetes_rate), !is.na(hypertension_rate))
    if (nrow(df) == 0) return(plotly_empty())
    
    cols <- make_region_colors(df$region)
    
    plot_ly(
      df,
      x = ~obesity_rate, y = ~diabetes_rate, z = ~hypertension_rate,
      type = "scatter3d", mode = "markers",
      color = ~region, colors = cols,
      marker = list(size = 5, opacity = 0.85),
      text = ~paste0(
        "<b>", country, "</b>",
        "<br>Region: ", region,
        "<br>Obesity: ", obesity_rate,
        "<br>Diabetes: ", diabetes_rate,
        "<br>Hypertension: ", hypertension_rate
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Obesity (%)"),
          yaxis = list(title = "Diabetes (%)"),
          zaxis = list(title = "Hypertension (%)")
        ),
        margin = list(l = 0, r = 0, t = 10, b = 0),
        legend = list(title = list(text = "<b>Region</b>"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    req(input$metric)
    df <- health_f() %>% filter(!is.na(.data[[input$metric]]))
    if (nrow(df) == 0) return(plotly_empty())
    
    cols <- make_region_colors(df$region)
    regs <- sort(unique(as.character(df$region)))
    
    p <- plot_ly()
    for (r in regs) {
      dfr <- df %>% filter(region == r)
      p <- p %>% add_boxplot(
        y = dfr[[input$metric]],
        name = r,
        boxpoints = "outliers",
        fillcolor = cols[[r]],
        marker = list(opacity = 0.65),
        line = list(width = 1, color = "rgba(15,23,42,0.35)")
      )
    }
    
    p %>%
      layout(
        yaxis = list(title = paste0(gsub("_"," ", input$metric), " (%)")),
        xaxis = list(title = ""),
        margin = list(l = 80, r = 20, t = 10, b = 70)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Country snapshot
  output$country_cards <- renderUI({
    req(input$country_pick)
    df <- health_f()
    if (!input$country_pick %in% df$country) df <- health
    
    row <- df %>% filter(country == input$country_pick) %>% slice_head(n = 1)
    if (nrow(row) == 0) return(tags$p("No data available for this country."))
    
    val_or_na <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f%%", x))
    
    tagList(
      layout_columns(
        col_widths = c(3,3,3,3),
        div(class="metric-card", div(class="metric-label","Diabetes"), div(class="metric-value", val_or_na(row$diabetes_rate))),
        div(class="metric-card", div(class="metric-label","Obesity"), div(class="metric-value", val_or_na(row$obesity_rate))),
        div(class="metric-card", div(class="metric-label","Hypertension"), div(class="metric-value", val_or_na(row$hypertension_rate))),
        div(class="metric-card", div(class="metric-label","Smoking"), div(class="metric-value", val_or_na(row$smoking_rate)))
      ),
      br(),
      layout_columns(
        col_widths = c(6,6),
        div(class="metric-card", div(class="metric-label","Physical inactivity"), div(class="metric-value", val_or_na(row$physical_inactivity))),
        div(class="metric-card", div(class="metric-label","Region"), div(class="metric-value", row$region))
      )
    )
  })
  
  # Country bar
  output$country_bar <- renderPlotly({
    req(input$country_pick)
    df <- health_f()
    if (!input$country_pick %in% df$country) df <- health
    
    row <- df %>% filter(country == input$country_pick) %>% slice_head(n = 1)
    if (nrow(row) == 0) return(plotly_empty())
    
    long <- row %>%
      select(all_of(num_cols)) %>%
      pivot_longer(everything(), names_to = "indicator", values_to = "value") %>%
      mutate(indicator = gsub("_"," ", indicator))
    
    bar_cols <- c("#F7CAD0", "#CDB4DB", "#BDE0FE", "#A8DADC", "#B7E4C7")
    
    plot_ly(
      long,
      x = ~reorder(indicator, value),
      y = ~value,
      type = "bar",
      marker = list(color = bar_cols),
      hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -20, automargin = TRUE),
        yaxis = list(title = "Value (%)"),
        margin = list(l = 80, r = 20, t = 10, b = 90)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Country table
  output$country_table <- renderDT({
    req(input$country_pick)
    df <- health_f()
    if (!input$country_pick %in% df$country) df <- health
    
    d <- df %>% filter(country == input$country_pick) %>%
      select(country, region, all_of(num_cols))
    
    datatable(
      d,
      options = list(pageLength = 6, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
