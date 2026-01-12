# ============================================================
# Global Health Indicators — Interactive Explorer (Pastel)
# Includes:
# 1) Map (pastel choropleth + optional centroid points)
# 2) Scatter (2D)
# 3) Scatter (3D)
# 4) Boxplots
# 5) Art View (Cloud 2D + Nebula 3D) with jitter-multiplied points + opacity
# 6) Art View (Nebula 3D Animated) auto-rotating camera
# 7) Table
# Copy-paste into app.R
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
library(htmltools)
library(htmlwidgets)

# ---- 1) Read data ----
health <- read_csv("health_data.csv", show_col_types = FALSE)

# ---- 2) Ensure numeric columns are numeric ----
num_cols <- c(
  "diabetes_rate", "obesity_rate", "hypertension_rate",
  "smoking_rate", "physical_inactivity"
)

health <- health %>%
  mutate(across(all_of(num_cols), ~ as.numeric(.)))

# ---- 3) Country name fixes for matching map shapefile ----
name_map <- c(
  "USA" = "United States of America",
  "UK"  = "United Kingdom"
)

health <- health %>%
  mutate(
    country_match = if_else(
      country %in% names(name_map),
      unname(name_map[country]),
      country
    )
  )

# ---- 4) Load world map + join health data ----
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    name_long = as.character(name_long),
    name      = as.character(name)
  )

world_joined <- world %>%
  left_join(health, by = c("name_long" = "country_match"))

# Fallback if nothing matched
if (sum(!is.na(world_joined$country)) == 0) {
  world_joined <- world %>%
    left_join(health, by = c("name" = "country_match"))
}

# ---- Pastel palettes ----
map_palette_name <- "YlGnBu"

pastel_region_colors <- c(
  "#A8DADC", "#BDE0FE", "#CDB4DB", "#FFC8DD",
  "#FFE5B4", "#B7E4C7", "#F1FA8C", "#D0F4DE"
)

# ---- Helper: stable region color map (even if regions vary) ----
make_region_colors <- function(regions) {
  regs <- sort(unique(regions))
  cols <- setNames(rep(pastel_region_colors, length.out = length(regs)), regs)
  cols
}

# ---- Helper: jitter-multiply points for "art" ----
make_cloud_points <- function(df, xcol, ycol, zcol = NULL,
                              copies = 30, jitter_amt = 0.25,
                              seed = 123) {
  set.seed(seed)
  
  df2 <- df %>%
    filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]))
  
  if (!is.null(zcol)) {
    df2 <- df2 %>% filter(!is.na(.data[[zcol]]))
  }
  
  if (nrow(df2) == 0) return(df2)
  
  idx <- rep(seq_len(nrow(df2)), each = copies)
  out <- df2[idx, , drop = FALSE]
  
  xr <- range(df2[[xcol]], na.rm = TRUE)
  yr <- range(df2[[ycol]], na.rm = TRUE)
  xsd <- (xr[2] - xr[1]) * jitter_amt
  ysd <- (yr[2] - yr[1]) * jitter_amt
  
  out[[xcol]] <- out[[xcol]] + rnorm(nrow(out), mean = 0, sd = xsd)
  out[[ycol]] <- out[[ycol]] + rnorm(nrow(out), mean = 0, sd = ysd)
  
  if (!is.null(zcol)) {
    zr <- range(df2[[zcol]], na.rm = TRUE)
    zsd <- (zr[2] - zr[1]) * jitter_amt
    out[[zcol]] <- out[[zcol]] + rnorm(nrow(out), mean = 0, sd = zsd)
  }
  
  out
}

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty", version = 5),
  titlePanel("Global Health Indicators — Interactive Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h5("Map"),
      selectInput("map_metric", "Map metric:", choices = num_cols, selected = "diabetes_rate"),
      checkboxInput("show_points", "Show country points on map (where available)", value = TRUE),
      
      hr(),
      
      h5("2D Scatter"),
      sliderInput("point_size", "Point size (2D):", min = 2, max = 10, value = 5),
      selectInput("xvar", "X variable (2D):", choices = num_cols, selected = "obesity_rate"),
      selectInput("yvar", "Y variable (2D):", choices = num_cols, selected = "diabetes_rate"),
      checkboxInput("log_scale", "Log scale X (2D)", value = FALSE),
      
      hr(),
      
      h5("Boxplots"),
      selectInput("box_metric", "Boxplot metric:", choices = num_cols, selected = "diabetes_rate"),
      
      hr(),
      
      h5("Art View (Cloud / Nebula)"),
      selectInput("art_x", "Art X:", choices = num_cols, selected = "obesity_rate"),
      selectInput("art_y", "Art Y:", choices = num_cols, selected = "diabetes_rate"),
      selectInput("art_z", "Art Z (for 3D):", choices = num_cols, selected = "hypertension_rate"),
      
      sliderInput("cloud_copies", "Multiply points (copies per country):",
                  min = 5, max = 250, value = 60, step = 5),
      
      sliderInput("cloud_jitter", "Cloud mode (jitter / dreaminess):",
                  min = 0.00, max = 0.60, value = 0.18, step = 0.02),
      
      sliderInput("cloud_opacity", "Transparency (opacity):",
                  min = 0.05, max = 1.00, value = 0.25, step = 0.05),
      
      checkboxInput("cloud_by_region", "Color by region (else single pastel)", value = TRUE),
      
      hr(),
      h5("3D Animation"),
      checkboxInput("auto_rotate", "Auto-rotate Nebula (3D animation)", value = TRUE),
      sliderInput("rotate_speed", "Rotate speed (higher = slower)", min = 10, max = 120, value = 45, step = 5),
      
      helpText("Tip: increase copies + jitter, lower opacity for a soft 'cloud' look.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = 600)),
        tabPanel("Scatter (2D)", plotlyOutput("scatter", height = 600)),
        tabPanel("Scatter (3D)", plotlyOutput("scatter3d", height = 600)),
        tabPanel("Boxplots", plotlyOutput("boxplots", height = 600)),
        tabPanel("Art View (Cloud 2D)", plotlyOutput("art_cloud2d", height = 600)),
        tabPanel("Art View (Nebula 3D)", plotlyOutput("art_nebula3d", height = 600)),
        tabPanel("Art View (Nebula 3D Animated)", plotlyOutput("art_nebula3d_anim", height = 600)),
        tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # --------------------------
  # MAP
  # --------------------------
  metric_vals <- reactive({
    req(input$map_metric)
    world_joined[[input$map_metric]]
  })
  
  pal <- reactive({
    colorNumeric(
      palette = map_palette_name,
      domain  = metric_vals(),
      na.color = "transparent"
    )
  })
  
  output$map <- renderLeaflet({
    req(input$map_metric)
    metric <- input$map_metric
    w <- world_joined
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      w$name_long,
      metric,
      ifelse(is.na(w[[metric]]), "no data", round(w[[metric]], 2))
    ) %>% lapply(HTML)
    
    m <- leaflet(w) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor = ~pal()(w[[metric]]),
        weight = 0.6,
        color = "#7a7a7a",
        fillOpacity = 0.75,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#444444",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal(),
        values = metric_vals(),
        title = metric,
        position = "bottomright"
      )
    
    if (isTRUE(input$show_points)) {
      cent <- st_centroid(st_geometry(w))
      coords <- st_coordinates(cent)
      keep <- !is.na(w[[metric]])
      
      if (any(keep)) {
        m <- m %>%
          addCircleMarkers(
            lng = coords[keep, 1],
            lat = coords[keep, 2],
            radius = 4,
            stroke = FALSE,
            fillOpacity = 0.6,
            color = "#5C7AEA",
            label = labels[keep]
          )
      }
    }
    
    m
  })
  
  # --------------------------
  # 2D SCATTER
  # --------------------------
  output$scatter <- renderPlotly({
    req(input$xvar, input$yvar)
    
    df <- health %>%
      filter(!is.na(.data[[input$xvar]]), !is.na(.data[[input$yvar]]))
    
    cols <- make_region_colors(df$region)
    
    p <- ggplot(
      df,
      aes(
        x = .data[[input$xvar]],
        y = .data[[input$yvar]],
        color = region,
        text = paste0(
          "Country: ", country,
          "<br>Region: ", region,
          "<br>", input$xvar, ": ", .data[[input$xvar]],
          "<br>", input$yvar, ": ", .data[[input$yvar]]
        )
      )
    ) +
      geom_point(size = input$point_size, alpha = 0.85) +
      theme_minimal() +
      labs(x = input$xvar, y = input$yvar) +
      scale_color_manual(values = cols)
    
    if (isTRUE(input$log_scale)) {
      p <- p + scale_x_log10()
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = list(text = "Region")))
  })
  
  # --------------------------
  # 3D SCATTER (fixed axes)
  # --------------------------
  output$scatter3d <- renderPlotly({
    df <- health %>%
      filter(!is.na(obesity_rate), !is.na(diabetes_rate), !is.na(hypertension_rate))
    
    cols <- make_region_colors(df$region)
    
    plot_ly(
      data = df,
      x = ~obesity_rate,
      y = ~diabetes_rate,
      z = ~hypertension_rate,
      color = ~region,
      colors = cols,
      text = ~paste0(
        "Country: ", country,
        "<br>Region: ", region,
        "<br>Obesity: ", obesity_rate,
        "<br>Diabetes: ", diabetes_rate,
        "<br>Hypertension: ", hypertension_rate
      ),
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 6, opacity = 0.85)
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Obesity rate (%)"),
          yaxis = list(title = "Diabetes rate (%)"),
          zaxis = list(title = "Hypertension rate (%)")
        ),
        legend = list(title = list(text = "Region"))
      )
  })
  
  # --------------------------
  # BOXPLOTS (by region, selected metric)
  # --------------------------
  output$boxplots <- renderPlotly({
    req(input$box_metric)
    
    df <- health %>%
      filter(!is.na(.data[[input$box_metric]]))
    
    cols <- make_region_colors(df$region)
    regs <- sort(unique(df$region))
    
    p <- plot_ly()
    for (r in regs) {
      dfr <- df %>% filter(region == r)
      p <- p %>%
        add_boxplot(
          y = dfr[[input$box_metric]],
          name = r,
          boxpoints = "outliers",
          marker = list(opacity = 0.6),
          line = list(width = 1),
          fillcolor = cols[[r]]
        )
    }
    
    p %>%
      layout(
        title = paste0("Boxplots by Region: ", input$box_metric),
        yaxis = list(title = input$box_metric),
        xaxis = list(title = "Region")
      )
  })
  
  # --------------------------
  # ART VIEW: CLOUD 2D
  # --------------------------
  output$art_cloud2d <- renderPlotly({
    req(input$art_x, input$art_y)
    
    base_df <- health %>%
      filter(!is.na(.data[[input$art_x]]), !is.na(.data[[input$art_y]]))
    
    cloud_df <- make_cloud_points(
      df = base_df,
      xcol = input$art_x,
      ycol = input$art_y,
      copies = input$cloud_copies,
      jitter_amt = input$cloud_jitter,
      seed = 123
    )
    
    if (nrow(cloud_df) == 0) return(plotly_empty())
    
    if (isTRUE(input$cloud_by_region)) {
      cols <- make_region_colors(cloud_df$region)
      p <- plot_ly(
        data = cloud_df,
        x = ~.data[[input$art_x]],
        y = ~.data[[input$art_y]],
        type = "scatter",
        mode = "markers",
        color = ~region,
        colors = cols,
        text = ~paste0("Country: ", country, "<br>Region: ", region),
        marker = list(size = 6, opacity = input$cloud_opacity)
      )
    } else {
      p <- plot_ly(
        data = cloud_df,
        x = ~.data[[input$art_x]],
        y = ~.data[[input$art_y]],
        type = "scatter",
        mode = "markers",
        text = ~paste0("Country: ", country, "<br>Region: ", region),
        marker = list(size = 6, opacity = input$cloud_opacity, color = "#CDB4DB")
      )
    }
    
    p %>%
      layout(
        title = "Art View: Cloud (2D)",
        xaxis = list(title = input$art_x),
        yaxis = list(title = input$art_y)
      )
  })
  
  # --------------------------
  # ART VIEW: NEBULA 3D (static)
  # --------------------------
  output$art_nebula3d <- renderPlotly({
    req(input$art_x, input$art_y, input$art_z)
    
    base_df <- health %>%
      filter(
        !is.na(.data[[input$art_x]]),
        !is.na(.data[[input$art_y]]),
        !is.na(.data[[input$art_z]])
      )
    
    nebula_df <- make_cloud_points(
      df = base_df,
      xcol = input$art_x,
      ycol = input$art_y,
      zcol = input$art_z,
      copies = input$cloud_copies,
      jitter_amt = input$cloud_jitter,
      seed = 456
    )
    
    if (nrow(nebula_df) == 0) return(plotly_empty())
    
    if (isTRUE(input$cloud_by_region)) {
      cols <- make_region_colors(nebula_df$region)
      p <- plot_ly(
        data = nebula_df,
        x = ~.data[[input$art_x]],
        y = ~.data[[input$art_y]],
        z = ~.data[[input$art_z]],
        type = "scatter3d",
        mode = "markers",
        color = ~region,
        colors = cols,
        text = ~paste0("Country: ", country, "<br>Region: ", region),
        marker = list(size = 4, opacity = input$cloud_opacity)
      )
    } else {
      p <- plot_ly(
        data = nebula_df,
        x = ~.data[[input$art_x]],
        y = ~.data[[input$art_y]],
        z = ~.data[[input$art_z]],
        type = "scatter3d",
        mode = "markers",
        text = ~paste0("Country: ", country, "<br>Region: ", region),
        marker = list(size = 4, opacity = input$cloud_opacity, color = "#BDE0FE")
      )
    }
    
    p %>%
      layout(
        title = "Art View: Nebula (3D)",
        scene = list(
          xaxis = list(title = input$art_x),
          yaxis = list(title = input$art_y),
          zaxis = list(title = input$art_z),
          camera = list(eye = list(x = 1.6, y = 1.6, z = 1.1))
        )
      )
  })
  
  # --------------------------
  # ART VIEW: NEBULA 3D ANIMATED (auto-rotate camera)
  # --------------------------
  output$art_nebula3d_anim <- renderPlotly({
    req(input$art_x, input$art_y, input$art_z)
    
    base_df <- health %>%
      filter(
        !is.na(.data[[input$art_x]]),
        !is.na(.data[[input$art_y]]),
        !is.na(.data[[input$art_z]])
      )
    
    nebula_df <- make_cloud_points(
      df = base_df,
      xcol = input$art_x,
      ycol = input$art_y,
      zcol = input$art_z,
      copies = input$cloud_copies,
      jitter_amt = input$cloud_jitter,
      seed = 999
    )
    
    if (nrow(nebula_df) == 0) return(plotly_empty())
    
    cols <- if (isTRUE(input$cloud_by_region)) make_region_colors(nebula_df$region) else NULL
    
    p <- if (isTRUE(input$cloud_by_region)) {
      plot_ly(
        data = nebula_df,
        x = ~.data[[input$art_x]],
        y = ~.data[[input$art_y]],
        z = ~.data[[input$art_z]],
        type = "scatter3d",
        mode = "markers",
        color = ~region,
        colors = cols,
        text = ~paste0("Country: ", country, "<br>Region: ", region),
        marker = list(size = 4, opacity = input$cloud_opacity)
      )
    } else {
      plot_ly(
        data = nebula_df,
        x = ~.data[[input$art_x]],
        y = ~.data[[input$art_y]],
        z = ~.data[[input$art_z]],
        type = "scatter3d",
        mode = "markers",
        text = ~paste0("Country: ", country, "<br>Region: ", region),
        marker = list(size = 4, opacity = input$cloud_opacity, color = "#BDE0FE")
      )
    }
    
    p <- p %>%
      layout(
        title = "Art View: Nebula (3D Animated)",
        scene = list(
          xaxis = list(title = input$art_x),
          yaxis = list(title = input$art_y),
          zaxis = list(title = input$art_z),
          camera = list(eye = list(x = 1.6, y = 1.6, z = 1.1))
        )
      )
    
    if (isTRUE(input$auto_rotate)) {
      p <- p %>%
        onRender(sprintf("
          function(el, x){
            var gd = document.getElementById(el.id);
            var t = 0;
            var speed = %d; // higher = slower
            function rotate(){
              t += 0.02;
              var r = 1.8;
              var eye = {x: r*Math.cos(t), y: r*Math.sin(t), z: 1.1};
              Plotly.relayout(gd, {'scene.camera.eye': eye});
              setTimeout(rotate, speed);
            }
            rotate();
          }
        ", input$rotate_speed))
    }
    
    p
  })
  
  # --------------------------
  # TABLE
  # --------------------------
  output$table <- DT::renderDataTable({
    DT::datatable(health, options = list(pageLength = 10), rownames = FALSE)
  })
}

# ---- Run app ----
shinyApp(ui, server)
