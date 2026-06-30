library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(DT)
library(tidyr)
library(htmltools)
library(stringr)

# -----------------------------
# 1) Read data
# -----------------------------
csv_candidates <- c("health_data.csv", file.path("..", "health_data.csv"))
csv_file <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_file) || length(csv_file) == 0) stop("health_data.csv not found.")

health <- read_csv(csv_file, show_col_types = FALSE)
names(health) <- tolower(names(health))

required_cols <- c("region","country","diabetes_rate","obesity_rate",
                   "hypertension_rate","smoking_rate","physical_inactivity")
missing_cols <- setdiff(required_cols, names(health))
if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse=", ")))

num_cols <- c("diabetes_rate","obesity_rate","hypertension_rate","smoking_rate","physical_inactivity")

health <- health %>%
  mutate(across(all_of(num_cols), ~suppressWarnings(as.numeric(.))),
         country = str_trim(as.character(country)),
         region  = str_trim(as.character(region)))

# -----------------------------
# 2) Colours
# -----------------------------
pastel_regions <- c("#F7CAD0","#BDE0FE","#B7E4C7","#CDB4DB",
                    "#FFE5B4","#A8DADC","#FFC8DD","#D0F4DE")

make_region_colors <- function(regions) {
  regs <- sort(unique(as.character(regions)))
  setNames(rep(pastel_regions, length.out=length(regs)), regs)
}

pastel_gradient <- c("#F7CAD0","#CDB4DB","#BDE0FE","#A8DADC","#B7E4C7")

plotly_pastel_colorscale <- list(
  list(0.00,"#F7CAD0"), list(0.25,"#CDB4DB"),
  list(0.50,"#BDE0FE"), list(0.75,"#A8DADC"), list(1.00,"#B7E4C7")
)

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = "NCD Global Health Indicator",
  theme = bs_theme(version=5, bootswatch="flatly", base_font=font_google("Inter")),
  
  header = tagList(tags$style(HTML("
    :root{--navy:#213547;--ink:#0f172a;--muted:#64748b;--bg:#f6f7fb;--border:rgba(15,23,42,0.10);}
    body{background:var(--bg) !important;}
    .navbar{background:var(--navy) !important;min-height:76px;}
    .navbar .navbar-brand,.navbar .nav-link{color:#fff !important;}
    .navbar .nav-link.active{font-weight:800;border-bottom:2px solid rgba(255,255,255,0.75);}
    .bslib-sidebar-layout{gap:24px;align-items:flex-start;}
    .bslib-grid{gap:24px !important;}
    .bslib-sidebar{background:#fff;border:1px solid var(--border);border-radius:18px;padding:20px;}
    .card{border-radius:18px !important;border:1px solid var(--border) !important;
          box-shadow:0 12px 30px rgba(15,23,42,0.07);overflow:hidden;}
    .card-header{background:#fff !important;border-bottom:1px solid var(--border) !important;
                 font-weight:850;color:var(--ink);padding:16px 18px !important;font-size:18px;}
    .card-body{padding:16px 18px !important;}
    .metric-grid{display:grid;grid-template-columns:1fr;gap:14px;}
    .metric-card{border:1px solid var(--border);border-radius:16px;
                 background:linear-gradient(180deg,#fff,#fbfbfe);padding:16px;}
    .metric-label{color:var(--muted);font-size:12px;font-weight:850;text-transform:uppercase;letter-spacing:.06em;}
    .metric-value{font-size:34px;font-weight:900;color:var(--ink);margin-top:4px;}
    .sidebar-step{color:var(--muted);font-weight:900;font-size:12px;text-transform:uppercase;letter-spacing:.08em;margin-top:6px;}
    .sidebar-title{font-size:22px;font-weight:900;color:var(--ink);margin:8px 0 14px 0;}
    .small-note{color:var(--muted);font-size:13px;}
    .viz-fill{height:100%;width:100%;}
    .viz-fill .plotly{height:100% !important;width:100% !important;}
  "))),
  
  sidebar = sidebar(
    width=380, open="desktop",
    div(class="sidebar-step","Step 1"),
    div(class="sidebar-title","Choose an indicator"),
    selectInput("metric", label=NULL,
                choices=c("Diabetes rate (%)"="diabetes_rate","Obesity rate (%)"="obesity_rate",
                          "Hypertension rate (%)"="hypertension_rate","Smoking rate (%)"="smoking_rate",
                          "Physical inactivity (%)"="physical_inactivity"),
                selected="hypertension_rate"),
    hr(),
    div(class="sidebar-step","Step 2"),
    div(class="sidebar-title","Filter"),
    selectInput("region","Region",choices=c("All",sort(unique(health$region))),selected="All"),
    hr(),
    div(class="sidebar-step","Heat plot"),
    sliderInput("top_n","Show top countries",min=8,max=60,value=25,step=1),
    hr(),
    div(class="small-note",paste("Loaded:",csv_file))
  ),
  
  nav_panel("Dashboard",
            layout_columns(col_widths=c(4,8),
                           card(height="520px", card_header("Global snapshot"),
                                card_body(uiOutput("snapshot_cards"), br(),
                                          div(class="small-note","Updates based on region and indicator selection."))),
                           card(height="520px", full_screen=TRUE, card_header("World Map (Choropleth)"),
                                card_body(div(class="viz-fill", plotlyOutput("choropleth", height="100%"))))
            ),
            br(),
            layout_columns(col_widths=c(6,6),
                           card(height="560px",full_screen=TRUE,card_header("Heat plot: Countries × indicators"),
                                card_body(div(class="viz-fill",plotlyOutput("heat",height="100%")))),
                           card(height="560px",full_screen=TRUE,card_header("Scatter (2D): Obesity vs selected indicator"),
                                card_body(div(class="viz-fill",plotlyOutput("scatter2d",height="100%"))))
            ),
            br(),
            layout_columns(col_widths=c(6,6),
                           card(height="560px",full_screen=TRUE,card_header("Scatter (3D): Obesity, Diabetes, Hypertension"),
                                card_body(div(class="viz-fill",plotlyOutput("scatter3d",height="100%")))),
                           card(height="560px",full_screen=TRUE,card_header("Boxplot: Selected indicator by region"),
                                card_body(div(class="viz-fill",plotlyOutput("boxplot",height="100%"))))
            )
  ),
  
  nav_panel("Country profile",
            layout_columns(col_widths=c(4,8),
                           card(height="240px",card_header("Select country"),
                                card_body(selectInput("country_pick","Country",
                                                      choices=sort(unique(health$country)),
                                                      selected=sort(unique(health$country))[1]))),
                           card(height="240px",card_header("Country snapshot"),card_body(uiOutput("country_cards")))
            ),
            br(),
            layout_columns(col_widths=c(7,5),
                           card(height="560px",full_screen=TRUE,card_header("Indicator profile"),
                                card_body(div(class="viz-fill",plotlyOutput("country_bar",height="100%")))),
                           card(height="560px",full_screen=TRUE,card_header("Country data"),
                                card_body(DTOutput("country_table")))
            )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  health_f <- reactive({
    df <- health
    if (!is.null(input$region) && input$region != "All")
      df <- df %>% filter(region == input$region)
    df
  })
  
  observeEvent(input$region, {
    choices <- sort(unique(health_f()$country))
    if (!length(choices)) choices <- sort(unique(health$country))
    updateSelectInput(session,"country_pick",choices=choices,selected=choices[1])
  }, ignoreInit=TRUE)
  
  # ── Snapshot ───────────────────────────────────────────────
  output$snapshot_cards <- renderUI({
    req(input$metric)
    df <- health_f() %>% filter(!is.na(.data[[input$metric]]))
    mean_v <- if(nrow(df)>0) mean(df[[input$metric]],na.rm=TRUE) else NA_real_
    med_v  <- if(nrow(df)>0) median(df[[input$metric]],na.rm=TRUE) else NA_real_
    tagList(div(class="metric-grid",
                div(class="metric-card",div(class="metric-label","Mean"),
                    div(class="metric-value",ifelse(is.na(mean_v),"NA",sprintf("%.1f%%",mean_v)))),
                div(class="metric-card",div(class="metric-label","Median"),
                    div(class="metric-value",ifelse(is.na(med_v),"NA",sprintf("%.1f%%",med_v)))),
                div(class="metric-card",div(class="metric-label","Countries with data"),
                    div(class="metric-value",nrow(df)))
    ))
  })
  
  # ── Choropleth map (plotly only, zero leaflet/sf/terra) ────
  output$choropleth <- renderPlotly({
    req(input$metric)
    df     <- health_f()
    metric <- input$metric
    
    plot_ly(
      df,
      type       = "choropleth",
      locations  = ~country,
      locationmode = "country names",
      z          = ~.data[[metric]],
      text       = ~paste0("<b>",country,"</b><br>",
                           gsub("_"," ",metric),": ",
                           ifelse(is.na(.data[[metric]]),"no data",
                                  sprintf("%.1f%%",.data[[metric]]))),
      hovertemplate = "%{text}<extra></extra>",
      colorscale = plotly_pastel_colorscale,
      colorbar   = list(title=gsub("_"," ",metric)),
      marker     = list(line=list(color="rgba(150,150,150,0.5)",width=0.5))
    ) %>%
      layout(
        geo = list(
          showframe      = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#9aa4b2",
          projection     = list(type="natural earth"),
          bgcolor        = "rgba(0,0,0,0)",
          showland       = TRUE,
          landcolor      = "#f0f0f0",
          showocean      = TRUE,
          oceancolor     = "#e8f4f8"
        ),
        margin = list(l=0,r=0,t=10,b=0),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── Heat ───────────────────────────────────────────────────
  output$heat <- renderPlotly({
    req(input$metric, input$top_n)
    df <- health_f() %>%
      filter(!is.na(.data[[input$metric]])) %>%
      arrange(desc(.data[[input$metric]])) %>%
      slice_head(n=input$top_n) %>%
      select(country, all_of(num_cols))
    if(!nrow(df)) return(plotly_empty())
    mat <- df %>%
      pivot_longer(all_of(num_cols),names_to="indicator",values_to="value") %>%
      mutate(indicator=gsub("_"," ",indicator),
             country=factor(country,levels=rev(unique(df$country))))
    plot_ly(mat,x=~indicator,y=~country,z=~value,type="heatmap",
            colorscale=plotly_pastel_colorscale,
            hovertemplate="<b>%{y}</b><br>%{x}: %{z:.1f}%<extra></extra>") %>%
      layout(xaxis=list(title="",tickangle=-15,automargin=TRUE),
             yaxis=list(title="",automargin=TRUE),
             margin=list(l=150,r=20,t=10,b=80)) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── Scatter 2D ─────────────────────────────────────────────
  output$scatter2d <- renderPlotly({
    req(input$metric)
    df <- health_f() %>% filter(!is.na(obesity_rate),!is.na(.data[[input$metric]]))
    if(!nrow(df)) return(plotly_empty())
    cols <- make_region_colors(df$region)
    p <- ggplot(df,aes(x=obesity_rate,y=.data[[input$metric]],color=region,
                       text=paste0("<b>",country,"</b><br>Region: ",region,
                                   "<br>Obesity: ",obesity_rate,"<br>",
                                   gsub("_"," ",input$metric),": ",.data[[input$metric]]))) +
      geom_point(size=4.3,alpha=0.85) + scale_color_manual(values=cols) +
      theme_minimal(base_size=14) +
      labs(x="Obesity rate (%)",y=paste0(gsub("_"," ",input$metric)," (%)")) +
      theme(legend.position="right",panel.grid.minor=element_blank(),
            axis.title=element_text(face="bold"))
    ggplotly(p,tooltip="text") %>%
      layout(legend=list(title=list(text="<b>Region</b>")),
             margin=list(l=80,r=20,t=10,b=70)) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── Scatter 3D ─────────────────────────────────────────────
  output$scatter3d <- renderPlotly({
    df <- health_f() %>%
      filter(!is.na(obesity_rate),!is.na(diabetes_rate),!is.na(hypertension_rate))
    if(!nrow(df)) return(plotly_empty())
    cols      <- make_region_colors(df$region)
    pt_cols   <- unname(cols[as.character(df$region)])
    plot_ly(df,x=~obesity_rate,y=~diabetes_rate,z=~hypertension_rate,
            type="scatter3d",mode="markers",
            marker=list(size=8,opacity=0.9,color=pt_cols,
                        line=list(color="rgba(15,23,42,0.35)",width=1)),
            text=~paste0("<b>",country,"</b><br>Region: ",region,
                         "<br>Obesity: ",obesity_rate,"<br>Diabetes: ",diabetes_rate,
                         "<br>Hypertension: ",hypertension_rate),
            hovertemplate="%{text}<extra></extra>") %>%
      layout(scene=list(xaxis=list(title="Obesity (%)"),yaxis=list(title="Diabetes (%)"),
                        zaxis=list(title="Hypertension (%)"),aspectmode="cube",
                        camera=list(eye=list(x=1.6,y=1.6,z=1.2))),
             margin=list(l=0,r=0,t=10,b=0)) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── Boxplot ────────────────────────────────────────────────
  output$boxplot <- renderPlotly({
    req(input$metric)
    df <- health_f() %>% filter(!is.na(.data[[input$metric]]))
    if(!nrow(df)) return(plotly_empty())
    cols <- make_region_colors(df$region)
    p <- plot_ly()
    for(r in sort(unique(df$region))) {
      dfr <- df %>% filter(region==r)
      p <- p %>% add_boxplot(y=dfr[[input$metric]],name=r,boxpoints="outliers",
                             fillcolor=cols[[r]],marker=list(opacity=0.65),
                             line=list(width=1,color="rgba(15,23,42,0.35)"))
    }
    p %>% layout(yaxis=list(title=paste0(gsub("_"," ",input$metric)," (%)")),
                 xaxis=list(title=""),margin=list(l=80,r=20,t=10,b=70)) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── Country cards ──────────────────────────────────────────
  output$country_cards <- renderUI({
    req(input$country_pick)
    df  <- health_f()
    if(!input$country_pick %in% df$country) df <- health
    row <- df %>% filter(country==input$country_pick) %>% slice_head(n=1)
    if(!nrow(row)) return(tags$p("No data available."))
    f <- function(x) ifelse(is.na(x),"NA",sprintf("%.1f%%",x))
    tagList(layout_columns(col_widths=c(3,3,3,3),
                           div(class="metric-card",div(class="metric-label","Diabetes"),div(class="metric-value",f(row$diabetes_rate))),
                           div(class="metric-card",div(class="metric-label","Obesity"),div(class="metric-value",f(row$obesity_rate))),
                           div(class="metric-card",div(class="metric-label","Hypertension"),div(class="metric-value",f(row$hypertension_rate))),
                           div(class="metric-card",div(class="metric-label","Smoking"),div(class="metric-value",f(row$smoking_rate)))
    ))
  })
  
  # ── Country bar ────────────────────────────────────────────
  output$country_bar <- renderPlotly({
    req(input$country_pick)
    df  <- health_f()
    if(!input$country_pick %in% df$country) df <- health
    row <- df %>% filter(country==input$country_pick) %>% slice_head(n=1)
    if(!nrow(row)) return(plotly_empty())
    long <- row %>% select(all_of(num_cols)) %>%
      pivot_longer(everything(),names_to="indicator",values_to="value") %>%
      mutate(indicator=gsub("_"," ",indicator))
    plot_ly(long,x=~reorder(indicator,value),y=~value,type="bar",
            marker=list(color=pastel_gradient),
            hovertemplate="<b>%{x}</b><br>%{y:.1f}%<extra></extra>") %>%
      layout(xaxis=list(title="",tickangle=-20,automargin=TRUE),
             yaxis=list(title="Value (%)"),margin=list(l=80,r=20,t=10,b=90)) %>%
      config(displayModeBar=FALSE)
  })
  
  # ── Country table ──────────────────────────────────────────
  output$country_table <- renderDT({
    req(input$country_pick)
    df <- health_f()
    if(!input$country_pick %in% df$country) df <- health
    df %>% filter(country==input$country_pick) %>%
      select(country,region,all_of(num_cols)) %>%
      datatable(options=list(pageLength=6,scrollX=TRUE),rownames=FALSE)
  })
}

shinyApp(ui, server)