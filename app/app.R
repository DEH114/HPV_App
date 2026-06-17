library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

months_list <- list(
  "January"=1,"February"=2,"March"=3,"April"=4,
  "May"=5,"June"=6,"July"=7,"August"=8,
  "September"=9,"October"=10,"November"=11,"December"=12
)

dateBoxes <- function(id, label, default_month=3, default_year=2025) {
  div(
    tags$label(label, style="font-size:12px;font-weight:600;color:#6b7280;
                              display:block;margin-bottom:5px;"),
    div(style="display:grid;grid-template-columns:2fr 1.5fr;gap:6px;",
        selectInput(paste0(id,"_month"), label=NULL,
                    choices=months_list, selected=default_month),
        numericInput(paste0(id,"_year"), label=NULL,
                     value=default_year, min=2000, max=2100, step=1)
    ),
    div(style="display:grid;grid-template-columns:2fr 1.5fr;gap:6px;
               font-size:10px;color:#9ca3af;text-align:center;margin-top:2px;",
        div("Month"), div("Year")
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family:'Segoe UI',Arial,sans-serif; background:#f8fafc; }
    .well { background:#ffffff; border:1px solid #e2e8f0; border-radius:10px; box-shadow:none; }
    .section-label { font-size:11px; font-weight:700; text-transform:uppercase;
                     letter-spacing:0.6px; color:#6b7280; margin-bottom:8px; }
    .upload-hint { font-size:11px; color:#9ca3af; margin-top:3px; }
    .btn-primary { background-color:#4472C4 !important; border-color:#4472C4 !important;
                   font-weight:600; border-radius:7px; }
    .btn-success { font-weight:600; border-radius:7px; width:100%; margin-top:6px; }
    .format-box  { background:#f1f5f9; border-radius:8px; padding:12px 14px;
                   font-size:12px; color:#6b7280; margin-top:4px; }
    .format-box code { background:#e2e8f0; padding:1px 5px; border-radius:3px; }
    hr { border-color:#e2e8f0; }
    input[type=number] { padding:5px 6px; }
    .form-group { margin-bottom:10px; }
  "))),
  
  div(style="background:#ffffff;border-bottom:1px solid #e2e8f0;padding:16px 28px;
             margin-bottom:24px;display:flex;align-items:center;gap:12px;",
      div(style="width:36px;height:36px;background:#4472C4;border-radius:8px;
               display:flex;align-items:center;justify-content:center;",
          tags$svg(xmlns="http://www.w3.org/2000/svg",width="20",height="20",
                   viewBox="0 0 24 24",fill="none",stroke="white",
                   `stroke-width`="2.5",`stroke-linecap`="round",`stroke-linejoin`="round",
                   tags$polyline(points="22 12 18 12 15 21 9 3 6 12 2 12"))
      ),
      div(
        tags$strong("HPV Vaccination Chart Generator",style="font-size:17px;color:#1a1a2e;")
      )
  ),
  
  fluidRow(
    column(3, wellPanel(
      div(class="section-label","Upload Data"),
      fileInput("file_data",label=NULL,accept=".csv",placeholder="Choose CSV..."),
      div(class="upload-hint","Needs Month, Counts, Aggregate columns"),
      
      hr(),
      div(class="section-label","Set Date Start"),
      dateBoxes("year1_start","Year 1 start", default_month=3, default_year=2025),
      div(style="font-size:11px;color:#9ca3af;margin-top:6px;",
          "Year 2 begins automatically with the first data point after Year 1."
      ),
      
      hr(),
      div(class="section-label","Configure"),
      sliderInput("goal_pct","Goal increase (%)",min=0,max=100,value=25,step=1),
      textInput("chart_title","Chart title",value="Monthly Cumulative HPV Vaccinations"),
      textInput("year1_label","Year 1 label",value="Year 1 (Baseline)"),
      textInput("year2_label","Year 2 label",value="Year 2 (Intervention)"),
      
      hr(),
      div(class="section-label", "Download"),
      div(
  style = "font-size:12px;color:#6b7280;white-space:pre-line;",
  "Download is available in the full R Shiny version.\nIn this browser version, use your browser's screenshot or Print and Save as PDF."
),
      
      hr(),
      div(class="format-box",
          tags$strong("CSV format",style="color:#374151;display:block;margin-bottom:6px;"),
          "Three columns required:",tags$br(),
          tags$code("Month")," — e.g. ",tags$em("10/1/23"),tags$br(),
          tags$code("Counts")," — monthly dose count",tags$br(),
          tags$code("Aggregate")," — running total",tags$br(),tags$br(),
          "Rows outside Year 1 / Year 2 windows are ignored."
      )
    )),
    
    column(9, wellPanel(
      div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
          div(class="section-label",style="margin-bottom:0;","Preview"),
          uiOutput("status_badge")
      ),
      plotOutput("preview_plot",height="85vh")
    ))
  )
)

# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Assemble dates from boxes ─────────────────────────────────────────────
  year1_start <- reactive({
  req(input$year1_start_month, input$year1_start_year)
  as.Date(sprintf("%04d-%02d-01",
                  as.integer(input$year1_start_year),
                  as.integer(input$year1_start_month)))
})
  
  # Year 2 starts the month after the last Year 1 row
  year2_start <- reactive({
    req(raw_data(), year1_start())
    last_y1_month <- raw_data() %>%
      filter(Month >= year1_start()) %>%
      arrange(Month) %>%
      dplyr::slice(12) %>%
      pull(Month)
    # First day of the following month
    as.Date(format(last_y1_month + 32, "%Y-%m-01"))
  })
  
  # ── Parse CSV ─────────────────────────────────────────────────────────────
  raw_data <- reactive({
    req(input$file_data)
    df <- read.csv(input$file_data$datapath)
    df$Month <- as.Date(df$Month,
                        tryFormats=c("%m/%d/%y","%m/%d/%Y","%Y-%m-%d"))
    df %>% arrange(Month)
  })
  
  # ── Year 1: rows from year1_start up to (not including) year2_start ───────
  baseline_data <- reactive({
    req(raw_data(), year1_start(), year2_start())
    df <- raw_data() %>%
      filter(Month >= year1_start() & Month < year2_start()) %>%
      mutate(
        Aggregate = cumsum(Counts),
        month_num = row_number(),
        cal_month = as.integer(format(Month, "%m"))
      )
    shiny::validate(shiny::need(nrow(df) > 0,
                                "No data found in Year 1 window. Check your start dates."))
    df
  })
  
  # ── Year 2: rows from year2_start onward ──────────────────────────────────
  # Key fix: align Year 2 rows to Year 1 x-axis positions by matching
  # calendar month, so a Sep Year2 bar lands on the Sep x-position
  # regardless of how many months of Year 2 data exist.
  intervention_data <- reactive({
    req(raw_data(), year2_start(), baseline_data())
    base <- baseline_data()
    
    df <- raw_data() %>%
      filter(Month >= year2_start()) %>%
      mutate(
        Aggregate    = cumsum(Counts),
        cal_month    = as.integer(format(Month, "%m"))
      )
    shiny::validate(shiny::need(nrow(df) > 0,
                                "No data found in Year 2 window. Check your start dates."))
    
    # Map each Year 2 row to the x-position of the matching month in Year 1
    df <- df %>%
      left_join(base %>% select(cal_month, x_pos_mapped = month_num),
                by = "cal_month") %>%
      filter(!is.na(x_pos_mapped))   # drop any months not present in Year 1
    
    df
  })
  
  # ── Goal line: one point per Year 1 month, scaled by goal % ──────────────
  goal_data <- reactive({
    base <- baseline_data()
    intv <- intervention_data()
    req(nrow(base) > 0, nrow(intv) > 0)
    
    # Goal is always defined over the full Year 1 range
    goal_cum <- cumsum(base$Counts * (1 + input$goal_pct / 100))
    data.frame(x_pos_mapped = base$month_num, Goal = goal_cum)
  })
  
  # ── ggplot ────────────────────────────────────────────────────────────────
  build_plot <- reactive({
    base <- baseline_data()
    intv <- intervention_data()
    goal <- goal_data()
    
    bar_data <- bind_rows(
      base %>% mutate(Series = input$year1_label, x_pos = month_num - 0.22),
      intv %>% mutate(Series = input$year2_label, x_pos = x_pos_mapped + 0.22)
    )
    
    y_max     <- max(bar_data$Aggregate, goal$Goal, na.rm=TRUE)
    y_ceiling <- ceiling(y_max / 50) * 50 + 50
    n_months  <- nrow(base)
    x_labels  <- c("", format(base$Month, "%b"))
    
    ggplot() +
      geom_bar(data=bar_data,
               aes(x=x_pos, y=Aggregate, fill=Series),
               stat="identity", width=0.4) +
      geom_line(data=goal,
                aes(x=x_pos_mapped, y=Goal),
                color="gray40", linewidth=1.2) +
      geom_point(data=goal,
                 aes(x=x_pos_mapped, y=Goal),
                 color="gray40", size=3) +
      scale_fill_manual(
        values=setNames(c("#4472C4","#ED7D31"),
                        c(input$year1_label, input$year2_label)),
        name=NULL
      ) +
      scale_x_continuous(
        breaks=0:n_months,
        labels=x_labels,
        expand=c(0.02,0.02)
      ) +
      scale_y_continuous(
        breaks=seq(0, y_ceiling, 50),
        limits=c(0, y_ceiling),
        expand=c(0,0)
      ) +
      labs(
        title    = input$chart_title,
        subtitle = paste0(
          input$year1_label," (blue): ",
          format(year1_start(),"%b %Y"),"–",
          format(year2_start() - 1, "%b %Y"),"  |  ",
          input$year2_label," (orange)  |  ",
          "Goal: +",input$goal_pct,"% over Year 1"
        ),
        x=NULL, y="Cumulative Vaccinations per Year"
      ) +
      theme_classic(base_size=14) +
      theme(
        plot.title         = element_text(face="bold", size=15, hjust=0.5),
        plot.subtitle      = element_text(size=10, hjust=0.5, color="gray35"),
        axis.text.x        = element_text(size=11),
        axis.title         = element_text(face="bold"),
        panel.grid.major.y = element_line(color="gray90", linewidth=0.4),
        legend.position    = "top",
        legend.text        = element_text(size=9)
      )
  })
  
  # ── Table grob ────────────────────────────────────────────────────────────
  build_table <- reactive({
    base <- baseline_data()
    intv <- intervention_data()
    
    # Build a full-length Year 2 vector aligned to Year 1 positions
    n_base    <- nrow(base)
    intv_vals <- rep(NA_real_, n_base)
    intv_vals[intv$x_pos_mapped] <- intv$Aggregate
    
    table_df <- data.frame(
      Series = c(input$year1_label, input$year2_label),
      rbind(base$Aggregate, intv_vals)
    )
    colnames(table_df) <- c("", format(base$Month, "%b"))
    
    n_cols      <- ncol(table_df)
    fill_matrix <- matrix(
      c(rep("#D9E1F2", n_cols), rep("#FCE4D6", n_cols)),
      nrow=2, ncol=n_cols, byrow=TRUE
    )
    tt <- ttheme_minimal(
      base_size=13,
      core    = list(fg_params=list(hjust=0.5),
                     bg_params=list(fill=as.vector(fill_matrix), alpha=0.6)),
      colhead = list(fg_params=list(fontface="bold", hjust=0.5))
    )
    tableGrob(table_df, rows=NULL, theme=tt)
  })
  
  # ── Preview ───────────────────────────────────────────────────────────────
  output$preview_plot <- renderPlot({
    req(baseline_data(), intervention_data())
    grid.arrange(build_plot(), build_table(), nrow=2, heights=c(3,1.8))
  })
  
  # ── Status badge ──────────────────────────────────────────────────────────
  output$status_badge <- renderUI({
    if (is.null(input$file_data)) {
      tags$span("Upload a CSV to preview", style="font-size:12px;color:#9ca3af;")
    } else if (nrow(baseline_data()) > 0 && nrow(intervention_data()) > 0) {
      n_intv    <- nrow(intervention_data())
      n_base    <- nrow(baseline_data())
      msg <- if (n_intv < n_base)
        paste0("● Ready — Year 2 has ", n_intv, " of ", n_base, " months so far")
      else
        "● Ready"
      tags$span(msg, style="font-size:12px;font-weight:600;color:#16a34a;")
    } else {
      tags$span("⚠ Check year boundary dates",
                style="font-size:12px;font-weight:600;color:#d97706;")
    }
  })
}

shinyApp(ui, server)
