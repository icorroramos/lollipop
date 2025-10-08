# shiny_app_12/app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

ui <- fluidPage(
  titlePanel("Lollipop plots for scenario analyses visualisation"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot_type", "Plot type:",
                   choices = c("Deterministic" = "det", "Probabilistic" = "psa"),
                   inline = TRUE),
      fileInput("file", "Upload CSV", accept = c(".csv")),
      checkboxInput("has_header", "CSV has header", value = TRUE),
      selectInput("currency", "Currency:", choices = c("€", "£", "$"), selected = "€"),
      helpText(
        "Deterministic CSV: 2 columns -> scenario, INMB", br(),
        "Probabilistic CSV: 3 columns -> scenario, mean_NMB, sd_NMB", br(),
        "Order in the CSV = order of Y-axis labels."
      ),
      downloadButton("download_png", "Download PNG")
    ),
    mainPanel(plotOutput("plot", height = "600px"))
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$file)
    if (isTRUE(input$has_header)) {
      df <- suppressWarnings(read_csv(input$file$datapath, show_col_types = FALSE))
    } else {
      df <- suppressWarnings(read.csv(input$file$datapath, header = FALSE, stringsAsFactors = FALSE))
      df <- tibble::as_tibble(df)
    }
    validate(need(ncol(df) %in% c(2,3), "CSV must have 2 columns (deterministic) or 3 columns (probabilistic)."))
    df
  })
  
  tidy_data <- reactive({
    df <- raw_data()
    if (input$plot_type == "det") {
      validate(need(ncol(df) == 2, "Deterministic plot requires 2 columns: scenario, INMB."))
      colnames(df)[1:2] <- c("scenario", "INMB")
      df <- df %>% transmute(
        scenario = as.character(.data$scenario),
        INMB = suppressWarnings(as.numeric(.data$INMB))
      )
      validate(need(all(!is.na(df$INMB)), "INMB column must be numeric."))
    } else {
      validate(need(ncol(df) == 3, "Probabilistic plot requires 3 columns: scenario, mean_NMB, sd_NMB."))
      colnames(df)[1:3] <- c("scenario", "mean_NMB", "sd_NMB")
      df <- df %>% transmute(
        scenario = as.character(.data$scenario),
        mean_NMB = suppressWarnings(as.numeric(.data$mean_NMB)),
        sd_NMB   = suppressWarnings(as.numeric(.data$sd_NMB))
      )
      validate(need(all(!is.na(df$mean_NMB)), "mean_NMB must be numeric."))
      validate(need(all(!is.na(df$sd_NMB) & df$sd_NMB >= 0), "sd_NMB must be numeric and non-negative."))
    }
    df$scenario <- factor(df$scenario, levels = df$scenario)
    df
  })
  
  axis_labeller <- reactive({
    label_number(accuracy = 1, big.mark = ",", decimal.mark = ".", prefix = input$currency)
  })
  money_text <- reactive({
    function(x) paste0(input$currency, formatC(x, format = "f", digits = 0, big.mark = ",", decimal.mark = "."))
  })
  
  plot_obj <- reactive({
    df <- tidy_data()
    
    base_theme <- theme_minimal(base_size = 18) +
      theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 17),
        axis.text  = element_text(size = 15),
        legend.position = "right"
      )
    
    if (input$plot_type == "det") {
      # ---- Deterministic ----
      spacing_factor <- 0.6
      scn_levels <- levels(df$scenario)
      n_scn <- length(scn_levels)
      slack <- n_scn - n_scn * spacing_factor
      trim_top_frac <- 1.1
      upper_limit <- (n_scn + 0.5) - slack * trim_top_frac
      
      y_rng <- range(df$INMB, na.rm = TRUE)
      span  <- diff(y_rng)
      pad   <- if (is.finite(span) && span > 0) span * 0.07 else max(1, abs(y_rng[1])) * 0.07
      y_lims <- c(y_rng[1] - pad*2, y_rng[2] + pad*2)
      
      df <- df %>%
        mutate(
          is_base   = scenario == "Base case",
          hjust_pos = if_else(INMB > 0, -0.3, 1.3),
          idx       = as.integer(scenario),
          y_pos     = idx * spacing_factor,
          shape_grp = if_else(is_base, "Base case", "Scenario")
        )
      
      p <- ggplot(df, aes(x = y_pos)) +
        geom_segment(aes(xend = y_pos, y = 0, yend = INMB, color = INMB), size = 1) +
        geom_point(aes(y = INMB, color = INMB, shape = shape_grp), size = 4) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
        geom_text(aes(y = INMB, label = money_text()(INMB), hjust = hjust_pos),
                  color = "black", size = 4.5) +
        scale_color_gradient2(
          low = "red", mid = "gray90", high = "forestgreen", midpoint = 0,
          name   = paste0("INMB (", input$currency, ")"),
          labels = axis_labeller()
        ) +
        scale_shape_manual(
          values = c("Scenario" = 16, "Base case" = 17),
          name   = NULL,
          labels = c("Base-case", "Scenario")
        ) +
        guides(shape = guide_legend(order = 1), color = guide_colorbar(order = 2)) +
        scale_y_continuous(labels = axis_labeller(), limits = y_lims, expand = expansion(mult = c(0.01, 0.02))) +
        scale_x_continuous(
          breaks = seq_len(n_scn) * spacing_factor,
          labels = scn_levels,
          limits = c(0.5, upper_limit),
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        labs(
          title = "INMB — Deterministic",
          x = NULL,
          y = paste0("INMB (", input$currency, ")")
        ) +
        coord_flip() +
        base_theme
      
    } else {
      # ---- Probabilistic: INMB on x, Scenarios on y (horizontal violins) ----
      set.seed(42)
      n_psa <- 1000
      psa <- df %>% rowwise() %>% do({
        tibble(scenario = .$scenario, NMB = rnorm(n_psa, mean = .$mean_NMB, sd = .$sd_NMB))
      }) %>% ungroup()
      
      det_points <- df %>% transmute(
        scenario,
        NMB = mean_NMB,
        shape_grp = if_else(scenario == "Base case", "Base case", "Scenario")
      )
      
      # Limits from PSA quantiles (for symmetric shape; no dropped rows)
      q <- quantile(psa$NMB, probs = c(0.005, 0.995), na.rm = TRUE, names = FALSE)
      rng <- diff(q)
      pad <- if (is.finite(rng) && rng > 0) rng * 0.03 else 1
      x_lims <- c(q[1] - pad*2.5, q[2] + pad*2.5)
      
      p <- ggplot() +
        geom_violin(
          data = psa, aes(y = scenario, x = NMB),
          fill = "grey", alpha = 0.22, color = NA, scale = "width",
          trim = FALSE, na.rm = TRUE, orientation = "y"
        ) +
        geom_segment(
          data = det_points,
          aes(y = scenario, yend = scenario, x = 0, xend = NMB, color = NMB),
          size = 1
        ) +
        geom_point(
          data = det_points,
          aes(y = scenario, x = NMB, color = NMB, shape = shape_grp),
          size = 4
        ) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
        geom_text(
          data = det_points %>% mutate(hjust_pos = if_else(NMB > 0, -0.3, 1.3)),
          aes(y = scenario, x = NMB, label = money_text()(NMB), hjust = hjust_pos),
          color = "black", size = 4.5
        ) +
        scale_color_gradient2(
          low = "red", mid = "gray90", high = "forestgreen", midpoint = 0,
          name   = paste0("INMB (", input$currency, ")"),
          labels = axis_labeller()
        ) +
        scale_shape_manual(
          values = c("Scenario" = 16, "Base case" = 17),
          name   = NULL,
          labels = c("Base case", "Scenario")
        ) +
        guides(shape = guide_legend(order = 1), color = guide_colorbar(order = 2)) +
        scale_x_continuous(labels = axis_labeller()) +
        labs(
          title = "INMB — Probabilistic (PSA)",
          x = paste0("INMB (", input$currency, ")"),
          y = NULL
        ) +
        coord_cartesian(xlim = x_lims) +  # zoom without dropping data
        base_theme
    }
    p
  })
  
  #output$plot <- renderPlot({ print(plot_obj()) })
  output$plot <- renderPlot({ print(plot_obj()) },
                            height = function() {
                              if (input$plot_type == "det") {
                                # ~28 px per row, capped
                                n <- length(levels(tidy_data()$scenario))
                                max(400, min(800, 28 * n))
                              } else {600}
                              }
                            )
  
  
  output$download_png <- downloadHandler(
    filename = function() paste0("inmb_plot_", input$plot_type, ".png"),
    content = function(file) {
      p <- plot_obj()
      w_px <- session$clientData$output_plot_width; if (is.null(w_px)) w_px <- 800
      h_px <- session$clientData$output_plot_height; if (is.null(h_px)) h_px <- 600
      dpi <- 96
      ggplot2::ggsave(file, plot = p, device = "png",
                      width = w_px / dpi, height = h_px / dpi, dpi = dpi, units = "in", bg = "white")
    }
  )
}

shinyApp(ui, server)
