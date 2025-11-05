#' @title Metaboprep Shiny App
#' @description Launch a Shiny app to explore the Metaboprep object
#' @param metaboprep an object of class Metaboprep
#' @return Runs a Shiny app
#' 
#' @include class_metaboprep.R
#' @importFrom shiny navbarPage tabPanel sidebarLayout sidebarPanel selectizeInput plotOutput updateSelectizeInput reactive renderPlot
#' @importFrom DT DTOutput
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @importFrom reshape2 melt
#' @importFrom plotly ggplotly plotlyOutput renderPlotly layout event_data
#' @importFrom bslib bs_theme
#' @export
shiny_app <- new_generic("shiny_app", c("metaboprep"), function(metaboprep) { S7_dispatch() })
#' @name shiny_app
method(shiny_app, Metaboprep) <- function(metaboprep) {
  
  side_bar_width   <- 3
  
  ui <- navbarPage(
    "Metaboprep Explorer",
    
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      base_font = bslib::font_google("Roboto"),
      heading_font = bslib::font_google("Roboto Slab"),
      code_font = bslib::font_google("Fira Code")
    ),
    
    # Overview page
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 width = side_bar_width,
                 selectizeInput("feature", "Choose feature:", choices = NULL),
                 selectizeInput("sample",  "Choose sample:", choices = NULL),
                 selectizeInput("layer",   "Choose data layer:", choices = NULL)
               ),
               mainPanel(
                 h3("Missingness"),
                 withSpinner(plotOutput("overview_missingness")),
                 h3("Exclusions"),
                 plotlyOutput("overview_exclusions"),
                 DT::DTOutput("overview_excl_table")
               )
             )
    ),
    
    # Features page
    tabPanel("Features",
             sidebarLayout(
               sidebarPanel(
                 width = side_bar_width,
                 selectizeInput("feature_f", "Choose feature:", choices = NULL),
                 selectizeInput("layer_f",   "Choose data layer:", choices = NULL)
               ),
               mainPanel(
                 h3("Scatter"),
                 withSpinner(plotOutput("feature_scatter")),
                 h3("Histogram"),
                 withSpinner(plotOutput("feature_histogram")),
                 h3("Summary statistics"),
                 DT::DTOutput("feature_table")
               )
             )
    ),
    
    # Samples page
    tabPanel("Samples",
             sidebarLayout(
               sidebarPanel(
                 width = side_bar_width,
                 selectizeInput("sample_s", "Choose sample:", choices = NULL),
                 selectizeInput("layer_s",  "Choose data layer:", choices = NULL)
               ),
               mainPanel(
                 plotOutput("sample_plot1"),
                 plotOutput("sample_plot2")
               )
             )
    ),
    
    # Table page
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 width = side_bar_width,
                 selectizeInput("layer_t", "Choose data layer:", choices = NULL),
                 selectizeInput("select_rows", "Select rows (samples):", choices = NULL, multiple = TRUE),
                 selectizeInput("select_cols", "Select columns (features):", choices = NULL, multiple = TRUE)
               ),
               mainPanel(
                 DT::DTOutput("overview_table")
               )
             )
    )
  )
  
  server <- function(input, output, session) {
    
    ################
    # select inputs
    ################
    updateSelectizeInput(
      session, "sample",
      choices = dimnames(metaboprep@data)[[1]],
      selected = dimnames(metaboprep@data)[[1]][1],
      server = TRUE
    )
    updateSelectizeInput(
      session, "feature",
      choices = dimnames(metaboprep@data)[[2]],
      selected = dimnames(metaboprep@data)[[2]][1],
      server = TRUE
    )
    updateSelectizeInput(
      session, "layer",
      choices = dimnames(metaboprep@data)[[3]],
      selected = dimnames(metaboprep@data)[[3]][1],
      server = TRUE
    )
    updateSelectizeInput(
      session, "layer_f",
      choices = dimnames(metaboprep@data)[[3]],
      selected = dimnames(metaboprep@data)[[3]][length(dimnames(metaboprep@data)[[3]])],
      server = TRUE
    )
    updateSelectizeInput(
      session, "feature_f",
      choices = dimnames(metaboprep@data)[[2]],
      selected = dimnames(metaboprep@data)[[2]][1],
      server = TRUE
    )
    updateSelectizeInput(
      session, "layer_s",
      choices = dimnames(metaboprep@data)[[3]],
      selected = dimnames(metaboprep@data)[[3]][length(dimnames(metaboprep@data)[[3]])],
      server = TRUE
    )
    updateSelectizeInput(
      session, "sample_s",
      choices = dimnames(metaboprep@data)[[1]],
      selected = dimnames(metaboprep@data)[[1]][1],
      server = TRUE
    )
    updateSelectizeInput(
      session, "layer_t",
      choices = dimnames(metaboprep@data)[[3]],
      selected = dimnames(metaboprep@data)[[3]][length(dimnames(metaboprep@data)[[3]])],
      server = TRUE
    )
    updateSelectizeInput(
      session, "select_rows",
      choices = dimnames(metaboprep@data)[[1]],
      # selected = dimnames(metaboprep@data)[[3]][1],  
      server = TRUE
    )
    updateSelectizeInput(
      session, "select_cols",
      choices = dimnames(metaboprep@data)[[2]],
      # selected = dimnames(metaboprep@data)[[2]][1],
      server = TRUE
    )
    
    ################
    # Reactive data
    ################
    # exclusions data.frame
    excl_dt <- reactive({
      if (length(unlist(metaboprep@exclusions)) > 0) {
        excl_vec <- unlist(metaboprep@exclusions)
        parts    <- strsplit(names(excl_vec), "\\.", fixed = FALSE)
        exclusions_df <- data.frame(
          id     = unname(excl_vec),
          type   = vapply(parts, `[`, "", 1),
          excl_reason = reason_clean <- sub("\\d+$", "", vapply(parts, `[`, "", 2)),
          stringsAsFactors = FALSE
        )
      } else {
        exclusions_df <- data.frame(
          id     = character(),
          type   = character(),
          excl_reason = character(),
          stringsAsFactors = FALSE
        )
      }
      exclusions_df
    })
    
    # exclusions summary
    excl_summary <- reactive({
      agg <- aggregate(
        id ~ type + excl_reason,
        data = excl_dt(),
        FUN = function(x) paste(x, collapse = ", ")
      )
      agg$count <- sapply(strsplit(agg$id, ", "), length)
      
      # tooltip: show first 10 IDs only
      agg$id_short <- sapply(strsplit(agg$id, ", "), function(ids) {
        n <- length(ids)
        paste(c(ids[1:min(5, n)], if (n>5) "..."), collapse = ", ")
      })
      agg
    })
    
    # long data with annotations ====
    data_long <- reactive({
      req(input$layer)
      
      # the data
      df <- metaboprep@data[, , input$layer, drop = TRUE]
      df <- reshape2::melt(df, varnames = c("sample_id", "feature_id"), value.name = "value")
      df$sample_id  <- factor(df$sample_id,  levels = unique(df$sample_id))
      df$feature_id <- factor(df$feature_id, levels = unique(df$feature_id))
      
      # exclusions
      df$status <- ifelse(is.na(df$value), "missing", "present")
      df$excl_reason <- NA_character_
      excl <- excl_dt()
      
      excl_s <- excl[excl$type == "samples", c("id", "excl_reason"), drop = FALSE]
      if (nrow(excl_s) > 0) {
        df   <- merge(df, excl_s, by.x = "sample_id", by.y = "id", all.x = TRUE, suffixes = c("", "_s"))
        df$status[!is.na(df$excl_reason_s) & df$status == "present"] <- "excluded"
        df$excl_reason[!is.na(df$excl_reason_s)] <- df$excl_reason_s[!is.na(df$excl_reason_s)]
        df$excl_reason_s <- NULL
      }
      
      excl_f <- excl[excl$type == "features", c("id", "excl_reason"), drop = FALSE]
      if (nrow(excl_f) > 0) {
        df <- merge(df, excl_f, by.x = "feature_id", by.y = "id", all.x = TRUE, suffixes = c("", "_f"))
        df$status[!is.na(df$excl_reason_f) & df$status == "present"] <- "excluded"
        df$excl_reason[!is.na(df$excl_reason_f)] <- df$excl_reason_f[!is.na(df$excl_reason_f)]
        df$excl_reason_f <- NULL
      }
      
      df
    })
    
    # feature data ====
    feature_data <- reactive({
      req(input$layer_f, input$feature_f)
      
      data <- metaboprep@data[, , input$layer_f, drop = TRUE]
      data <- data[, input$feature_f, drop = FALSE]
      
      df   <- data.frame(feature_id = input$feature_f, level = data[, 1], index = seq_len(length(data)))
      ss   <- feature_describe(data)
      ss   <- ss[c(1,11, 2:10, 12:16)]
      rownames(ss) <- input$feature_f
      
      nsd <- 5
      extreme_line <- ss[["mean"]] + (nsd * ss[["sd"]])
      
      list(
        df  = df, 
        sum = ss,
        extreme_line = extreme_line
      )
    })
    
    
    ##############
    # Overview tab
    ##############
    # plot - missingness ====
    output$overview_missingness <- renderPlot({

      ggplot(data_long(), aes(x = feature_id, y = sample_id, fill = status)) +
        geom_tile() +
        scale_fill_manual(values = c(
          "missing"  = "red",
          "excluded" = "blue",
          "present"  = "lightgrey"
        )) +
        theme_minimal() +
        labs(x = "Features", y = "Samples") +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          panel.grid = element_blank()
        )
      
    })
    
    # plot - exclusions ====
    output$overview_exclusions <- renderPlotly({
      
      p <- ggplot(excl_summary(), aes(
        x = excl_reason,
        y = count,
        fill = type,
        text = paste("Count:", count, "<br>Type:", type, "<br>Reason:", excl_reason, "<br>IDs:", id_short)
      )) +
        geom_col(position = "dodge") +
        scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
        theme_minimal() +
        labs(x = "Exclusion reason", y = "Count", fill = "Type") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = "text") |>
        layout(clickmode = "event+select")
    })
    
    # table - exclusions ====
    output$overview_excl_table <- DT::renderDT({
      d <- event_data("plotly_click")
      if (is.null(d)) return(NULL)
      
      type     <- excl_summary()[d$x, "type"]
      reason   <- excl_summary()[d$x, "excl_reason"]
      dat      <- excl_dt()
      dat      <- dat[dat$type==type & dat$excl_reason==reason, ]
      dat$type <- NULL
      colnames(dat)[which(colnames(dat)=="excl_reason")] <- "Exclusion reason"
      if (type=="samples") colnames(dat)[which(colnames(dat)=="id")] <- "Sample ID"
      if (type=="features") colnames(dat)[which(colnames(dat)=="id")] <- "Feature ID"
      
      dat
    })
    
    
    ##############
    # Feature tab
    ##############
    # plot feature scatter ====
    output$feature_scatter <- renderPlot({

      dat <- feature_data()
  
      ggplot(dat[["df"]], aes(x = index, y = level)) +
        geom_point(alpha = 0.6) +
        labs(x = "sample index", y = paste0(dat[["df"]][["feature_id"]][1], " level")) +
        geom_hline(yintercept = dat[["extreme_line"]], color = "red", linetype="dashed") +
        theme_bw()
    })
    
    # plot feature histogram ====
    output$feature_histogram <- renderPlot({
      
      dat <- feature_data()

      ggplot(dat[["df"]], aes(x = level)) +
        geom_histogram( fill = "blue", alpha = 0.6, color = "white", bins=30) +
        labs(x = "frequency", x = paste0(dat[["df"]][["feature_id"]][1], " level")) +
        geom_vline(xintercept = dat[["extreme_line"]], color = "red", linetype="dashed") +
        theme_bw()
    })
    
    # table - feature summary stats ====
    output$feature_table <- DT::renderDT({
      
      dat <- feature_data()
      ss  <- dat[["sum"]]
      rownames(ss) <- dat[["df"]][["feature_id"]][1]
      
      DT::datatable(ss, options = list(pageLength = 20)) |>
        DT::formatRound(digits = 3, columns = colnames(ss))
    })
    
    
    ##############
    # Sample tab
    ##############
    # plot 1 ====
    output$sample_plot1 <- renderPlot({
      req(input$feature, input$sample, input$layer)
      
      df <- data.frame(
        sample_id  = as.factor(dimnames(metaboprep@data)[[1]]),
        value      = metaboprep@data[, input$feature, input$layer, drop = TRUE], 
        excluded   = dimnames(metaboprep@data)[[1]] %in% unname(unlist(metaboprep@exclusions$samples))
      )
      
      ggplot(df, aes(x = sample_id, y = value, color = excluded)) +
        geom_point() +
        geom_vline(xintercept = which(as.character(df$sample_id) == input$sample), color = "red", linetype = "dashed") +
        scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red"), name = "Excluded") +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    })
    
    
    ##############
    # Table tab
    ##############
    # table 1 ====
    output$overview_table <- DT::renderDT({
      req(input$layer_t)
      
      df <- metaboprep@data[ , , input$layer, drop = TRUE]
      
      if (!is.null(input$select_rows)) {
        df <- df[input$select_rows, , drop = FALSE]
      }
      if (!is.null(input$select_cols)) {
        df <- df[, input$select_cols, drop = FALSE]
      }
      
      DT::datatable(df, options = list(pageLength = 20)) |>
        DT::formatRound(digits = 3, columns = colnames(df))
    })
    
    
    
  }
  
  shinyApp(ui, server)
}