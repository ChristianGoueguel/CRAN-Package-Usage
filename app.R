library(cranlogs)
library(shiny)
library(shinyjs)
library(visNetwork)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(tools)
library(DT)
library(igraph)
library(colourpicker)

################################################################################

ui <- fluidPage(
  theme = bslib::bs_theme(
    base_font = sass::font_google("Roboto", local = TRUE),
    version = 5, 
    bootswatch = "darkly"),
  tags$head(
    tags$style(HTML("
      #dep_network {
        width: 100%;
        height: 800px !important;
        background-color: #222;
        border: 1px solid #444;
      }
    "))
  ),
  titlePanel(
    title = "CRAN Package Usage", 
    windowTitle = "CRAN Package Usage App"
    ),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      position = "left",
      fluid = TRUE,
      class = "p-3 border rounded",
      selectizeInput(
        inputId = "package_name", 
        label = "Select CRAN Packages:",
        choices = NULL,
        multiple = TRUE,
        options = list(maxItems = 20)
        ),
      radioButtons(
        inputId = "time_unit", 
        label = "Time Unit:",
        choices = c(
          "Daily" = "daily",
          "Weekly" = "weekly",
          "Monthly" = "monthly"
          ),
        selected = "daily"
        ),
      dateInput(
        inputId = "from_date",
        label = "From:",
        value = Sys.Date() - 33,
        max = Sys.Date(),
        width = "200px"
        ),
      dateInput(
        inputId = "to_date", 
        label = "To:",
        value = Sys.Date() - 2,
        max = Sys.Date(),
        width = "200px"
        ),
      actionButton(
        inputId = "submit", 
        label = "Load Data",
        class = "btn-primary"
        ),
      br(),
      br(),
      h4("Download Summary"),
      tags$div(
        style = "height: 410px; overflow-y: scroll;",
        verbatimTextOutput(
          outputId = "download_summary",
          placeholder = FALSE
          )
        )
      ),
    mainPanel = mainPanel(
      class = "p-3 border rounded",
      tabsetPanel(
        id = "tabset",
        type = "pills",
        tabPanel(
          "Usage",
          hr(),
          plotOutput(
            outputId = "usage_plot", 
            height = "400px"
            ),
          br(),
          plotOutput(
            outputId = "cumul_plot", 
            height = "400px"
            )
          ),
        tabPanel(
          "Peak Usage",
          hr(),
          numericInput(
            inputId = "numbPeak", 
            label = "Select Peak Count:", 
            value = 2, 
            min = 1,
            max = 10,
            step = 1,
            width = "150px"
          ),
          plotOutput(
            outputId = "peak_download_plot",  
            height = "425px"
            ),
          br(),
          DTOutput(
            outputId = "peak_download_table",
            width = NULL
            )
          ),
        tabPanel(
          "Stats",
          hr(),
          selectInput(
            inputId = "time_unit_selection",
            label = "Select Week or Month:",
            choices = NULL,
            selected = NULL
          ),
          br(),
          plotOutput(
            outputId = "stats_plot",
            height = "400px"
          ),
          br(),
          hr(),
          DTOutput(
            outputId = "descriptive_stats",
            width = NULL
            )
        ),
        tabPanel(
          "Network",
          hr(),
          fluidRow(
            column(
              width = 12,
              h3("Dependency Network"),
              div(
                style = "background-color: black; padding: 20px; border-radius: 5px;",
                visNetworkOutput(
                  outputId = "package_deps_network", 
                  height = "600px"
                  )
                )
              )
            )
          ),
        tabPanel(
          "Metadata",
          hr(),
          fluidRow(
            column(
              width = 12, 
              h3("Metadata"), 
              DTOutput(
                outputId = "package_info",
                width = NULL
                )
              )
            ),
          br(),
          fluidRow(
            column(
              width = 12, 
              h3("Dependencies"), 
              DTOutput(
                outputId = "package_deps",
                width = NULL
                )
              )
            )
          ),
        tabPanel(
          "Save",
          hr(),
          fluidRow(
            column(
              width = 3,
              downloadButton(
                outputId = "download_data", 
                label = "Download CSV", 
                class = "btn-primary"
                ),
              br(), 
              br(),
              downloadButton(
                outputId = "download_usagePlot", 
                label = "Download usagePlot", 
                class = "btn-success"
                ),
              br(), 
              br(),
              downloadButton(
                outputId = "download_cumulPlot", 
                label = "Download cumulPlot", 
                class = "btn-success"
                ),
              ),
            column(
              width = 9,
              DTOutput(
                outputId = "data_table",
                width = NULL
                )
              )
            )
          )
        )
      )
    ),
  )

################################################################################

server <- function(input, output, session) {
  pkg_db <- reactive({
    as.data.frame(available.packages(repos = "https://cloud.r-project.org"))
  })
  
  observe({
    updateSelectizeInput(
      session, "package_name",
      choices = sort(pkg_db()$Package),
      selected = c("ggplot2", "dplyr", "rlang", "Rcpp", "glue"),
      server = TRUE,
      options = list(
        maxItems = 20,
        placeholder = 'Select packages'
        )
      )
    
    req(get_download_stats())
    data <- get_download_stats()
    time_group <- switch(
      input$time_unit,
      "weekly" = "weekly",
      "monthly" = "monthly"
    )
    if (input$time_unit %in% c("weekly", "monthly")) {
      updateSelectInput(
        session,
        "time_unit_selection",
        choices = unique(data[[time_group]]),
        selected = unique(data[[time_group]])[1]
      )
    }
    })

  ############################################  
  # Function to get package(s) download number
  ############################################
  
  get_download_stats <- eventReactive(input$submit, {
    req(input$package_name)
    from_date <- input$from_date
    to_date <- input$to_date
    
    withProgress(message = 'Loading...', {
      all_downloads <- lapply(input$package_name, function(pkg) {
        incProgress(1 / length(input$package_name))
        downloads <- try(cran_downloads(pkg, from = from_date, to = to_date))
        if(inherits(downloads, "try-error")) {
          return(NULL)
        }
        downloads$package <- pkg
        return(downloads)
      })
    })
    downloads <- bind_rows(all_downloads[!sapply(all_downloads, is.null)])
    if(nrow(downloads) == 0) {
      return(NULL)
    }
    downloads %>%
      mutate(
        weekly = floor_date(date, "week"),
        monthly = floor_date(date, "month")
      )
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      get_download_stats() %>% 
        transmute(
          package = package,
          year = year(date),
          day = day(date),
          month = month(monthly, label = TRUE, abbr = FALSE),
          download = count) %>%
        relocate(month, .after = package) %>% 
        relocate(day, .after = month) %>%
        relocate(year, .after = day) %>%
        relocate(download, .after = year),
      options = list(pageLength = 5),
      rownames = FALSE
      ) %>%
      DT::formatStyle(
      columns = c("package", "month", "day", "year", "download"),
      backgroundColor = "rgb(25, 25, 25)",
      color = "white"
      )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        get_download_stats() %>% 
          transmute(
            package = package,
            year = year(date),
            day = day(date),
            month = month(monthly, label = TRUE, abbr = FALSE),
            download = count) %>%
          relocate(month, .after = package) %>% 
          relocate(day, .after = month) %>%
          relocate(year, .after = day) %>%
          relocate(download, .after = year), 
        file, 
        row.names = FALSE
        )
      }
    )
  
  ################
  # Color mapping
  ################
  
  color_mapping <- reactiveVal()
  xcolor <- function(x) {
    color_mapping(setNames(character(0), character(0)))  
    unique_packages <- sort(unique(x))
    
    new_colors <- Polychrome::createPalette(
      N = length(unique_packages), 
      seedcolors = c(
        "#ff0000", "#00ff00", "#0000ff", "#FF00FF", 
        "#FFFF00", "#00FFFF", "#FFA500"
        ), 
      range = c(30, 90),
      target = "normal",
      M = 50000
    )
    
    updated_mapping <- setNames(new_colors, unique_packages)
    color_mapping(updated_mapping)
    updated_mapping
  }
  
  
  ######################################################
  # Plot package(s) download based on selected time unit
  ######################################################
  
  usage_plot_reactive <- reactive({
    req(get_download_stats())
    data <- get_download_stats()
    time_group <- switch(
      input$time_unit,
      "daily" = "date",
      "weekly" = "weekly",
      "monthly" = "monthly"
    )
    
    palette <- xcolor(data$package)
    
    data %>%
      group_by(package, !!sym(time_group)) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      ggplot() +
      aes(x = !!sym(time_group), y = count, color = package) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = palette) +
      labs(
        x = NULL,
        y = NULL,
        title = paste(stringr::str_to_title(input$time_unit), "Downloads")
      ) +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey20"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "black"),
        legend.position = "bottom"
      )
  })
  output$usage_plot <- renderPlot({
    usage_plot_reactive()
  })
  output$download_usagePlot <- downloadHandler(
    filename = function() {
      paste("usagePlot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = usage_plot_reactive(), device = "png", width = 16, height = 8)
    }
  )
  
  
  ################################
  # Plot cumulative downloads
  ################################
  
  cumul_plot_reactive <- reactive({ 
    req(get_download_stats())
    data <- get_download_stats()
    palette <- xcolor(data$package)
    
    data %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate(cumulative = cumsum(count)) %>% 
      ggplot() +
      aes(x = date, y = cumulative, color = package) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = palette) +
      labs(
        x = NULL,
        y = NULL,
        title = "Cumulative Downloads"
      ) +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey20"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "black"),
        legend.position = "bottom"
      )
  })
  output$cumul_plot <- renderPlot({
    cumul_plot_reactive()
  })
  output$download_cumulPlot <- downloadHandler(
    filename = function() {
      paste("cumulPlot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = cumul_plot_reactive(), device = "png", width = 16, height = 8)
    }
  )
  
  #############
  # Peak Usage
  #############
  
  output$peak_download_table <- DT::renderDataTable({
    req(input$numbPeak)
    DT::datatable(
      get_download_stats() %>%
        select(package, date, count) %>%
        rename(download = count) %>%
        group_by(package) %>%
        arrange(desc(download)) %>%
        slice_max(order_by = download, n = input$numbPeak, with_ties = FALSE) %>%
        ungroup(),
      options = list(pageLength = 5),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        columns = c("package", "date", "download"),
        backgroundColor = "rgb(25, 25, 25)",
        color = "white"
      )
  })
  
  peak_download_plot_reactive <- reactive({
    req(input$numbPeak)
    data <- get_download_stats() %>%
      select(package, date, count) %>%
      rename(download = count) %>%
      group_by(package) %>%
      slice_max(order_by = download, n = input$numbPeak, with_ties = FALSE) %>%
      ungroup()
    
    palette <- xcolor(data$package)
    
    data %>%
      group_by(package) %>%
      ggplot() +
      aes(x = as.character(date), y = download, fill = package) +
      geom_col(position = "stack", width = .3, show.legend = TRUE) +
      scale_fill_manual(values = palette) +
      labs(x = " ", y = " ", title = "Peak Download Days") +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey20"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.position = "bottom"
      )
  })
  
  output$peak_download_plot <- renderPlot({
    peak_download_plot_reactive()
  })
  
  
  ############
  # Stats Plot
  ############
  
  stats_reactive <- reactive({
    req(get_download_stats(), input$time_unit_selection)
    data <- get_download_stats()
    palette <- xcolor(data$package)
    
    time_group <- switch(
      input$time_unit,
      "weekly" = "weekly",
      "monthly" = "monthly"
    )
    
    formatted_time_selection <- if (input$time_unit == "monthly") {
      format(as.Date(paste0(input$time_unit_selection, "-01")), "%B %Y")
    } else {
      date <- input$time_unit_selection
      paste(year(date), "-", "W", isoweek(date))
    }
    
    title_prefix <- if (input$time_unit == "weekly") {
      "Downloads in the week of "
    } else {
      "Downloads in "
    }
    
    filtered_data <- data %>%
      filter(!!sym(time_group) == input$time_unit_selection)
    
    p1 <- filtered_data %>%
      group_by(package) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      ggplot() +
      aes(x = count, y = factor(package), fill = package) +
      geom_col(width = 0.8, show.legend = TRUE) +
      scale_fill_manual(values = palette) +
      labs(x = " ", y = " ") +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey20"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "black"),
        legend.position = "bottom"
      )
    
    p2 <- filtered_data %>%
      group_by(package) %>%
      ggplot() +
      aes(x = count, y = factor(package), fill = package) +
      geom_boxplot(
        staplewidth = 0.5, 
        color = "white",
        outlier.fill = NULL,
        outlier.shape = 21,
        outlier.size = 3,
        show.legend = TRUE
        ) +
      scale_fill_manual(values = palette) +
      labs(x = " ", y = " ") +
      theme_dark(base_size = 15) +
      theme(
        text = element_text(color = "white"),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey30"),
        panel.grid.minor = element_line(color = "grey20"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "black"),
        legend.position = "bottom"
      )
    
    ggpubr::annotate_figure(
      ggpubr::ggarrange(
        p1, p2,
        align = "hv",
        legend = "bottom",
        common.legend = TRUE
      ),
      fig.lab = NULL,
      fig.lab.face = "plain",
      fig.lab.size = 0,
      bottom = NULL,
      top = ggpubr::text_grob(paste(title_prefix, formatted_time_selection), color = "white", size = 18),
      left = NULL,
      right = NULL
    ) + theme(
      plot.background = element_rect(fill = "black", color = "white"))
  })
  
  output$stats_plot <- renderPlot({
    stats_reactive()
  })
  
  
  output$descriptive_stats <- DT::renderDataTable({
    req(get_download_stats(), input$time_unit_selection)
    data <- get_download_stats()
    
    time_group <- switch(
      input$time_unit,
      "weekly" = "weekly",
      "monthly" = "monthly"
    )
    
    filtered_data <- data %>%
      filter(!!sym(time_group) == input$time_unit_selection)
    
    descriptive_stats <- filtered_data %>%
      group_by(package) %>%
      summarise(
        mean = mean(count, na.rm = TRUE) %>% round(digits = 2),
        median = median(count, na.rm = TRUE) %>% round(digits = 2),
        sd = sd(count, na.rm = TRUE) %>% round(digits = 2),
        iqr = IQR(count, na.rm = TRUE) %>% round(digits = 2),
        min = min(count, na.rm = TRUE) %>% round(digits = 2),
        max = max(count, na.rm = TRUE) %>% round(digits = 2),
        total = sum(count, na.rm = TRUE) %>% round(digits = 2),
        .groups = "drop"
      ) %>%
      arrange(desc(mean))
    
    DT::datatable(
      descriptive_stats,
      options = list(
        pageLength = 5,
        dom = 'tBip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  
  
  ######################
  # Package(s) Metadata
  ######################
  
  get_package_info <- function(pkg_name) {
    tryCatch({
      pkg_info <- pkgsearch::cran_packages(pkg_name)
      if(nrow(pkg_info) == 0) {
        return(
          data.frame(
            Package = pkg_name,
            Version = "Not found",
            Title = "Not found",
            Maintainer = "Not found",
            License = "Not found",
            NeedsCompilation = "Unknown",
            `Date/Publication` = as.Date(NA),
            stringsAsFactors = FALSE
          )
        )
      }
      
      safe_get <- function(field, default = "Unknown") {
        val <- pkg_info[[field]]
        if(is.null(val) || all(is.na(val)) || length(val) == 0) {
          return(default)
        }
        return(val[1])
      }
      
      data.frame(
        Package = pkg_name,
        Version = safe_get("Version"),
        Title = safe_get("Title"),
        Maintainer = safe_get("Maintainer"),
        License = safe_get("License"),
        Compilation = safe_get("NeedsCompilation"),
        Released = as.Date(safe_get("Date/Publication", NA)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, 
    error = function(e) {
      data.frame(
        Package = pkg_name,
        Version = "Error",
        Title = "Error retrieving package information",
        Maintainer = "Unknown",
        License = "Unknown",
        Compilation = "Unknown",
        Released = as.Date(NA),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
  }
  
  output$package_info <- renderDT({
    info_list <- lapply(input$package_name, get_package_info)
    info_df <- do.call(rbind, info_list)
    char_cols <- setdiff(names(info_df), "Released")
    info_df[char_cols] <- lapply(info_df[char_cols], as.character)
    
    DT::datatable(
      info_df,
      options = list(
        pageLength = 20,
        dom = 't',
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = which(names(info_df) == "Description"),
            width = "300px",
            render = DT::JS(
              "function(data, type, row) {
              if (type === 'display' && data != null) {
                return '<div style=\"max-width: 300px; white-space: normal;\">' + data + '</div>';
              }
              return data;
            }"
            )
          )
        )
      ),
      rownames = FALSE,
      escape = FALSE
    ) %>%
      DT::formatStyle(
        columns = names(info_df),
        backgroundColor = "rgb(25, 25, 25)",
        color = "white"
      ) %>%
      DT::formatDate(
        columns = "Released",
        method = "toLocaleDateString"
      )
  })
  
  
  #########################
  # Package(s) Dependencies
  #########################
  
  output$package_deps <- renderDT({
    req(input$package_name)
    
    get_deps <- function(pkg, dep_type) {
      deps <- tools::package_dependencies(pkg, db = pkg_db(), which = dep_type)[[1]]
      if(is.null(deps)) return("None")
      paste(deps, collapse = ", ")
      
    }
    
    deps_list <- lapply(input$package_name, function(pkg) {
      data.frame(
        Package = rep(pkg, 4),
        Type = c("Depends", "Imports", "Suggests", "Enhances"),
        Dependencies = c(
          get_deps(pkg, "Depends"),
          get_deps(pkg, "Imports"),
          get_deps(pkg, "Suggests"),
          get_deps(pkg, "Enhances")
        )
      )
    })
    
    deps_df <- do.call(rbind, deps_list)
    
    DT::datatable(
      deps_df,
      options = list(
        pageLength = 60,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        columns = names(deps_df),
        backgroundColor = "rgb(25, 25, 25)",
        color = "white"
      )
  })
  
  #####################
  # Package(s) Network
  #####################
  
  # Regular Dependencies
  output$package_deps_network <- renderVisNetwork({
    req(input$package_name)
    get_deps <- function(pkg, dep_type) {
      deps <- tools::package_dependencies(
        packages = pkg, 
        db = pkg_db(), 
        which = dep_type, 
        recursive = FALSE, 
        reverse = FALSE
        )[[1]]
      if(is.null(deps)) return(character(0))
      deps
    }
    
    edges_list <- lapply(input$package_name, function(pkg) {
      depends <- get_deps(pkg, "Depends")
      imports <- get_deps(pkg, "Imports")
      suggests <- get_deps(pkg, "Suggests")
      enhances <- get_deps(pkg, "Enhances")
      rbind(
        if(length(depends) > 0) data.frame(from = pkg, to = depends, type = "Depends", color = "#9900cc"),
        if(length(imports) > 0) data.frame(from = pkg, to = imports, type = "Imports", color = "black"),
        if(length(suggests) > 0) data.frame(from = pkg, to = suggests, type = "Suggests", color = "red"),
        if(length(enhances) > 0) data.frame(from = pkg, to = enhances, type = "Enhances", color = "gold")
      )
    })
    
    edges_df <- do.call(rbind, edges_list)
    all_packages <- unique(c(edges_df$from, edges_df$to))
    
    nodes_df <- data.frame(
      id = all_packages,
      label = all_packages,
      value = sapply(all_packages, function(pkg) {
        sum(edges_df$from == pkg | edges_df$to == pkg)
      }),
      color = ifelse(all_packages %in% input$package_name, "#000080", "#00b300"),
      font.color = ifelse(all_packages %in% input$package_name, "black", "black"),
      title = paste("<p style='color: black;'>", all_packages, "</p>")
    )
    
    visNetwork(
      nodes = nodes_df, 
      edges = edges_df, 
      background = "white"
      ) %>%
      visEdges(
        arrows = "to",
        smooth = list(enabled = TRUE, type = "curvedCCW", roundness = 0.1),
        shadow = list(enabled = TRUE)
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength = 100,
          springConstant = 0.08,
          avoidOverlap = 0.1
        )
      ) %>%
      visLayout(randomSeed = 123) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE,
          degree = 1,
          hover = TRUE
        ),
        nodesIdSelection = TRUE
      ) %>%
      visLegend(
        addNodes = data.frame(
          label = c("Package", "Dependency"),
          color = c("#000080", "#00b300"),
          shape = "dot",
          font.color = "black"
        ),
        addEdges = data.frame(
          label = c("Depends", "Imports", "Suggests", "Enhances"),
          color = c("#9900cc", "black", "red", "gold")
        ),
        useGroups = FALSE,
        width = 0.17
      ) %>%
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE
      )
  })
  
  
  ##############################
  # Package(s) Download Summary
  ##############################
  
  output$download_summary <- renderText({
    req(get_download_stats())
    data <- get_download_stats()
    
    summary_stats <- data %>%
      group_by(package) %>%
      summarise(
        total_downloads = format(sum(count), big.mark = ","),
        avg_downloads_per_day = format(round(mean(count), 0), big.mark = ","),
        avg_downloads_per_week = format(round(sum(count) / n_distinct(weekly), 0), big.mark = ","),
        avg_downloads_per_month = format(round(sum(count) / n_distinct(monthly), 0), big.mark = ","),
        .groups = "drop"
      )
    
    paste(
      sapply(1:nrow(summary_stats), function(i) {
        stats <- summary_stats[i, ]
        sprintf(
          "Package: %s\nTotal Downloads: %s\nAverage Downloads per Day: %s\nAverage Downloads per Week: %s\nAverage Downloads per Month: %s\n\n",
          stats$package,
          stats$total_downloads,
          stats$avg_downloads_per_day,
          stats$avg_downloads_per_week,
          stats$avg_downloads_per_month
        )
      }),
      collapse = ""
    )
  })
}

shinyApp(ui, server)



