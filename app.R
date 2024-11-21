library(cranlogs)
library(shiny)
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
  theme = bslib::bs_theme(version = 4, bootswatch = "darkly"),
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
    sidebarPanel(
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
        value = Sys.Date() - 30,
        max = Sys.Date()
        ),
      dateInput(
        inputId = "to_date", 
        label = "To:",
        value = Sys.Date(),
        max = Sys.Date()
        ),
      actionButton(
        inputId = "submit", 
        label = "Load Data",
        class = "btn-primary"
        ),
      h3("Summaries"),
      tags$div(
        style = "height: 410px; overflow-y: scroll;",
        verbatimTextOutput("download_summary")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          "Usage",
          hr(),
          plotOutput("usage_plot", height = "400px"),
          br(),
          plotOutput("cumul_plot", height = "400px")
          ),
        tabPanel(
          "Metadata",
          br(),
          fluidRow(
            column(
              width = 12, 
              h3("Metadata"), 
              DTOutput("package_info")
              )
            ),
          br(),
          fluidRow(
            column(
              width = 12, 
              h3("Dependencies"), 
              DTOutput("package_deps")
              )
            )
          ),
        tabPanel(
          "Network",
          br(),
          fluidRow(
            column(
              width = 12,
              h3("Dependency Network"),
              div(
                style = "background-color: black; padding: 20px; border-radius: 5px;",
                visNetworkOutput("package_deps_network", height = "600px")
                )
              )
            )
          ),
        tabPanel(
          "Save",
          br(),
          fluidRow(
            column(
              width = 3,
              hr(),
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
              hr()
              ),
            column(
              width = 9,
              DTOutput("data_table")
              )
            )
          )
        )
      )
    )
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
    })

  ############################################  
  # Function to get package(s) download number
  ############################################
  
  get_download_stats <- eventReactive(input$submit, {
    req(input$package_name)
    from_date <- input$from_date
    to_date <- input$to_date
    
    withProgress(message = 'Fetching download stats...', {
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
    unique_packages <- unique(data$package)
    palette <- Polychrome::createPalette(length(unique_packages), c("#ff0000", "#00ff00", "#0000ff"))
    
    data %>%
      group_by(package, !!sym(time_group)) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      ggplot() +
      aes(x = !!sym(time_group), y = count, color = package) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = setNames(palette, unique_packages)) +
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
    unique_packages <- unique(data$package)
    palette <- Polychrome::createPalette(length(unique_packages), c("#ff0000", "#00ff00", "#0000ff"))
    
    data %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate(cumulative = cumsum(count)) %>% 
      ggplot() +
      aes(x = date, y = cumulative, color = package) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = setNames(palette, unique_packages)) +
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
        Package = rep(pkg, 3),
        Type = c("Depends", "Imports", "Suggests"),
        Dependencies = c(
          get_deps(pkg, "Depends"),
          get_deps(pkg, "Imports"),
          get_deps(pkg, "Suggests")
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
  
  # Function to include network visualization
  output$package_deps_network <- renderVisNetwork({
    req(input$package_name)
    get_deps <- function(pkg, dep_type) {
      deps <- tools::package_dependencies(pkg, db = pkg_db(), which = dep_type)[[1]]
      if(is.null(deps)) return(character(0))
      deps
    }
    
    edges_list <- lapply(input$package_name, function(pkg) {
      depends <- get_deps(pkg, "Depends")
      imports <- get_deps(pkg, "Imports")
      suggests <- get_deps(pkg, "Suggests")
      rbind(
        if(length(depends) > 0) data.frame(from = pkg, to = depends, type = "Depends", color = "#9900cc"),
        if(length(imports) > 0) data.frame(from = pkg, to = imports, type = "Imports", color = "black"),
        if(length(suggests) > 0) data.frame(from = pkg, to = suggests, type = "Suggests", color = "red")
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
    
    visNetwork(nodes_df, edges_df, background = "white") %>%
      visEdges(
        arrows = "to"
        #smooth = list(enabled = TRUE, type = "curvedCW")
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength = 100,
          springConstant = 0.08
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
          label = c("Depends", "Imports", "Suggests"),
          color = c("#9900cc", "black", "red")
        ),
        useGroups = FALSE,
        width = 0.15
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



