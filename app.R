library(cranlogs)
library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(patchwork)
library(tidyr)
library(purrr)
library(tools)
library(plotly)
library(DT)
library(visNetwork)

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
  titlePanel("CRAN Package Usage"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "package_name", "Select CRAN Packages:",
        choices = NULL,
        multiple = TRUE,
        options = list(maxItems = 10)
        ),
      radioButtons(
        "time_unit", "Time Unit:",
        choices = c(
          "Daily" = "daily",
          "Weekly" = "weekly",
          "Monthly" = "monthly"
          ),
        selected = "daily"
        ),
      dateInput(
        "from_date", "From:",
        value = Sys.Date() - 30,
        max = Sys.Date()
        ),
      dateInput(
        "to_date", "To:",
        value = Sys.Date(),
        max = Sys.Date()
        ),
      actionButton(
        "submit", "Get Download Stats",
        class = "btn-primary"
        ),
      conditionalPanel(
        condition = "input.tabset === 'Network'",
        checkboxGroupInput(
          "dep_types", 
          "Dependency Types:",
          choices = c("Depends", "Imports", "Suggests", "Enhances"),
          selected = c("Depends", "Imports")
          ),
        sliderInput(
          "network_depth", 
          "Dependency Depth:",
          min = 1, 
          max = 3, 
          value = 1, 
          step = 1
          ),
        actionButton(
          "refresh_network", 
          "Refresh Network",
          class = "btn-primary"
          )
      ),
      h3("Summary"),
      tags$div(
        style = "height: 350px; overflow-y: scroll;",
        verbatimTextOutput("download_summary")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          "Downloads",
          plotOutput("download_plot", height = "400px"),
          br(),
          plotOutput("total_downloads", height = "400px")
          ),
        tabPanel(
          "Metadata",
          br(),
          fluidRow(
            column(
              12, 
              h3("Information"), 
              DTOutput("package_info")
              )
            ),
          br(),
          fluidRow(
            column(
              12, 
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
              12, 
              h3("Package Dependency Network"), 
              div(
                style = "position: relative;",
                visNetworkOutput("dep_network", height = "800px")
                ), 
              verbatimTextOutput("network_debug")
              )
            )
          )
        )
      )
    )
  )

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
        maxItems = 10,
        placeholder = 'Select packages'
        )
      )
    })

  # Function to get download stats
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
  
  # Plot the downloads based on selected time unit
  output$download_plot <- renderPlot({
    req(get_download_stats())
    data <- get_download_stats()
    time_group <- switch(
      input$time_unit,
      "daily" = "date",
      "weekly" = "weekly",
      "monthly" = "monthly"
      )
    data %>%
      group_by(package, !!sym(time_group)) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      ggplot() +
      aes(x = !!sym(time_group), y = count, color = package) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x = NULL,
        y = NULL,
        title = paste(stringr::str_to_title(input$time_unit), "Downloads")) +
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
  
  # Plot total downloads
  output$total_downloads <- renderPlot({
    req(get_download_stats())
    get_download_stats() %>%
      group_by(package) %>%
      arrange(date) %>%
      mutate(cumulative = cumsum(count)) %>% 
      ggplot() +
      aes(x = date, y = cumulative, color = package) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
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
        legend.position = "none"
      )
  })
  
  # Display package metadata
  output$package_info <- renderDT({
    req(input$package_name)
    safe_get_info <- function(pkg_name) {
      tryCatch({
        pkg_info <- pkg_db()[pkg_db()$Package == pkg_name, , drop = FALSE]
        if(nrow(pkg_info) == 0) {
          return(
            data.frame(
              Package = pkg_name,
              Version = "Not found",
              Published = as.Date(NA),
              Title = "Not found",
              Author = "Not found",
              Maintainer = "Not found",
              License = "Not found",
              NeedsCompilation = "Unknown",
              stringsAsFactors = FALSE
              )
            )
          }
        
        # Safe getter function
        safe_get <- function(field, default = "Unknown") {
          val <- pkg_info[[field]]
          if(is.null(val) || all(is.na(val)) || length(val) == 0) {
            return(default)
          }
          return(val[1])
        }
        
        # Create data frame with proper handling of each field
        data.frame(
          Package = pkg_name,
          Version = safe_get("Version"),
          Published = as.Date(safe_get("Published", NA)),
          Title = safe_get("Title"),
          Author = safe_get("Author"),
          Maintainer = safe_get("Maintainer"),
          License = safe_get("License"),
          NeedsCompilation = safe_get("NeedsCompilation"),
          stringsAsFactors = FALSE,
          check.names = FALSE
          )
        }, 
        error = function(e) {
          data.frame(
            Package = pkg_name,
            Version = "Error",
            Published = as.Date(NA),
            Title = "Error retrieving package information",
            Author = "Unknown",
            Maintainer = "Unknown",
            License = "Unknown",
            NeedsCompilation = "Unknown",
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          }
        )
      }
    
    # Get info for all selected packages
    info_list <- lapply(input$package_name, safe_get_info)
    info_df <- do.call(rbind, info_list)
    
    # Ensure all character columns
    char_cols <- setdiff(names(info_df), "Published")
    info_df[char_cols] <- lapply(info_df[char_cols], as.character)
    
    # Create the datatable
    DT::datatable(
      info_df,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE,
      escape = FALSE  # Allow HTML in cells if present
    ) %>%
      DT::formatStyle(
        columns = names(info_df),
        backgroundColor = "rgb(25, 25, 25)",
        color = "white"
      ) %>%
      DT::formatDate(
        columns = "Published",
        method = "toLocaleDateString"
      )
  })
  
  # Get recursive dependencies
  get_recursive_deps <- reactive({
    req(input$package_name, input$dep_types)
    
    # Initialize empty data structures
    nodes <- data.frame(
      id = character(),
      label = character(),
      level = numeric(),
      group = character(),
      stringsAsFactors = FALSE
    )
    
    edges <- data.frame(
      from = character(),
      to = character(),
      type = character(),
      stringsAsFactors = FALSE
    )
    
    visited <- character()
    
    add_deps <- function(pkg, depth = 0) {
      if(depth > input$network_depth || pkg %in% visited) {
        return()
      }
      
      visited <<- c(visited, pkg)
      
      # Always add the current package as a node
      if(!pkg %in% nodes$id) {
        new_node <- data.frame(
          id = pkg,
          label = pkg,
          level = depth,
          group = ifelse(pkg %in% input$package_name, "selected", "dependency"),
          stringsAsFactors = FALSE
        )
        nodes <<- rbind(nodes, new_node)
      }
      
      # Process each dependency type
      for(dep_type in input$dep_types) {
        # Safely get dependencies
        deps <- tryCatch({
          pkg_deps <- tools::package_dependencies(
            pkg, 
            db = pkg_db(), 
            which = dep_type
            )[[1]]
          if(is.null(pkg_deps)) character(0) else pkg_deps
        }, error = function(e) {
          warning(
            sprintf(
              "Error getting dependencies for package %s: %s", 
              pkg, 
              e$message
              )
            )
          character(0)
          }
        )
        
        # If there are dependencies, add edges and process them
        if(length(deps) > 0) {
          # Create edges data frame
          new_edges <- data.frame(
            from = rep(pkg, length(deps)),
            to = deps,
            type = rep(dep_type, length(deps)),
            stringsAsFactors = FALSE
          )
          edges <<- rbind(edges, new_edges)
          # Recursively process each dependency
          lapply(deps, function(d) add_deps(d, depth + 1))
        }
      }
    }
    
    # Process each selected package
    lapply(input$package_name, function(pkg) add_deps(pkg))
    # Ensure we have at least the selected packages as nodes
    if(nrow(nodes) == 0) {
      nodes <- data.frame(
        id = input$package_name,
        label = input$package_name,
        level = 0,
        group = "selected",
        stringsAsFactors = FALSE
      )
    }
    list(
      nodes = unique(nodes),
      edges = unique(edges)
    )
  })
  
  # Render the network visualization with improved error handling
  output$dep_network <- renderVisNetwork({
    req(get_recursive_deps())
    deps <- get_recursive_deps()
    
    # Create the network with error checking
    visNetwork(
      nodes = deps$nodes,
      edges = deps$edges
    ) %>%
      visGroups(groupname = "selected", color = "#ff7f0e") %>%
      visGroups(groupname = "dependency", color = "#1f77b4") %>%
      visEdges(
        arrows = "to",
        color = list(
          color = "#666666",
          highlight = "#ff3333"
        )
      ) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE,
          degree = 1,
          hover = TRUE
        ),
        nodesIdSelection = TRUE
      ) %>%
      visLayout(
        randomSeed = 123,
        improvedLayout = TRUE
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          centralGravity = 0.01,
          springLength = 100,
          springConstant = 0.08
        ),
        stabilization = list(
          enabled = TRUE,
          iterations = 1000
        )
      ) %>%
      visInteraction(
        hideEdgesOnDrag = TRUE,
        hover = TRUE
      ) %>%
      addFontAwesome() %>%
      visLegend(
        width = 0.1,
        position = "left",
        main = "Package Types",
        addNodes = list(
          list(label = "Selected Package", group = "selected"),
          list(label = "Dependency", group = "dependency")
        ),
        addEdges = list(
          list(label = "Depends/Imports", color = "#666666")
        )
      )
  })
  
  # Display package dependencies
  output$package_deps <- renderDT({
    req(input$package_name)
    
    # Create a function to safely get dependencies
    get_deps <- function(pkg, dep_type) {
      deps <- tools::package_dependencies(
        pkg, 
        db = pkg_db(), 
        which = dep_type
        )[[1]]
      if(is.null(deps)) return("None")
      paste(deps, collapse = ", ")
    }
    
    # Create dependency data frame for all packages
    deps_list <- lapply(input$package_name, function(pkg) {
      data.frame(
        Package = rep(pkg, 3),
        DependencyType = c("Depends", "Imports", "Suggests"),
        Dependencies = c(
          get_deps(pkg, "Depends"),
          get_deps(pkg, "Imports"),
          get_deps(pkg, "Suggests")
        )
      )
    })
    
    # Combine all dependencies into one data frame
    deps_df <- do.call(rbind, deps_list)
    
    DT::datatable(
      deps_df,
      options = list(
        pageLength = 15,
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
  
  # Display the download statistics summary
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