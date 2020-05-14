# Define packages 
package_names <- c(
  "DT", 
  "ggplot2", 
  "lubridate", 
  "plotly", 
  "plyr", 
  "scales", 
  "shiny", 
  "shinycssloaders", 
  "shinythemes", 
  "tidyverse"
)

# Install packages if they haven't been installed previously
install_packages <- lapply(package_names, FUN = function(x) if(! require(x, character.only = TRUE)) install.packages(x))

# Load packages
load_packages <- lapply(package_names, require, character.only = TRUE)

# Include functions for use in the tornado plot
source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes and friends.R")

# Define UI
ui <- navbarPage(
  windowTitle = HTML("Parameter sweep app"),
  title = div("Parameter sweep app", style = "margin-right: 48px;"),
  tabPanel("Scatter plot",
           sidebarPanel(
             uiOutput("output_file"),
             uiOutput("y_axis"),
             uiOutput("x_axis"),
             uiOutput("geom_point_size"),
             width = 3
           ),
           mainPanel(
             div("This app will not work correctly if ", tags$strong("outcomes.summary.df"), " or ", tags$strong("parms.tried.df"), " does not exist in your session's global environment.", style = "background-color: #E5F0F8; color: #428bca; border: 1px solid #428bca; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
             plotlyOutput("get_scatter_plot") %>% withSpinner(color = "#428bca"), br(), br(), br(),
             width = 9
           )
  ),
  tabPanel("Tornado plot",
           sidebarPanel(
             uiOutput("outcome_variable"),
             uiOutput("method"),
             uiOutput("order"),
             uiOutput("label"),
             uiOutput("plot_bin_width"),
             width = 3
           ),
           mainPanel(
             div("This app will not work correctly if ", tags$strong("outcomes.summary.df"), " or ", tags$strong("parms.tried.df"), " does not exist in your session's global environment.", style = "background-color: #E5F0F8; color: #428BCA; border: 1px solid #428bca; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
             plotlyOutput("get_tornado_plot") %>% withSpinner(color = "#428bca"), br(), br(), br(),
             width = 9
           )
  ),
  tabPanel("Data table",
           sidebarPanel(
             uiOutput("output_file3"),
             width = 3
           ),
           mainPanel(
             div("This app will not work correctly if ", tags$strong("outcomes.summary.df"), " or ", tags$strong("parms.tried.df"), " does not exist in your session's global environment.", style = "background-color: #E5F0F8; color: #428BCA; border: 1px solid #428bca; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
             DTOutput("get_data") %>% withSpinner(color = "#428bca"), br(), br(),
             width = 9
           )
  ),
  tags$head(tags$style(HTML('
      // Custom CSS here
    ')))
)

# Define server logic
server <- function(input, output) {
  # Cache select data structures
  cached <- reactiveValues()
  
  # Get object from vector element
  get_object <- function(x) {
    t <- tryCatch(get(x), error = function(e) "Not found")
    if(length(t) < 2) object <- NULL else object <- t
    return(object)
  }
  
  # Build a slider to adjust the bin width on the tornado plot
  output$plot_bin_width <- renderUI({
    if(is.null(cached$object)) {
      return()
    } else {
      if(is.null(input$plot_bin_width)) {
        plot_bin_width <- 0.25
      } else {
        plot_bin_width <- input$plot_bin_width
      }
      sliderInput("plot_bin_width", "Bin width", min = 0.25, max = 1, step = 0.25, value = plot_bin_width)
    }
  })
  
  # Build a slider to adjust the dot size on the scatter plot
  output$geom_point_size <- renderUI({
    if(is.null(cached$object)) {
      return()
    } else {
      if(is.null(input$geom_point_size)) {
        geom_point_size <- 1.5
      } else {
        geom_point_size <- input$geom_point_size
      }
      sliderInput("geom_point_size", "Dot size", min = 0.1, max = 5.0, step = 0.1, value = geom_point_size)
    }
  })
  
  # Render data object in a searchable/sortable table
  output$get_data <- renderDT(
    {
      cached$object <- get_object(input$output_file3)
    },
    extensions = c("Buttons", "Scroller"), 
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible = FALSE, targets = c())),
      pageLength = 25, 
      dom = "Bfrtip", 
      buttons = c("colvis", "copy", "csv", "excel", "pdf"), 
      deferRender = TRUE, 
      searchDelay = 500,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
        "}"
      )
    )
  )
  
  # Build label menu for the tornado plot
  output$label <- renderUI({
    if(is.null(cached$object)) { 
      return() 
    } else {
      options <- c("No", "Yes")
      names(options) <- options
      radioButtons("label", label = "Add label", choices = options, selected = "No")
    }
  })
  
  # Build method menu for partial correlation computation
  output$method <- renderUI({
    if(is.null(cached$object)) { 
      return() 
    } else {
      
      options <- c("kendall-partial-correlation-slow",
                   "pearson-partial-correlation-fast",
                   "pearson-partial-correlation-slow",
                   "spearman-partial-correlation-slow",
                   "t-test"
      )
      names(options) <- c("Kendall", "Pearson (fast)", "Pearson (slow)", "Spearman", "T-test")
      radioButtons("method", label = "Method", choices = options)
    }
  })
  
  # Build order menu for the tornado plot
  output$order <- renderUI({
    if(is.null(cached$object)) { 
      return() 
    } else {
      options <- c("No", "Yes")
      names(options) <- options
      radioButtons("order", label = "Order by absolute value", choices = options, selected = "No")
    }
  })
  
  # Build scatter plot
  output$get_scatter_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(is.null(cached$object) | is.null(input$x_axis) | is.null(input$y_axis)) {
      return()
    } else {
      point_size <- 2
      element_text_size <- 12
      ggplotly(ggplot(cached$object, aes(x = !!rlang::sym(input$x_axis), y = !!rlang::sym(input$y_axis))) +
                 geom_point(color = "#428bca", size = input$geom_point_size) +
                 scale_y_continuous(labels = comma) +
                 scale_x_continuous(labels = comma) +
                 theme_minimal() +
                 theme(
                   plot.title = element_text(size = element_text_size),
                   axis.title.x = element_text(size = element_text_size),
                   axis.title.y = element_text(size = element_text_size),
                   legend.text = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none"
                 ))
    }
  })
  
  # Build tornado plot
  output$get_tornado_plot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    if(is.null(cached$object) | is.null(input$outcome_variable) | is.null(input$method)) {
      return()
    } else {
      what.matters = Assess.covariate.importance(outcomes.summary.df,names(parms.tried.df), input$outcome_variable, method = input$method)
      dat <- tibble(variable = names(what.matters), value = what.matters)
      dat$variable <- factor(dat$variable)
      if(input$order == "Yes") {
        dat$variable <- fct_reorder(dat$variable, abs(dat$value), .desc = FALSE)
      } else {
        dat$variable <- fct_reorder(dat$variable, dat$value, .desc = FALSE)
      }
      if(input$label == "Yes") {
        label_content <- round(dat$value, 3)
      } else {
        label_content <- ""
      }
      point_size <- 2
      element_text_size <- 12
      ggplotly(ggplot(dat, aes(x = variable, y = value)) +
                 geom_bar(color = "#428bca", fill = "#428bca", stat = "identity", width = input$plot_bin_width) +
                 geom_text(label = label_content, size = 3.5, hjust = -3) +
                 coord_flip() +
                 theme_minimal() +
                 ylab(paste0("Strength of correlation with ", input$outcome_variable)) +
                 theme(
                   plot.title = element_text(size = element_text_size),
                   axis.title.y = element_blank(),
                   axis.title.x = element_text(size = element_text_size),
                   legend.text = element_blank(),
                   legend.title = element_blank(),
                   legend.position = "none"
                 ))
    }
  })
  
  # Build output file menu
  output$output_file <- renderUI({
    if(is.null(cached$files)) {
      # Check if objects exists
      output_files <- unlist(unname(sapply(c("outcomes.summary.df", "parms.tried.df"), function(x) if(exists(x)) x)))
      
      # Conditional menu based on whether output_files is null
      if(is.null(output_files)) {
        files <- "No output files exist"
      } else {
        files <- c("Please select...", output_files)
      }
      names(files) <- files
      cached$files <- files
    } 
    selected_option <- ifelse(length(cached$files[cached$files != "parms.tried.df"]) > 1, cached$files[cached$files != "parms.tried.df"][2], cached$files[cached$files != "parms.tried.df"][1])
    selectInput("output_file", "Output file", choices = cached$files[cached$files != "parms.tried.df"], selected = selected_option)
  })
  
  # Build output file menu
  output$output_file3 <- renderUI({
    selected_option <- ifelse(length(cached$files) > 1, cached$files[2], cached$files[1])
    selectInput("output_file3", "Output file", choices = cached$files, selected = selected_option)
  })
  
  # Build x_axis menu
  output$x_axis <- renderUI({
    if(is.null(cached$object)) {
      return()
    } else {
      options <- sort(names(cached$object))
      names(options) <- options
      options <- options[options != "etiquette"]
      cached$options <- options
      selectInput("x_axis", "X axis", choices = options, selected = options[2])
    }
  })
  
  # Build y_axis menu
  output$y_axis <- renderUI({
    cached$object <- get_object(input$output_file)
    if(is.null(cached$object)) {
      return()
    } else {
      cached$parameters_swept <- get_object("parms.tried.df")
      if(!is.null(cached$parameters_swept)) {
        options <- sort(names(cached$object)[! names(cached$object) %in% names(cached$parameters_swept)])
      } else {
        options <- sort(names(cached$object))
      }
      names(options) <- options
      options <- options[options != "etiquette"]
      cached$options <- options
      selectInput("y_axis", "Y axis", choices = options)
    }
  })
  
  # Build outcome variable menu
  output$outcome_variable <- renderUI({
    if(is.null(cached$object)) {
      return()
    } else {
      options <- sort(cached$options[! cached$options %in% names(cached$parameters_swept)])
      names(options) <- options
      selectInput("outcome_variable", "Outcome variable", choices = options, selected = options[1])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)