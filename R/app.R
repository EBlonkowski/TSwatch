
#' @import shiny
#' @importFrom tsibble index as_tsibble
#' @importFrom purrr map is_empty
NULL

################################################################################
## Timeplot module

timeplot_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        plotly::plotlyOutput(ns("plot"))
    )
}

timeplot_module <- function(input, output, session, data) {
    output$plot <- plotly::renderPlotly({
        tsdata <- data()
        timeseries_plot(tsdata)
    })
}

################################################################################
## Season plot module

seasonplot_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        uiOutput(ns("select_period")),
        plotOutput(ns("plot"))
        #plotOutput(ns("subseries_plot"))
    )
}

seasonplot_module <- function(input, output, session, data) {

    
    output$select_period <- renderUI({
        ns <- session$ns
        tsdata <- data()
        
        selectInput(
            ns("period"),
            label = "Period choice",
            choices = common_periods(tsdata))
        
    })
    
    output$plot <- renderPlot({
        tsdata <- data()
        gg_season(tsdata, labels = 'left', y = measured_vars(tsdata)[1]) #+
            #theme_dark()
    })
    
    
    output$subseries_plot <- renderPlot({
        tsdata <- data()
        gg_subseries(tsdata, y = measured_vars(tsdata)[1]) #+
        #theme_dark()
    })
}

################################################################################
## Versus plot module

versusplot_UI <- function(id) {
    ns <- NS(id)
    
    
    uiOutput(ns('select'))
}

versusplot_module <- function(input, output, session, data) {
    # input check
    assertthat::assert_that(is_tsibble(data))
    
    output$select <- renderUI({
        ns <- session$ns
        # if the time series is univariate
        if(purrr::is_empty(key(data))) {
            return (tagList('Need a multivariate time series for plotting'))
        }
        cols <- key_data(data)$key
        tagList(
            selectInput(ns("variable"), "X axis variable:", 
                        setNames(as.list(cols), cols)),
            plotOutput(ns("plot"))
        )
    })
    
    output$plot <- renderPlot({
        print(input$variable)
        versus_plot(data, input$variable)
    })
    
}

################################################################################
## Variable and key selection module


select_UI <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns('select'))
}

select_module <- function(input, output, session, data) {
    # input check
    assertthat::assert_that(is_tsibble(data))
    
    output$select <- renderUI({
        ns <- session$ns
        
        # if we have only one key AND one key column
        if(n_keys(data) >= 1 && !is_empty(key_vars(data))) {
            # we save the unique keys
            observation_units <- key_data(data)[[1]]
        }
        variables <- measured_vars(data)
        
        tagList(
            # if there is a single time series (single obervation unit / key)
            if(n_keys(data) < 2) {
                # if we have at least one key column
                if(!is_empty(key_vars(data)))
                    paste("Only one time series: ", observation_units)
            } else {
                
                checkboxGroupInput(
                    ns("key"), key(data)[[1]],
                    setNames(as.list(observation_units), observation_units),
                    selected = observation_units[1]
                    )
            },
            
                checkboxGroupInput(ns("variable"), "Variables:", 
                            setNames(as.list(variables), variables),
                            selected = variables)        
            )
    })
    
    # Key/variables selection
    # compute the cut TSE
    selected_ts <- reactive({
        shiny::validate(
            # app state is corrupted
            need(is_tsibble(data), '[cor_cut_ctl] Need a tsibble')
        )
        
        
        # if we have different keys
        if(n_keys(data) > 1) {
            key_column <- key(data)[[1]]
            
            # select variables and keys
            selected <- filter(data, !!key_column %in% input$key) %>%
                select(!!index(data), !!key_column, !!(input$variable))
            
        } else {
            selected <- select(data, !!index(data), !!(input$variable))
        }
        
        shiny::validate(
            # app state is corrupted
            need(is_tsibble(selected), '[cor_cut_ctl] Need a tsibble')
        )
        
        print(selected)
        return(selected)
    })
    
    return(selected_ts)
}

# Define UI for miles per gallon app ----
ui <- fluidPage(theme=shinythemes::shinytheme('darkly'),
                
    pageWithSidebar(
    # App title ----
    NULL,
    # Sidebar panel for inputs ----
    column(4, 
           imageOutput('logo', height = "50%", width = "50%"), 
           sidebarPanel(
               select_UI('select'),
               width = 12
           )
        ),
    # Main panel for displaying outputs ----
    mainPanel(
        
        # Output: Formatted text for caption ----
        h3(textOutput("caption")),
        
        tabsetPanel(id = 'tabset',
            # time series panel
            tabPanel("time plot",
                timeplot_UI("timeplot")  
            ),
            # seasonal panel
            tabPanel("seasonal plot",
                seasonplot_UI("seasonplot")
            ),
            # versus panel
            tabPanel("versus plot",
                     versusplot_UI('versusplot')
                     )
        )
    ))
)


#' Transforms a tsibble to long format tibble.
#' 
#' @return Should output a tsibble of the format index, key, variable, value.
#'  If there is only one key, then it outputs index, variable, value. 
#'  If there is only one variable then it outputs index, key, variable.
#'  If there is only one key and one variable, then it outputs index, variable.
#' 
tsibble_to_long <- function(tsdata) {
    r <- as_tibble(tsdata) %>%
        # put
        tidyr::gather(
            "variable", "value",
            -!!index(tsdata), -!!key_vars(tsdata)
            )
    
    # add a key column if missing
    if(is_empty(key_vars(tsdata))){
        r$key = 0
    }
    r
}

#' Time plot.
#'
#' Facet by variables, all keys on same graph
#' 
timeseries_plot <- function(tsdata) {
    print(tsdata)
    # get index and keys information
    data <- as_tibble(tsdata)
    keys <- key_vars(tsdata)
    
    assertthat::assert_that(length(keys) <= 1L) # only one key colum

    # convert key column to character
    if(length(keys) == 1L)
        data <- mutate(data, !!keys := as.character(!!sym(keys)))
    
    measures_plot <- map(
        measured_vars(tsdata),
        function(var) {
            plotly::plot_ly(
                x = ~data[[tsibble::index(tsdata)]], y = ~data[[var]], 
                color = if(length(keys) == 1) ~data[[keys]] else "#9E0142",
                # only show legend if there is more than one key and if it is
                # the first plot (otherwise legend will be duplicated)
                showlegend = n_keys(tsdata) > 1 &&
                    var == measured_vars(tsdata)[1],
                type = 'scattergl', mode = 'line', colors = 'Spectral',
                # 4000 is good for 32 measures
                # for one plot should be around 500
                height = 250 + 125*length(measured_vars(tsdata))
                ) %>%
                plotly::layout(
                    yaxis = list(
                        title = var, color = '#ffffff', zeroline = FALSE
                    )
                    #margin = list(l = 50, r = 12, t = 0, b = 0)
                )
        })
    
    plotly::subplot(measures_plot, nrows = length(measures_plot), shareX = TRUE,
            titleY = TRUE, margin = 0.005) %>% 
        plotly::layout(
            plot_bgcolor='#222222',
            paper_bgcolor= '#222222',
            yaxis = list(zeroline = FALSE, showline = FALSE),
            xaxis = list(color = '#ffffff', title = '', zeroline = FALSE),
            legend = list(orientation = 'h',
                          font = list(color = '#ffffff'))
            #margin = list(l = 50, r = 12, t = 0, b = 0)
        )
    
}

#' Plots all variables against one xvar.
#' 
#' @param xvar Variable to put on x axis.
versus_plot <- function(tsdata, xvar) {
    assertthat::is.string(xvar)
    assertthat::assert_that(xvar %in% key_data(tsdata)$key)
    
    as_tibble(tsdata) %>%
        spread(key, value) %>%
        select(-index) %>%
        gather(key = variable, value = value, -!!xvar) %>%
        ggplot(aes_string(x = xvar, y = 'value',
                          group = 'variable', color = 'variable')) + 
        geom_point()
}

#' Converts ts timeries to tsibble format.
#' 
#' @param pivot_longer Boolean. If TRUE assumes that each column is a different
#' unit of observation. If FALSE assumes that each column is a different
#' variable.
#' 
#' @return tsibble object
#' 
convert_ts <- function(ts_data, pivot_longer = TRUE) {
    is_subdaily <- (frequency(ts_data) %% 365) == 0
    # if data is subdaily and a multiple of minute interval
    if(is_subdaily && (60*24) %% (frequency(ts_data)/365) == 0) {
        period_in_minutes = (60*24) / (frequency(ts_data)/365)
        as_tsibble(ts_data, pivot_longer = pivot_longer) %>%
            group_by_key() %>%
            index_by(index2 = 
                         round_date(
                             index,
                             paste(period_in_minutes, 'min')
                             )) %>%
            dplyr::summarise_all(list(mean)) %>%
            rename(index = index2) %>%
            return ()
    } 
    else {
        return (as_tsibble(ts_data))
    }
        
}

#' Visualises a time series.
#' 
#' @param pivot_longer Logical. If TRUE each column is an observation unit or
#' key, if FALSE each column is taken to be a variable. Does not apply if data
#' is already a tsibble.
look_at_ts <- function(data, pivot_longer = TRUE) {
    # Here we convert the data to tsibble
    print(
        paste('Converting data to tsibble. Original format: ',
              paste(class(data), collapse = ', ')
              )
        )
    tsdata = convert_ts(data, pivot_longer = pivot_longer)
    print(tsdata)
    
    # for now we only accept time series that have a single key column
    shiny::need(
        length(key_vars(tsdata)) < 2,
        "for now we only accept time series that have a single key column")
    
    app <- shinyApp(
        ui, 
        server = function(input, output, session) {
            
            # this makes sure the window is closed after closing app in
            # web browser
            session$onSessionEnded(function() {
                stopApp()
            })
            
            output$logo <- renderImage({
                list(src = system.file("images", "logo.svg", package = "TSwatch"),
                     height = 150,
                     #contentType = "image/svg",
                     alt = "TSwatch")
            }, deleteFile = FALSE)
        
            selected_ts <- callModule(select_module, 'select', tsdata)
            callModule(timeplot_module, 'timeplot', selected_ts)
            callModule(seasonplot_module, 'seasonplot', selected_ts)
            callModule(versusplot_module, 'versusplot', tsdata)
        }
            )
    runApp(app, launch.browser = TRUE)
}
