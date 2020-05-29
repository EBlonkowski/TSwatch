
#' @import shiny
#' @import dplyr
#' @import tibble
#' @import tsibble
#' @importFrom purrr map is_empty
#' @importFrom assertthat assert_that
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
        cat(file=stderr(), '[timeplot/plot] updating time series plot', '\n')
        cat(file=stderr(), '[timeplot/plot] dim(tsdata)', dim(tsdata), '\n')
        return (timeseries_plot(tsdata))
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
            choices = fabletools::common_periods(tsdata))
        
    })
    
    output$plot <- renderPlot({
        #print("renderPlot: hi")
        tsdata <- data()
        #library(fpp2)
        #assertthat::assert_that(all(tsdata$value == convert_ts(a10)$value))
        #print(tsdata)
        #print("renderPlot: bye")
        feasts::gg_season(tsdata, labels = 'left') +
            ggdark::dark_theme_gray()
        #feasts::gg_season(tsdata, labels = 'left', y = measured_vars(tsdata)[1]) #+
            #theme_dark()
    })
    
    
    # output$subseries_plot <- renderPlot({
    #     tsdata <- data()
    #     feasts::gg_subseries(tsdata, y = measured_vars(tsdata)[1]) #+
    #     #theme_dark()
    # })
}

################################################################################
## Versus plot module

versusplot_UI <- function(id) {
    ns <- NS(id)
    
    
    uiOutput(ns('select'))
}

versusplot_module <- function(input, output, session, data) {
    
    output$select <- renderUI({
        ns <- session$ns
        tsdata <- data()
        # if the time series is univariate
        if(purrr::is_empty(measured_vars(tsdata))) {
            return (tagList('Need a multivariate time series for plotting'))
        }
        cols <- measured_vars(tsdata)
        tagList(
            selectInput(ns("variable"), "X axis variable:", 
                        setNames(as.list(cols), cols)),
            plotOutput(ns("plot"))
        )
    })
    
    output$plot <- renderPlot({
        print(input$variable)
        tsdata <- data()
        versus_plot(tsdata, input$variable)
    })
    
}

################################################################################
## Variable and key selection module


select_UI <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns('select'))
}

select_module <- function(input, output, session, data) {
    cat(file=stderr(), "[select_module] initializing selection module", "\n")
    
    # input check
    assert_that(tsibble::is_tsibble(data))
    output$select <- renderUI({
        cat(file=stderr(), "[select/select] updating selection UI", "\n")
        
        ns <- session$ns
        # if we have only one key AND one key column
        if(n_keys(data) >= 1 && !is_empty(key_vars(data))) {
            # we save the unique keys
            observation_units <- key_data(data)[[1]]
        }
        variables <- measured_vars(data)

        ui_tags <- tagList(
            # if there is a single time series (single obervation unit / key)
            if(n_keys(data) < 2) {
                # if we have at least one key column
                if(!is_empty(key_vars(data)))
                    paste("Only one time series: ", observation_units)
            } else {
                
                checkboxGroupInput(
                    ns("key"), key_vars(data)[[1]],
                    setNames(as.list(observation_units), observation_units),
                    selected = observation_units[1]
                    )
            },
            
                checkboxGroupInput(ns("variable"), "Variables:", 
                            setNames(as.list(variables), variables),
                            selected = variables)        
            )
        
        return (ui_tags)
        
    })
    
    # Key/variables selection
    # compute the cut TSE
    selected_ts <- reactive({
        cat(file=stderr(), "[select/selected_ts] says hi", "\n")
        
        shiny::validate(
            # app state is corrupted
            need(is_tsibble(data), '[cor_cut_ctl] Need a tsibble')
        )
        
        if (!is.character(input$key) | !is.character(input$variable)) {
            cat(file=stderr(), "[selected_ts] no selection returning original", "\n")
            return (data)
            
        }
        
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
        cat(file=stderr(), "[select/selected_ts] says bye", "\n")
        
        return(selected)
    })
    
    cat(file=stderr(), "[select_module] says bye", "\n")
    
    return(selected_ts)
}




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
#'@export 
timeseries_plot <- function(tsdata) {
    # get index and keys information
    data <- as_tibble(tsdata)
    keys <- key_vars(tsdata)
    assert_that(length(keys) <= 1L) # only one key colum

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


#' Computes information about columns of a tsibble object.
#'
#' @param tsdata A tsibble object.
#'
#' @return tibble data frame containing metadata about each column:
#'      is numeric, is measured, is key and is time index.
tsibble_colinfo <- function(tsdata) {
    
    assert_that(is_tsibble(tsdata))
    
    numeric_cols <- tsdata %>% map_lgl(is.numeric)
    
    tibble(col_name = names(numeric_cols), is_numeric = numeric_cols) %>%
        # flag the measured variables
        mutate(is_measured = col_name %in% measured_vars(tsdata)) %>%
        # flag the key variables
        mutate(is_key = col_name %in% key_vars(tsdata)) %>%
        # flag the time index
        mutate(is_time_index = col_name %in% index_var(tsdata))
    
    
}


#' Plots all variables against one xvar.
#' 
#' 
#' @param tsdata (tsibble) Data to be plotted, tsibble time series object, 0 or one keys at least 2 measured variables.
#' @param xvar (string) Variable to put on x axis.
versus_plot <- function(tsdata, xvar) {
    
    
    assertthat::is.string(xvar)
    assert_that(xvar %in% measured_vars(tsdata), msg="[versus_plot] xvar is not a measured variable")
    assert_that(length(key(tsdata)) <= 1, msg = "[versus_plot] more than 1 keys not supported")
    # symbols for the columns of interest:
    sym_xvar = sym(xvar)
    # if no key use NULL otherwise the key
    if(!length(key(tsdata))) {
        the_key = NULL
    } else {
        the_key = key(tsdata)[[1]]
    }
    # we only take the key and the numeric measured variables
    col_selection <- tsibble_colinfo(tsdata) %>%
        filter(is_numeric | is_key, !is_time_index) %>%
        pull(col_name)
    
    yvars = setdiff(col_selection, c(as.character(xvar), the_key))
    assert_that(length(yvars)>0, msg="[versus_plot] Need at least 2 measured variables")
    
    as_tibble(tsdata) %>%
        select(all_of(col_selection)) %>% # remove the time variable
        pivot_longer(yvars, names_to = "yvar__", values_to = "value__") %>%
        ggplot(aes(x=!!sym_xvar, y=value__, color=!!the_key)) +
        geom_point() +
        ylab('') +
        facet_wrap(~yvar__, scales="free_y", ncol=1) + 
        ggdark::dark_theme_gray() +
        theme(panel.background = element_rect(fill = '#222222'),
              plot.background = element_rect(fill = '#222222', colour='#222222'))
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
        tsibble::as_tsibble(ts_data, pivot_longer = pivot_longer) %>%
            tsibble::group_by_key() %>%
            tsibble::index_by(index2 = 
                lubridate::round_date(
                             index,
                             paste(period_in_minutes, 'min')
                             )) %>%
            dplyr::summarise_all(list(mean)) %>%
            rename(index = index2) %>%
            return ()
    } 
    else {
        return (tsibble::as_tsibble(ts_data))
    }
        
}


# Define UI
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
                                    # tabPanel("seasonal plot",
                                    #          seasonplot_UI("seasonplot")
                                    # ),
                                    # versus panel
                                    tabPanel("versus plot",
                                             versusplot_UI('versusplot')
                                    )
                        )
                    ))
)

#' Visualises a time series.
#' 
#' @param pivot_longer Logical. If TRUE each column is an observation unit or
#' key, if FALSE each column is taken to be a variable. Does not apply if data
#' is already a tsibble.
#' 
#' @export
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
        length(tsibble::key_vars(tsdata)) < 2,
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
            #callModule(seasonplot_module, 'seasonplot', selected_ts)
            callModule(versusplot_module, 'versusplot', selected_ts)
        }
            )
    runApp(app, launch.browser = TRUE)
}
