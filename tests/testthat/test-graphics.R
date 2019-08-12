
test_that("Conversion to long tsibble, 1 variable many keys", {
    converted <- convert_ts(visnights)
    longts <- tsibble_to_long(converted)
    expect_is(longts, 'tbl')
    expect_true(
        setequal(colnames(longts), c('index', 'key', 'variable', 'value'))
        )
})

test_that("Conversion to long tsibble, many variable one key", {
    converted <- convert_ts(elecdemand, FALSE)
    longts <- tsibble_to_long(converted)
    expect_is(longts, 'tbl')
    expect_true(
        setequal(colnames(longts), c('index', 'key', 'variable', 'value'))
    )
})

test_that("Conversion to long tsibble, one variable one key", {
    converted <- convert_ts(a10)
    longts <- tsibble_to_long(converted)
    expect_is(longts, 'tbl')
    expect_true(
        setequal(colnames(longts), c('index', 'key', 'variable', 'value'))
    )
})

test_that("timeseries_plot works with univariate a10", {
    # should produce a single plot with a single trace
    p <- timeseries_plot(convert_ts(a10)) %>%
        plotly_build()
    expect_length(p$x$data, 1) # one trace
    # should produce a single plot with many traces of different colors
    # the legend should indicate which key is which
    p <- timeseries_plot(convert_ts(visnights)) %>%
        plotly_build()
    # one trace for each key
    expect_length(p$x$data, n_keys(convert_ts(visnights))) 
    
    # should produce 3 plots with shared X axis, no legends
    p <- timeseries_plot(convert_ts(elecdemand, FALSE)) %>%
        plotly_build()
    expect_length(p$x$data, 3) # one trace for each value
    
    #1:10[11]
    # there should be no error
    expect_true(TRUE)
    
})