library(fpp2)
library(lubridate)
library(tsibble)
library(purrr)

test_that("convert_ts elecdemand dataset", {
    converted <- convert_ts(elecdemand, pivot_longer = FALSE)
    # all times should be multiples of 30 minutes
    converted$index %>%
        minute %>%
        unique %>%
        expect_equal(c(0, 30))
    # the time series should be regular
    expect_true(is_regular(converted))
    # the number of observations should be constant
    expect_equal(
        nrow(converted) * length(measured_vars(converted)),
        reduce(dim(elecdemand), `*`)
    )
    
})



test_that("convert_ts visnights dataset", {
    converted <- convert_ts(visnights)
    # all months should be trimester starts (1, 4, 7 or 10)
    # since the data is quaterly
    converted$index %>%
        month %>%
        unique %>%
        expect_equal(c(1, 4, 7, 10))
    # the time series should be regular
    expect_true(is_regular(converted))
    # the number of observations should be constant
    expect_equal(
        nrow(converted) * length(measured_vars(converted)),
        reduce(dim(visnights), `*`)
    )
    
})

test_that("tsibble_colinfo correct meta on pedestrian dataset", {
    r <- tsibble_colinfo(pedestrian)
    
    expect_true(all(
        c("col_name", "is_numeric", "is_measured", "is_key", "is_time_index") %in% colnames(r)))
    
    # "Sensor" is the key column
    expect_true(column_to_rownames(r, 'col_name')['Sensor', 'is_key'])
    
    expect_equal(r$is_measured %>% sum, 3) # we expect 3 measured variables
})
