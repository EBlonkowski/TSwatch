# TSWatch: Time series visualization for the data scientist


## Use case


*   __Time series data exploration__: explore patterns in time series data

    There are different challenges coming with time series EDA:
    
    1.  **Long time series** -> when plotting 10k+ points in a typical plot it becomes quickly over crowded and difficult to make out interesting patterns.
    
        **Solution** a zoomable time series plot.
        With a few clicks of the mouse you can restrict the plot to a certain time interval.
    
    1.  **Many time series** -> It is very common to have multiple time series interacting.
        If dealing with sensor data, there would be one stream per sensor.
        If dealing with sales data, we could have one stream per store or even product.
        
        **Solution** You can select the time series interactively in the application.
        The selection has 2 axis: keys and measured variables.


*   __Forecast comparison__: we are forecasting one or multiple time series.
    We want to compare the forecast to the real and compare the different forecast.
    There are many forecast: 1 step ahead, 2 steps ahead, different algorithms.
    

    
## Install locally

First you'll need to have devtools installed:

```R
install.packages('devtools')
library('devtools')
devtools::install(".")
```

## Example of use

Let's see an example with the a10 univariate dataset from fpp2:

```R
library(fpp2) # for a10 dataset
library(TSwatch)
TSwatch::look_at_ts(a10)
```

Pedestrian dataset:

```R
library(TSwatch)
library(tsibble)
TSwatch::look_at_ts(pedestrian)
```

##  Descrition

### Time series plot

We have one panel per selected **measured variables**.

In each panel we have one line plot per selected **key**.


### Versus plot

It is a group of scatter plots.

What is sure is that a single measure variable is on the horizontal axis.

For each variable we have one panel.
Each panel is a scatter plot with `xvar` horizontal and the variable vertical.
There is one color per key.


##  Assumptions about the data

Work with:

*   tsibble time series data
*   Other formats that can be converted to tsibble: ts, mts...
*   Support for multivariate time series (multiple value columns)
*   Support for multiple time series in the same dataset (through a key column)
*   Support for regular or irregular time series
*   Warning: only one key column is supported 
    Time series in tsibble format can in general be indexed by multiple key values.
    Currently we only support single key indexing.
    If you want to visualize a multikey dataset you'll have to filter it so that only one key column is left.
    
    
    
## Development

Running the tests:

```R
devtools::test(".")
```

Tested with:

*   __a10__ univariate, regular, single time series "ts" object from fpp2 package.
*   __pedestrian__ multivariate, multi time series, "tsibble" object from tsibble package.
    Regular hourly though not all time series have the same time span.
*   __hh_budget__ multivariate, multi time series, "tsibble" object from tsibbledata package.
    Regular yearly data.

### Architecture

Different variables are assumed to belong to different scales and go to different panels.

Different keys same variable goes to the same panel.

1 panel per variable, 1 trace per key.

So for the forecasting exploration we would need to put all the forecasts as different keys.
This would force duplicating the external variables, which is annoying but not so bad.




### Vocabulary

*   **measured variable**
*   **key**
