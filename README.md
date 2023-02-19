# Package *refreshr*

## What is *refreshr*?

*refreshr* allows you to create dataframes/tables that are refreshable.
That means they have information about their (online) data source baked
into them (as attributes) and can be updated from that source using a
simple call of the `refresh()` function. The dataframe can then be
shared with coworkers (e.g. as an RData file) and the recipient does not
need to care about how he can update the data. If he wants the data
updated from the original source *refreshr* will do the job for him.

## How to make a dataframe/table refreshable?

The function `make_refreshable()` converts a conventional
dataframe/table into a refreshable dataframe/table. This is done by
specifying a `load_code` that is essentially the code you would call to
download the data from the original data source.

Sometimes, you want to process the raw data that is downloaded from the
remote data source. This can be achieved using the `prep_code` argument
of the `make_refreshable()`. `prep_code` stores a code that is
automatically applied to the raw data from the the data source after the
data has been refreshed.

Let us take as an example US labor market data provided by the U.S.
Bureau of Labor Statistics (BLS). We want to download this data from
BLS’ public website and filter it for the overall unemployment rate
(data series `LNS14000000`); the overall dataset contains many more data
series beyond the overall unemployment rate.

First, we load the data:

    library(refreshr)
    library(data.table)
    library(dplyr)

    ## 
    ## Attache Paket: 'dplyr'

    ## Die folgenden Objekte sind maskiert von 'package:data.table':
    ## 
    ##     between, first, last

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     filter, lag

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    data <- fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", sep="\t")
    data <- filter(data, series_id=="LNS14000000")

Then we make `data` refreshable:

    data_refresh <- make_refreshable(data,
                        load_code = "fread(\"https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData\", 
                                  sep=\"\t\")",
              prep_code = "filter(#, series_id==\"LNS14000000\")")

The `#` in the data preparation code `prep_code` is not an R comment but
a reference to the refreshable dataframe.

We have now a refreshable dataframe:

    class(data_refresh)

    ## [1] "refreshr"   "data.table" "data.frame"

    is.refreshr(data_refresh)

    ## [1] TRUE

We could now save our dataframe, e.g. with

    save(data_refresh, file = "refresh.RData")

and share it with other people.

If we want to refresh the data we just need to call

    data_refresh <- refresh(data_refresh)

    ## Origina data set had 901 rows, updated dataset has 901.

The function `uptodate()` confirms, that the data in our dataframe is
up-to-date:

    uptodate(data_refresh)

    ## [1] TRUE

If we have a look at the properties of the refreshable dataframe

    properties(data_refresh)

    ## Last refresh: 2023-02-19 13:28:15
    ## Data source: https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData
    ## Structure: 901 rows | 5 columns
    ## Load code: fread("https://download.bls.gov/pub/time.series/ln/ln.data.1.AllData", 
    ##                            sep=" ")
    ## Preparation code: filter(#, series_id=="LNS14000000")

we see that confirmed by the date/timestamp of the last update.
