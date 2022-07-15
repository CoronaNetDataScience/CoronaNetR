
![](https://www.coronanet-project.org/images/logo_transparent_slim.png)

# CoronaNetR Package

CoronaNet is a database on government responses to the COVID-19. To
date, this database provides the most comprehensive and granular
documentation of such government policies in the world, capturing data
for 18 broad policy categories alongside many other dimensions,
including the initiator, target, and timing of a policy. This R package
offers efficient and user-friendly access to the CoronaNet data.

## Latest Updates

*Version 0.1.0* includes the first released function for the CoronaNetR, `get_event()`, offering users easy access to the CoronaNet Event Dataset.

## Installation

You can install the development version of CoronaNetR as
follows:

``` r
devtools::install_github("CoronaNetDataScience/CoronaNet")
```

# Core Functions

## Event Data: `get_event()`

Access CoronaNet’s Event Dataset. The function allows you to filter by
policy type (`type`), policy subtype (`type_sub_cat`), and date (`from`
and `to`).

``` r
get_event(countries = "All", type = "All", type_sub_cat = "All", from = "2019-12-31", to = "2027-07-01")
```

# Data Filtering Options

## Policy Type (`type`) and Subtype Options (`type_sub_cat`)
