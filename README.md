
![](https://www.coronanet-project.org/images/logo_transparent_slim.png)

# CoronaNetR Package

CoronaNet is a database on government responses to the COVID-19. To
date, this database provides the most comprehensive and granular
documentation of such government policies in the world, capturing data
for 18 broad policy categories alongside many other dimensions,
including the initiator, target, and timing of a policy. This R package
offers efficient and user-friendly access to the CoronaNet data via an
HTTP API back-end.

If you prefer to access the end point yourself, the API access point is
postgrest-1572524110.us-east-2.elb.amazonaws.com/public_release_allvars
and can be queried using read-only HTTP requests with the [postgrest API
syntax](https://postgrest.org/en/stable/api.html#logical-operators).

## Latest Updates

*Version 0.2.0* includes the first public beta API access using the
`get_event()` function, offering users easy access to the CoronaNet
Event Dataset as R data frames.

## Installation

You can install the development version of CoronaNetR as follows:

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

There are a lot of records that do not yet have end dates, so these are
included by default. To exclude them, set the `include_no_end_date`
argument to `FALSE`.

By default a set of columns is included that can identify each record,
but only includes the policy target, type and description. To change
this default, you can pass a character vector to the `default_columns`
argument. However, it is better to leave that argument unchanged and add
any additional desired columns as a character vector to the
`additional_columns` argument. A full list of columns is available in
[our
codebook](https://www.coronanet-project.org/assets/CoronaNet_Codebook_Panel.pdf).

# Data Filtering Options

## Policy Type (`type`) and Subtype Options (`type_sub_cat`)

The CoronaNet dataset has 20 main policy types and 258 policy sub-types
(not all sub types are available for all main types). By default, the
`get_events()` function will select all policy types and sub-types, but
more specific criteria can be set by passing a character vector of
relevant types/sub-types to the `type` and `type_sub_cat` arguments. To
see a list of available policy types, please see [our
codebook](https://www.coronanet-project.org/assets/CoronaNet_Codebook_Panel.pdf).

## Date

You can select a subset of the records using the `from` and `to`
arguments (both must be specified) in YYYY-MM-DD format as a character
value. These arguments correspond to the coded policy begin and end
dates for a given policy record.

## Country

You can select a specific country to access records using the `country`
argument. This will also pull sub-national policies for that country. A
sub-national record will have a value in either the `province` or `city`
column of the record.
