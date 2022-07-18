
![](https://www.coronanet-project.org/images/logo_transparent_slim.png)

# CoronaNetR Package

CoronaNetR is a database on government responses to the COVID-19. To
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
devtools::install_github("CoronaNetDataScience/CoronaNetR")
```

# Core Functions

## Event Data: `get_event()`

Access CoronaNet’s Event Dataset. The function allows you to filter by
policy type (`type`), policy subtype (`type_sub_cat`), and date (`from`
and `to`).

``` r
head(get_event(countries = "All", type = "All", type_sub_cat = "All", from = "2019-12-31", to = "2020-01-10"))
```

    ## Rows: 64 Columns: 28

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (23): record_id, policy_id, entry_type, update_type, update_level, upda...
    ## lgl   (2): target_intl_org, target_other
    ## date  (3): date_announced, date_start, date_end

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 6 × 28
    ##   record_id       policy_id entry_type update_type update_level update_level_var
    ##   <chr>           <chr>     <chr>      <chr>       <chr>        <chr>           
    ## 1 R_3n788do2QRzm… 8472080   new_entry  <NA>        <NA>         <NA>            
    ## 2 R_1Leg47dGNydF… 8856106   new_entry  <NA>        <NA>         <NA>            
    ## 3 R_1mn2ZAJLH6yM… 5425631Gd new_entry  <NA>        <NA>         <NA>            
    ## 4 R_2AMF4PcSVHjx… 1884124   new_entry  <NA>        <NA>         <NA>            
    ## 5 R_2CdDjoSFr0ms… 8936839   new_entry  <NA>        <NA>         <NA>            
    ## 6 R_2EHtseyXyuvG… 7214487   new_entry  <NA>        <NA>         <NA>            
    ## # … with 22 more variables: date_announced <date>, date_start <date>,
    ## #   date_end <date>, date_end_spec <chr>, country <chr>,
    ## #   init_country_level <chr>, province <chr>, target_init_same <chr>,
    ## #   target_country <chr>, target_province <chr>, target_city <chr>,
    ## #   target_intl_org <lgl>, target_other <lgl>, target_who_what <chr>,
    ## #   target_who_gen <chr>, target_direction <chr>, compliance <chr>,
    ## #   enforcer <chr>, city <chr>, type <chr>, type_sub_cat <chr>, …

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

## Policy Intensity Indexes: `get_policy_scores`

You can download our six different policy intensity indexes, which are
covered in [this paper](https://osf.io/preprints/socarxiv/rn9xk/), and
aggregate our data into daily country-level policy intensity scores
using the `get_policy_scores` function:

``` r
head(get_policy_scores(from="2020-01-01",
                       to="2020-01-10"))
```

    ## Rows: 10930 Columns: 7

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): country, index
    ## dbl  (4): med_estimate, high_estimate, low_estimate, SD_estimate
    ## date (1): date_policy

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 6 × 7
    ##   country  index date_policy med_estimate high_estimate low_estimate SD_estimate
    ##   <chr>    <chr> <date>             <dbl>         <dbl>        <dbl>       <dbl>
    ## 1 Afghani… Busi… 2020-01-01        -1.80         -1.67        -1.93       0.0800
    ## 2 Afghani… Heal… 2020-01-01        -0.388        -0.359       -0.419      0.0183
    ## 3 Afghani… Heal… 2020-01-01        -1.05         -0.847       -1.28       0.131 
    ## 4 Afghani… Masks 2020-01-01        -0.817        -0.530       -1.11       0.181 
    ## 5 Afghani… Scho… 2020-01-01        -1.50         -1.34        -1.67       0.105 
    ## 6 Afghani… Soci… 2020-01-01        -1.11         -0.905       -1.29       0.125

These scores are estimated with measurement error, both the standard
deviation of the uncertainty of the scores and a high/low uncertainty
interval. This information is useful for checking results for robustness
to coding and other kinds of errors in the scores. These indexes are
periodically updated as we add more records to our CoronaNet database.
The indexes currently run from January 1st, 2020 to April 29th, 2021 for
over 180 countries.

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
