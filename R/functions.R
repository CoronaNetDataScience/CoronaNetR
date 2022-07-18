#' Access CoronaNet Event Data API
#'
#' Use this function to obtain the latest policy event data from CoronaNet via an http API.
#'
#' This function offers programmatic access to the CoronaNet public release dataset, comprising
#' over 80,000 distinct policy records and 93 fields. The dataset is updated regularly as policy
#' coding continues. The entire dataset can be downloaded through this function, although by default
#' it selects a subset of columns (see argument details below). To access additional columns, use the
#' `additional_columns` argument and include a character vector of column names. For a full list of
#' possible columns, see the [CoronaNet codebook](https://www.coronanet-project.org/assets/CoronaNet_Codebook_Panel.pdf).
#'
#' For more information about the data creation, see [our paper](https://www.nature.com/articles/s41562-020-0909-7).
#'
#' Citation:
#'
#' Cheng, Cindy; Barcelo, Joan; Spencer Hartnett, Allison; Kubinec, Robert and Messerschmidt, Luca. "COVID-19 Government Response Event Dataset (CoronaNet v1.0)." **Nature Human Behavior** 4, pp. 756-768 (2020).
#'
#' See code examples for demonstration of filtering syntax.
#'
#' @param countries A character vector of country name(s), e.g., c("Yemen", "Saudi Arabia"). "All" is used as the default.
#' @param type A character vector of policy types, e.g., c("Lockdown", "Curfew"). "All" is used as the default. See https://www.coronanet-project.org/taxonomy.html? for a list of policy types.
#' @param type_sub_cat A character vector of policy types, e.g., c("Self-testing", "Drive-in testing centers"). "All" is used as the default. See https://www.coronanet-project.org/taxonomy.html? for a list of policy subtypes and their related policy types.
#' @param default_columns A character vector specifying the minimum set of columns of data to retrieve. Defaults to record/policy ID, dates,
#'                policy targets, policy type and sub-type, and description
#' @param additional_columns By default NULL. Select additional columns to include with the query.
#' @param from A character vector for the earliest start date, e.g., "2019-12-31".
#' @param to A character vector for the last end date, e.g., "2019-06-01".
#' @param include_no_end_date TRUE/FALSE - whether to include policy records that do not yet have an end date.
#'        By default set to TRUE (this is a lot of records).
#'
#' @return A dataframe with one record per COVID-19 policy
#' @export
#' @import httr
#' @importFrom utils URLencode
#' @examples
#' # Grab all data for Saudi Arabia from first 4 months of pandemic
#'
#' saudi_data <- get_event(countries = "Saudi Arabia", type = "All",
#' type_sub_cat = "All", from = "2019-12-31", to = "2020-04-30")
#'
#' # Each row is one policy record
#' saudi_data
#'
#' # Use the additional_columns argument to add additional columns
#' # beyond the default set
#' # In this case, we'll add the link column to get the URLs
#' # for underlying public sources
#' saudi_data_links <- get_event(countries = "Saudi Arabia", type = "All",
#'                               type_sub_cat = "All",
#'                               from = "2019-12-31", to = "2020-04-30",
#'                               additional_columns = "link")
#'
#' head(saudi_data_links$link)
#'
#' # look at a specific policy type
#'
#' saudi_data_subcat <- get_event(countries = "Saudi Arabia",
#'                                type = "Lockdown",
#'                                type_sub_cat = "All",
#'                                from = "2019-12-31", to = "2020-04-30")
#'
#' head(saudi_data_subcat$description)
get_event <- function(countries = "All",
                      type = "All",
                      type_sub_cat = "All",
                      default_columns = c("record_id","policy_id",
                                          "entry_type","update_type",
                                          "update_level","update_level_var",
                                          "date_announced",
                                          "date_start","date_end","date_end_spec",
                                          "country","init_country_level","province",
                                          "target_init_same",'target_country',
                                          "target_province","target_city","target_intl_org",
                                          "target_other","target_who_what","target_who_gen",
                                          "target_direction","compliance","enforcer",
                                          "city","type","type_sub_cat","description"),
                      additional_columns = NULL,
                      from = "2019-12-31",
                      to = "2022-01-01",
                      include_no_end_date=TRUE) {

  # Errors/Warnings ----

  if(length(type) == 1 &
     any(type %in%
         c("Internal Border Restrictions", "Lockdown", "Anti-Disinformation Measures", "Other Policy Not Listed Above", "Declaration of Emergency")) &
     !any(type_sub_cat %in% "All")) {

    stop("ERROR: This policy type has no subtypes; `type_sub_cat` should be specified as `All` with this policy type")

  }

  if(include_no_end_date) {
    # need to add date_start lte condition to date_end NULL condition
    date_filter <- paste0("or=(and(date_start.gte.",
                          from,
                          ",date_end.lte.",
                          to,
                          "),",
                          "and(date_start.lte.",to,",date_end.is.null))")


  } else {

    date_filter <- paste0("date_start=gte.",
                          from,
                          "&date_end=lte.",
                          to)
  }

  if(type[1]=="All") {

    type <- NULL

  } else {

    if(length(type)>1) {

      type <- paste0('type=.in(',paste0(type,collapse=','),')')

    } else {

      type <- paste0('type=eq.',type)

    }

  }

  if(type_sub_cat[1]=="All") {

    type_sub_cat <- NULL

  } else {

    if(length(type_sub_cat)>1) {

      type <- paste0('type_sub_cat=in.(',paste0(type_sub_cat,collapse=','),')')

    } else {

      type <- paste0('type_sub_cat=eq.',type_sub_cat)

    }


  }

  if(countries[1]=="All") {

    countries <- NULL

  } else {

    if(length(countries)>1) {

      countries <- paste0('country=in.(',paste0(countries,collapse=','),')')

    } else {

      countries <- paste0('country=eq.',countries)

    }

  }


  columns <- paste0("select=",paste0(c(default_columns,additional_columns),collapse=","))

  prod_query <- paste0(c(columns,date_filter,type,type_sub_cat,countries), collapse="&")

  cor_query <- GET(URLencode(paste0("postgrest-1572524110.us-east-2.elb.amazonaws.com/public_release_allvars?",
                                    prod_query)),
                   add_headers(Accept="text/csv"))

  coronanet <- content(cor_query,type="text/csv",encoding="UTF-8")

  # Return all-country coronanet data
  return(coronanet)

  # All Countries ----

  # If all countries...
  # if(any(countries %in% "All")) {
  #
  #   # ... and all policy types
  #   if(any(type %in% "All")) {
  #
  #     if(any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #
  #       if(include_no_end_date) {
  #
  #         allcountry_call <- paste0("?or=(and(date_start.gte.",
  #                                   from,
  #                                   ",date_end.lte.",
  #                                   to,
  #                                   "),date_end.is.null)"
  #                                   )
  #
  #       } else {
  #
  #
  #         allcountry_call <- ""
  #
  #       }
  #
  #
  #       # Fetch all-country data
  #
  #
  #     }
  #
  #     if(!any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       allcountry_call <- paste0("SELECT * FROM public_release WHERE date_start >= '",
  #                                 # Insert minimum 'from' date
  #                                 from,
  #                                 # Insert maximum 'to' date
  #                                 "' AND (date_end <= '",
  #                                 to,
  #                                 "' OR date_end IS NULL)",
  #                                 # Insert policy subtype
  #                                 "' AND type_sub_cat IN ",
  #                                 paste0("(", paste0(sprintf("'%s'", type_sub_cat), collapse = ", "), ")")
  #       )
  #
  #       # Fetch all-country data
  #       coronanet <- DBI::dbGetQuery(public_con, allcountry_call)
  #
  #       # Return all-country coronanet data
  #       return(coronanet)
  #
  #     }
  #
  #   }
  #
  #   # ... but not all policy types
  #   if(!any(type %in% "All")){
  #
  #     if(any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       allcountry_call <- paste0("SELECT * FROM public_release WHERE date_start >= '",
  #                                 # Insert minimum 'from' date
  #                                 from,
  #                                 # Insert maximum 'to' date
  #                                 "' AND (date_end <= '",
  #                                 to,
  #                                 "' OR date_end IS NULL)",
  #                                 # Insert policy type(s)
  #                                 " AND type IN ",
  #                                 paste0("(", paste0(sprintf("'%s'", type), collapse = ", "), ")")
  #       )
  #
  #       # Fetch all-country data
  #       coronanet <- DBI::dbGetQuery(public_con, allcountry_call)
  #
  #       # Return all-country coronanet data
  #       return(coronanet)
  #
  #     }
  #
  #     if(!any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       allcountry_call <- paste0("SELECT * FROM public_release WHERE date_start >= '",
  #                                 # Insert minimum 'from' date
  #                                 from,
  #                                 # Insert maximum 'to' date
  #                                 "' AND (date_end <= '",
  #                                 to,
  #                                 "' OR date_end IS NULL)",
  #                                 # Insert policy type(s)
  #                                 " AND type IN ",
  #                                 paste0("(", paste0(sprintf("'%s'", type), collapse = ", "), ")"),
  #                                 # Insert policy subtype
  #                                 "' AND type_sub_cat IN ",
  #                                 paste0("(", paste0(sprintf("'%s'", type_sub_cat), collapse = ", "), ")")
  #       )
  #
  #       # Fetch all-country data
  #       coronanet <- DBI::dbGetQuery(public_con, allcountry_call)
  #
  #       # Return all-country coronanet data
  #       return(coronanet)
  #
  #     }
  #
  #   }
  #
  # }
  #
  # # Subsetted Countries ----
  #
  # # If not all countries...
  # if(!any(countries %in% "All")) {
  #
  #   # ... but all policy types
  #   if(any(type %in% "All")) {
  #
  #     if(any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       country_call <- paste0("SELECT * FROM public_release WHERE country IN ",
  #                              # Insert country/multiple countries
  #                              paste0("(", paste0(sprintf("'%s'", countries), collapse = ", "), ")"),
  #                              " AND date_start >= '",
  #                              # Insert minimum 'from' date
  #                              from,
  #                              # Insert maximum 'to' date
  #                              "' AND (date_end <= '",
  #                              to,
  #                              "' OR date_end IS NULL)"
  #       )
  #
  #       # Fetch single-country data
  #       coronanet_country <- DBI::dbGetQuery(public_con, country_call)
  #
  #       # Return country data
  #       return(coronanet_country)
  #
  #     }
  #
  #     if(!any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       country_call <- paste0("SELECT * FROM public_release WHERE country IN ",
  #                              # Insert country/multiple countries
  #                              paste0("(", paste0(sprintf("'%s'", countries), collapse = ", "), ")"),
  #                              " AND date_start >= '",
  #                              # Insert minimum 'from' date
  #                              from,
  #                              # Insert maximum 'to' date
  #                              "' AND (date_end <= '",
  #                              to,
  #                              "' OR date_end IS NULL)",
  #                              # Insert policy subtype
  #                              "' AND type_sub_cat IN ",
  #                              paste0("(", paste0(sprintf("'%s'", type_sub_cat), collapse = ", "), ")")
  #       )
  #
  #       # Fetch single-country data
  #       coronanet_country <- DBI::dbGetQuery(public_con, country_call)
  #
  #       # Return country data
  #       return(coronanet_country)
  #
  #   }
  #
  #   # ... and not all policy types
  #   if(!any(type %in% "All")) {
  #
  #     if(any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       country_call <- paste0("SELECT * FROM public_release WHERE country IN ",
  #                              # Insert country or multiple countries
  #                              paste0("(", paste0(sprintf("'%s'", countries), collapse = ", "), ")"),
  #                              " AND date_start >= '",
  #                              # Insert minimum 'from' date
  #                              from,
  #                              # Insert maximum 'to' date
  #                              "' AND (date_end <= '",
  #                              to,
  #                              "' OR date_end IS NULL)",
  #                              # Insert policy type(s)
  #                              " AND type IN ",
  #                              paste0("(", paste0(sprintf("'%s'", type), collapse = ", "), ")")
  #       )
  #
  #       # Fetch single-country data
  #       coronanet_country <- DBI::dbGetQuery(public_con, country_call)
  #
  #       # Return country data
  #       return(coronanet_country)
  #
  #     }
  #
  #
  #     if(!any(type_sub_cat %in% "All")) {
  #
  #       # Create SQL statement with filtered public_release table
  #       country_call <- paste0("SELECT * FROM public_release WHERE country IN ",
  #                              # Insert country or multiple countries
  #                              paste0("(", paste0(sprintf("'%s'", countries), collapse = ", "), ")"),
  #                              " AND date_start >= '",
  #                              # Insert minimum 'from' date
  #                              from,
  #                              # Insert maximum 'to' date
  #                              "' AND (date_end <= '",
  #                              to,
  #                              "' OR date_end IS NULL)",
  #                              # Insert policy type(s)
  #                              " AND type IN ",
  #                              paste0("(", paste0(sprintf("'%s'", type), collapse = ", "), ")"),
  #                              # Insert policy subtype
  #                              "' AND type_sub_cat IN ",
  #                              paste0("(", paste0(sprintf("'%s'", type_sub_cat), collapse = ", "), ")")
  #       )
  #
  #       # Fetch single-country data
  #       coronanet_country <- DBI::dbGetQuery(public_con, country_call)
  #
  #       # Return country data
  #       return(coronanet_country)
  #
  #     }
  #   }
  #   }
  # }
}


#' Download Policy Intensity Scores
#'
#' This function accesses the latest policy intensity scores showing the level of COVID-19 policy-making activity
#' in a given country with measurement error.
#'
#' Use this function to access the latest policy intensity scores for six types: ,
#'
#' 1. Business
#' 2. Health Monitoring
#' 3. Health Resources
#' 4. Masks
#' 5. Schools
#' 6. Social Distancing
#'
#' By default, all six indices for all countries are downloaded, running from the index start at January 1, 2020
#' until April 29, 2021. The indices are periodically updated with new data as the CoronaNet project continues
#' coding policies and integrating external datasets.
#'
#' The default scores are Normally-distributed with a mean of 0. These scores should be used for inference and
#' modeling, such as with regression. Alternatively, you can set the `scaled` argument to TRUE to access
#' scores that have been scaled to be strictly within 0 and 100. These are useful for descriptive visualization.
#'
#' You can read more about the index construction and evaluation
#' at [this preprint](https://osf.io/preprints/socarxiv/rn9xk/).
#'
#' Citation:
#'
#' Kubinec, Robert; Barcelo, Joan; Goldzsmidt, Rafael; Grujic, Vanja; Model, Timothy; Schenk, Caress; Cheng, Cindy; Hale, Thomas; Spencer Hartnett, Allison; Messerschmidt, Luca; Petherick, Anna, and Thorvaldsdottir, Svanhildur. "Cross-National Measures of the Intensity of COVID-19 Public Health Policies." SocArchiv (2022). https://osf.io/preprints/socarxiv/rn9xk/
#'
#' Because the indices were produced with a measurement model, they include measurement error. The most likely
#' estimate is in the `med_estimate` column, and the uncertainty interval high/low are in the
#' `low_estimate`\`high_estimate` columns. The `SD_estimate` column has the standard deviation of the
#' measurement error. These measurement error estimates can be used either in a model that explicitly incorporates
#' the SD of the measurement error (so-called errors-in-variables models) or by using the `low_estimate` and
#' `high_estimate` scores as robustness checks. The default for estimation and modeling should be the most likely
#' `med_estimate` column.
#' @return A data frame with one row per policy intensity score per country
#' @param countries Specify a specific country to query By default all countries.
#' @param type Specify a specific index to query. By default all types.
#' @param from The beginning time period in YYYY-MM-DD format.
#' @param to The end time period in YYYY-MM-DD format. At present the index goes until 04-29-2021.
#' @param scaled Whether to use scores that are scaled between 0 and 100. By default FALSE. Only use
#'        scaled scores for description and visualization, not inference/modeling.
#' @export
#' @examples
#' # Download policy intensity scores of all types for all countries
#'
#' policy_scores <- get_policy_scores()
#'
#' # Download policy intensity scores for Japan and China
#'
#' japan_scores <- get_policy_scores(countries=c("Japan","China"))
#'
#' # Get scaled scores (0 to 100)
#'
#' japan_scores_scaled <- get_policy_scores(countries=c("Japan","China"),
#'                         scaled=TRUE)
get_policy_scores <- function(countries = "All",
                              type = "All",
                              from = "2019-12-31",
                              to = "2021-07-01",
                              scaled=FALSE) {


  if(scaled) {

    table <- "policy_intensity_scaled"

  } else {

    table <- 'policy_intensity'

  }


  if(type[1]=="All") {

    type <- NULL

  } else {

    if(length(type)>1) {

      type <- paste0('modtype=in.(',paste0(type,collapse=','),')')

    } else {

      type <- paste0('modtype=eq.',type)

    }

  }


  date_filter <- paste0("date_policy=gte.",
                        from,
                        "&date_policy=lte.",
                        to)

  if(countries[1]=="All") {

    countries <- NULL

  } else {

    if(length(countries)>1) {

      countries <- paste0('country=in.(',paste0(countries,collapse=','),')')

    } else {

      countries <- paste0('country=eq.',countries)

    }

  }

  prod_query <- paste0(c(date_filter,type,countries), collapse="&")

  score_query <- GET(URLencode(paste0("postgrest-1572524110.us-east-2.elb.amazonaws.com/", table,"?",
                                    prod_query)),
                   add_headers(Accept="text/csv"))

  scores <- content(score_query,type="text/csv",encoding="UTF-8")

  # Return all-country coronanet data
  return(scores)


}
