#' Get CoronaNet Event Data
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
                      to = "2027-07-01",
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

  if(type=="All") {

    type <- NULL

  } else {

    if(length(type)>1) {

      type <- paste0('type=in(',paste0(type,collapse=','),')')

    } else {

      type <- paste0('type=eq.',type)

    }

  }

  if(type_sub_cat=="All") {

    type_sub_cat <- NULL

  } else {

    if(length(type_sub_cat)>1) {

      type <- paste0('type_sub_cat=in(',paste0(type_sub_cat,collapse=','),')')

    } else {

      type <- paste0('type_sub_cat=eq.',type_sub_cat)

    }


  }

  if(countries=="All") {

    countries <- NULL

  } else {

    if(length(countries)>1) {

      countries <- paste0('country=in(',paste0(countries,collapse=','),')')

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
