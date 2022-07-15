#' Get CoronaNet Event Data
#'
#' @param countries A character vector of country name(s), e.g., c("Yemen", "Saudi Arabia"). "All" is used as the default.
#' @param type A character vector of policy types, e.g., c("Lockdown", "Curfew"). "All" is used as the default. See https://www.coronanet-project.org/taxonomy.html? for a list of policy types.
#' @param type_sub_cat A character vector of policy types, e.g., c("Self-testing", "Drive-in testing centers"). "All" is used as the default. See https://www.coronanet-project.org/taxonomy.html? for a list of policy subtypes and their related policy types.
#' @param columns A character vector specifying the minimum set of columns of data to retrieve. Defaults to record/policy ID, dates,
#'                policy targets, policy type and sub-type, and description
#' @param additional_columns By default NULL. Select additional columns to include with the query.
#' @param from A character vector for the earliest start date, e.g., "2019-12-31".
#' @param to A character vector for the last end date, e.g., "2019-06-01".
#' @param include_no_end_date TRUE/FALSE - whether to include policy records that do not yet have an end date.
#'        By default set to false (this is a lot of records).
#'
#' @return A dataframe
#'
#' @examples get_event(countries = "All", type = "All", type_sub_cat = "All", from = "2019-12-31", to = "2027-07-01")

get_event <- function(countries = "All",
                      type = "All",
                      type_sub_cat = "All",
                      default_columns = c("record_id","policy_id",
                                  "entry_type","update_type",
                                  "update_level","update_level_var",
                                  "date_announced",
                                  "date_start","date_end","date_end_spec",
                                  "country","init_country_level","province",
                                  "city","type","type_sub_cat","description"),
                      additional_columns = NULL,
                      from = "2019-12-31",
                      to = "2027-07-01",
                      include_no_end_date=FALSE) {

  # Errors/Warnings ----

  if(length(type) == 1 &
     any(type %in%
         c("Internal Border Restrictions", "Lockdown", "Anti-Disinformation Measures", "Other Policy Not Listed Above", "Declaration of Emergency")) &
     !any(type_sub_cat %in% "All")) {

    stop("ERROR: This policy type has no subtypes; `type_sub_cat` should be specified as `All` with this policy type")

  }

  date_filter <- paste0("date_start=gte.",
                        from,
                        "&date_end=lte.",
                        to)

  columns <- paste0("select=",paste0(default_columns,additional_columns,collapse=","))

  prod_query <- paste0(c(date_filter,columns), collapse="&")

  cor_query <- GET(paste0("postgrest-1572524110.us-east-2.elb.amazonaws.com/public_release_allvars?",
                          prod_query),
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
