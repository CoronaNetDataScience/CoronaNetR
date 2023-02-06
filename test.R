# Get all policy information for Saudi Arabia for the first four months of the pandemic
saudi_data <- get_event(countries = "Saudi Arabia", type = "All",
type_sub_cat = "All", from = "2019-12-31", to = "2020-04-30")

 # Each row is one policy record
 saudi_data

# Use the additional_columns argument to add additional columns beyond the default set
# In this case, we'll add the link column to get the URLs for underlying public sources
 saudi_data_links <- get_event(countries = "Saudi Arabia", type = "All",
 type_sub_cat = "All", from = "2019-12-31", to = "2020-04-30",additional_columns = "link")

 head(saudi_data_links$link)

 # look at a sub category

 saudi_data_subcat <- get_event(countries = "Saudi Arabia", type = "Lockdown",
                               type_sub_cat = "All", from = "2019-12-31", to = "2020-04-30")

 head(saudi_data_subcat$description)
