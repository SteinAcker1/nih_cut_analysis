create_pubmed_query <- function(terms, df, start_year, verbose=TRUE) {
  df <- df |> 
    mutate(pubmed_search_term = paste0("(", Activity, " ", IC, Serial.Number, "[Grants and Funding])")) |> 
    mutate(Project.Terms = tolower(Project.Terms)) |> 
    mutate(Project.Abstract = tolower(Project.Abstract))
  terms_str <- paste(terms, collapse="|")
  matches_bool <- str_detect(df$Project.Terms, terms_str)
  new_df <- df[matches_bool,]
  if(verbose) {
    print(paste0(nrow(new_df), " projects found with terms ", paste(terms, collapse=", ")))
    print(paste0(new_df$Project.Title, ", total funding $", new_df$Total.Cost))
  }
  query_nodate <- paste(new_df$pubmed_search_term, collapse=" OR ")
  query <- paste0(query_nodate, " AND (", start_year, ":2025[pdat])")
  return(query)
}