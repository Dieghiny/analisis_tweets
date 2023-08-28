tz_cdmx <- function(rtweet_df){
  rtweet_df <- rtweet_df %>% 
    mutate(created_at = with_tz(created_at,
                                tzone = "America/Mexico_City")) %>%
    mutate(account_created_at = with_tz(account_created_at, 
                                        tzone = "America/Mexico_City")) %>%
    dplyr::filter(created_at >= fecha_inicio) %>%
    dplyr::filter(created_at <= fecha_final) %>% 
    arrange(desc(created_at)) %>% 
    clean_names()
  return(rtweet_df)
}