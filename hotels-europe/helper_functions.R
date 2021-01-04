get_save_hotels_data <- function(hotel_prices_source, 
                                 hotel_features_source, 
                                 join_on = 'hotel_id', 
                                 folder_to_save = 'data/',
                                 filename_csv = 'hotels.csv'){
  ## Purpose: function to save external data to the internal folder structure
  
  hotel_prices <- read.csv(hotel_prices_source)
  hotel_features <- read.csv(hotel_features_source)
  
  hotel <- merge(hotel_prices, hotel_features, by = join_on)
  
  filepath <- paste0(folder_to_save, filename_csv)
  write.table(hotel, filepath, sep = ',', dec = '.', row.names = F)
  
  return(filepath)
}

filepath <- get_save_hotels_data(hotel_prices_source = url("https://osf.io/p6tyr/download"),
                     hotel_features_source = url("https://osf.io/utwjs/download"))

