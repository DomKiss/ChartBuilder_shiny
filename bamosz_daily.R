# manual inputs
curr_date <- Sys.Date()-15      #yesterday
directory_path <- "C:/Users/hunter5/Downloads" #EZT TUTI UJRA MEG KELL ADNI EGY SZERVEREN



prev_date <- curr_date-14    #2 weeks before
cob_curr_date <- format(curr_date, "%Y.%m.%d")      #format date
cob_prev_date <- format(prev_date, "%Y.%m.%d")  #format date
  print(paste("Current date is: ", curr_date, sep=""))
  print(paste("Previous date is: ", prev_date, sep=""))
  print(paste("File directory is: ", directory_path, sep=""))

remDr$open()
remDr$navigate(url)
  Sys.sleep(10)
bamosz_scraper(cob_prev_date,cob_curr_date) #scraper function from misc
  Sys.sleep(30)
remDr$close()
  print("Data downloaded from bamosz")


# kivalasztja melyik fajl kell
files_df <- file.info(list.files(directory_path, full.names = T)) 
file_name <- rownames(files_df)[which.max(files_df$mtime)]
  print(paste("Sequence started on file ", file_name, sep=""))


# basic data strukuralas
final_dataframes <- bamosz_data_structurer(file_name) #data structurer function from misc
categories_df <- as.data.frame(final_dataframes[1])
timeseries_df <- as.data.frame(final_dataframes[2])
  print("Basic structuring finished for categories and timeseries dfs")

# categories df final touches
categories_df <- categories_finisher(categories_df)
  print("Final touches on categories table finished")
  print("Categories table is ready")

# currency table creator
currency_df <- currency_table_creator(categories_df, curr_date, prev_date)
  print("Currency table is ready")  

# exchange figures to HUF
timeseries_df <- huf_exchanger(timeseries_df, categories_df, currency_df)
  print("HUF exchange in timeseries df finished")  

# date table creator
dates_df <- date_table_creator(timeseries_df, curr_date, prev_date)
  print("Date table is ready")
  
# timeseries df yield calculator and drop where yield is NA
timeseries_df <- yield_calculator(timeseries_df, dates_df)
timeseries_df <- drop_na(timeseries_df, EFF_HOZAM)
  print("Timeseries table is ready")
  

  
  
# UPLOAD TABLES

