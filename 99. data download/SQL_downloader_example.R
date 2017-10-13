# Import from SQL database

# Comment: as this table is quite big, I prefer to download only new entries and merge them with an old database

data_download_date <- format(Sys.Date(), "%Y%m%d")
library("RMySQL")

# Find old database

list_of_files <- list.files(path = paste0(project_directory,"dbsnaps"), pattern = "^file_name", full.names = T)
details <- file.info(list_of_files)
details = details[with(details, order(as.POSIXct(mtime))), ]
last_file <- rownames(details)[[length(rownames(details))]]

emails <- readRDS(paste0(last_file))
start <- max(emails$sent_date)

# Download new database
drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="username", pass="PassWord", dbname = "dbName", host = "123.456.789.123", port = 1234)
query <- paste0("SELECT id, email FROM dbName.file_name where sent_date >= ", start)
rs <- dbSendQuery(con, query)

emails_new <- fetch(rs,n=-1)
dbHasCompleted(rs)
on.exit(dbDisconnect(con))


# Merge databases
library("data.table")
setnames(emails_new, "email", "emailTo")
emails_upd <- rbind(emails,emails_new)
emails_upd<- emails_upd[!duplicated(id),]

saveRDS(emails_upd, paste0(project_directory, "dbsnaps/file_name", data_download_date,".rds"))

rm(list=setdiff(ls(all=TRUE), global_parameters))
