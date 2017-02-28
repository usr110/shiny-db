db_cred <- read.csv("db-info.csv", header = T, stringsAsFactors = F)

library(dplyr)
my_db <- src_mysql(
  dbname = db_cred$schema,
  host = db_cred$host,
  user = db_cred$username,
  password = db_cred$pwd
)

# get the first 5 rows:
my_db %>% tbl("lsoa") %>% head(5)


library(DBI)

conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = db_cred$schema,
  host = db_cred$host,
  username = db_cred$username,
  password = db_cred$pwd
)
  
rs <- dbSendQuery(conn, "
                  SELECT `lsoa`.`code`, `lsoa`.`name`, `lsoa`.`geometry`, AsText(`lsoa`.`bbox`) as bbox
                  FROM `lsoa`
                  WHERE MBRIntersects(`lsoa`.`bbox`, ST_GeomFromText('Polygon((
                          -1.57516479492187 53.3677624135012 ,  
                          -1.57516479492187 53.4087132383714 ,  
                          -1.36058807373047 53.4087132383714 ,  
                          -1.36058807373047 53.3677624135012 ,  
                          -1.57516479492187 53.3677624135012 
                ))'));
                                  
")

dbdata <- dbFetch(rs)

dbDisconnect(conn)
