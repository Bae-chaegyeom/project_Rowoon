install.packages("DBI")
install.packages("pool")

print("여기까지는 되나")
library(DBI)
library(pool)

print("여기는?")

pool <- dbPool(
    drv = RMySQL::MySQL(),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PW"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = 3306
)
print("여기서  안되는건가?")
dbSendQuery(con, 'set character set "utf8"')

# db<-dbSendQuery(con,SQL query)

test_query <- dbGetQuery(
    con,
    "SELECT as2.title FROM application a
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON as2.id = ass.applicationStepId
WHERE a.productName = 'AI 부트캠프 7기'
GROUP BY as2.title "
)

print(test_query)
data <- fetch(db, n = -1)

dbDisconnect(con)