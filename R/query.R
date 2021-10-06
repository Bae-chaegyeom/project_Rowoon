install.packages("RMySQL")
library(RMySQL)

print(Sys.getenv())

con <- dbConnect(MySQL(),
    user = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = 3306
)

dbSendQuery(con, 'set character set "utf8"')

test_query <- dbGetQuery(
    con,
    "SELECT as2.title FROM application a
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON as2.id = ass.applicationStepId
WHERE a.productName = 'AI 부트캠프 7기'
GROUP BY as2.title "
)
dbDisconnect(con)

print(test_query)
data <- fetch(test_query, n = -1)


