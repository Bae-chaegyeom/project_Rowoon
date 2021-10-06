install.packages("RMySQL")
print("여기까지는 되나")
library(RMySQL)
print("여기는?")

con <- dbConnect(MySQL(), user = `${{ secrets.DB_USER}}`, password = `${{ secrets.DB_PW}}`, dbname = `${{ secrets.DB_NAME}}`, host = `${{ secrets.DB_HOST}}`)
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

test_query
data <- fetch(db, n = -1)

dbDisconnect(con)