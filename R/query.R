install.packages("RMySQL")
install.packages("httr")
library(RMySQL)
library(httr)

print(Sys.getenv())
slack_url <- Sys.getenv("SLACK_URL")
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
    "SELECT plz.name, plz.stDate, plz.elapsedTime, plz.remainingTime, plz.appStart, (
SELECT count(user.id) FROM application a3
JOIN application_step_submission ass2 ON ass2.applicationId = a3.id
JOIN application_step as4 ON as4.id = ass2.applicationStepId
JOIN user on user.id = a3.userId
WHERE plz.test = as4.id
AND a3.status = 'pending'
AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
GROUP BY as4.title
) as subCom FROM (
SELECT p.name,
p.id,
date(p.applyStartDate) as stDate,
DATEDIFF(now(),date(p.applyStartDate)) as elapsedTime ,
DATEDIFF(now(),date(p.applyEndDate)) as remainingTime,
(
SELECT count(DISTINCT user.id) FROM application a2
JOIN user ON user.id = a2.userId
WHERE a2.productId = p.id
AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
)as appStart,
(
SELECT  MAX(as3.id) FROM application_step as3
WHERE p.id = as3.productId
LIMIT 1
) as test
FROM application a
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON as2.id = ass.applicationStepId
JOIN product p ON p.id = a.productId
JOIN user ON user.id = a.userId
WHERE p.isPublished = 1
AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND a.status = 'pending'
AND a.productName NOT LIKE '%소프트웨어%'
GROUP BY a.productName) AS plz"
)
dbDisconnect(con)

nowTime <- Sys.time()
startSub <- paste0("*지원상황* \n>쿼리 기준시간 : ", nowTime, "\n")
startSub

for (i in 1:nrow(test_query)) {
    pName <- test_query[i, ]$name

    msub <- paste0("\n>*", test_query[i, ]$name, "*", "\n>누적 지원 수 : ", test_query[i, ]$appStart, "명", "\n>누적 지원 완료 수 : ", test_query[i, ]$subCom, "명", "\n>D-Day : ", "D", test_query[i, ]$remainingTime, "일", "\n>최종 전환률 : ", round((test_query[i, ]$subCom / test_query[i, ]$appStart) * 100, 1), "%\n")

    startSub <- paste0(startSub, msub)
}
r <- POST(slack_url, body = list(text = startSub), encode = "json")