library(RMySQL)
library(slackr)
library(stringr)

print(Sys.getenv())

## slackr 셋팅
create_config_file(
    filename = "~/.slackr",
    token = Sys.getenv("SLACK_TOKEN"),
    incoming_webhook_url = Sys.getenv("SLACK_URL"),
    username = "춘식이",
    channel = "배채겸 [ Admission / PM ]"
)
slackr_setup()

### DB연결
con <- dbConnect(MySQL(),
    user = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = 3306
)
dbSendQuery(con, 'set character set "utf8"')

DBTime <- dbGetQuery(
    con, "SELECT now()"
)

### 기수에서 숫자부분만 가져오기 Ex) AI 부트캠프 10기
### AI부트캠프는 동시기수 개강이니까 -2해서 변경 그러니까 숫자로 바꿔야됨
### AI는 동시개강이기 때문에 -2해서 반환 다른 캠프는 -1기수 해서 반환
get_former_generation <- function(data) {
    ### 현재 기수
    present <- data$productName
    print("무슨 기수이길래 안되는데?")
    print(present)
    if (as.numeric(gsub("\\D", "", present)) == 1) {
        previous <- 0
        print("여기서 문제인거 같은데?")
        print(previous)
        return(previous)
    }
    # if (str_detect(present, "AI") == TRUE) {
    #     previous <- str_replace(present, gsub("\\D", "", present), as.character(as.numeric(gsub("\\D", "", present)) - 2))
    # } else {
    previous <- str_replace(present, gsub("\\D", "", present), as.character(as.numeric(gsub("\\D", "", present)) - 1))
    # }
    return(previous)
}

### Github 시간이 UTC 기준이기 때문에 9시간을 더해서 한국시간과 일치시킵니다.
nowTime <- Sys.time() + 32400
currentHour <- as.POSIXlt(nowTime)$hour


### 현재 지원과정 중인 기수를 가져옵니다
query_published_product <- dbGetQuery(
    con,
    "SELECT p.id, p.name, p.applyStartDate, DATEDIFF(now(),date(p.applyStartDate)) as elapsedTime , DATEDIFF(now(),date(p.applyEndDate)) as remainingTime FROM product p
JOIN application a ON a.productId = p.id
WHERE DATEDIFF(DATE_ADD(now(), INTERVAL 9 HOUR) ,DATE_ADD(date(p.applyEndDate), INTERVAL 9 HOUR)) <= 0
AND DATE_ADD(date(p.serviceStartDate), INTERVAL 9 HOUR) > DATE_ADD(now(), INTERVAL 9 HOUR)
AND DATE_ADD(date(p.applyStartDate), INTERVAL 9 HOUR) < DATE_ADD(now(), INTERVAL 9 HOUR)
AND p.name NOT LIKE '%test%'
AND p.name NOT LIKE '%테스트%'
AND p.name NOT LIKE '%copy%'
AND p.name NOT LIKE '%결제%'
AND p.name NOT LIKE '%빗썸%'
AND a.paymentType = 'gov_funding'
GROUP BY p.name "
)

## 공사중 메세지를 위한 변수
AI <- FALSE
PM <- FALSE
GM <- FALSE
BE <- FALSE
# SE <- FALSE
Dev <- FALSE
FB <- FALSE
BB <- FALSE


### 메세지 시작 템플릿
# ch <- Sys.getenv("SLACK_CHANNEL")
# slackStartMsg <- paste0("*지원상황*", "\n>쿼리 기준시간 : ", currentHour, "시", "\n>이탈 : 지원취소를 하지 않았지만, 다음 단계를 진행하지 않은지 7일 이상 된 인원", "\n\n>춘식이를 소개합니다.", "\n>https://www.notion.so/codestates/d7aaa720460a4291824f408004a312bb", "\n\n")
# slackr_msg(
#     txt = slackStartMsg,
#     channel = "#team-admission",
#     username = "춘식이",
#     token = Sys.getenv("SLACK_TOKEN"),
#     thread_ts = NULL,
#     reply_broadcast = FALSE,
# )
pNameCheck <- ""
for (i in 1:nrow(query_published_product)) {
    pdi <- query_published_product[i, ]$id
    # print(pdi)



    # query_published_product[i, ]$elapsedTime
    # query_published_product[i, ]$remainingTime
    ## productId가지고 누적 지원 수 가져오는 쿼리
    q1 <- paste("SELECT a.productName, count(user.id) as startAppCount FROM application a
JOIN user ON user.id = a.userId
WHERE a.productId =", pdi, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND DATE_FORMAT(DATE_ADD(a.createdAt, INTERVAL 9 HOUR), '%Y-%m-%d %H:%i') < DATE_FORMAT(NOW(), '%Y-%m-%d 10:00')")

    stApp <- dbGetQuery(con, q1)
    ## 테스트를 위해 열어둔 상품 확인 q1 쿼리시 크루를 제외하기때문에 application에서 null값이 반환되며 R에서는 NA로 표기됨
    ## 따라서 stApp$productName이 NA인지 확인후 NA라면 해당 반복문은 skip하는 조건
    if (is.na(stApp$productName)) {
        next
    }


    if (str_detect(stApp$productName, "AI") == TRUE) {
        AI <- TRUE
    }
    if (str_detect(stApp$productName, "프로덕트") == TRUE) {
        PM <- TRUE
    }
    if (str_detect(stApp$productName, "그로스") == TRUE) {
        GM <- TRUE
    }
    if (str_detect(stApp$productName, "블록체인") == TRUE) {
        BE <- TRUE
    }
    # if (str_detect(stApp$productName, "소프트웨어") == TRUE) {
    #     SE <- TRUE
    # }
    if (str_detect(stApp$productName, "DevOps") == TRUE) {
        Dev <- TRUE
    }
    if (str_detect(stApp$productName, "프론트엔드") == TRUE) {
        FB <- TRUE
    }
    if (str_detect(stApp$productName, "백엔드") == TRUE) {
        BB <- TRUE
    }

    ## 프로덕트 이름
    # stApp$productName
    ## 지원시작 인원
    # stApp$startAppCount

    ## productId로 마지막 단계 찾는 쿼리
    q2 <- paste("SELECT MAX(as2.order) as lastStep FROM application_step as2
WHERE as2.productId =", pdi)

    ## 변수에 order담기
    lastOrder <- dbGetQuery(con, q2)
    laOrder <- lastOrder$lastStep


    ## productId와 applyingStatus = submitted 조건으로 지원완료인원 쿼리
    q3 <- paste("SELECT COUNT(DISTINCT user.id) as compAppCount FROM application a
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON ass.applicationStepId = as2.id
JOIN user ON user.id = a.userId
WHERE a.productId =", pdi, "AND as2.order =", laOrder, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND a.applyingStatus = 'submitted'
AND DATE_FORMAT(DATE_ADD(ass.createdAt, INTERVAL 9 HOUR), '%Y-%m-%d %H:%i') <  DATE_FORMAT(NOW(), '%Y-%m-%d 10:00')")

    comApp <- dbGetQuery(con, q3)

    ## 지원 완료 인원
    # comApp$compAppCount

    ## 어제까지 지원 인원 수 쿼리
    q4 <- paste("SELECT count(user.id) as yesterDayStartAppCount FROM application a
JOIN user ON user.id = a.userId
WHERE a.productId =", pdi, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND DATE_FORMAT(DATE_ADD(a.createdAt, INTERVAL 9 HOUR), '%Y-%m-%d %H:%i') < DATE_FORMAT(DATE_SUB(now(), INTERVAL 1 DAY), '%Y-%m-%d 10:00')")

    yesStartApp <- dbGetQuery(con, q4)
    ## 어제 지원 시작 인원
    # yesStartApp$yesterDayStartAppCount

    ## 어제까지 지원 완료 수 쿼리
    q5 <- paste("SELECT COUNT(DISTINCT user.id) as yesterDayCompAppCount FROM application a
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON ass.applicationStepId = as2.id
JOIN user ON user.id = a.userId
WHERE a.productId =", pdi, "AND as2.order =", laOrder, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND a.applyingStatus = 'submitted'
AND DATE_FORMAT(DATE_ADD(ass.createdAt, INTERVAL 9 HOUR), '%Y-%m-%d %H:%i') <  DATE_FORMAT(DATE_SUB(NOW(), INTERVAL 1 DAY), '%Y-%m-%d 10:00')")

    yesComApp <- dbGetQuery(con, q5)
    ## 어제 지원 완료 인원
    # yesComApp$yesterDayCompAppCount

    ## 최종 전환률
    conversionRate <- round((comApp$compAppCount / stApp$startAppCount) * 100, 1)


    ## 이전기수 확인 대기자 상품 버림
    ## 이전기수 이름 확인하기
    previousGen <- get_former_generation(stApp)
    print("여기는 잘 되는지 보자")
    print(previousGen)
    ## 이전기수가 0이면 이전기수에 대한 쿼리부분은 넘기도록 조건문 처리
    if (previousGen != 0) {
        q6 <- paste0("SELECT p.id FROM product p WHERE p.name = '", previousGen, "'")
        ### 첫기수가 아닐때만 동작

        previousGenPId <- dbGetQuery(con, q6)



        ## 이전기수 지원 인원 쿼리
        ### DISTINCT 조건을 통해 지원취소 후 재지원 인원 제거 AI 8기에서 확인 총 825명이지만 같은 user.id로 재지원 이력 20건, 크루지원 7건 제외후 798명 맞는 것으로 확인
        q7 <- paste("SELECT a.productName, count(DISTINCT user.id) as startAppCount FROM application a
JOIN user ON user.id = a.userId
WHERE a.productId =", previousGenPId, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND a.applyingStatus <> 'cancelled'")

        preGenApp <- dbGetQuery(con, q7)

        # preGenApp$startAppCount
        # preGenApp$productName

        ## 이전기수 마지막 단계 확인
        q8 <- paste("SELECT MAX(as2.order) as lastStep FROM application_step as2
WHERE as2.productId =", previousGenPId)

        ## 이전기수 지원 완료 인원 확인

        preGenLaOr <- dbGetQuery(con, q8)


        q9 <- paste("SELECT COUNT(DISTINCT user.id) as compAppCount FROM application a
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON ass.applicationStepId = as2.id
JOIN user ON user.id = a.userId
WHERE a.productId =", previousGenPId, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND a.applyingStatus = 'submitted'")

        preGenComApp <- dbGetQuery(con, q9)

        # preGenComApp$compAppCount
    }

    ### 지원 취소 인원
    q10 <- paste("SELECT a.productName, count(DISTINCT user.id) as canceledAppCount FROM application a
JOIN user ON user.id = a.userId
WHERE a.productId = ", pdi, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com'
AND a.applyingStatus = 'cancelled'")
    canceledApp <- dbGetQuery(con, q10)
    # canceledApp$canceledAppCount

    ### 마지막 제출 단계가 사전과제 이전인데 7일 이상 진행이 없는 인원 (이탈)
    ## AIB,BEB의 경우 마지막에서 -1 미만 다른 캠프는 마지막단계 미만에서 쿼리
    if (str_detect(stApp$productName, "AI") == TRUE | str_detect(stApp$productName, "블록체인") == TRUE) {
        bounceOrder <- laOrder - 1
    } else {
        bounceOrder <- laOrder
    }


    q11 <- paste("SELECT COUNT(bounce.id) as bounceNum FROM (
SELECT user.id, user.name, a.productName, afs.title, afs.submittedAt, MAX(as2.order) as lastOr, DATEDIFF(now(), date(afs.submittedAt)) as ttime FROM application_form_submission afs
JOIN application a ON a.id = afs.applicationId
JOIN user ON user.id = a.userId
JOIN application_step_submission ass ON ass.applicationId = a.id
JOIN application_step as2 ON as2.id = ass.applicationStepId
WHERE afs.id IN (
	SELECT MAX(id) FROM application_form_submission afs2 GROUP BY afs2.applicationId
	)
AND a.productId =", pdi, "AND DATEDIFF(now(), date(afs.submittedAt)) > 7
AND as2.order < ", bounceOrder, "AND user.role <> 'admin'
AND user.email NOT LIKE '%@codestates.com' GROUP BY user.id) as bounce")

    bounceNum <- dbGetQuery(con, q11)



    ## 이전기수 전환률
    if (previousGen != 0) {
        preGenConversionRate <- round((preGenComApp$compAppCount / preGenApp$startAppCount) * 100, 1)
    }

    ## 부트캠프별 목표인원 변수처리
    if (str_detect(stApp$productName, "AI") == TRUE) {
        targetNumberOfPeople <- 396
    } else if (str_detect(stApp$productName, "프로덕트") == TRUE) {
        targetNumberOfPeople <- 165
    } else if (str_detect(stApp$productName, "그로스") == TRUE) {
        targetNumberOfPeople <- 264
    } else if (str_detect(stApp$productName, "블록체인") == TRUE) {
        targetNumberOfPeople <- 149
    } else if (str_detect(stApp$productName, "프론트엔드") == TRUE) {
        targetNumberOfPeople <- 462
    } else if (str_detect(stApp$productName, "백엔드") == TRUE) {
        targetNumberOfPeople <- 462
    } else if (str_detect(stApp$productName, "DevOps") == TRUE) {
        targetNumberOfPeople <- 165
    }

    ## 부트캠프별 정원 변수처리
    if (str_detect(stApp$productName, "AI") == TRUE) {
        personnelNum <- 120
    } else if (str_detect(stApp$productName, "프로덕트") == TRUE) {
        personnelNum <- 50
    } else if (str_detect(stApp$productName, "그로스") == TRUE) {
        personnelNum <- 80
    } else if (str_detect(stApp$productName, "블록체인") == TRUE) {
        personnelNum <- 45
    } else if (str_detect(stApp$productName, "프론트엔드") == TRUE) {
        personnelNum <- 140
    } else if (str_detect(stApp$productName, "백엔드") == TRUE) {
        personnelNum <- 140
    } else if (str_detect(stApp$productName, "DevOps") == TRUE) {
        personnelNum <- 50
    }

    slackMsg <- paste0("\n*", stApp$productName, "* ( D+", query_published_product[i, ]$elapsedTime, " / ", "D-", abs(query_published_product[i, ]$remainingTime), " )")
    slackMsg
    slackMsg <- paste0(slackMsg, "\n>지원: ", stApp$startAppCount, "(:small_red_triangle:", stApp$startAppCount - yesStartApp, ")", " / 완료: ", comApp$compAppCoun, "(:small_red_triangle:", comApp$compAppCoun - yesComApp, ")", "\n>취소: ", canceledApp$canceledAppCount, " / 이탈: ", bounceNum$bounceNum, "\n>목표: ", targetNumberOfPeople)
    if (comApp$compAppCount < round(targetNumberOfPeople / 2.5)) {
        slackMsg <- paste0(slackMsg, " :red_circle:", "\n>정원: ", personnelNum)
    } else if (comApp$compAppCount < targetNumberOfPeople) {
        slackMsg <- paste0(slackMsg, " :large_yellow_circle:", "\n>정원: ", personnelNum)
    } else {
        slackMsg <- paste0(slackMsg, " :large_green_circle:", "\n>정원: ", personnelNum)
    }

    if (round((comApp$compAppCount / personnelNum) * 100, 1) < 50) {
        slackMsg <- paste0(slackMsg, " :red_circle:", "\n>최종 전환: ", conversionRate, "% ")
    } else if (round((comApp$compAppCount / personnelNum) * 100, 1) < 100) {
        slackMsg <- paste0(slackMsg, " :large_yellow_circle:", "\n>최종 전환: ", conversionRate, "% ")
    } else {
        slackMsg <- paste0(slackMsg, " :large_green_circle:", "\n>최종 전환: ", conversionRate, "% ")
    }

    if (conversionRate < 5) {
        slackMsg <- paste0(slackMsg, ":red_circle:")
    } else if (conversionRate <= 15) {
        slackMsg <- paste0(slackMsg, ":large_yellow_circle:")
    } else {
        slackMsg <- paste0(slackMsg, ":large_green_circle:")
    }

    if (previousGen == 0) {
        slackMsg <- paste0(slackMsg, "\n>", "\n> 첫 기수 입니다.")
    } else {
        slackMsg <- paste0(slackMsg, "\n>", "\n>이전: ", preGenApp$startAppCount, " / ", preGenComApp$compAppCount, " / ", preGenConversionRate, "%\n")
    }


    slackr_msg(
        txt = slackMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}





if (AI == FALSE) {
    underConstructionMsg <- paste0("\n*", "AI 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}
if (PM == FALSE) {
    underConstructionMsg <- paste0("\n*", "프로덕트 매니지먼트 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}
if (GM == FALSE) {
    underConstructionMsg <- paste0("\n*", "그로스 마케팅 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}
if (FB == FALSE) {
    underConstructionMsg <- paste0("\n*", "프론트엔드 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}
if (BB == FALSE) {
    underConstructionMsg <- paste0("\n*", "백엔드 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}
if (BE == FALSE) {
    underConstructionMsg <- paste0("\n*", "블록체인 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}
if (Dev == FALSE) {
    underConstructionMsg <- paste0("\n*", "데브옵스 부트캠프", "*", "\n>:hammer_and_wrench:모집 준비중:hammer_and_wrench:")
    slackr_msg(
        txt = underConstructionMsg,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )
}

chunsickNoti <- paste0("지원자 양&질 실험을 통해 수강생의 퀄리티를 유지하기 위해 3.3배수의 모집이 필요한 것으로 파악되어 모집목표 인원을 3.3배수로 변경하여 표시합니다. :pray:")
    slackr_msg(
        txt = chunsickNoti,
        channel = "#team-admission",
        username = "춘식이",
        token = Sys.getenv("SLACK_TOKEN"),
        thread_ts = NULL,
        reply_broadcast = FALSE,
    )