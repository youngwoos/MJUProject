초록
====

본 보고서는 직종별 구인/구직의 추이에 기초한 분석을 바탕으로 이루어졌다.
분석 초기에는 노동의 수요 측면을 구인자 수, 노동의 공급 측면을 구직자
수로 설정하여 년도\_월별 초과 공급에 집중하였다. 현대 사회의 노동 시장은
실업상태가 항상 존재하는 수요부족경제로써, 공급에서 수요를 제외한 잉여
노동량이 중요하게 논의되기 때문이다. 각 년도의 초과 수요의 그래프별
추이는 총 3가지 특징이 나타났다. 첫 번째는 초과 공급이 (-)음의 값을
나타내는 직종들이 나타났다는 점이다. 이공 계열로 대표되는 기계관련직,
재료관련직, 화학관련직과 식품가공관련직, 환경/인쇄/목재/가구/공예 및
생산 단순직, 농림어업과 관련된 생산직에서 이와 같은 초과 수요가
나타났다. 두 번째는 위 여섯 직종들의 초과 수요분이 점차 감소한다는
것이다. 마지막으로 15년도 초과 공급의 크기가 전체적으로 감소했다는 점
역시 눈에 띄었다. 이와 같은 특징들은 기존에 일손이 부족하다고 여겨졌던
이공계 직종들으로의 노동 공급이 점차 증가해왔음을 의미한다.

분석 주제
=========

2013년부터 2015년까지의 직종별 구인구직 데이터를 통해 직종별 노동 수급에
대해 살펴볼 예정이다. 특히 직종별로 구직자 수에서 구인자 수를 차감한
초과 공급를 연도별로 비교하여 노동 시장의 추세를 살피고, 그 원인에 대해
분석할 예정이다.

데이터 선정
-----------

-산업경제 일자리 구인구직 및
취업현황(서울연구소데이터서비스:2013년~2015년 자료)

데이터 선정 이유
----------------

본 자료는 고용노동부 고용정보시스템의 홈페이지 측정량을 표본으로
작성되기 때문에 신뢰성을 확보하고 있다. 또한 매년 매달마다의 데이터를
보유하고 있으며, 구인자 수와 구직자 수의 자료가 분류되어 있기 때문에
노동 시장의 동향을 관찰하기 용이하다고 판단했다.

데이터 소개
-----------

### 출처

서울연구소데이터서비스(<http://data.si.re.kr/node/38>)

### 특성

본 데이터는 매년, 매월 작성되기 때문에 시계열 추이를 통해 노동 시장을
분석하기 유리하다. 또한 임금별, 학력별, 구직 형태별, 직종별로 분석
표본의 변수를 다양화하고 있으며, 구인자 수와 구직자 수, 취업자 수에 따라
데이터를 분류한다. \#\#\#구성

1개월치 데이터: 임금, 학력, 구직 형태별, 직종별 구인자수(구직자수와
취업자수) 시간: 2013년\_1월 ~ 2015\_ 12월

분석과정
========

분석에 필요한 패키지들을 실행한다.

    library(dplyr)
    library(ggplot2)

데이터 특징 살펴보기
--------------------

    D_13 <- read.csv("2013_D.csv")   #13년도 구인 자료를 불러온다.
    D_13_1 <- D_13 %>%               #직종별 구인 평균치를 구한다.  
      group_by(job) %>% 
      summarise(D_M = mean(total))

    S_13 <- read.csv("2013_S.csv")   #13년도 구직 자료를 불러온다.
    S_13_1 <- S_13 %>%               #직종별 구직 평균치를 구한다.
      group_by(job) %>% 
      summarise(S_M = mean(total))

    DS_13 <- left_join(D_13_1, S_13_1, by = 'job')       # 직종별 구인/구직 평균치를 하나의 데이터를 만든다.
    DS_13 <- DS_13 %>%                # 구인 평균치에서 구직 평균치를 빼서 초과 공급치를 파생변수로 추가한다.
      group_by(job) %>% 
      mutate(ex_s = S_M - D_M)

    ggplot(data = DS_13, aes(x = job, y = ex_s)) +   # 직종별 구인 평균치를 그래프로 나타낸다.
      geom_col() +
      ggtitle("13년도 직종별 초과 공급 평균치") +
      ylim(-10000,50000)

    D_14 <- read.csv("2014_D.csv")
    D_14_1 <- D_14 %>%
      group_by(job) %>% 
      summarise(D_M = mean(total))

    S_14 <- read.csv("2014_S.csv")
    S_14_1 <- S_14 %>% 
      group_by(job) %>% 
      summarise(S_M = mean(total))

    DS_14 <- left_join(D_14_1, S_14_1, by = 'job')
    DS_14 <- DS_14 %>%
      group_by(job) %>% 
      mutate(ex_s = S_M - D_M)

    ggplot(data = DS_14, aes(x = job, y = ex_s)) +
      geom_col()+
      ggtitle("14년도 직종별 초과 공급 평균치") +
      ylim(-10000,50000)

    D_15 <- read.csv("2015_D.csv")
    D_15_1 <- D_15 %>%
      group_by(job) %>% 
      summarise(D_M = mean(total))

    S_15 <- read.csv("2015_S.csv")
    S_15_1 <- S_15 %>% 
      group_by(job) %>% 
      summarise(S_M = mean(total))

    DS_15 <- left_join(D_15_1, S_15_1, by = 'job')
    DS_15 <- DS_15 %>%
      group_by(job) %>% 
      mutate(ex_s = S_M - D_M)

    ggplot(data = DS_15, aes(x = job, y = ex_s)) +
      geom_col()+
      ggtitle("15년도 직종별 초과 공급 평균치") +
      ylim(-10000,50000)

위에 나타난 3년간의 직종별 초과 수요 그래프의 추이를 살펴보면 다음과
같은 3가지 특징이 나타난다.

1.  초과 공급이 지속적으로 음(-)의 값을 갖는 직종이
    존재한다.(o,p,q,u,v,w) 이는 각각 기계관련직, 재료관련직,
    화학관련직,식품가공관련직, 환경/인쇄/목재/가구/공예 및 생산 단순직,
    농림어업 관련직이다.

2.  위 여섯 직종들의 음(-)의 절대값은 점차 감소한다. 즉 초과 수요가
    줄어들고 있다는 의미이다.

3.  양(+)의 값을 갖는 직종들의 초과 공급은 2015년에 전체적으로 감소한다.

4.  초과 수요 상태에서 초과 공급 상태로 바뀌는 직종이 있다.(s,r) 이는
    각각 섬유/의복 관련직, 전기/전자 관련직이다.

초과 수요 직종 : 음(-)의 절대값은 왜 점차 감소할까?
---------------------------------------------------

    AD <- read.csv("2013_D.csv")   #3년치 직종별 구인 자료를 통합합니다.
    BD <- read.csv("2014_D.csv")
    CD <- read.csv("2015_D.csv")

    TD <- bind_rows(AD,BD,CD)

    write.csv(TD, "13_15_TD.csv")  #통합한 자료를 csv로 저장합니다.  


    AS <- read.csv("2013_S.csv")   #3년치 직종별 구직 자료를 통합합니다.
    BS <- read.csv("2014_S.csv")
    CS <- read.csv("2015_S.csv")

    TS <- bind_rows(AS,BS,CS)

    write.csv(TS, "13_15_TS.csv")  #통합한 자료를 csv로 저장합니다.

자료를 효과적으로 분석하기 위해 하나의 데이터로 통합했다.

    TDV <- TD %>% 
      filter(job %in% c("o","p","q","u","v","w")) %>%    #c()에 해당하는 직종의 시간별 구인 평균치를 구한다.
      group_by(date) %>% 
      summarise(D_M=mean(total))

    TSV <- TS %>% 
      filter(job %in% c("o","p","q","u","v","w")) %>%    #c()에 해당하는 직종의 시간별 구직 평균치를 구한다.
      group_by(date) %>% 
      summarise(S_M=mean(total))
      
    TSDV <- left_join(TDV,TSV, by = 'date' )    # 두 데이터를 통합한다.

    ggplot(TSDV,aes(x=date)) +
      geom_line(aes(y=S_M, group=2,colour = "S_M")) +
      geom_line(aes(y=D_M,group=1, colour = "D_M")) +
      geom_point(aes(y=S_M, colour = "S_M")) +
      geom_point(aes(y=D_M, colour = "D_M")) +
      scale_colour_manual(" ",breaks = c("S_M", "D_M"),
                             values = c("red", "blue")) +
      ggtitle("초과 수요 직종들의 구인 D/ 구직 S 추이") +
      ylim(0,17000) 

        #두 평균치를 시계열 그래프로 나타낸다.

이 그래프는 초과 공급이 음(-)의 값을 갖는 여섯 직종의 구인/구직 평균치의
추이를 나타낸다. 빨간 선은 구직자 수 평균을 나타내고, 파란 선은 구인자
수 평균을 나타낸다. 시간이 갈수록 구직자 수가 증가하는 추세를 보이는데,
특히 15년도 후반에 와서는 두 그래프가 교차되기도 한다. 이는 노동 공급의
증가가 노동 시장의 균형을 가져와 노동의 초과 수요분을 점차 상쇄함을
나타낸다.

또한 위 여섯 직종은 다음과 같이 분류할 수 있다.

-이공계 관련직:기계관련직, 재료관련직, 화학관련직(o,p,q)

-생산 관련직:식품가공관련직, 환경/인쇄/목재/가구/공예 및 생산 단순직,
농림어업 관련직(u,v,w)

따라서 위 시계열 그래프를 두 항목으로 분류 후 측정하여, 두 직군에서의
노동 수급 추세를 살펴볼 수 있다.

### 이공계 관련직 추이

    TDV_E <- TD %>% 
      filter(job %in% c("o","p","q")) %>%
      group_by(date) %>% 
      summarise(D_ME=mean(total))

    TSV_E <- TS %>% 
      filter(job %in% c("o","p","q")) %>%
      group_by(date) %>% 
      summarise(S_ME=mean(total))

    TSDV_E <- left_join(TDV_E,TSV_E, by = 'date' )

    ggplot(TSDV_E,aes(x=date)) +
      geom_line(aes(y=S_ME, group=2,colour = "S_ME")) +
      geom_line(aes(y=D_ME,group=1, colour = "D_ME")) +
      geom_point(aes(y=S_ME, colour = "S_ME")) +
      geom_point(aes(y=D_ME, colour = "D_ME")) +
      scale_colour_manual("",
                          breaks = c("S_ME", "D_ME"),
                          values = c("red", "blue")) +
      ggtitle("이공계 직종 구인D / 구직S 평균치 추이") + 
      ylim(0,17000) 

전체적으로 여섯 직종 모두를 포함한 그래프와 거의 동일한 추이를 보여준다.
단, 구인자 수도 감소하는 형태가 더욱 뚜렷해 보이는데, 이는 관련 직종으로
노동 공급이 증가하면서 노동에 대한 수요가 점차 감소함을 나타내는 것이다.

### 생산 관련직 추이

    TDV_P <- TD %>% 
      filter(job %in% c("u","v","w")) %>%
      group_by(date) %>% 
      summarise(D_MP=mean(total))

    TSV_P <- TS %>% 
      filter(job %in% c("u","v","w")) %>%
      group_by(date) %>% 
      summarise(S_MP=mean(total))

    TSDV_P <- left_join(TDV_P,TSV_P, by = 'date' )

    ggplot(TSDV_P,aes(x=date)) +
      geom_line(aes(y=S_MP, group=2,colour = "S_MP")) +
      geom_line(aes(y=D_MP,group=1, colour = "D_MP")) +
      geom_point(aes(y=S_MP, colour = "S_MP")) +
      geom_point(aes(y=D_MP, colour = "D_MP")) +
      scale_colour_manual("",
                          breaks = c("S_MP", "D_MP"),
                          values = c("red", "blue")) +
      ggtitle("생산 직종 구인D / 구직S 평균치 추이") +
      ylim(0,17000)

그래프의 전반부까지 구직자 수보다 구인자 수가 더 많은 '초과수요'상태를
나타낸다. 반면 2014년도 중반부터 두 변수가 대략 비슷해지면서 노동의
수요량과 공급량이 일치하기 시작한다.

*두 그래프를 통해 얻어낸 결론 : 위 여섯 직종에서의 노동 공급량이
증가하면서 초과 수요가 점차 감소한다.*

### 초과 공급 직종 : 양의 값은 왜 2015년에 감소할까?

    TD2 <- TD %>% 
      filter(job != "o" &
             job != "p" &
             job != "q" &
             job != "u" &
             job != "v" &
             job != "w" ) %>% 
      group_by(date) %>% 
      summarise(MD=mean(total))

    TS2 <- TS %>%
      filter(job != "o" &
             job != "p" &
             job != "q" &
             job != "u" &
             job != "v" &
             job != "w" ) %>%
      group_by(date) %>% 
      summarise(MS=mean(total))

    TDS2 <- left_join(TD2, TS2, by = 'date')

    ggplot(TDS2,aes(x=date)) +
      geom_line(aes(y=MS, group=2,colour = "MS")) +
      geom_line(aes(y=MD,group=1, colour = "MD")) +
      geom_point(aes(y=MS, colour = "MS")) +
      geom_point(aes(y=MD, colour = "MD")) +
      scale_colour_manual("",
                          breaks = c("MS", "MD"),
                          values = c("red", "blue")) +
      ggtitle("전체 직종 구인D / 구직S 평균치 추이")

위는 초과 공급이 가장 큰 세 직종의 구인/구직자 수의 평균을 시계열로
나타낸 그래프이다. 2015년 후반에 주목하면 구인자 수의 평균치와 구직자
수의 평균치가 교차한다. 구직 상황이 급격한 회복세를 보이는 이유는 당시
정부에서 취업 장려 보조금을 지급했기 때문이다.

*그래프를 통해 얻어낸 결론 : 2015년 말에 노동의 수요가 증가하고 공급이
감소함에 따라 초과 공급이 감소했다.*

음(-)에서 양(+)으로 : 초과 수요 상태에서 초과 공급상태로 바뀌는 직종
--------------------------------------------------------------------

     TDC <- TD %>% 
      filter(job %in% c("s","r")) %>% 
      group_by(date) %>% 
      summarise(CD = mean(total))

     TSC <- TS %>% 
       filter(job %in% c("s","r")) %>% 
       group_by(date) %>% 
       summarise(CS = mean(total))
     
     TSDC <- left_join(TDC, TSC, by = 'date')
     
     ggplot(TSDC,aes(x=date)) +
      geom_line(aes(y=CS, group=2,colour = "CS")) +
      geom_line(aes(y=CD,group=1, colour = "CD")) +
      geom_point(aes(y=CS, colour = "CS")) +
      geom_point(aes(y=CD, colour = "CD")) +
      scale_colour_manual("",breaks = c("CD", "CS"),
                             values = c("red", "blue")) +
      ggtitle("초과 수요 직종 구인D / 구직S 평균치 추이")

논의
====

위 데이터는 최근 이공계에서의 점차 노동 공급이 증가하면서 부족했던
일손이 채워진다는 것을 보여준다. 하지만 이는 이중적으로 이공계에서의
취업도 점차 어려워 질 것임을 시사한다. 또한 2015년도 말에 구직 여건이
개선되면서 노동의 초과 공급이 감소하는 특징을 드러내기도 한다.

하지만 위 특징들은 2013년부터 2015년까지 3년간의 표본 데이터의 분석이
기반이 되었다는 점에서 단기적인 관찰 결과에 불과하다고 할 수 있다. 노동
시장의 전반적인 추세을 살펴보기 위해서는 좀 더 장기적인 데이터를
표본으로 고려해볼 필요가 있다. 또한 <전체 구인/구직 평균치 추이>에서
2015년 말기에서 나타난 구직 여건의 개선이 2016년에 어떤 형태로
이어지는지 알 수 있다면, 비교적 최근의 고용 상황을 알아볼 수 있었을
것이다. 전년도의 자료가 찾지 못해서 반영하지 못한 점이 아쉽다. 원본
데이터에는 위에 언급된 다양한 변수들이 지역별로 정리되어 있었지만, 과제
기간이 촉박하여 이를 분석 과정에 반영하지 못했다. 지역별 직종의 비중과
초과 공급 상황을 고려한다면 현재 한국의 전반적인 구인/구직 상황을
자세하게 다를 수 있을 것이다.
