범죄와 날씨의 관계
================

``` r
library(grid)
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.4.2

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:gridExtra':
    ## 
    ##     combine

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.2

``` r
library(Rmisc)
```

    ## Warning: package 'Rmisc' was built under R version 3.4.2

    ## Loading required package: lattice

    ## Loading required package: plyr

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 3.4.2

``` r
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 3.4.2

    ## Loading required package: magrittr

초록
====

이 연구는 범죄 발생에 날씨가 어떤 영향을 미치는지 분석하였다. 연구에 사용된 자료는 e-국가지표, 통계청, 기상청, 공공데이터포털 자료이다. 월별로 범죄 발생 건수를 분석해보면 기온이 낮은 1-2월에는 범죄 발생 건수가 줄어들지만 비교적 따뜻한 3-4월에는 다시 증가하는 패턴을 보인다. 이에 주목하여 *'기온이나 날씨가 범죄에 영향을 주었을 것이다'*를 가설로 세워 분석해보았다. 우선 연간 범죄 발생 건수와 평균 기온을 그래프로 그려 비교해 보았는데 매년의 범죄 발건 건수와 평균 기온이 어떠한 관계가 있음을 알아냈다. 이후 봄, 여름, 가을, 겨울별로 구분지어 이 상관관계가 존재하는지, 어떤 계절에 범죄가 더 발생하는지 확인하였다. 결론적으로 기온이 높을 때 범죄가 더 많이 발생하는 현상을 알게 되었다. 다만 날씨에는 기온 이외에도 다양한 요인이 존재하기 때문에 추가적인 분석이 필요하다.

------------------------------------------------------------------------

분석주제
========

매년 다양한 범죄가 예측되지 않은 상태에서 발생하게 된다. 범죄가 발생이 이후에 사후 조치를 하는 것 보다는 발생이 일어나 않게 예방하는 것이 더 중요하다. 따라서 범죄 발생이 어떠한 요인에 영향을 받는지 조사해보았다. 09년에 범죄 발생건수가 대체적으로 높았다가 10-11년에 급격히 하락하는 모습에 주목하여 이 시점에 어떠한 일이 있었는지 조사해보았다. 초기조사에서 11년 1-2월에 상당한 폭설이 왔음을 알게되었고 날씨가 범죄가 영향을 줄 것이라 생각하여 *'기온이나 날씨가 범죄에 영향을 주었을 것이다'*를 가정하여 조사해 보았다.

------------------------------------------------------------------------

분석 데이터
===========

날씨가 범죄와 연관이 있는지 알아보기 위해 연 평균 기온, 계절별 평균 기온, 연간 범죄 발생, 월간 범죄 발생 데이터를 이용했다. 연간 기온의 변화 이외에도 계절간의 기온 차이가 계절간 범죄 건수에 영향을 미칠 것이라 판단하여 통계청이 제시한 기준에 따라 데이터를 봄(3월~5월), 여름(6월~8월), 가을(9월~11월), 겨울(12월~다음해 2월)로 구분지었다.

------------------------------------------------------------------------

분석
====

``` r
#연간 범죄 발생 건수
crime <- read.csv("범죄 지역.csv")
crime <- `crime` %>%
  filter(범죄별 == "범죄발생 총건수(A)[건]", 발생지별 == "합계" ) %>%
  select(시점, 범죄.발생지)

total_crime <- ggplot(data = crime, aes(x = 시점, y = 범죄.발생지, group = 1)) + 
  geom_line() + geom_point(size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("[ Crime ]")

total_crime
```

![](https://github.com/mizykk/mizy/blob/master/unnamed-chunk-2-1.png)

`Crime` 를 통해 범죄 발생 건수가 05년과 10년, 14년에 급격하게 하락함을 알 수 있다.

``` r
#09년-11년
n_t <- read.csv("9년_11년.csv")
colnames(n_t)[1] <- "Year"
ggplot(data = n_t, aes(x = Year , y = 합계, group = 1)) +
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("[ 2009~2011년 월별 범죄 발생 건수 ]")
```

![](https://github.com/mizykk/mizy/blob/master/unnamed-chunk-3-1.png)

       2009-2011년에 범죄 발생 건수가 급격하게 하락한 뒤 다시 증가하는 현상을 좀 더 자세히 알아보고자  *09년 ~ 11년의 월별 범죄 발생 건수*를 분석해보았다.   
    이 그래프를 통해 범죄 발생 건수가 2009년 6월 이후 점차 하락하다가 2010년 1-2월에 상당히 줄어 든 것을 알 수 있다. 전년도 12월보다 1-2월에 급격히 하락 한 후, 3-4월에 다시 증가하는 이 패턴은 2009년과 2011년에도 나타났다. 이에 따라 1-2월은 다른 월에 비해 춥지만 3-4월은 따스하다는 점에 주목하였고 *'기온이나 날씨가 범죄에 영향을 주었을 것이다'*를 가정하여 조사해 보았다. 

<hr/>
평균 기온과 범죄 건수의 관계
----------------------------

### &lt;<연간 비교>&gt;

``` r
#연 평균 기온
tem_season <- read_excel("계절별 기온 평균.xls")
colnames(tem_season)[1] <- 'Year'
colnames(tem_season)[2] <- 'mean'

total_temperature <- ggplot(data = tem_season, aes(x = Year, y = mean, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#연 평균 기온과 연간 범죄 발생 건수 비교
figure <- ggarrange(total_crime, total_temperature,   
                    ncol = 2, nrow=1)
annotate_figure(figure, 
                top = text_grob("연간 비교", color = "red", face = "bold", size = 15),
                left = text_grob("범죄",  color = "Blue", face = "bold"), 
                right = text_grob("기온", color = "Blue", face = "bold"))
```

![](https://github.com/mizykk/mizy/blob/master/unnamed-chunk-4-1.png)

범죄 발생 그래프에서 05년과 10년에 급격하게 범죄 발생 건수가 하락한 것과 유사하게 연간 평균 기온 그래프에서도 05년과 10년에는 평년보다 기온이 낮게 나타났다. 두 그래프에서 나타나는 깊은 하락 구간을 주목하면, 두 그래프가 매우 유사한 패턴을 보이고 있음을 확인할 수 있다. 평균 기온이 하락하는 구간은 쉽게 말해, 여름에 덜 덥고 겨울에 더 춥다고 생각할 수 있다. 그러나 이는 평균의 함정을 유발 할 수 있으며, 실제로 그런 추측이 맞는지 확인하기 위해 4계절을 나누어 분석했다.

계절별 비교
===========

``` r
#계절별 범죄 데이터가 02년~13년으로 되어 있기 때문에 기준에 맞춰 기온 데이터 추출
tem_s <- tem_season %>% filter(Year >= 2002 & Year <= 2013) 

#월별 범죄 건수 
s <- read.csv("월별 범죄 건수.csv")
s <- s %>% filter(범죄별 == "합계")
##봄 범죄 건수 
spring <- data.frame(year = c(2002:2013),
                       crime = c(173211.3, 163206.3, 180858.7, 180858.7, 146855.3, 
                              167788,175122.7, 175122.7, 165951, 167491.3, 
                              163213, 165799.7))
##봄 범죄 그래프
spring_crime <- ggplot(data = spring, aes(x = year, y = crime, group = 1)) +
  geom_point() + geom_line()

##봄 기온
spring_temperature <- ggplot(data = tem_s, aes(x = Year, y = 봄, group = 1)) +
  geom_point() + geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##여름 범죄 건수
summer <- data.frame(year = c(2002:2013),
                       crime = c(161693.7, 182493.3, 181865.3, 163033.3, 152333, 170465.7,
                              186942, 198297, 168424, 168222.3, 174152.7, 179307))

##여름 범죄 그래프
summer_crime <- ggplot(data = summer, aes(x = year, y = crime, group = 1)) +
  geom_point() + geom_line()

##여름 기온 그래프 
summer_temperature <- ggplot(data = tem_s, aes(x = Year, y = 여름, group = 1)) +
  geom_point() + geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##가을 범죄 건수
fall <- data.frame(year = c(2002:2013),
                     crime = c(175024, 169195.3, 167842.3, 160926.3, 166018, 
                            167561, 202226.7, 175024, 160418, 150812.3, 
                            169985.7, 171892))

##가을 범죄 그래프
fall_crime <- ggplot(data = fall, aes(x = year, y = crime, group = 1)) +
  geom_point() + geom_line()

##가을 기온 그래프 
fall_temperature <- ggplot(data = tem_s, aes(x = Year, y = 가을, group = 1)) +
  geom_point() + geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##겨울 범죄 건수
winter <- data.frame(year = c(2002:2013),
                       crime = c(146092, 159424, 157917, 144425.7, 149631.3, 148481, 
                              172297.3, 142977.3, 151718, 141476, 162744.3, 174595))

##겨울 범죄 그래프 
winter_crime <- ggplot(data = winter, aes(x = year, y = crime, group = 1)) +
  geom_point() + geom_line()

##겨울 기온 그래프 
winter_temperature <- ggplot(data = tem_s, aes(x = Year, y = 겨울, group = 1)) +
  geom_point() + geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

계절별 기온과 범죄의 관계
=========================

1.  

``` r
#계절별 기온과 계절별 범죄 발생 건수 비교
figure2 <- ggarrange(spring_crime, spring_temperature, 
                     summer_crime, summer_temperature,
                     fall_crime, fall_temperature,
                     winter_crime, winter_temperature,
                     ncol = 2, nrow=4)

annotate_figure(figure2,
                top = text_grob("계절별 비교", color = "red", face = "bold", size = 15),
                left = text_grob("범죄",  color = "Blue", face = "bold"), 
                right = text_grob("기온", color = "Blue", face = "bold"))
```

![](https://github.com/mizykk/mizy/blob/master/unnamed-chunk-6-1.png)

봄의 경우, 범죄 발생 건수가 급격하게 줄어든 2006년에는 평년에 비해 기온이 낮고, 이후 기온이 증가함에 따라 범죄건수도 증가한다. 2009년 보다 평균 기온이 크게 떨어지는 2010년에 범죄 건수도 감소한다. 같은 맥락으로 여름의 경우에도 평년 보다 온도가 높으면 범죄의 수가 감소한다. 반면 가을과 겨울의 경우에는 평년 보다 온도가 낮을수록(추울수록) 범죄가 하락했다.

1.  

``` r
#계절별 평균 기온
tem_s2 <- data.frame(Year = c(2002:2013))
tem_s2$연간 <- as.numeric(tem_s$mean)
tem_s2$봄 <- as.numeric(tem_s$봄)
tem_s2$여름 <- as.numeric(tem_s$여름)
tem_s2$가을 <- as.numeric(tem_s$가을)
tem_s2$겨울 <- as.numeric(tem_s$겨울)
tem_s2
```

    ##    Year 연간   봄 여름 가을 겨울
    ## 1  2002 12.6 12.7 23.1 12.5  0.9
    ## 2  2003 12.5 11.9 22.3 14.7  1.4
    ## 3  2004 13.2 12.2 24.0 14.7  0.5
    ## 4  2005 12.3 11.7 24.1 14.8 -0.1
    ## 5  2006 12.9 11.5 23.6 15.1  2.4
    ## 6  2007 13.2 12.1 23.8 14.5  0.7
    ## 7  2008 12.9 12.5 23.7 15.1  1.7
    ## 8  2009 13.0 12.6 23.3 14.7  0.5
    ## 9  2010 12.7 10.8 24.9 14.5 -0.7
    ## 10 2011 12.4 11.0 24.0 15.3 -0.4
    ## 11 2012 12.3 12.2 24.7 13.7 -1.0
    ## 12 2013 12.9 11.6 25.4 14.6  1.5

``` r
ggplot(data = tem_s2, aes(x = Year, y = 봄, group = 1, col = "봄")) + geom_line() +
  geom_line(data = tem_s2, aes(x = Year, y = 여름, group = 1, col = "여름")) +
  geom_line(data = tem_s2, aes(x = Year, y = 가을, group = 1, col = "가을")) +
  geom_line(data = tem_s2, aes(x = Year, y = 겨울, group = 1, col = "겨울"))
```

![](https://github.com/mizykk/mizy/blob/master/unnamed-chunk-7-1.png)

``` r
#계절별 범죄 건수
ggplot(data = spring, aes(x = year, y = crime, group = 1, col = "봄")) + geom_line() +
  geom_line(data = summer, aes(x = year, y = crime, group = 1, col = "여름")) +
  geom_line(data = fall, aes(x = year, y = crime, group = 1, col = "가을")) +
  geom_line(data = winter, aes(x = year, y = crime, group = 1, col = "겨울"))
```

![](https://github.com/mizykk/mizy/blob/master/unnamed-chunk-7-2.png)

*계절별 범죄 건수 그래프*에서 기온(평균)이 낮은 겨울에는 범죄 발생 건수가 적게 나타났고 기온이 높은 여름에는 범죄 발생 건수가 높게 나타났다. *계절별 기온과 계절별 범죄 발생 건수 비교 (1), (2)*를 종합하면, 계절별로도 기온이 높을 때는 범죄가 더 발생하고 기온이 낮을 때는 범죄가 덜 발생하는 것을 알 수 있다. 전자의 경우 불쾌지수와 연관지어 생각해 볼 수 있다. 기온이 높을수록 불쾌지수가 높아져, 그만큼 범죄가 증가했다고 추측해볼 수 있다. 반면 평년 보다 기온이 낮을 때, 말하자면 평소 보다 추울 때는 야외 활동이 줄어들기 때문에 범죄가 감소했을 수 있다.

4. 결론
=======

날씨는 기온 뿐만 아니라 구름, 강수량, 태풍, 습도 등 다양한 요소로 구성되어 있으며 같은 시점이라도 각 지역별로 다소 차이가 존재한다. 기온에 대하여만 분석했다는 점과 전국을 통합하여 날씨의 세부적인 요인과 지역별 차이를 고려한 추가적인 분석이 필요하다. 본 연구를 통해 기온이 높아지면 범죄 발생도 증가하는 것을 알 수 있다.
