1. 요약
=======

### 이 연구의 목적은 재정 수준과 나이와의 상관관계를 알아보는 데 있다. 이를 위해서 가설을 세웠고 그 가설을 검증할 수 있는 데이터를 선정해 분석했다. 마지막으로 분석한 결과의 한계점을 알아봤다.

2. 분석 주제
============

### "재정 수준이 높은 지역일수록 평균연령이 높을 것이다."라는 가설을 세웠고 가설을 검증하기 위해서 평균연령과 재산세를 상관 분석했다.

3. 데이터 선정
==============

### 데이터 선정 이유

이 데이터를 선정한 이유는 "2016 지방세 징수(구별).xls"에는 구별 재산세
데이터가 있고 "2016 평균연령.xls"에는 구별 평균연령 데이터가 있기
때문이다.

### 데이터 소개

이 데이터에서 사용한 재산세는 시세가 아닌 구세다. 시세는 시에서 거둬들인
세금을 의미하고 구세는 구에서 거둬드린 세금을 의미한다.

### 용어설명

-   평균연령은 외국인을 제외하고 {각 세 연령별 인구×(연령+0.5)}의 총합을
    총인구로 나눈 값이다.
-   지방세는 지방자치단체가 징수하는 조세로, 국가의 수권 때문에 주로
    지방자치단체가 담당하는 지역사회의 복지를 증진하기 위한
    목적에서 징수된다.

### 데이터 출처

2016 지방세 징수(구별) 데이터의 출처는 서울특별시 세무과다. 2016
평균연령(구별) 데이터의 출처는 서울특별시 통계데이터담당관이다.

데이터 파악하기
---------------

    #데이터 불러오기
    library(readxl)

    ## Warning: package 'readxl' was built under R version 3.4.2

    df1 <- read_excel("2016 지방세 징수(구별).xls", col_names = T)
    df2 <- read_excel("2016 평균연령(구별).xls", col_names = T)

    #데이터 가공하기
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    df1 <- df1 %>% filter(기간 != '기간' & 자치구별 != '서울시' & 자치구별 != '본청') %>% select(`보통세__11`, `자치구별`)
    df2 <- df2 %>% filter(지역 != '합계') %>% select(`기간`, `지역`, `전체평균연령`)
    df1 <- rename(df1, 지역 = 자치구별)
    df_total <- left_join(df2, df1, by = '지역')
    df_total<-rename(df_total, 재산세=보통세__11)
    df_total[, 4] <- sapply(df_total[, 4], function(`재산세`){as.numeric(as.character(`재산세`))})

    #데이터 파악하기
    str(df_total)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    25 obs. of  4 variables:
    ##  $ 기간        : chr  "2016" "2016" "2016" "2016" ...
    ##  $ 지역        : chr  "종로구" "중구" "용산구" "성동구" ...
    ##  $ 전체평균연령: num  43.4 43.8 42.7 41.3 40.5 42.3 42.2 41.5 43.5 42.4 ...
    ##  $ 재산세      : num [1:25, 1] 77699 96793 80559 65107 62759 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr "재산세"

    head(df_total)

    ## # A tibble: 6 x 4
    ##    기간     지역 전체평균연령 재산세
    ##   <chr>    <chr>        <dbl>  <dbl>
    ## 1  2016   종로구         43.4  77699
    ## 2  2016     중구         43.8  96793
    ## 3  2016   용산구         42.7  80559
    ## 4  2016   성동구         41.3  65107
    ## 5  2016   광진구         40.5  62759
    ## 6  2016 동대문구         42.3  61529

4. 분석
=======

    ##지방소득세와 평균연령의 상관관계 분석
    cor.test(df_total$`재산세`, df_total$`전체평균연령`)

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df_total$재산세 and df_total$전체평균연령
    ## t = -2.6006, df = 23, p-value = 0.01599
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.7336326 -0.1004772
    ## sample estimates:
    ##       cor 
    ## -0.476683

    ###p-value 값이 0.05보다 작기 때문에 두 변수는 상관관계가 있다고 볼 수 있다.

    ##가설 검증
    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.4.2

    ggplot(data = df_total, aes(x = `재산세`, y = `전체평균연령`)) + 
      geom_point(shape=19, size=2, colour = 'red') + 
      ggtitle('재산세와 평균연령의 관계') +
      stat_smooth(method=lm, level=0.95) +
      geom_text(aes(label = `지역`, size = 1, vjust = -1, hjust = 0))

![](재산세와_평균연령의_상관관계_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ###재정상태가 좋을수록 평균연령이 낮았다.


    df_total %>% arrange(`전체평균연령`) %>% 
      select(`지역`) %>% 
      head(3)

    ## # A tibble: 3 x 1
    ##     지역
    ##    <chr>
    ## 1 서초구
    ## 2 강남구
    ## 3 양천구

    #재산세를 가장 많이 거둬들인 세 지역은 강남구, 서초구, 송파구였고 전체평균연령이 가장 낮은 세 지역 서초구, 강남구, 양천구였다.

5. 한계
=======

첫째, 재산이 많더라도 탈세의 가능성이 있을 수 있기 때문에 재산세와 재정
수준을 동일시하는 것은 한계가 있다. 둘째, 2016년 서울시로 한정 지었기
때문에 일반화할 수 없다. 셋째, 재산과 연령이 아닌 구별 재산세와
평균연령을 변수로 사용했기 때문에 결과가 정확하지 않을 수 있다.
