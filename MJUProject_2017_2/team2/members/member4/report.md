**벤처기업 업종별 성비분석**
============================

**60160286 안은영**
-------------------

**1.요약(초록):**
-----------------

사용하기로 했던 raw 데이터인 BC.csv를 불러와서 데이트의 속성을View()를 통해 확인한다. 그 중 나의 분석에 필요한 업종과 남성,여성 종사자 수를 select함수를 사용해서 뽑아낸후에 새로운 데이터인 data2를 만든다. data2의 변수명을 알아보기 편하게 변경한 후에 변수타입과 결측치를 확인한다. 성비를 남성종사자수/총종사자수\*100으로 설정하여 변수를 data2에추가한다. dplyr함수로 업종별 성비에 대해 알아보고 난뒤에 ggplot을 활용해 표로 출력하였다.

**2.분석주제:**
---------------

업종별로 성비의 차이가 클까? 크다면 어떤업종이 가장 성비가 불균형할지 알아보도록한다.

**3.데이터 선정:**
==================

-이유: 곧 취업을 해야한는 대학생들에게 미리 어떤업종에 남성과 여성이 어느정도 비율로 일을 하고 있는지를 파악하는데 도움이 되도록하기 위해서 raw데이터로 벤처기업 정밀실태조사 데이터를 선정하였다.

-데이터소개: 출처-공공데이터 포털(<https://www.data.go.kr/dataset/3043469/fileData.do>) 특성-2050여개의 행으로 2012년부터2016년까지의 벤처기업 실태조사에 대한 자료를 담고있다. 구성-업종,회사주소,대표이사 최종학력 등 벤처기업의 다양한 요소들에 대한 변수를 제공하고 있다.

**4.분석**
----------

``` r
#
library(ggplot2)
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#raw데이터 불러오기
data<-read.csv("BC.csv",stringsAsFactors = F)

#데이터 속성 살펴보기
View(data)

#필요한 데이터 추출하기
data2<-data%>%select(3,77,78)
View(data2)
head(data2)
```

    ##   X.쿼터..업종 C1.1..2015년.총.정규직.수...남성
    ## 1            7                                6
    ## 2            4                               65
    ## 3            4                               30
    ## 4            7                               35
    ## 5            2                               26
    ## 6            5                               19
    ##   C1.1..2015년.총.정규직.수...여성
    ## 1                                8
    ## 2                               13
    ## 3                               18
    ## 4                               11
    ## 5                                3
    ## 6                                5

``` r
#변수명 바꾸기
data2<-rename(data2,work="X.쿼터..업종")
data2<-rename(data2,man="C1.1..2015년.총.정규직.수...남성")
data2<-rename(data2,woman="C1.1..2015년.총.정규직.수...여성")
head(data2)
```

    ##   work man woman
    ## 1    7   6     8
    ## 2    4  65    13
    ## 3    4  30    18
    ## 4    7  35    11
    ## 5    2  26     3
    ## 6    5  19     5

``` r
#변수타입확인 및 결측치 찾기 
class(data2$work)
```

    ## [1] "integer"

``` r
class(data2$man)
```

    ## [1] "integer"

``` r
class(data2$woman)
```

    ## [1] "character"

``` r
data2$woman<-as.integer(data2$woman)
```

    ## Warning: 강제형변환에 의해 생성된 NA 입니다

``` r
class(data2$woman)
```

    ## [1] "integer"

``` r
table(is.na(data2$work))
```

    ## 
    ## FALSE 
    ##  2049

``` r
table(is.na(data2$man))
```

    ## 
    ## FALSE 
    ##  2049

``` r
table(is.na(data2$woman))
```

    ## 
    ## FALSE  TRUE 
    ##  2048     1

``` r
data2<-data2%>%filter(!is.na(woman))
table(is.na(data2$woman))
```

    ## 
    ## FALSE 
    ##  2048

``` r
#변수 추가하기
data2$total<-data2$man+data2$woman
head(data2)
```

    ##   work man woman total
    ## 1    7   6     8    14
    ## 2    4  65    13    78
    ## 3    4  30    18    48
    ## 4    7  35    11    46
    ## 5    2  26     3    29
    ## 6    5  19     5    24

``` r
data2<-data2%>%mutate(man_ratio=man/total)
head(data2)
```

    ##   work man woman total man_ratio
    ## 1    7   6     8    14 0.4285714
    ## 2    4  65    13    78 0.8333333
    ## 3    4  30    18    48 0.6250000
    ## 4    7  35    11    46 0.7608696
    ## 5    2  26     3    29 0.8965517
    ## 6    5  19     5    24 0.7916667

``` r
#업종별 성비그래프 구하기
data3<-data2%>%group_by(work)%>%summarise(median_m=median(man_ratio))
data3
```

    ## # A tibble: 8 x 2
    ##    work  median_m
    ##   <int>     <dbl>
    ## 1     1 0.7333333
    ## 2     2 0.7647059
    ## 3     3 0.7857143
    ## 4     4 0.7500000
    ## 5     5 0.7777778
    ## 6     6 0.7549407
    ## 7     7 0.7572254
    ## 8     8 0.7550000

``` r
ggplot(data=data3,aes(x=work,y=median_m))+geom_point()
```

![](report_files/figure-markdown_github/unnamed-chunk-1-1.png)

**5.논의**
----------

-한계점: 업종 구분기준을 매우 광범위하게 두고 측정을 한 결과이기 때문에 실제로 대학생이 원하는 세부적인 분야의 성비와 다를 수 있다. -추후분석방향: 업종구분기준을 더 세세하게 나누어서 대분류,중분류,소분류에 따른 업종별 성비에 대해 분석을 해보는 것이 좋다고 생각한다.
