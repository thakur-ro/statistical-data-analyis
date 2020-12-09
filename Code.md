Applied\_Stats\_Project
================
Rohit Thakur
12/2/2020

Installing packages and dependencies

``` r
library(readr)
library(ggplot2)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v purrr   0.3.4     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(modelr)
```

ANOVA ANALYSIS: By Elizabeth Trauger

``` r
#Clear workspace
rm(list=ls())
#import dataset
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
dir<-"C:/Users/rthak/Downloads"
var<-"student-mat"
df<-read_csv(file.path(dir,paste0(var,".csv")))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character(),
    ##   age = col_double(),
    ##   Medu = col_double(),
    ##   Fedu = col_double(),
    ##   traveltime = col_double(),
    ##   studytime = col_double(),
    ##   failures = col_double(),
    ##   famrel = col_double(),
    ##   freetime = col_double(),
    ##   goout = col_double(),
    ##   Dalc = col_double(),
    ##   Walc = col_double(),
    ##   health = col_double(),
    ##   absences = col_double(),
    ##   G1 = col_double(),
    ##   G2 = col_double(),
    ##   G3 = col_double()
    ## )
    ## i Use `spec()` for the full column specifications.

``` r
df<- df %>%
  mutate(score = (G1+G2+G3)/3)

hist(df$score, freq = F,xlab='Math Score', main=NULL)
curve(dnorm(x, mean=mean(df$score), sd=sd(df$score)),col=2,add=TRUE)
```

![](Code_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> Anova tests for
the variables famsize, sex, higher, famsup and schoolsup

``` r
df$famsize<-as.factor(df$famsize)
summary(aov(score~famsize, data=df))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## famsize       1     37   36.70   2.697  0.101
    ## Residuals   393   5348   13.61

``` r
df$sex<-as.factor(df$sex)
summary(aov(score~sex, data=df))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## sex           1     55   55.06    4.06 0.0446 *
    ## Residuals   393   5329   13.56                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df$higher<-as.factor(df$higher)
summary(aov(score~higher, data=df))
```

    ##              Df Sum Sq Mean Sq F value   Pr(>F)    
    ## higher        1    193  193.32   14.64 0.000152 ***
    ## Residuals   393   5191   13.21                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
df$famsup<-as.factor(df$famsup)
summary(aov(score~famsup, data=df))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## famsup        1     20   20.40   1.495  0.222
    ## Residuals   393   5364   13.65

``` r
df$schoolsup<-as.factor(df$schoolsup)
summary(aov(score~schoolsup, data=df))
```

    ##              Df Sum Sq Mean Sq F value  Pr(>F)   
    ## schoolsup     1    102  102.01   7.589 0.00614 **
    ## Residuals   393   5282   13.44                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Descriptive statistics for the variables sex, higher, schoolsup, and
famsize

``` r
describeBy(df$score, df$sex)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: F
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 208 10.33 3.61     10   10.35 3.46 1.33 18.67 17.33 -0.04    -0.44 0.25
    ## ------------------------------------------------------------ 
    ## group: M
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 187 11.07 3.76     11    11.2 3.95 1.67 19.33 17.67 -0.26    -0.23 0.27

``` r
describeBy(df$score, df$higher)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: no
    ##    vars  n mean   sd median trimmed  mad  min max range skew kurtosis   se
    ## X1    1 20 7.65 3.42   7.67    7.73 3.95 1.67  13 11.33 -0.1    -1.13 0.77
    ## ------------------------------------------------------------ 
    ## group: yes
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 375 10.84 3.64     11   10.91 3.46 1.33 19.33    18 -0.14    -0.34 0.19

``` r
describeBy(df$score, df$schoolsup)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: no
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 344 10.88 3.83     11      11 3.95 1.33 19.33    18 -0.25     -0.4 0.21
    ## ------------------------------------------------------------ 
    ## group: yes
    ##    vars  n mean   sd median trimmed  mad  min   max range skew kurtosis   se
    ## X1    1 51 9.36 2.28      9    9.24 2.47 5.33 16.33    11 0.51     0.33 0.32

``` r
describeBy(df$score, df$famsup)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: no
    ##    vars   n  mean   sd median trimmed  mad min   max range  skew kurtosis  se
    ## X1    1 153 10.97 3.75     11   11.06 3.46   2 19.33 17.33 -0.16    -0.37 0.3
    ## ------------------------------------------------------------ 
    ## group: yes
    ##    vars   n mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 242 10.5 3.66  10.33   10.56 3.46 1.33 18.33    17 -0.13    -0.37 0.24

``` r
describeBy(df$score, df$famsize)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: GT3
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 281 10.49 3.76  10.33   10.55 3.95 1.33 19.33    18 -0.14    -0.44 0.22
    ## ------------------------------------------------------------ 
    ## group: LE3
    ##    vars   n  mean   sd median trimmed  mad min   max range  skew kurtosis   se
    ## X1    1 114 11.16 3.52     11   11.18 3.21   2 18.67 16.67 -0.08    -0.23 0.33

ANOVA ANALYSIS and PAIRWISE COMPARISON STUDY Continued: By Regan Kelly

``` r
#Address and romantic are categorical (factors) variables - grouping variables
df$address<-as.factor(df$address)
df$romantic<-as.factor(df$romantic)

#For address variable, fit by a linear model aov(), then produce the ANOVA table
fit1=aov(score~address, data=df)
#One-way ANOVA of average math score over address
anova(fit1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##            Df Sum Sq Mean Sq F value  Pr(>F)  
    ## address     1   62.0  61.990  4.5772 0.03302 *
    ## Residuals 393 5322.5  13.543                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Summary statistics by address
describeBy(df$score,df$address)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: R
    ##    vars  n mean   sd median trimmed  mad min   max range skew kurtosis  se
    ## X1    1 88 9.94 3.71   9.83    9.88 3.71   2 18.67 16.67 0.09    -0.38 0.4
    ## ------------------------------------------------------------ 
    ## group: U
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 307 10.89 3.67     11   10.99 3.95 1.33 19.33    18 -0.21    -0.31 0.21

``` r
#For romantic variable, fit by a linear model aov(), then produce the ANOVA table
fit2=aov(score~romantic, data=df)
#One-way ANOVA of average math score over romantic
anova(fit2)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##            Df Sum Sq Mean Sq F value  Pr(>F)  
    ## romantic    1   56.8  56.826  4.1918 0.04128 *
    ## Residuals 393 5327.7  13.556                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Summary statistics by romantic status
describeBy(df$score,df$romantic)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: no
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 263 10.95 3.68  10.67   10.95 3.46 1.67 19.33 17.67 -0.01    -0.39 0.23
    ## ------------------------------------------------------------ 
    ## group: yes
    ##    vars   n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
    ## X1    1 132 10.14 3.69   10.5   10.33 3.71 1.33 17.67 16.33 -0.39    -0.58 0.32

``` r
#Study time is categorical (factor) variable - grouping variable
df$studytime<-as.factor(df$studytime)

#For studytime variable, fit by a linear model aov(), then produce the ANOVA table
fit3=aov(score~studytime, data=df)

#One-way ANOVA of average math score over studytime (can do with either command)
anova(fit3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##            Df Sum Sq Mean Sq F value  Pr(>F)  
    ## studytime   3  121.2  40.399  3.0012 0.03047 *
    ## Residuals 391 5263.3  13.461                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Multiple pairwise comparison
#Tukey's HSD adjustment for multiple comparison
TukeyHSD(fit3, conf.level=0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = score ~ studytime, data = df)
    ## 
    ## $studytime
    ##           diff         lwr      upr     p adj
    ## 2-1 0.18879269 -0.95401520 1.331601 0.9739685
    ## 3-1 1.39731380 -0.09669204 2.891320 0.0762642
    ## 4-1 1.47442681 -0.56820267 3.517056 0.2460810
    ## 3-2 1.20852111 -0.14469688 2.561739 0.0987970
    ## 4-2 1.28563412 -0.65639592 3.227664 0.3208229
    ## 4-3 0.07711301 -2.09026426 2.244490 0.9997225

``` r
#Check ANOVA assumptions
#Check assumption of equal variance
#Plot residuals versus predicted values
plot(fit1, which=1)
```

![](Code_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(fit2, which=1)
```

![](Code_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
plot(fit3, which=1)
```

![](Code_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

Regression Analysis: By Rohit Thakur

``` r
#removing NAs if any
math_scores<-as_tibble(na.omit(df))
print(math_scores)
```

    ## # A tibble: 395 x 34
    ##    school sex     age address famsize Pstatus  Medu  Fedu Mjob  Fjob  reason
    ##    <chr>  <fct> <dbl> <fct>   <fct>   <chr>   <dbl> <dbl> <chr> <chr> <chr> 
    ##  1 GP     F        18 U       GT3     A           4     4 at_h~ teac~ course
    ##  2 GP     F        17 U       GT3     T           1     1 at_h~ other course
    ##  3 GP     F        15 U       LE3     T           1     1 at_h~ other other 
    ##  4 GP     F        15 U       GT3     T           4     2 heal~ serv~ home  
    ##  5 GP     F        16 U       GT3     T           3     3 other other home  
    ##  6 GP     M        16 U       LE3     T           4     3 serv~ other reput~
    ##  7 GP     M        16 U       LE3     T           2     2 other other home  
    ##  8 GP     F        17 U       GT3     A           4     4 other teac~ home  
    ##  9 GP     M        15 U       LE3     A           3     2 serv~ other home  
    ## 10 GP     M        15 U       GT3     T           3     4 other other home  
    ## # ... with 385 more rows, and 23 more variables: guardian <chr>,
    ## #   traveltime <dbl>, studytime <fct>, failures <dbl>, schoolsup <fct>,
    ## #   famsup <fct>, paid <chr>, activities <chr>, nursery <chr>, higher <fct>,
    ## #   internet <chr>, romantic <fct>, famrel <dbl>, freetime <dbl>, goout <dbl>,
    ## #   Dalc <dbl>, Walc <dbl>, health <dbl>, absences <dbl>, G1 <dbl>, G2 <dbl>,
    ## #   G3 <dbl>, score <dbl>

Creating target variable as average math score

``` r
drops <- c("G1","G2","G3")
#removing G1,G2 and G3 since we added score as average of those three
df<-df[ , !(names(df) %in% drops)]
```

Visualizations to identify interesting relationships in datasets

``` r
#Are number of absences related to health?

#Number of students in different health categories
math_scores%>%
  ggplot(aes(x=health))+
  geom_bar()+
  labs(x="Health Categories(5-healthiest)", y="count",
       title="Majority of students have good health")+
  theme_minimal()
```

![](Code_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#Number of absences for each school
math_scores%>%
  ggplot(aes(x=absences,fill=as.factor(school)))+
  geom_density(alpha=0.5)+
  labs(x="Number of Absences",
       title="Distribution of number of absences in each school within each health group")+
  facet_wrap(~health)+
  guides(fill=guide_legend(title="schools"))+
  theme_light()
```

![](Code_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> Which school
has higher average math score

``` r
math_scores%>%
  ggplot(aes(x=score,y=school))+
  geom_jitter()+
  geom_boxplot()+
  labs(x="Average Math Score",y="School",
       title="School GP has higher median Math Score")+
  theme_minimal()
```

![](Code_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> How study time
and romantic relationship affect math score

``` r
df%>%
  select(studytime,romantic,score)%>%
  ggplot(aes(x=studytime,y=score,group=studytime))+
  geom_jitter()+
  geom_boxplot()+
  labs(x="Study time",y="Average Math Score",
       title="Math score is affected by romantic relationship")+
  facet_wrap(~romantic)+
  theme_light()
```

![](Code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> How extra
curricular activities affect math scores

``` r
df%>%
  select(activities,score)%>%
  ggplot(aes(x=score))+
  geom_histogram()+
  facet_wrap(~activities)+
  labs(x="Average Math Score",y="Count",
       title="Distribution of Average math score is\nright skewed for students performing activities")+
  theme_minimal()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> Stepwise model
selection based on AIC score Multiple linear regression

``` r
#multiple linear regression
model_fit<-lm(score~.,data=df)
step(model_fit)
```

    ## Start:  AIC=965.67
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
    ##     failures + schoolsup + famsup + paid + activities + nursery + 
    ##     higher + internet + romantic + famrel + freetime + goout + 
    ##     Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - reason      3     16.96 3698.1 961.49
    ## - guardian    2     10.28 3691.4 962.78
    ## - Fedu        1      0.00 3681.1 963.67
    ## - famrel      1      0.06 3681.2 963.68
    ## - nursery     1      0.14 3681.3 963.69
    ## - activities  1      1.04 3682.2 963.79
    ## - Pstatus     1      1.16 3682.3 963.80
    ## - Dalc        1      2.25 3683.4 963.92
    ## - Walc        1      2.30 3683.4 963.92
    ## - school      1      2.93 3684.0 963.99
    ## - paid        1      3.37 3684.5 964.03
    ## - internet    1      5.38 3686.5 964.25
    ## - traveltime  1      5.97 3687.1 964.31
    ## - address     1      9.84 3691.0 964.73
    ## - Medu        1     11.85 3693.0 964.94
    ## - Fjob        4     72.54 3753.7 965.38
    ## - absences    1     16.11 3697.2 965.40
    ## - higher      1     17.97 3699.1 965.60
    ## - age         1     18.49 3699.6 965.65
    ## <none>                    3681.1 965.67
    ## - freetime    1     19.02 3700.1 965.71
    ## - health      1     28.01 3709.1 966.67
    ## - famsize     1     30.70 3711.8 966.95
    ## - romantic    1     39.38 3720.5 967.88
    ## - famsup      1     55.71 3736.8 969.61
    ## - studytime   3     94.30 3775.4 969.67
    ## - sex         1     77.74 3758.9 971.93
    ## - goout       1     79.50 3760.6 972.11
    ## - Mjob        4    138.17 3819.3 972.23
    ## - schoolsup   1     96.25 3777.4 973.87
    ## - failures    1    333.57 4014.7 997.94
    ## 
    ## Step:  AIC=961.49
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Fedu + Mjob + Fjob + guardian + traveltime + studytime + 
    ##     failures + schoolsup + famsup + paid + activities + nursery + 
    ##     higher + internet + romantic + famrel + freetime + goout + 
    ##     Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - guardian    2     11.57 3709.6 958.72
    ## - Fedu        1      0.01 3698.1 959.49
    ## - nursery     1      0.05 3698.1 959.50
    ## - famrel      1      0.10 3698.2 959.50
    ## - activities  1      0.28 3698.4 959.52
    ## - Pstatus     1      0.94 3699.0 959.59
    ## - school      1      2.18 3700.3 959.72
    ## - Dalc        1      2.55 3700.6 959.76
    ## - Walc        1      2.83 3700.9 959.79
    ## - paid        1      4.61 3702.7 959.98
    ## - internet    1      5.58 3703.7 960.09
    ## - traveltime  1      6.82 3704.9 960.22
    ## - address     1      7.31 3705.4 960.27
    ## - Medu        1     13.65 3711.7 960.94
    ## - higher      1     17.86 3715.9 961.39
    ## - age         1     18.08 3716.2 961.42
    ## <none>                    3698.1 961.49
    ## - freetime    1     18.98 3717.1 961.51
    ## - absences    1     19.38 3717.5 961.55
    ## - Fjob        4     77.05 3775.1 961.64
    ## - famsize     1     30.54 3728.6 962.74
    ## - health      1     36.29 3734.4 963.35
    ## - romantic    1     40.18 3738.3 963.76
    ## - famsup      1     54.63 3752.7 965.28
    ## - studytime   3     95.24 3793.3 965.53
    ## - sex         1     75.29 3773.4 967.45
    ## - goout       1     85.07 3783.1 968.47
    ## - Mjob        4    153.12 3851.2 969.52
    ## - schoolsup   1     98.59 3796.7 969.88
    ## - failures    1    341.00 4039.1 994.33
    ## 
    ## Step:  AIC=958.72
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Fedu + Mjob + Fjob + traveltime + studytime + failures + 
    ##     schoolsup + famsup + paid + activities + nursery + higher + 
    ##     internet + romantic + famrel + freetime + goout + Dalc + 
    ##     Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Fedu        1      0.08 3709.7 956.73
    ## - famrel      1      0.13 3709.8 956.74
    ## - activities  1      0.32 3710.0 956.76
    ## - nursery     1      0.66 3710.3 956.79
    ## - Pstatus     1      1.36 3711.0 956.87
    ## - school      1      1.46 3711.1 956.88
    ## - Walc        1      1.78 3711.4 956.91
    ## - Dalc        1      2.00 3711.6 956.94
    ## - paid        1      5.02 3714.7 957.26
    ## - internet    1      5.11 3714.8 957.27
    ## - traveltime  1      5.12 3714.8 957.27
    ## - address     1     10.26 3719.9 957.82
    ## - age         1     10.80 3720.5 957.87
    ## - Medu        1     11.51 3721.2 957.95
    ## - Fjob        4     74.57 3784.2 958.58
    ## <none>                    3709.6 958.72
    ## - absences    1     22.04 3731.7 959.06
    ## - higher      1     22.37 3732.0 959.10
    ## - freetime    1     22.56 3732.2 959.12
    ## - famsize     1     29.37 3739.0 959.84
    ## - romantic    1     38.14 3747.8 960.76
    ## - health      1     38.26 3747.9 960.78
    ## - famsup      1     53.11 3762.8 962.34
    ## - studytime   3    100.20 3809.9 963.25
    ## - sex         1     76.86 3786.5 964.82
    ## - goout       1     93.68 3803.3 966.58
    ## - schoolsup   1     97.24 3806.9 966.94
    ## - Mjob        4    156.45 3866.1 967.04
    ## - failures    1    331.20 4040.8 990.50
    ## 
    ## Step:  AIC=956.73
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Mjob + Fjob + traveltime + studytime + failures + schoolsup + 
    ##     famsup + paid + activities + nursery + higher + internet + 
    ##     romantic + famrel + freetime + goout + Dalc + Walc + health + 
    ##     absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - famrel      1      0.13 3709.9 954.75
    ## - activities  1      0.30 3710.0 954.76
    ## - nursery     1      0.64 3710.4 954.80
    ## - Pstatus     1      1.38 3711.1 954.88
    ## - school      1      1.49 3711.2 954.89
    ## - Walc        1      1.87 3711.6 954.93
    ## - Dalc        1      2.06 3711.8 954.95
    ## - paid        1      4.96 3714.7 955.26
    ## - internet    1      5.23 3715.0 955.29
    ## - traveltime  1      5.29 3715.0 955.30
    ## - address     1     10.20 3719.9 955.82
    ## - age         1     10.81 3720.5 955.88
    ## - Medu        1     16.86 3726.6 956.52
    ## <none>                    3709.7 956.73
    ## - absences    1     21.95 3731.7 957.06
    ## - freetime    1     22.50 3732.2 957.12
    ## - higher      1     22.74 3732.5 957.15
    ## - Fjob        4     83.47 3793.2 957.52
    ## - famsize     1     29.34 3739.1 957.84
    ## - health      1     38.18 3747.9 958.78
    ## - romantic    1     38.21 3747.9 958.78
    ## - famsup      1     53.28 3763.0 960.36
    ## - studytime   3    100.80 3810.5 961.32
    ## - sex         1     76.91 3786.6 962.84
    ## - goout       1     93.61 3803.3 964.58
    ## - schoolsup   1     97.16 3806.9 964.95
    ## - Mjob        4    156.38 3866.1 965.04
    ## - failures    1    338.99 4048.7 989.27
    ## 
    ## Step:  AIC=954.75
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Mjob + Fjob + traveltime + studytime + failures + schoolsup + 
    ##     famsup + paid + activities + nursery + higher + internet + 
    ##     romantic + freetime + goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - activities  1      0.31 3710.2 952.78
    ## - nursery     1      0.65 3710.5 952.82
    ## - Pstatus     1      1.38 3711.2 952.89
    ## - school      1      1.42 3711.3 952.90
    ## - Walc        1      1.77 3711.6 952.93
    ## - Dalc        1      2.10 3712.0 952.97
    ## - paid        1      5.03 3714.9 953.28
    ## - traveltime  1      5.25 3715.1 953.30
    ## - internet    1      5.29 3715.2 953.31
    ## - address     1     10.15 3720.0 953.83
    ## - age         1     10.69 3720.5 953.88
    ## - Medu        1     17.01 3726.9 954.55
    ## <none>                    3709.9 954.75
    ## - absences    1     21.87 3731.7 955.07
    ## - higher      1     22.85 3732.7 955.17
    ## - freetime    1     23.57 3733.4 955.25
    ## - Fjob        4     83.54 3793.4 955.54
    ## - famsize     1     29.41 3739.3 955.87
    ## - health      1     38.18 3748.0 956.79
    ## - romantic    1     38.80 3748.7 956.86
    ## - famsup      1     53.47 3763.3 958.40
    ## - studytime   3    101.31 3811.2 959.39
    ## - sex         1     78.00 3787.9 960.97
    ## - goout       1     93.79 3803.6 962.61
    ## - schoolsup   1     97.04 3806.9 962.95
    ## - Mjob        4    156.33 3866.2 963.05
    ## - failures    1    341.38 4051.2 987.52
    ## 
    ## Step:  AIC=952.78
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Mjob + Fjob + traveltime + studytime + failures + schoolsup + 
    ##     famsup + paid + nursery + higher + internet + romantic + 
    ##     freetime + goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - nursery     1      0.61 3710.8 950.84
    ## - Pstatus     1      1.53 3711.7 950.94
    ## - school      1      1.60 3711.8 950.95
    ## - Walc        1      1.80 3712.0 950.97
    ## - Dalc        1      1.99 3712.2 950.99
    ## - paid        1      5.13 3715.3 951.33
    ## - internet    1      5.30 3715.5 951.34
    ## - traveltime  1      5.34 3715.5 951.35
    ## - age         1     10.57 3720.7 951.90
    ## - address     1     10.67 3720.8 951.91
    ## - Medu        1     16.81 3727.0 952.56
    ## <none>                    3710.2 952.78
    ## - absences    1     21.81 3732.0 953.09
    ## - higher      1     22.55 3732.7 953.17
    ## - freetime    1     23.29 3733.5 953.25
    ## - Fjob        4     84.13 3794.3 953.64
    ## - famsize     1     29.29 3739.5 953.89
    ## - health      1     38.27 3748.4 954.83
    ## - romantic    1     39.36 3749.5 954.95
    ## - famsup      1     53.22 3763.4 956.40
    ## - studytime   3    101.00 3811.2 957.39
    ## - sex         1     77.97 3788.1 958.99
    ## - goout       1     94.86 3805.0 960.75
    ## - schoolsup   1     98.06 3808.2 961.08
    ## - Mjob        4    156.39 3866.6 961.09
    ## - failures    1    341.07 4051.2 985.52
    ## 
    ## Step:  AIC=950.84
    ## score ~ school + sex + age + address + famsize + Pstatus + Medu + 
    ##     Mjob + Fjob + traveltime + studytime + failures + schoolsup + 
    ##     famsup + paid + higher + internet + romantic + freetime + 
    ##     goout + Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Pstatus     1      1.44 3712.2 949.00
    ## - school      1      1.71 3712.5 949.03
    ## - Dalc        1      1.88 3712.7 949.04
    ## - Walc        1      1.91 3712.7 949.05
    ## - paid        1      4.87 3715.6 949.36
    ## - traveltime  1      5.50 3716.3 949.43
    ## - internet    1      5.56 3716.3 949.43
    ## - age         1     10.42 3721.2 949.95
    ## - address     1     10.65 3721.4 949.98
    ## - Medu        1     16.34 3727.1 950.58
    ## <none>                    3710.8 950.84
    ## - absences    1     21.66 3732.4 951.14
    ## - higher      1     22.66 3733.4 951.25
    ## - freetime    1     23.41 3734.2 951.33
    ## - Fjob        4     83.84 3794.6 951.67
    ## - famsize     1     28.73 3739.5 951.89
    ## - health      1     38.27 3749.0 952.90
    ## - romantic    1     39.64 3750.4 953.04
    ## - famsup      1     53.13 3763.9 954.46
    ## - studytime   3    100.51 3811.3 955.40
    ## - sex         1     77.54 3788.3 957.01
    ## - goout       1     95.97 3806.7 958.93
    ## - Mjob        4    156.16 3866.9 959.13
    ## - schoolsup   1     98.87 3809.7 959.23
    ## - failures    1    340.50 4051.3 983.52
    ## 
    ## Step:  AIC=949
    ## score ~ school + sex + age + address + famsize + Medu + Mjob + 
    ##     Fjob + traveltime + studytime + failures + schoolsup + famsup + 
    ##     paid + higher + internet + romantic + freetime + goout + 
    ##     Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - school      1      1.66 3713.9 947.17
    ## - Dalc        1      1.78 3714.0 947.19
    ## - Walc        1      1.88 3714.1 947.20
    ## - paid        1      4.65 3716.9 947.49
    ## - internet    1      5.09 3717.3 947.54
    ## - traveltime  1      5.55 3717.8 947.59
    ## - age         1     10.72 3722.9 948.14
    ## - address     1     10.79 3723.0 948.14
    ## - Medu        1     17.97 3730.2 948.90
    ## <none>                    3712.2 949.00
    ## - higher      1     22.95 3735.2 949.43
    ## - freetime    1     23.06 3735.3 949.44
    ## - absences    1     23.12 3735.3 949.45
    ## - Fjob        4     85.04 3797.3 949.94
    ## - famsize     1     31.29 3743.5 950.31
    ## - health      1     38.57 3750.8 951.08
    ## - romantic    1     39.39 3751.6 951.17
    ## - famsup      1     53.66 3765.9 952.67
    ## - studytime   3     99.77 3812.0 953.47
    ## - sex         1     77.10 3789.3 955.12
    ## - goout       1     96.24 3808.5 957.11
    ## - schoolsup   1     98.75 3811.0 957.37
    ## - Mjob        4    157.75 3870.0 957.44
    ## - failures    1    339.41 4051.6 981.56
    ## 
    ## Step:  AIC=947.17
    ## score ~ sex + age + address + famsize + Medu + Mjob + Fjob + 
    ##     traveltime + studytime + failures + schoolsup + famsup + 
    ##     paid + higher + internet + romantic + freetime + goout + 
    ##     Dalc + Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Dalc        1      1.62 3715.5 945.35
    ## - Walc        1      1.79 3715.7 945.36
    ## - traveltime  1      4.68 3718.6 945.67
    ## - paid        1      4.90 3718.8 945.70
    ## - internet    1      4.91 3718.8 945.70
    ## - age         1      9.10 3723.0 946.14
    ## - address     1      9.54 3723.4 946.19
    ## - Medu        1     17.99 3731.9 947.08
    ## <none>                    3713.9 947.17
    ## - absences    1     21.68 3735.6 947.47
    ## - freetime    1     23.82 3737.7 947.70
    ## - higher      1     24.05 3737.9 947.72
    ## - Fjob        4     85.32 3799.2 948.15
    ## - famsize     1     32.09 3746.0 948.57
    ## - romantic    1     39.01 3752.9 949.30
    ## - health      1     39.06 3752.9 949.31
    ## - famsup      1     56.54 3770.4 951.14
    ## - studytime   3     98.17 3812.0 951.48
    ## - sex         1     75.88 3789.8 953.16
    ## - goout       1     97.56 3811.4 955.42
    ## - Mjob        4    156.93 3870.8 955.52
    ## - schoolsup   1     99.94 3813.8 955.66
    ## - failures    1    342.78 4056.7 980.05
    ## 
    ## Step:  AIC=945.35
    ## score ~ sex + age + address + famsize + Medu + Mjob + Fjob + 
    ##     traveltime + studytime + failures + schoolsup + famsup + 
    ##     paid + higher + internet + romantic + freetime + goout + 
    ##     Walc + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - Walc        1      0.56 3716.1 943.41
    ## - paid        1      4.53 3720.0 943.83
    ## - internet    1      4.72 3720.2 943.85
    ## - traveltime  1      5.11 3720.6 943.89
    ## - address     1      9.74 3725.2 944.38
    ## - age         1      9.99 3725.5 944.41
    ## - Medu        1     17.06 3732.6 945.15
    ## <none>                    3715.5 945.35
    ## - absences    1     21.60 3737.1 945.64
    ## - freetime    1     22.54 3738.0 945.73
    ## - higher      1     23.57 3739.1 945.84
    ## - Fjob        4     83.77 3799.3 946.15
    ## - famsize     1     31.43 3746.9 946.67
    ## - romantic    1     39.13 3754.6 947.48
    ## - health      1     39.58 3755.1 947.53
    ## - famsup      1     57.10 3772.6 949.37
    ## - studytime   3     99.27 3814.8 949.76
    ## - sex         1     74.49 3790.0 951.19
    ## - goout       1     96.57 3812.1 953.48
    ## - Mjob        4    159.95 3875.4 953.99
    ## - schoolsup   1    102.00 3817.5 954.04
    ## - failures    1    346.08 4061.6 978.52
    ## 
    ## Step:  AIC=943.41
    ## score ~ sex + age + address + famsize + Medu + Mjob + Fjob + 
    ##     traveltime + studytime + failures + schoolsup + famsup + 
    ##     paid + higher + internet + romantic + freetime + goout + 
    ##     health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - internet    1      4.71 3720.8 941.91
    ## - traveltime  1      4.96 3721.0 941.93
    ## - paid        1      5.19 3721.2 941.96
    ## - address     1      9.36 3725.4 942.40
    ## - age         1      9.92 3726.0 942.46
    ## - Medu        1     16.87 3732.9 943.19
    ## <none>                    3716.1 943.41
    ## - freetime    1     22.46 3738.5 943.79
    ## - absences    1     23.05 3739.1 943.85
    ## - higher      1     23.58 3739.6 943.90
    ## - Fjob        4     83.21 3799.3 944.15
    ## - famsize     1     32.33 3748.4 944.83
    ## - health      1     39.07 3755.1 945.54
    ## - romantic    1     39.22 3755.3 945.55
    ## - famsup      1     57.69 3773.7 947.49
    ## - studytime   3     99.09 3815.1 947.80
    ## - sex         1     80.44 3796.5 949.86
    ## - Mjob        4    160.05 3876.1 952.06
    ## - schoolsup   1    102.26 3818.3 952.13
    ## - goout       1    108.76 3824.8 952.80
    ## - failures    1    345.52 4061.6 976.52
    ## 
    ## Step:  AIC=941.91
    ## score ~ sex + age + address + famsize + Medu + Mjob + Fjob + 
    ##     traveltime + studytime + failures + schoolsup + famsup + 
    ##     paid + higher + romantic + freetime + goout + health + absences
    ## 
    ##              Df Sum of Sq    RSS    AIC
    ## - traveltime  1      4.98 3725.7 940.43
    ## - paid        1      6.25 3727.0 940.57
    ## - age         1     11.90 3732.7 941.17
    ## - address     1     12.05 3732.8 941.18
    ## - Medu        1     16.60 3737.4 941.66
    ## <none>                    3720.8 941.91
    ## - higher      1     22.62 3743.4 942.30
    ## - freetime    1     22.67 3743.4 942.31
    ## - Fjob        4     80.92 3801.7 942.40
    ## - absences    1     26.04 3746.8 942.66
    ## - famsize     1     31.98 3752.7 943.29
    ## - romantic    1     36.95 3757.7 943.81
    ## - health      1     42.74 3763.5 944.42
    ## - famsup      1     56.02 3776.8 945.81
    ## - studytime   3    106.72 3827.5 947.08
    ## - sex         1     83.61 3804.4 948.68
    ## - schoolsup   1    102.07 3822.8 950.60
    ## - Mjob        4    160.87 3881.6 950.62
    ## - goout       1    106.09 3826.9 951.01
    ## - failures    1    347.01 4067.8 975.13
    ## 
    ## Step:  AIC=940.43
    ## score ~ sex + age + address + famsize + Medu + Mjob + Fjob + 
    ##     studytime + failures + schoolsup + famsup + paid + higher + 
    ##     romantic + freetime + goout + health + absences
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - paid       1      6.46 3732.2 939.12
    ## - age        1     11.93 3737.7 939.70
    ## - Medu       1     17.97 3743.7 940.33
    ## - address    1     18.84 3744.6 940.43
    ## <none>                   3725.7 940.43
    ## - higher     1     22.87 3748.6 940.85
    ## - freetime   1     23.51 3749.3 940.92
    ## - Fjob       4     80.94 3806.7 940.92
    ## - absences   1     26.87 3752.6 941.27
    ## - famsize    1     29.92 3755.7 941.59
    ## - romantic   1     38.38 3764.1 942.48
    ## - health     1     42.44 3768.2 942.91
    ## - famsup     1     58.71 3784.5 944.61
    ## - studytime  3    110.95 3836.7 946.02
    ## - sex        1     82.75 3808.5 947.11
    ## - schoolsup  1    102.10 3827.9 949.11
    ## - Mjob       4    163.70 3889.4 949.42
    ## - goout      1    109.08 3834.8 949.83
    ## - failures   1    349.23 4075.0 973.83
    ## 
    ## Step:  AIC=939.12
    ## score ~ sex + age + address + famsize + Medu + Mjob + Fjob + 
    ##     studytime + failures + schoolsup + famsup + higher + romantic + 
    ##     freetime + goout + health + absences
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## - age        1     11.21 3743.4 938.30
    ## - Medu       1     18.01 3750.2 939.02
    ## <none>                   3732.2 939.12
    ## - address    1     19.24 3751.4 939.15
    ## - Fjob       4     76.83 3809.0 939.17
    ## - freetime   1     22.37 3754.6 939.48
    ## - higher     1     25.64 3757.9 939.82
    ## - absences   1     26.69 3758.9 939.93
    ## - famsize    1     29.67 3761.9 940.25
    ## - romantic   1     37.78 3770.0 941.10
    ## - health     1     44.79 3777.0 941.83
    ## - famsup     1     52.63 3784.8 942.65
    ## - studytime  3    113.12 3845.3 944.91
    ## - sex        1     80.73 3812.9 945.57
    ## - Mjob       4    162.69 3894.9 947.97
    ## - schoolsup  1    103.98 3836.2 947.97
    ## - goout      1    107.13 3839.3 948.30
    ## - failures   1    365.97 4098.2 974.07
    ## 
    ## Step:  AIC=938.3
    ## score ~ sex + address + famsize + Medu + Mjob + Fjob + studytime + 
    ##     failures + schoolsup + famsup + higher + romantic + freetime + 
    ##     goout + health + absences
    ## 
    ##             Df Sum of Sq    RSS    AIC
    ## <none>                   3743.4 938.30
    ## - Fjob       4     77.06 3820.5 938.35
    ## - Medu       1     21.14 3764.6 938.53
    ## - absences   1     21.78 3765.2 938.59
    ## - freetime   1     22.98 3766.4 938.72
    ## - address    1     23.37 3766.8 938.76
    ## - famsize    1     28.69 3772.1 939.32
    ## - higher     1     30.53 3774.0 939.51
    ## - health     1     41.49 3784.9 940.66
    ## - romantic   1     41.70 3785.1 940.68
    ## - famsup     1     48.76 3792.2 941.41
    ## - studytime  3    107.64 3851.1 943.50
    ## - sex        1     83.06 3826.5 944.97
    ## - schoolsup  1     93.48 3836.9 946.05
    ## - Mjob       4    164.75 3908.2 947.32
    ## - goout      1    117.90 3861.3 948.55
    ## - failures   1    396.74 4140.2 976.09

    ## 
    ## Call:
    ## lm(formula = score ~ sex + address + famsize + Medu + Mjob + 
    ##     Fjob + studytime + failures + schoolsup + famsup + higher + 
    ##     romantic + freetime + goout + health + absences, data = df)
    ## 
    ## Coefficients:
    ##  (Intercept)          sexM      addressU    famsizeLE3          Medu  
    ##      9.87869       1.06741       0.60704       0.62342       0.30146  
    ##   Mjobhealth     Mjobother  Mjobservices   Mjobteacher    Fjobhealth  
    ##      1.39509      -0.27335       0.76746      -0.87310       0.27760  
    ##    Fjobother  Fjobservices   Fjobteacher    studytime2    studytime3  
    ##     -0.48618      -0.24924       1.30366       0.30654       1.60381  
    ##   studytime4      failures  schoolsupyes     famsupyes     higheryes  
    ##      1.30916      -1.50044      -1.51296      -0.76708       1.38656  
    ##  romanticyes      freetime         goout        health      absences  
    ##     -0.72899       0.26517      -0.52432      -0.24393       0.03067

Based on ANOVA results we finetune our model given by step function to
include following features

``` r
#Final Model
model_fit1<-lm(formula = score ~ sex + Medu + studytime + 
    failures + schoolsup + famsup + goout , data = df)
summary(model_fit1)
```

    ## 
    ## Call:
    ## lm(formula = score ~ sex + Medu + studytime + failures + schoolsup + 
    ##     famsup + goout, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.6630 -2.2614  0.2028  2.2173  9.0855 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   10.8475     0.7429  14.601  < 2e-16 ***
    ## sexM           0.8981     0.3637   2.469  0.01398 *  
    ## Medu           0.5136     0.1609   3.192  0.00153 ** 
    ## studytime2     0.2868     0.4185   0.685  0.49356    
    ## studytime3     1.2955     0.5605   2.311  0.02134 *  
    ## studytime4     1.1918     0.7350   1.621  0.10575    
    ## failures      -1.5713     0.2353  -6.677 8.54e-11 ***
    ## schoolsupyes  -1.2173     0.5033  -2.419  0.01604 *  
    ## famsupyes     -0.7066     0.3548  -1.992  0.04711 *  
    ## goout         -0.4274     0.1518  -2.815  0.00513 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.292 on 385 degrees of freedom
    ## Multiple R-squared:  0.225,  Adjusted R-squared:  0.2069 
    ## F-statistic: 12.42 on 9 and 385 DF,  p-value: < 2.2e-16

Checking if any assumptions are violated

``` r
plot(model_fit1)
```

![](Code_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](Code_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->![](Code_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->![](Code_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->
