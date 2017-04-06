Homework 7 Answer Key
================
Melinda K. Higgins, PhD.
April 6, 2017

------------------------------------------------------------------------

Homework 7 - Assignment
-----------------------

Recall the NHANES dataset that we used in Lesson 10.

1.  In the dataset there is a discrete variable called SleepTrouble indicating whether each participant has trouble sleeping or not. You are going to build a set of classifiers for this dependent variable. You may use any (set of) independent variable(s) you like except for the variable callsed SleepHrsNight. For each of the model types (null model, logistic regression, decision tree, random forest, k-nearest neighbor) do the following:

    1A. Build the classifier.

    1B. Report its effectiveness on the NHANES dataset.

    1C. Make an appropriate visualization of this model.

    1D. Interpret the results. What have you learned about people's sleeping habits?

2.  Repeat problem 1 except now you are to use the quantitative variable called SleepHrsNight. The model types are as follows: null model, multiple regression, regression tree, random forest.

------------------------------------------------------------------------

Homework 7 - Answer Key
-----------------------

Given the instructions providing in the assignment (above), it is useful to review the code and examples provided in "lesson 10" in Dr. Hertzberg's Github repository at <https://github.com/vhertzb/Lesson10>.

### Load the NHANES dataset and review the variables included

``` r
# load the NHANES package with the NHANES dataset
library(NHANES)
```

    ## Warning: package 'NHANES' was built under R version 3.3.3

``` r
# create a data object for the NHANES dataset
dat1 <- NHANES

# list all of the variables included
names(dat1)
```

    ##  [1] "ID"               "SurveyYr"         "Gender"          
    ##  [4] "Age"              "AgeDecade"        "AgeMonths"       
    ##  [7] "Race1"            "Race3"            "Education"       
    ## [10] "MaritalStatus"    "HHIncome"         "HHIncomeMid"     
    ## [13] "Poverty"          "HomeRooms"        "HomeOwn"         
    ## [16] "Work"             "Weight"           "Length"          
    ## [19] "HeadCirc"         "Height"           "BMI"             
    ## [22] "BMICatUnder20yrs" "BMI_WHO"          "Pulse"           
    ## [25] "BPSysAve"         "BPDiaAve"         "BPSys1"          
    ## [28] "BPDia1"           "BPSys2"           "BPDia2"          
    ## [31] "BPSys3"           "BPDia3"           "Testosterone"    
    ## [34] "DirectChol"       "TotChol"          "UrineVol1"       
    ## [37] "UrineFlow1"       "UrineVol2"        "UrineFlow2"      
    ## [40] "Diabetes"         "DiabetesAge"      "HealthGen"       
    ## [43] "DaysPhysHlthBad"  "DaysMentHlthBad"  "LittleInterest"  
    ## [46] "Depressed"        "nPregnancies"     "nBabies"         
    ## [49] "Age1stBaby"       "SleepHrsNight"    "SleepTrouble"    
    ## [52] "PhysActive"       "PhysActiveDays"   "TVHrsDay"        
    ## [55] "CompHrsDay"       "TVHrsDayChild"    "CompHrsDayChild" 
    ## [58] "Alcohol12PlusYr"  "AlcoholDay"       "AlcoholYear"     
    ## [61] "SmokeNow"         "Smoke100"         "Smoke100n"       
    ## [64] "SmokeAge"         "Marijuana"        "AgeFirstMarij"   
    ## [67] "RegularMarij"     "AgeRegMarij"      "HardDrugs"       
    ## [70] "SexEver"          "SexAge"           "SexNumPartnLife" 
    ## [73] "SexNumPartYear"   "SameSex"          "SexOrientation"  
    ## [76] "PregnantNow"

``` r
# other packages needed
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
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.3.3

### Investigate the 2 Outcomes of Interest `SleepTrouble` and `SleepHrsNight`

``` r
class(dat1$SleepTrouble)
```

    ## [1] "factor"

``` r
summary(dat1$SleepTrouble)
```

    ##   No  Yes NA's 
    ## 5799 1973 2228

``` r
class(dat1$SleepHrsNight)
```

    ## [1] "integer"

``` r
summary(dat1$SleepHrsNight)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   2.000   6.000   7.000   6.928   8.000  12.000    2245

So, `SleepTrouble` is a "Factor" with 2 levels "No" and "Yes" with some missing data "NA"s.

But `SleepHrsNight` is a numeric variable (specifically an integer) with values ranging from 2 to 12 hours per night.

### Visualize `SleepTrouble` Outcome of Interest

NOTE: `ggplot()` codes based on examples at "Cookbook for R" website for the "R Graphics Cookbook" book at <http://www.cookbook-r.com/Graphs/>.

``` r
# Bar chart of Frequency/Counts for Skeep Trouble with NAs
dat1 %>%
  ggplot(aes(x=SleepTrouble, fill=SleepTrouble)) +
    geom_bar(stat="count", colour="black") +
    ggtitle("Frequency of Subjects with Sleep Trouble")
```

![](Hmwk7AnswerKey_files/figure-markdown_github/unnamed-chunk-3-1.png)

### You'll notice the following for `SleepTrouble`:

1.  It is a factor with 2 levels with values of "No and "Yes", which we can see by running `head(dat1$SleepTrouble)`. It is important to note this since some "classifier" procedures and functions in R assume that the "target" variable is coded 0 or 1.

``` r
head(dat1$SleepTrouble)
```

    ## [1] Yes  Yes  Yes  <NA> Yes  <NA>
    ## Levels: No Yes

1.  The majority of the subjects do NOT have Sleep Trouble - most are "No"s
2.  There are also a decent number of NAs which will be removed in the final analyses - or at least ignored. It will be important to know how the chosen classifier function or procedure handles missing NA data.

### Visualize `SleepHrsNight` Outcome of Interest

``` r
# Histogram overlaid with kernel density curve
dat1 %>% 
  ggplot(aes(x=SleepHrsNight)) + 
    geom_histogram(aes(y=..density..), 
                   binwidth=1,
                   colour="black", fill="yellow") +
    geom_density(alpha=.2, fill="blue", adjust=2) +
    ggtitle("Histogram Density Plot of Sleep Hours Per Night")
```

    ## Warning: Removed 2245 rows containing non-finite values (stat_bin).

    ## Warning: Removed 2245 rows containing non-finite values (stat_density).

![](Hmwk7AnswerKey_files/figure-markdown_github/unnamed-chunk-5-1.png)

### You'll notice the following for `SleepHrsNight`:

1.  There were still quite a few missing values as seen when we ran the `summary(dat1$SleepHrsNight)` above. It will be important to know how the chosen classifier function or procedure handles missing NA data.
2.  Since this is a numeric outcome, the classifiers chosen will be performing "regression" based models as opposed to "category probability" type models like logistic regression.
3.  That said, the distribution of `SleepHrsNight` is reasonably symmetric and approximately normally distributed which is good with no obvious outliers, even though the range of sleep times is wide from 2 to 12 hours, which is interesting.

PART 1 - Build "classifiers" for `SleepTrouble`
-----------------------------------------------

For each of the model types (null model, logistic regression, decision tree, random forest, k-nearest neighbor):

-   1A. Build the classifier.
-   1B. Report its effectiveness on the NHANES dataset.
-   1C. Make an appropriate visualization of this model.
-   1D. Interpret the results. What have you learned about people's sleeping habits?
