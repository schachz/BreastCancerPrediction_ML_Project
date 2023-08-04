Breast Cancer Prediction Project
================
### Zach Schachter
### Revised July 20, 2023

``` r
knitr::opts_chunk$set(echo = TRUE)
options(stringsAsFactors = F) ## to prevent numeric values as factors, if you use read.csv

library(rstudioapi)
library(ggplot2)
library(dplyr)
```



``` r
library(tidyr)
library(tibble)
library(stringr)
```

## Data descriptions:

#### Reference

- Patrício, Miguel, José Pereira, Joana Crisóstomo, Paulo Matafome,
  Manuel Gomes, Raquel Seiça, and Francisco Caramelo. “Using Resistin,
  glucose, age and BMI to predict the presence of breast cancer.” BMC
  cancer 18, no. 1 (2018): 29.

- <https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra>

#### Dataset characteristics

There are 9 predictors, all quantitative, and a binary dependent
variable, indicating the presence or absence of breast cancer. The
predictors are anthropometric data and parameters which can be gathered
in routine blood analysis. Prediction models based on these predictors,
if accurate, can potentially be used as a biomarker of breast cancer.

Below are the details of the variables

- Quantitative Attributes:
  1.  Age (years)
  2.  BMI (kg/m2)
  3.  Glucose (mg/dL)
  4.  Insulin (µU/mL)
  5.  HOMA
  6.  Leptin (ng/mL)
  7.  Adiponectin (µg/mL)
  8.  Resistin (ng/mL)
  9.  MCP-1(pg/dL)
- Labels:
  1.  1=Healthy controls
  2.  2=Patients

### Loading in data and previewing it in a matrix.

``` r
df <- read.table("C:/Users/schac/Downloads/cancer.txt", header = T, sep = ",")
df[1:3, 1:4]
```

    ##   Age      BMI Glucose Insulin
    ## 1  48 23.50000      70   2.707
    ## 2  83 20.69049      92   3.115
    ## 3  82 23.12467      91   4.498

### Part 1.

How many people are with age \<=60 and Glucose \>= 100? Among them, what
is the percentage of Healthy controls?

``` r
df2 <- df[df$Age<=60 & df$Glucose>=100,]
nrow(df2)
```

    ## [1] 18

``` r
prop.table(table(df2$Classification))[[1]]
```

    ## [1] 0.1666667

``` r
# There are 18 people aged 60 or younger with glucose of 100 or greater. The percentage of healthy controls is 16.67%
```

There are 18 people aged 60 or younger with glucose of 100 or greater.
The percentage of healthy controls is 16.67%

### Part 2.

Write a function, the input is a vector. The output is a list of max,
min, mean, median, range,interquartile range (IQR) of the vector. If the
input vector contains any negative values, provide a warning message.
Then apply this function to the Resistin attribute of the data.

``` r
descriptive <- function(x){
  
  if (any(x < 0)) {
    warning("Negative Values Present")
  }
  
  max = max(x, na.rm = TRUE)
  min = min(x, na.rm = TRUE)
  median = median(x, na.rm = TRUE)
  range = range(x, na.rm = TRUE)[2] - range(x, na.rm = TRUE)[1]
  IQR = IQR(x, na.rm = TRUE)
  
  return(list(max=max, min=min, median=median, range=range, IQR=IQR))
  
}

descriptive(df$Resistin)
```

    ## $max
    ## [1] 82.1
    ## 
    ## $min
    ## [1] 3.21
    ## 
    ## $median
    ## [1] 10.82774
    ## 
    ## $range
    ## [1] 78.89
    ## 
    ## $IQR
    ## [1] 10.87344

### Part 3.

Draw a histogram for Adiponectin, filled by blue color. The height of
the histogram should represent the density instead of the frequency.
Also draw the density function (in red color) on top of the histogram.

``` r
ggplot(df, aes(x = Adiponectin)) + 
  geom_histogram(bins = 10, aes(y = ..density..),
                 colour = 1, fill = "blue") +
  geom_density(color="red")
```

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
![000012](https://github.com/schachz/BreastCancerPrediction_ML_Project/assets/100746204/d592ded5-3ab1-4b4a-a9bd-1fe089990529)

### Part 4.

We want to do some string manipulations for attribute MCP.1. For each of
the 116 instances, change 0,1,2,3,4 to A; change 5,6,7,8,9 to B; change
. to -. For example, 123.456 should become “AAA-ABB”. Note that you can
ignore 0s at the end. For example, 312.000 should become “AAA”; 354.600
should become “ABA-B” Print the first 4 instances after conversion. How
many instances have more letter A than letter B? (#A \> \#B)

``` r
d1 <- chartr('01234', 'AAAAA', df$MCP.1)
d2 <- chartr('56789', 'BBBBB', d1)
d3 <- chartr('.', '-', d2)
d3
```

    ##   [1] "AAB-AAA"  "ABB-BBB"  "BBA-BBB"  "BAB-AA"   "BBA-BA"   "BAA-AA"  
    ##   [7] "AABB-ABA" "ABA-BBA"  "AAB-BBB"  "AAB-AAA"  "ABA-B"    "ABA-B"   
    ##  [13] "AAA-BA"   "BAA-AA"   "ABB-AA"   "BA-BA"    "ABA-BA"   "AAA-ABB" 
    ##  [19] "ABA-AAB"  "BBB-AB"   "BAA-AAA"  "BBA-BBA"  "BAA-BBA"  "BAA-BBB" 
    ##  [25] "AAAB-BA"  "BBB-ABA"  "BBB-AB"   "AAAA-AA"  "BBB-BAB"  "BBA-AAA" 
    ##  [31] "BBA-BBB"  "BBB-BBA"  "ABB-BAA"  "BBA-AAB"  "ABA-BBB"  "BBB-AAB" 
    ##  [37] "BAA-BAA"  "ABA-ABB"  "ABB-BBB"  "BAB-ABA"  "BBB-BBB"  "ABB-AAB" 
    ##  [43] "AAB-ABA"  "ABA-AAA"  "AAA-BBB"  "AAB-BB"   "AAB-BAA"  "AAB-BAB" 
    ##  [49] "AAB-BBB"  "AAA-AAB"  "AB-BAA"   "ABB-BAB"  "BBA-AAA"  "ABA-BBB" 
    ##  [55] "BBA-BA"   "ABA-BAB"  "AAA-BAB"  "AAB-BBB"  "BA-B"     "BAA-BBA" 
    ##  [61] "ABB-ABB"  "BAA-AAB"  "BAB-BBA"  "ABB-AAA"  "ABB-AA"   "BAB-BBB" 
    ##  [67] "BAB-AAB"  "ABB-BBB"  "AAA-AA"   "AAA-BB"   "ABA-BB"   "AAA-BB"  
    ##  [73] "BAA-BB"   "ABB-BA"   "AAA"      "BAB-BAA"  "ABA-ABB"  "BBB-AAA" 
    ##  [79] "ABBB-AA"  "BBA-BBB"  "BAA-ABB"  "AAB-ABB"  "BAB-AAA"  "BBB-BBB" 
    ##  [85] "AAAA-BAA" "ABBB-AA"  "ABBB-AA"  "AABB-ABB" "ABBB-AA"  "BAA-BBB" 
    ##  [91] "BAB-ABA"  "BBA-AAB"  "BBA-BBB"  "BBB-ABA"  "ABB-AAA"  "BAA-ABB" 
    ##  [97] "AAB-AAB"  "ABA-AAA"  "ABB-AAA"  "ABA-BBB"  "BBA-AAA"  "ABB-ABB" 
    ## [103] "ABB-BAB"  "AAA-AAB"  "BBB-BAA"  "BBB-BAA"  "BAA-ABA"  "AAB-AB"  
    ## [109] "ABB-A"    "BB-AB"    "AAB-AB"   "ABB-AA"   "AAA-AB"   "AAA-AB"  
    ## [115] "ABA-AB"   "BA-AB"

``` r
counter = 0
for(i in 1:length(d3)){
  t <- table(unlist(strsplit(d3[[i]], ""), use.names=FALSE))
  a <- t["A"] 
  b <- t["B"] 
  if(is.na(b)){
    counter = counter+1
  }else{
    if(!is.na(a) & a>b){
      counter = counter+1
    }
  }
}  
counter  
```

    ## [1] 45

``` r
# 45 Instances have more letter A's than B's.
```

45 Instances have more letter A’s than B’s.

### Part 5.

Create a new variable “group” based on the variable Classification,
(1=Healthy controls, 2=Patients). Draw scattered plot of Glucose vs
Insulin, the color of each dot represents the group. For each group,
draw a linear regression line fitting the all data point in the same
group.

``` r
df$group = ifelse(df$Classification == 1, "Healthy controls", "Patients")

ggplot(df, aes(x=Glucose , y=Insulin, color=group)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
```

    ## `geom_smooth()` using formula = 'y ~ x'
![img2](https://github.com/schachz/BreastCancerPrediction_ML_Project/assets/100746204/2efba7ab-f480-4483-bc12-ca533685b447)


### Part 6.

Draw a boxplot of HOMA, stratified by the variable group, colored by the
variable group, facet_wrap by Age \<= 60; also make the y lab (i.e.,
HOMA) be red color.

``` r
df$Age_cat = ifelse(df$Age <= 60, "Age<=60", "Age>60")

ggplot(df, aes(x=group, y=HOMA, color=group)) + 
  geom_boxplot()+
  facet_wrap(~Age_cat) + 
  theme(axis.title.y = element_text(colour = "red"))
```
![img3](https://github.com/schachz/BreastCancerPrediction_ML_Project/assets/100746204/008a26a9-1cab-458c-b85f-7481883a237d)



### Part 7.

Sort the data by age (ascending) and Adiponectin (decending). Then print
out the first five instance of Resistin.

``` r
df2 <- df[with(df, order(Age, -Adiponectin)), ]
head(df2$Resistin,5)
```

    ## [1]  6.85000  5.14000 21.44366  4.58000 24.60330

### Part 8.

For each of the 9 attributes, compare the value in healthy controls
group and the patients group using wilcoxon rank test. We want to adjust
for multiple comparison and convert the resulting p-values to q-values
via Benjamini & Hochberg (1995) method (“BH” option). Which attributes
have q-value \< 0.05?

``` r
df$group2 = ifelse(df$Classification == 1, "Healthy", "Patients")

test <- df %>% 
  select(Age, BMI, Glucose, Insulin, HOMA, Leptin, Adiponectin, Resistin, MCP.1, group2) %>% 
  gather(key = Variable, value = value, -group2) %>% 
  group_by(group2, Variable) %>% 
  summarise(value = list(value)) %>% 
  spread(group2, value) %>% 
  group_by(Variable) %>% 
  mutate(p_value = wilcox.test(unlist(Healthy), unlist(Patients))$p.value,
         t_value = wilcox.test(unlist(Healthy), unlist(Patients))$statistic) %>% 
  select(Variable, p_value)
```

    ## `summarise()` has grouped output by 'group2'. You can override using the
    ## `.groups` argument.

``` r
test$q.value <- p.adjust(test$p_value, method = "BH")
test[test$q.value<0.05,]
```

    ## # A tibble: 3 × 3
    ## # Groups:   Variable [3]
    ##   Variable    p_value    q.value
    ##   <chr>         <dbl>      <dbl>
    ## 1 Glucose  0.00000102 0.00000917
    ## 2 HOMA     0.00290    0.00869   
    ## 3 Resistin 0.00186    0.00837

``` r
# Glucose, HOMA, and Resistin all have p-values of less than .05
```

Glucose, HOMA, and Resistin all have p-values of less than .05

### Part 9.

For each of the 9 attributes, calculate the mean value for healthy
controls group and the patients group respectively. The result should be
a 9 by 2 matrix, each row represents an attribute, and the two columns
represent healthy controls group and the patients group. Print the first
4 rows of your result.

``` r
mean_val <- df %>% group_by(group) %>% select(-Classification)%>%
               summarize(across(.cols = where(is.numeric),.fns = mean)) %>% 
               pivot_longer(cols = -group) %>% 
               pivot_wider(names_from = group, values_from = value) %>% 
               column_to_rownames('name')
               

mean_val[1:4,]
```

    ##         Healthy controls  Patients
    ## Age            58.076923  56.67188
    ## BMI            28.317336  26.98474
    ## Glucose        88.230769 105.56250
    ## Insulin         6.933769  12.51322

### Part 10.

Make a descriptive table with 9 rows and 3 columns. Each row represents
an attribute. The first column represents mean (SD) of the Healthy
control group; The second column represents mean (SD) of the Patients
group; The third column represents the p-value from the wilcox rank
test. Note that for mean (SD), only keep 1 digit after decimal point, if
there is any. For example, BMI/Healthy controls should be “28.3 (5.4)”.
For the p-value, only keep 3 effective digits (e.g., 0.123 or
$1.23\times 10^{-3}$ or 1.23e-3). However, 0.120 can be expressed as
0.12 as well. If p-value \< 0.05, put a “\*” to the right of the
p-value, Print out the whole descriptive table.

``` r
mean_sd <- df %>% select(Age, BMI, Glucose, Insulin, HOMA, Leptin, Adiponectin, Resistin, MCP.1, group) %>% 
  group_by(group) %>% 
  summarise_all(list(~ str_c(round(mean(.), 1), " (", round(sd(.), 1), ")")))  %>% 
  pivot_longer(cols = -group,) %>% 
  pivot_wider(names_from = group, values_from = value) %>% 
  column_to_rownames('name')

p_value <- test %>% column_to_rownames('Variable') %>% select(p_value)

desc_table <- merge(mean_sd, p_value, by = 'row.names', all = TRUE)

desc_table$p_value <- round(desc_table$p_value, 3)
desc_table$p_value[desc_table$p_value <0.001] = "<0.001" 
desc_table$p_value[desc_table$p_value <0.05] = paste0(desc_table$p_value[desc_table$p_value <0.05], "*")

desc_table %>%column_to_rownames('Row.names')
```

    ##             Healthy controls     Patients p_value
    ## Adiponectin       10.3 (7.6)   10.1 (6.2)   0.766
    ## Age                58.1 (19)  56.7 (13.5)   0.479
    ## BMI               28.3 (5.4)     27 (4.6)   0.202
    ## Glucose          88.2 (10.2) 105.6 (26.6) <0.001*
    ## HOMA               1.6 (1.2)    3.6 (4.6)  0.003*
    ## Insulin            6.9 (4.9)  12.5 (12.3)  0.027*
    ## Leptin           26.6 (19.3)  26.6 (19.2)   0.949
    ## MCP.1          499.7 (292.2)    563 (384)   0.504
    ## Resistin         11.6 (11.4)  17.3 (12.6)  0.002*
