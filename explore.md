Data + Desserts
================
Shylee Ezroni
August 14, 2018

### What does our data look like?

``` r
library(YaleToolkit)
```

    ## Loading required package: grid

``` r
df <- read.csv("sales.csv", stringsAsFactors = FALSE,
               header = TRUE, blank.lines.skip = FALSE, as.is = TRUE)

whatis(df) ## high level view of our data
```

    ##   variable.name      type missing distinct.values precision
    ## 1      order_id   numeric       0           10000         1
    ## 2          name character       0              28        NA
    ## 3    ordered_at character       0            9997        NA
    ## 4         price character       0              16        NA
    ## 5      quantity   numeric       0               3         1
    ## 6    line_total character       0              39        NA
    ##                   min                 max
    ## 1               10000               19999
    ## 2                     "SORBET" Watermelon
    ## 3 2018-01-01 11:30:00 2018-07-28 18:51:57
    ## 4              $-0.50               $4.00
    ## 5                   1                   3
    ## 6              $-0.50               $9.00

A really powerful tool I like to use is sapply. Here we'll use it to create a frequency table of each column of the dataframe, and get a view for what our data looks like.

``` r
## excluding order_id and ordered_at to reduce noise
sapply(names(df[,-c(1,3)]), function(x) table(df[x])) ## frequency tables of each column
```

    ## $name
    ## 
    ##                                           "BEVERAGE" Espresso 
    ##                           1488                           1026 
    ##         "BEVERAGE" Iced Coffee                 "BEVERAGE" Tea 
    ##                           1053                            993 
    ##            "CONE" Brownie Cone             "CONE" Cookie Cone 
    ##                           1048                           1080 
    ##      "CONE" Dipped Waffle Cone              "CONE" Sugar Cone 
    ##                           1061                           1086 
    ##             "CONE" Waffle Cone      "ICE CREAM" Candied Bacon 
    ##                           1003                           1110 
    ##     "ICE CREAM" Dark Chocolate "ICE CREAM" Double Fudge Chunk 
    ##                           1092                           1081 
    ##     "ICE CREAM" Dulce De Leche          "ICE CREAM" Earl Gray 
    ##                           1052                           1023 
    ##  "ICE CREAM" Maple Brown Sugar             "ICE CREAM" Matcha 
    ##                           1054                           1035 
    ##          "ICE CREAM" Mint Chip       "ICE CREAM" Peanut Fudge 
    ##                           1131                           1010 
    ##         "ICE CREAM" Rocky Road         "ICE CREAM" Strawberry 
    ##                           1077                           1020 
    ##       "ICE CREAM" Vanilla Bean          "ICE CREAM" Wildberry 
    ##                           1044                           1051 
    ##          "MISC" Ice Cream Cake          "SORBET" Blood Orange 
    ##                            999                           1062 
    ##                 "SORBET" Lemon                "SORBET" Lychee 
    ##                           1113                           1029 
    ##             "SORBET" Raspberry            "SORBET" Watermelon 
    ##                           1077                           1024 
    ## 
    ## $price
    ## 
    ## $-0.50 $-1.00 $-1.50 $-2.00 $-2.50 $-3.00 $-3.50 $-4.00  $0.50  $1.00 
    ##     21      5     60     17     64     19     42     54   2221   1139 
    ##  $1.50  $2.00  $2.50  $3.00  $3.50  $4.00 
    ##   5526   2144   6628   2150   5473   4359 
    ## 
    ## $quantity
    ## 
    ##     1     2     3 
    ## 10009  9841 10072 
    ## 
    ## $line_total
    ## 
    ##  $-0.50  $-1.00  $-1.50 $-10.50 $-12.00  $-2.00  $-2.50  $-3.00  $-3.50 
    ##       9      11      20      14      23      10      16      29      15 
    ##  $-4.00  $-4.50  $-5.00  $-6.00  $-7.00  $-7.50  $-8.00  $-9.00   $0.00 
    ##      17      18      28      12      13      20      19       8       2 
    ##   $0.50   $1.00   $1.50  $10.00  $10.50  $11.00  $12.00   $2.00   $2.50 
    ##     759    1095    2576       1    1831       1    1487    1122    2193 
    ##   $3.00   $3.50   $4.00   $4.50   $5.00   $6.00   $6.50   $7.00   $7.50 
    ##    2947    1854    2145    1842    2186    1418       1    1787    2241 
    ##   $8.00   $8.50   $9.00 
    ##    1413       2     737

``` r
# sapply(names(df), function(x) table(df[x])) ## frequency tables of each column
```

We see a few problems from the getgo:

-   order\_id is not unique
-   name includes product and flavors
-   price and line\_total include "-" for prices
-   name includes empty strings

Each section to follow will clean up what we found!

### Removing Duplicate Rows

``` r
df <- unique(df)
```

### Removing rows with no entries in name column

``` r
no_name <- sapply(c(1:nrow(df)), ## identifying names that are empty strings
                  function(x) nchar(df[x, "name"]) == 0) 
df <- df[no_name == FALSE,] ## removed rows with no name
```

### Cleaning up price and line\_total

-   negative values
-   recreating line\_total column

``` r
df$price <- gsub("-", "", df$price)  ## removing - signs from prices
df$price.f <- gsub("\\$", "", df$price) ## removing $ sign from prices
df$price.f <- as.numeric(df$price.f)  ## turning char into numeric
df$line_total <- df$price.f * df$quantity  ## recalculating line_total
df$line_total.f <- df$line_total  ## creating column with float values
df$line_total <- paste("$", df$line_total, sep = "")  ## returning to char
## gets prices with change ie. $10.5 and adds a 0 to the end
df$line_total[c(grep("\\.", df$line_total))] <- paste(df$line_total[c(grep("\\.",
                                                      df$line_total))], "0", sep = "")
## this gets the prices that dont have change ie. $3, and replaces with $3.00
df$line_total[-c(grep("\\.", df$line_total))] <- paste(df$line_total[-c(grep("\\.",
                                                df$line_total))], ".00", sep = "")  ## adding .00 to end of prices
head(df)
```

    ##   order_id                      name          ordered_at price quantity
    ## 1    10000  "ICE CREAM" Peanut Fudge 2018-01-01 11:30:00 $3.50        3
    ## 2    10000  "ICE CREAM" Peanut Fudge 2018-01-01 11:30:00 $3.50        1
    ## 3    10001        "SORBET" Raspberry 2018-01-01 12:14:54 $2.50        2
    ## 5    10001 "CONE" Dipped Waffle Cone 2018-01-01 12:14:54 $3.50        1
    ## 6    10002           "SORBET" Lychee 2018-01-01 12:23:09 $3.00        1
    ## 8    10002     "ICE CREAM" Earl Gray 2018-01-01 12:23:09 $0.50        3
    ##   line_total price.f line_total.f
    ## 1     $10.50     3.5         10.5
    ## 2      $3.50     3.5          3.5
    ## 3      $5.00     2.5          5.0
    ## 5      $3.50     3.5          3.5
    ## 6      $3.00     3.0          3.0
    ## 8      $1.50     0.5          1.5

### Making unique order\_id's

``` r
# order_id is not unique
df$order_id <- c(1:nrow(df))  ## replacing with unique numbers [1:27903]
length(unique(df$order_id))
```

    ## [1] 27903

### Cleaning up name column

-   product column
-   name (flavor) column

``` r
x <- data.frame(t(sapply(df$name, function(y) strsplit(y,split="\" ")[[1]])))
names(x) <- c("product", "name")  ## renaming col names
x$product <- gsub("\"", "", x$product) ## removing any quotes from products 
df <- df[,-which(names(df) %in% "name")]  ## removing old name column (programatically)
df <- cbind(df, x)  ## adding new cols to og df
df$name <- as.character(df$name)
head(df)
```

    ##   order_id          ordered_at price quantity line_total price.f
    ## 1        1 2018-01-01 11:30:00 $3.50        3     $10.50     3.5
    ## 2        2 2018-01-01 11:30:00 $3.50        1      $3.50     3.5
    ## 3        3 2018-01-01 12:14:54 $2.50        2      $5.00     2.5
    ## 5        4 2018-01-01 12:14:54 $3.50        1      $3.50     3.5
    ## 6        5 2018-01-01 12:23:09 $3.00        1      $3.00     3.0
    ## 8        6 2018-01-01 12:23:09 $0.50        3      $1.50     0.5
    ##   line_total.f   product               name
    ## 1         10.5 ICE CREAM       Peanut Fudge
    ## 2          3.5 ICE CREAM       Peanut Fudge
    ## 3          5.0    SORBET          Raspberry
    ## 5          3.5      CONE Dipped Waffle Cone
    ## 6          3.0    SORBET             Lychee
    ## 8          1.5 ICE CREAM          Earl Gray

``` r
#sapply(names(df), function(x) table(df[x])) ## frequency tables of each column 
```

### Barplots

``` r
#### icecream profits barplot ordered by greatest to least $$
p <- tapply(df$line_total.f, df$name, sum) ## total profit by flavor
p <- p[order(p, decreasing = TRUE)]  ## ordering least to greatest
barplot(p, main = "Ice Cream Profits", las = 2, col = rainbow(length(names(table(df$name)))))
```

![](explore_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
## product profits ordered by greatest to least $$
c <- tapply(df$line_total.f, df$product, sum) ## total profit by flavor
c <- c[order(c, decreasing = TRUE)]  ## ordering least to greatest
barplot(c, main = "Product Profits", las = 2, col = rainbow(length(names(table(df$product)))))
```

![](explore_files/figure-markdown_github/unnamed-chunk-9-1.png)
