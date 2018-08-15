library(YaleToolkit)
df <- read.csv("sales.csv", stringsAsFactors = FALSE,
               header = TRUE, blank.lines.skip = FALSE, as.is = TRUE)

whatis(df) ## high level view of our data

sapply(names(df), function(x) table(df[x])) ## frequency tables of each column 

# removing duplicate rows
df <- unique(df)

# removing rows with no entries (empty string) in name column
no_name <- sapply(c(1:nrow(df)), ## identifying names that are empty strings
                  function(x) nchar(df[x, "name"]) == 0) 
df <- df[no_name == FALSE,] ## removed rows with no name

# fixing prices (negative values and recreating line_total column)
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

# order_id is not unique
df$order_id <- c(1:nrow(df))  ## replacing with unique numbers [1:27903]

# name is all mixed up??, have a category column with the name of the thing (ice cream, sorbet..)
## creating df with 2 cols of product & name
x <- data.frame(t(sapply(df$name, function(y) strsplit(y,split="\" ")[[1]])))
names(x) <- c("product", "name")  ## renaming col names
x$product <- gsub("\"", "", x$product) ## removing any quotes from products 
df <- df[,-which(names(df) %in% "name")]  ## removing old name column (programatically)
df <- cbind(df, x)  ## adding new cols to og df
df$name <- as.character(df$name)
sapply(names(df), function(x) table(df[x])) ## frequency tables of each column 

# barplots
## icecream profits barplot ordered by greatest to least $$
p <- tapply(df$line_total.f, df$name, sum) ## total profit by flavor
p <- p[order(p, decreasing = TRUE)]  ## ordering least to greatest
barplot(p, main = "Ice Cream Profits", las = 2, col = rainbow(length(names(table(df$name)))))

## product profits ordered by greatest to least $$
c <- tapply(df$line_total.f, df$product, sum) ## total profit by flavor
c <- c[order(c, decreasing = TRUE)]  ## ordering least to greatest
barplot(c, main = "Product Profits", las = 2, col = rainbow(length(names(table(df$product)))))
