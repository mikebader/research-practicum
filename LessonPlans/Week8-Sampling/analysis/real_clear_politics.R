library(httr)
library(XML)
library(ggplot2)

rm(list=ls())

convert.factor <- function(x) {
    as.numeric(as.character(x))
}


rcpURL <- "https://www.realclearpolitics.com/epolls/other/2018_generic_congressional_vote-6185.html#polls"
rcp_content <- tempfile(fileext = ".html")
GET(url = rcpURL, user_agent("Mozilla/5.0"), write_disk(rcp_content))
rcp <- readHTMLTable(rcp_content)[[4]][-1,]

date.fmt <- "%m/%d"
rcp$end_date <- as.Date(gsub(".+?([0-9]+/[0-9]+)$","\\1",rcp$Date),date.fmt)
dem_gop <- c("Democrats (D)","Republicans (R)")
rcp[,c("D","R")] <- apply(rcp[,dem_gop],2,convert.factor)
rcp$diff <- rcp$D - rcp$R
rcp_mar18 <- rcp[1:9,]


## Let's suppose actual Dem advantage was 8 points
## Example question:
## http://msnbcmedia.msn.com/i/TODAY/z_Creative/18164%20NBCWSJ%20March%202018%20Social%20Trends%20Poll%20(Political%20Data%20Release).pdf









## Following two lines are here to help me remember the regex that I used to
## parse the final column
rcp$party <- gsub("(.+ ).*?([0-9\\.]+)$", "\\1", rcp$Spread)
rcp$adv <- as.numeric(
                gsub("(.+ ).*?([0-9\\.]+)$", "\\2", rcp$Spread)
                )
