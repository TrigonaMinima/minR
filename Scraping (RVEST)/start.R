
library("rvest")

url <- "http://101.datascience.community/2012/04/09/colleges-with-data-science-degrees/"

webpage <- html(url)

tbl <- html_node(webpage, "table")

df <- as.data.frame(html_table(tbl))
str(df)

links <- html_nodes(tbl, "a")
links <- html_attr(links, "href")

df$links <- links

write.csv(df, "courses.csv")