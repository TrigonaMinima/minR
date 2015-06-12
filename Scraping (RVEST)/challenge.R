
library("rvest")

url <- "http://www.mastersindatascience.org/schools/"

webpage <- html(url)

links <- html_nodes(webpage, ".state a")

# Links of each state page.
page_urls <- html_attr(links, "href")

# pages <- vector(mode = "list", length = length(df$url))
schools_t <- vector(mode = "list", length = length(page_urls))

# Get the html of all stage pages and extract the html of
# school listings on each page.
for (i in (1:length(page_urls))) {
    schools_t[[i]] <- html_nodes(html(page_urls[i]), ".schoolinfo")
}


# Extracts the details of each program-
# Department, Cost, Curriculum, Pre Requisite Coursework, Delivery, Length.
coursedetails <- function(program, j) {
    programname <- html_text(html_node(program, "a"))
    url <- html_attr(html_node(program, "a"), "href")

    details <- html_nodes(program, ".programdetails .detail")

    department <- html_text(html_node(details[1], ".detailvalue"))
    cost <- html_attr(html_node(details[2], ".detailvalue a"), "href")
    curriculum <- html_attr(html_node(details[3], ".detailvalue a"), "href")
    prereqcoursework <- html_text(html_node(details[4], ".detailvalue"))
    delivery <- html_text(html_node(details[5], ".detailvalue"))
    length <- html_text(html_node(details[6], ".detailvalue"))

    row <- c(programname,
        url,
        department,
        cost,
        curriculum,
        prereqcoursework,
        delivery,
        length)
    return(row)
}

# Main function which does all the heavy work. Extracts all the information
# and calls the "coursedetails" function and get back the details of each
# program offered. Then, writes the data frame in a csv.
courses <- function(schools_t) {
    courses_list <- data.frame(schoolName = character(),
        schoolLocation = character(),
        program = character(),
        url = character(),
        department = character(),
        cost = character(),
        curriculum = character(),
        prereqcoursework = character(),
        delivery = character(),
        length = character(),
        stringsAsFactors = F)

    j <- 1
    for (i in (1 : length(schools_t))) {
        school <- schools_t[[i]]
        for (l in (1 : length(school))) {
            schoolname <- html_text(html_node(school[l], ".schoolheader"))
            schoollocation <- html_text(html_node(school[l], ".schoollocation"))

            programs <- html_nodes(school[l], ".programs .schoolprogram")

            for (m in (1 : length(programs))) {
                details <- coursedetails(programs[m], j)
                print(j)

                courses_list[j, ] <- list(schoolname,
                    schoollocation,
                    details[1],
                    details[2],
                    details[3],
                    details[4],
                    details[5],
                    details[6],
                    details[7],
                    details[8])
                j <- j + 1
            }
        }
    }
    # print(courses_list)
    write.csv(courses_list, "courses_list.csv")
}

courses(schools_t)
