dbh <- 28.1
ba <- pi* (dbh^2)/40000
ba
height=16
dbh_num <-30.3
species_name <- "Shorea robusta"
num_of_trees <- 20L
is_tree_mothertree <- TRUE

class(dbh_num)
typeof(dbh_num)

class(species_name)
typeof(species_name)

class(num_of_trees)
typeof(num_of_trees)

class(is_tree_mothertree)
typeof(is_tree_mothertree)

is.numeric(dbh_num)
is.integer(is_tree_mothertree)

messy_dbh <- c(12,13,"22",11,"35.1",NA)
class(messy_dbh)

pi * (messy_dbh^2) / 40000 

clean_dbh <- as.numeric(messy_dbh)
class(clean_dbh)
pi* (clean_dbh^2)/40000

bad_entry <- "35.1cm"
as.numeric(bad_entry)

plot_num <- c(1,2,3)
plot_id_chr <- as.character(plot_num)
plot_id_chr
  
as.numeric(c(TRUE, FALSE, TRUE))
as.logical(c(1,0,5,0))

forest_types <- c("community", "private", "protected")
forest_factor <- as.factor(forest_types)
class(forest_factor)
levels(forest_factor)

plot_codes_factor <-factor(c("101", "102", "105"))
as.numeric(plot_codes_factor)
as.numeric(as.character(plot_codes_factor))

mixed_vec <-c(28.4, TRUE, "Shorea")
mixed_vec
class(mixed_vec)
 
dbh > 20 
dbh <= 10
species_name
species_name == "Shorea robusta"
species_name != "Pinus roxburghii"

(dbh > 30) & (height > 18)
(dbh >= 10) & (dbh <= 30)
(dbh <= 0) | (dbh > 150)
!(dbh < 0)

if (dbh >=30) {
    cat("classification: Sawtimber tree\n")
} else if (dbh >= 10) {
    cat("classification: Pole-sized tree\n")
} else {
    cat("classification: Sapling\n")
}
 
trees_dbh <- c(28.4, 35.1, 17.8, 42.0, 22.4)

trees_ba <- pi* (trees_dbh^2)/40000
round(trees_ba, digits =4)

ifelse(test = trees_dbh >=30, yes ="Large Stem", no ="Small/Medium Stem")
heights <- c(19.2, 22.4, NA, 25.1, 17.1)
mean(heights)
mean(heights, na.rm = TRUE)
is.na(heights)
sum(is.na(heights))
   
test_trees <-c(28.4, -12.0, 0, 999, NA)
test_trees <=0
test_trees > 150 & !is.na(test_trees)
any(test_trees <=0, na.rm = TRUE)

calculate_basal_area <- function(dbh_cm) {
    if (!is.numeric(dbh_cm)) {
        stop("Error:DBH must be numeric (measured in cm)!")
    } 
    if (any(dbh_cm <=0, na.rm = TRUE)) {
        stop("Error:DBH must be strictly greater than 0!")
    }
    if (any(dbh_cm > 150, na.rm = TRUE)) {
        warning("warning:DBH > 150 cm detected. Did you record in mm?")
    }
    ba_m2 <- pi* (dbh_cm^2)/40000
    return(ba_m2  )
}
calculate_basal_area(28.4)
calculate_basal_area(c(28.4, 999))
calculate_basal_area(-12)

calc_cv <- function(x) {
    cv <- (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
    return(round(cv, digits = 2))
}
calc_cv()
calc_cv(c(28.4, 35.1, 17.8, 42.0, 22.4))

trees <- read.csv("./GIT_CLASS/RAW/unit2_starter_trees.csv")
head(trees)
head(trees, 15)
nrow(trees)
ncol(trees)
names(trees)
str(trees)
 
trees$dbh_cm
calc_cv(trees$dbh_cm)
trees$ba_m2 <- calculate_basal_area(trees$dbh_cm)
head(trees)

trees$dbh_cm[trees$dbh_cm > 30]
trees$species[trees$dbh_cm > 30]
which.max(trees$dbh_cm)
trees[which.max(trees$dbh_cm), ]
 
 trees$size_class <- cut(
    trees$dbh_cm.
    breaks = c(0, 10, 30, Inf),
    labels = c("sapling", )
 )