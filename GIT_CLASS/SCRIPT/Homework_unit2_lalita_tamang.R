trees <- read.csv("./GIT_CLASS/SCRIPT/bagmati_raw_inventory.csv")

head(trees)
head(trees, 3)
nrow(trees)
ncol(trees)
names(trees)
str(trees)

cat("Total tree rows:, nrow(trees)\n")
cat("Total columns:, ncol(trees)\n")
cat("column names (case-sensitive):, \n")
print(names(trees))
cat("\nInternal structure:\n")
str(trees)


