
# Part 1: Read the CSV file and explore the data
trees <- read.csv("./GIT_CLASS/SCRIPT/bagmati_raw_inventory.csv")

head(trees)
head(trees, 3)
nrow(trees)
ncol(trees)
names(trees)
str(trees)

cat("Total tree rows:", nrow(trees), "\n")
cat("Total columns:", ncol(trees), "\n")
cat("column names (case-sensitive):, \n")
print(names(trees))
cat("\nInternal structure:\n")
str(trees)

cat("\nDBH_cm -> class:", class(trees$DBH_cm), "| typeof:", typeof(trees$DBH_cm), "\n")
cat("species -> class:", class(trees$species), "| typeof:", typeof(trees$species), "\n")

if(!is.numeric(trees$DBH_cm)) {
  cat("Warning: DBH_cm is not numeric;Converting with as.numeric()\n")
  trees$DBH_cm <- as.numeric(trees$DBH_cm)
}
if (is.numeric(trees$height_m)) {
  cat("Warning: height_m is numeric;Converting with as.character()\n")
  trees$height_m <- as.character(trees$height_m)
}

Part 2: Data Cleaning and Transformation
cat("NA in DBH_cm:", sum(is.na(trees$DBH_cm)), "\n")
cat("NA in species:", sum(is.na(trees$species)), "\n")
cat("NA in height_m:", sum(is.na(trees$height_m)), "\n")
 
cat("DBH_cm ==-99 :", sum(trees$DBH_cm == -99, na.rm = TRUE), "\n")
cat("DBH_cm ==-999 :", sum(trees$DBH_cm == -999, na.rm = TRUE), "\n")
cat("Height_m ==-99 :", sum(trees$height_m == -99, na.rm = TRUE), "\n")

cat("Trees with DBH_cm < 0:", sum(trees$DBH_cm < 0, na.rm = TRUE), "\n")
cat("Record Before correction:\n")
print(trees[trees$Plot_Id == "P17" & trees$Tree_ID == "T203", ])

t203_row <- which(trees$Plot_Id == "P17" & trees$Tree_ID == "T203")
trees$DBH_cm[t203_row] <- 29.9
cat("Record After correction:\n")
print(trees[t203_row, ])

# Biological Boundary Violation
trees$DBH_cm <= 0
sum(trees$DBH_cm <= 0, na.rm = TRUE)

# The Plot P17 Field Sheet Transcription Case
trees[trees$Plot_ID == "P17" & trees$Tree_ID == "T203", ]
trees$DBH_cm[trees$Plot_ID == "P17" & trees$Tree_ID == "T203"] <- 29.9

5.Millimetre Entry Errors:
# A common error in Nepal forestry is recording caliper measurements in millimeters instead of centimeters. Identify trees with $DBH > 150\text {cm}$ and $DBH < 900\text {cm}$.
Write a relational query to report the exact number of stems affected by this error.
Correct these rows by dividing DBH values by 10. 
cat("\n Millimetre Entry Errors:\n")
mm_flag <- trees$DBH_cm > 150 & trees$DBH_cm < 900
cat("Stems with 150 < DBH < 900:", sum(mm_flag, na.rm = TRUE), "\n")

mm_rows <- which(mm_flag)
cat("Values BEFORE correction:", trees$DBH_cm[mm_rows], "\n")
trees$DBH_cm[mm_rows] <- trees$DBH_cm[mm_rows] / 10
cat("Values AFTER correction :", trees$DBH_cm[mm_rows], "\n")

 PART 3: Logical subsetting & ecological queries

# Clean working subset: drop NA, non-positive (includes -99) and 999
keep <- !is.na(trees$DBH_cm) & trees$DBH_cm > 0 &
        trees$DBH_cm != -99 & trees$DBH_cm != 999
trees_clean <- trees[keep, ]
cat("Rows in trees:", nrow(trees), "| Rows in trees_clean:", nrow(trees_clean), "\n")
 
# 1. Size classes with cut()
# right = TRUE gives intervals (0,10], (10,30], (30,Inf]
trees_clean$size_class <- cut(trees_clean$DBH_cm,
                            breaks = c(0, 10, 30, Inf),
                            labels = c("Sapling", "Pole", "Sawtimber"),
                            right  = TRUE)
size_tab <- table(trees_clean$size_class)
cat("\nStem count per size class:\n")
print(size_tab)
cat("Dominant class:", names(size_tab)[which.max(size_tab)], "\n")
 
# 2. Champion tree
champ_row <- which.max(trees_clean$DBH_cm)
cat("\nChampion tree row number in trees_clean:", champ_row, "\n")
cat("Champion tree record:\n")
print(trees_clean[champ_row, c("Plot_ID", "Tree_ID", "Species", "DBH_cm", "Height_m")])
 
# 3. Target species: Shorea robusta (Sal)
# Species spelling is case-sensitive, so first check for variants.
cat("\nSpecies labels present (check for spelling/case variants):\n")
print(table(trees_clean$Species))
sal_idx <- trees_clean$Species == "Shorea robusta"
cat("Shorea robusta trees      :", sum(sal_idx, na.rm = TRUE), "\n")
cat("Mean DBH of Shorea robusta:",
    mean(trees_clean$DBH_cm[sal_idx], na.rm = TRUE), "cm\n")

PART 4: Defensive functions & stand variability
# 1. Basal area (m^2) = pi/4 * (DBH in metres)^2
calculate_basal_area <- function(dbh_cm) {
  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("DBH must be positive (> 0 cm). Found non-positive value(s).")
  }
  if (any(dbh_cm > 150, na.rm = TRUE)) {
    warning("DBH > 150 cm detected: possible measurement/unit error.")
  }
  (pi / 4) * (dbh_cm / 100)^2
}
 
trees_clean$ba_m2 <- calculate_basal_area(trees_clean$DBH_cm)
cat("\n--- Part 4.1: Basal area ---\n")
cat("Total basal area (m2):", sprintf("%.4f", sum(trees_clean$ba_m2, na.rm = TRUE)), "\n")
 
# 2. Coefficient of variation
calc_cv <- function(x) {
  (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
}
cv_dbh <- calc_cv(trees_clean$DBH_cm)
cat("\n--- Part 4.2: CV% ---\n")
cat("CV% of DBH_cm:", round(cv_dbh, 2), "%\n")
if (cv_dbh < 20) {
  cat("Interpretation: LOW structural variability (< 20%)\n")
} else if (cv_dbh > 30) {
  cat("Interpretation: HIGH structural variability (> 30%)\n")
} else {
  cat("Interpretation: MODERATE structural variability (20-30%)\n")
}
 
# 3. Model domain boundaries
# calculate_volume_example() comes from the course material. If it is not
# already loaded, this PLACEHOLDER (with a domain guard) is used instead.
# Replace it with your course version if you have one.
if (!exists("calculate_volume_example")) {
  calculate_volume_example <- function(dbh_cm, height_m) {
    if (dbh_cm <= 0 || height_m <= 0) stop("DBH and height must be positive.")
    if (dbh_cm < 10 || height_m < 5) {
      warning("Inputs outside calibrated domain (DBH >= 10 cm, Height >= 5 m): ",
              "volume estimate may be unreliable.")
    }
    0.5 * (pi / 4) * (dbh_cm / 100)^2 * height_m   # form factor 0.5 (placeholder)
  }
}
 
cat("\n--- Part 4.3: Volume model domain test ---\n")
vol_A <- calculate_volume_example(32.4, 22.1)
cat("Tree A volume (m3):", vol_A, "\n")
vol_B <- calculate_volume_example(6.5, 4.2)
cat("Tree B volume (m3):", vol_B, "\n")
 
# PART 5: Exploratory visualisation in Base R
   Height_m may still contain -99 sentinels; treat them as NA for plotting only.
plot_dat <- trees_clean
plot_dat$Height_m[which(plot_dat$Height_m == -99)] <- NA 
if (!dir.exists("figures")) dir.create("figures") 
png("figures/homework_unit2_diagnostics.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2))
 
# 1. Histogram of DBH with dashed red mean line
hist(plot_dat$DBH_cm, main = "Distribution of DBH",
     xlab = "DBH (cm)", col = "lightgreen", border = "white")
abline(v = mean(plot_dat$DBH_cm, na.rm = TRUE), col = "red", lty = 2, lwd = 2)
 
# 2. Histogram of Height with dashed blue median line
hist(plot_dat$Height_m, main = "Distribution of Height",
     xlab = "Height (m)", col = "lightblue", border = "white")
abline(v = median(plot_dat$Height_m, na.rm = TRUE), col = "blue", lty = 2, lwd = 2)
 
# 3. Boxplot of DBH by stand
boxplot(DBH_cm ~ Stand_ID, data = plot_dat,
        main = "DBH by Stand", xlab = "Stand ID", ylab = "DBH (cm)",
        col = "wheat")
 
# 4. Scatterplot DBH vs Height with linear trendline
plot(plot_dat$DBH_cm, plot_dat$Height_m,
     main = "DBH vs Height", xlab = "DBH (cm)", ylab = "Height (m)",
     pch = 16, col = "darkgreen")
abline(lm(Height_m ~ DBH_cm, data = plot_dat), col = "red", lwd = 2)
 
par(mfrow = c(1, 1))
dev.off()

cat("\nFigure saved to figures/homework_unit2_diagnostics.png\n")

