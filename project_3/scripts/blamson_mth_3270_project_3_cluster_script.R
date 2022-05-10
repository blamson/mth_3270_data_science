# -------------------------------------------[ Task 2 ]--------------------------------------------

# Create scaling function ----
my_scale <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Scale numeric data and remove NAs
scaled_df <-
    data_2017 %>%
    dplyr::select(fourcat, enrollment, student_faculty_ratio, tuition_and_fees, dif_white) %>%
    dplyr::mutate(
        across(c(enrollment, student_faculty_ratio, tuition_and_fees, dif_white),
               my_scale)
    ) %>%
    na.omit()

hclust <-
    scaled_df %>%
    dist(method = "euclidian") %>%
    hclust()

hclusters <- 
    cutree(hclust, k = 5)

scaled_df %>%
    select(-fourcat) %>%
    pairs(col = hclusters, pch = 19)

scaled_df %>%
    GGally::ggpairs(
        columns = 2:5,
        aes(color = fourcat, alpha = 0.7),
        diag = list(continuous = "blankDiag")
    )
