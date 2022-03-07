# Libraries ---------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(kableExtra)


# Read in / bind data -----------------------------------------------------------------------------
dictionary <- readxl::read_xlsx("project_1/HEsegDataviz_Dictionary.xlsx")
four_year <- readr::read_csv("project_1/HEsegDataviz_CollegeData_4-year_v5.csv")
two_year <- readr::read_csv("project_1/HEsegDataviz_CollegeData_2-year_v5.csv")

# Combine two and four year data sets by row ----
all_year <- dplyr::bind_rows(four_year, two_year)

# -----------------------[ Helper Functions ]------------------------------------------------------

# Calculates average differences for all demographics ---------------------------------------------
dif_mean_caclulator <- function(df) {
    df %>%
        
    # Select only difference columns ----
    dplyr::select(starts_with("dif")) %>%
        
    # apply the mean() function to all selected columns ----
    dplyr::summarise(
        dplyr::across(
            .cols = dplyr::everything(),
            .fns = mean, na.rm = TRUE
        )
    ) %>%
        
    # Rename columns ----
    dplyr::rename(
        White = "dif_white",
        Hispanic = "dif_hispa",
        Black = "dif_black",
        Asian = "dif_asian",
        AmericanIndian = "dif_amind",
        PacificIslander = "dif_pacis",
        Multiracial = "dif_twora"
    ) %>%
    
    # pivot longer to make data easier to work with ----
    tidyr::pivot_longer(
        cols = White:Multiracial,
        names_to = "Demographic",
        values_to = "Difference"
    )
}


# -------------------------[ QUESTION 1 / 2 ]------------------------------------------------------

# Generate general difference plot ----------------------------------------------------------------
all_year %>%
    
    # Utilize our mean helper function ----
    dif_mean_caclulator() %>%
    
    # Add in boolean difference column for coloring purposes ----
    dplyr::mutate(positive = Difference >= 0) %>%
    
    # Generate plot, reordering columns by magnitude of difference value ----
    ggplot(
        aes(
            x = reorder(Demographic, -abs(Difference)),
            y = Difference, 
            fill = positive
        )
    ) +
    
    # stat = "identity" used to force geom_bar to use values instead of count ----
    geom_bar(stat = "identity") +
    
    # Change legend labels ----
    scale_fill_discrete(labels = c("Under", "Over")) +
    
    # Create horizontal line at x-axis for readability of visual ----
    geom_abline(slope=0, intercept=0, col="black", lty = 1) +
    
    # Generate labels ----
    labs(title = "Demographic Representation Differences Between Colleges and Populations",
         subtitle = "Average differences for years 2009 - 2017",
         caption = "Positive values show over-representation in higher ed. 
                    Negative values show under-representation in higher ed.
                    Data available at: community.amstat.org/dataexpo/home"
         ) +
    ylab("Difference (%)") +
    xlab("Demographic") +
    
    # Rename legend title ----
    guides(fill = guide_legend(title = "Representation"))

# -------------------------[ QUESTION 3 ]----------------------------------------------------------

# Grouping by institution level -------------------------------------------------------------------
all_year %>%
    
    # Group the data by institution level ----
    dplyr::group_by(slevel) %>%
    
    # Calculate average differences ----
    dif_mean_caclulator() %>%
        
    # Generate plot, reordering columns by magnitude of difference value ----
    ggplot(
        aes(
            x = reorder(Demographic, -abs(Difference)),
            y = Difference, 
            fill = slevel
        )
    ) +
    
    # Pick out manual fill colors
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    
    # stat = "identity" used to force geom_bar to use values instead of count ----
    geom_bar(stat = "identity", position = "dodge") +
    
    # Create horizontal line at x-axis for readability of visual ----
    geom_abline(slope=0, intercept=0, col="black", lty = 1) +
    
    # Generate labels ----
    labs(title = "Demographic Representation Differences Between Colleges and Populations",
         subtitle = "Average differences for years 2009 - 2017",
         caption = "Positive values show over-representation in higher ed. 
                    Negative values show under-representation in higher ed.
                    Data available at: community.amstat.org/dataexpo/home"
         ) +
    ylab("Difference (%)") +
    xlab("Demographic") +
    
    # Rename legend title ----
    guides(fill = guide_legend(title = "Institution Level"))

# Grouping by level of selection ------------------------------------------------------------------

# Create a new variable based on level of selection ----
all_year %>%
    dplyr::mutate(
        how_selective = as.factor(dplyr::case_when(
            non_selective == 1 ~ "Non Selective",
            selective == 1 ~ "Selective",
            more_selective == 1 ~ "More Selective"
        ) )
    ) %>%
    
    # Group by that new variable ----
    dplyr::group_by(how_selective) %>%
    
    # Filter out NAs for how_selective ----
    dplyr::filter(!is.na(how_selective)) %>%
    
    # Calculate average differences ----
    dif_mean_caclulator() %>%
    
    # Generate plot, reordering columns by magnitude of difference value ----
    ggplot(
        aes(
            x = reorder(Demographic, -abs(Difference)),
            y = Difference, 
            fill = how_selective
        )
    ) +
        
    # 'stat = "identity"' used to force geom_bar to use values instead of count ----
    # 'position = dodge' allows for side-by-side plots based on factor. ----
    geom_bar(stat = "identity", position = "dodge") +
        
    # Pick out manual fill colors
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    
    # Create horizontal line at x-axis for readability of visual ----
    geom_abline(slope=0, intercept=0, col="black", lty = 1) +
        
    # Generate labels ----
    labs(title = "Demographic Representation Differences Between Colleges and Populations",
         subtitle = "Average differences for years 2009 - 2017\nData grouped by selectivity ranking",
         caption = "Positive values show over-representation in higher ed. 
                    Negative values show under-representation in higher ed.
                    Data available at: community.amstat.org/dataexpo/home") +
    ylab("Difference (%)") +
    xlab("Demographic") +
    
    # Rename legend title ----
    guides(fill = guide_legend(title = "Selectivity\nRanking"))

# Grouping by profit status -----------------------------------------------------------------------

all_year %>%
    
    # Create profit_status factor based on public, private, for-profit status ----
    dplyr::mutate(
        profit_status = as.factor(dplyr::case_when(
            public == 1 ~ "Public",
            private == 1 ~ "Private",
            forprofit == 1 ~ "For Profit"
        ) )
    ) %>%
    
    # Group by that new variable ----
    dplyr::group_by(profit_status) %>%
        
    # Filter out NAs for how_selective ----
    dplyr::filter(!is.na(profit_status)) %>%
        
    # Calculate average differences ----
    dif_mean_caclulator() %>%
        
    # Generate plot, reordering columns by magnitude of difference value ----
    ggplot(
        aes(
            x = reorder(Demographic, -abs(Difference)),
            y = Difference, 
            fill = profit_status
        )
    ) +
        
    # 'stat = "identity"' used to force geom_bar to use values instead of count ----
    # 'position = dodge' allows for side-by-side plots based on factor. ----
    geom_bar(stat = "identity", position = "dodge") +
        
    # Create horizontal line at x-axis for readability of visual ----
    geom_abline(slope=0, intercept=0, col="black", lty = 1) +
        
    # Generate labels ----
    labs(title = "Demographic Representation Differences Between Colleges and Populations",
         subtitle = "Average differences for years 2009 - 2017\nData grouped by type of funding",
         caption = "Positive values show over-representation in higher ed. 
                    Negative values show under-representation in higher ed.
                    Data available at: community.amstat.org/dataexpo/home") +
        ylab("Difference (%)") +
        xlab("Demographic") +
    
    # Rename legend title ----
    guides(fill = guide_legend(title = "Funding Type"))


# Group by profit status and plot difference over time --------------------------------------------

all_year %>%
    
    dplyr::mutate(
        profit_status = as.factor(dplyr::case_when(
            public == 1 ~ "Public",
            private == 1 ~ "Private",
            forprofit == 1 ~ "For Profit"
        ) )
    ) %>%
    
    # Group by that new variable ----
    dplyr::group_by(year, profit_status) %>%
        
    # Filter out NAs for how_selective ----
    dplyr::filter(!is.na(profit_status)) %>%
    
    # Select only difference columns ----
    dplyr::select(year, starts_with("dif")) %>%
        
    # apply the mean() function to all selected columns ----
    dplyr::summarise(
        dplyr::across(
            .cols = dplyr::everything(),
            .fns = mean, na.rm = TRUE
        )
    ) %>%
        
    # Rename columns ----
    dplyr::rename(
        White = "dif_white",
        Hispanic = "dif_hispa",
        Black = "dif_black",
        Asian = "dif_asian",
        AmericanIndian = "dif_amind",
        PacificIslander = "dif_pacis",
        Multiracial = "dif_twora"
    ) %>%
        
    # pivot longer to make data easier to work with ----
    tidyr::pivot_longer(
        cols = White:Multiracial,
        names_to = "Demographic",
        values_to = "Difference"
    ) %>%
    
    # Generate plot, reordering columns by magnitude of difference value ----
    ggplot(
        aes(
            x = year,
            y = Difference, 
            color = Demographic
        )
    ) +
    
    # Generate line plot ----
    geom_line(size = 1.05) +
    
    # Create multiple plots based on profit status ----
    facet_wrap(vars(profit_status)) +
        
    # Create horizontal line at x-axis for readability of visual ----
    geom_abline(slope=0, intercept=0, col="black", lty = 1) +
        
    # Generate labels ----
    labs(
        title = "Demographic Representation Differences Between Colleges and Populations",
        subtitle = "Average differences over time from 2009 - 2017",
        caption = "Positive values show over-representation in higher ed. 
                   Negative values show under-representation in higher ed.
                   Data available at: community.amstat.org/dataexpo/home"
    ) +
    ylab("Difference (%)") +
    xlab("Year") +
    
    # Modify x tick marks ----
    scale_x_continuous(breaks = c(seq(2010, 2016, by = 2)))
