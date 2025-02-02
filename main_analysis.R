###############################################################################
# LOAD LIBRARIES
###############################################################################
library(dplyr)
library(tidyr)
library(readxl)
library(pheatmap)
library(ggplot2)
library(ggsci)
library(metafor)
library(wbstats)

###############################################################################
# HELPER FUNCTIONS
###############################################################################

# Function to replace income level strings with abbreviations
replace_income_levels <- function(df) {
  df %>%
    mutate(income_level_iso3c = case_when(
      income_level_iso3c == "Low income" ~ "LIC",
      income_level_iso3c == "Lower middle income" ~ "LMC",
      income_level_iso3c == "High income" ~ "HIC",
      income_level_iso3c == "Upper middle income" ~ "UMC",
      TRUE ~ income_level_iso3c  # Keep non-matching values
    ))
}

# Function to pivot indicator data (columns starting with "X" represent years)
process_indicator_df <- function(df) {
  df %>%
    pivot_longer(
      cols = starts_with("X"),            # Select year columns
      names_to = "date",                  # New column name for years
      values_to = "value"                 # New column name for values
    ) %>%
    mutate(date = as.numeric(sub("X", "", date)))  # Remove the "X" and convert to numeric
}

###############################################################################
# 1. INPUT INDICATORS FROM Global Observatory on Health R&D (NO API)
###############################################################################

# Define income groups of interest
income_groups <- c("HIC", "LIC", "LMC", "UMC")

# --- Read the CSV files maintaining original column names ---

# (a) Health researchers (in full-time equivalent), as a proportion of all researchers
df_health_researchers <- read.csv(
  "~/Desktop/MedicalErrors2/data/indicators/income/Health researchers (in full-time equivalent), as a proportion of all researchers.csv", 
  header = TRUE, stringsAsFactors = FALSE
)
# Rename column and update income levels
df_health_researchers <- df_health_researchers %>%
  rename(income_level_iso3c = X) %>%
  replace_income_levels() %>%
  process_indicator_df()

# (b) Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)
df_grants <- read.csv(
  "~/Desktop/MedicalErrors2/data/indicators/income/Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT).csv", 
  header = TRUE, stringsAsFactors = FALSE
)
df_grants <- df_grants %>%
  rename(income_level_iso3c = Income.Group) %>%
  replace_income_levels() %>%
  process_indicator_df()

# (c) Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country
df_oda <- read.csv(
  "~/Desktop/MedicalErrors2/data/indicators/income/Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country.csv", 
  header = TRUE, stringsAsFactors = FALSE
)
df_oda <- df_oda %>%
  rename(income_level_iso3c = Income.Group) %>%
  replace_income_levels() %>%
  process_indicator_df()

###############################################################################
# 2. INPUT MAIN DATASET AND CREATE SUMMARY DATASET (data_summary)
###############################################################################

# Read the main dataset from Excel
data <- read_excel("~/Desktop/MedicalErrors2/data/data.xlsx", col_types = "text")

# Convert selected columns to numeric and filter rows with a valid 'Country by region'
data <- data %>%
  mutate_at(c(1, 4, 5), as.numeric) %>%
  filter(!is.na(`Country by region`))

# Replace "-" with NA in the "Cuartil" column
data$Cuartil <- na_if(data$Cuartil, "-")

# Create a summary dataset grouped by Country Income and Publication Year
data_summary <- data %>%
  group_by(`Country by Income`, `año de publicación`) %>%
  summarise(
    citations    = sum(`Número de citaciones`, na.rm = TRUE),
    publications = n(),
    Hindex       = mean(`Índice H`, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  rename(
    income_level_iso3c = `Country by Income`,
    date = `año de publicación`
  ) %>%
  replace_income_levels()

###############################################################################
# 3. MERGE INDICATOR DATA (Global Observatory) INTO data_summary
###############################################################################

# Create a named list of the indicator data frames
indicator_dataframes <- list(
  "Health researchers (in full-time equivalent), as a proportion of all researchers" = df_health_researchers,
  "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)" = df_grants,
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country" = df_oda
)

# Merge each indicator into data_summary using left_join (by income level and date)
for (indicator_name in names(indicator_dataframes)) {
  indicator_df <- indicator_dataframes[[indicator_name]] %>%
    select(income_level_iso3c, date, value)
  data_summary <- left_join(data_summary, indicator_df, by = c("income_level_iso3c", "date"))
  # Rename the joined column (which is named "value") to the indicator's name
  data_summary <- data_summary %>% rename(!!indicator_name := value)
  message(paste("Added data to data_summary for indicator:", indicator_name))
}

###############################################################################
# 4. INPUT INDICATORS USING WORLD BANK API
###############################################################################

# Define the World Bank indicator codes to query
wb_indicators <- c("SH.DTH.NCOM.ZS", "SH.DTH.COMM.ZS", "SH.XPD.CHEX.GD.ZS", "SP.DYN.CDRT.IN",
                   "SH.MED.BEDS.ZS", "SH.DYN.NMRT", "SH.DYN.MORT", "SP.DYN.IMRT.IN", "SP.DYN.AMRT.FE",
                   "SP.DYN.AMRT.MA", "SH.XPD.OOPC.PC.CD", "SH.MED.PHYS.ZS", "SH.SGR.CRSK.ZS",
                   "SH.SGR.IRSK.ZS", "SH.MED.SAOP.P5", "BM.GSR.ROYL.CD", "BX.GSR.ROYL.CD", "GB.XPD.RSDV.GD.ZS",
                   "SP.POP.SCIE.RD.P6")  # Add or remove codes as needed

# Get country information and filter for the income groups of interest
countries_info <- wb_countries()
countries_income <- countries_info %>%
  filter(income_level_iso3c %in% income_groups) %>%
  select(iso3c, income_level_iso3c)

# Initialize a vector to store the indicator labels (names)
all_indicators <- c()

# Loop over each indicator code from the World Bank
for (code in wb_indicators) {
  # 1. Download indicator data
  indicator_data <- wb_data(indicator = code, return_wide = TRUE)
  
  # 2. Extract the descriptive label of the indicator
  indicator_label <- attr(indicator_data[[code]], "label")
  
  # 3. Merge with countries_income to add income level
  indicator_data <- merge(indicator_data, countries_income, by = "iso3c")
  
  # 4. Rename the column with the indicator code to "value"
  colnames(indicator_data)[which(colnames(indicator_data) == code)] <- "value"
  
  # 5. Select relevant columns
  indicator_data <- indicator_data[, c("country", "date", "value", "last_updated", "income_level_iso3c")]
  
  # 6. Aggregate by income level and date (mean of the indicator values)
  indicator_data <- aggregate(value ~ date + income_level_iso3c, data = indicator_data, FUN = mean, na.rm = TRUE)
  
  # 7. Reorder columns: income_level_iso3c, date, value
  indicator_data <- indicator_data[, c("income_level_iso3c", "date", "value")]
  
  # 8. Merge the indicator into data_summary via a left join
  data_summary <- left_join(data_summary, indicator_data, by = c("income_level_iso3c", "date"))
  # The joined column will be named "value"; rename it to the indicator label
  data_summary <- data_summary %>% rename(!!indicator_label := value)
  
  # 9. Append the indicator label to the all_indicators vector
  all_indicators <- c(all_indicators, indicator_label)
  
  # 10. Create an object in the environment with the indicator label as its name (for later use)
  assign(indicator_label, indicator_data)
  
  message(paste("Processed and added indicator to data_summary:", indicator_label))
}

# Prepend the names of the three original indicators to all_indicators
all_indicators <- c(
  "Health researchers (in full-time equivalent), as a proportion of all researchers",
  "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)",
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country",
  all_indicators
)

###############################################################################
# 5. DESCRIPTIVE ANALYSIS
###############################################################################

# Create a line plot of publications over time by income group
ggplot(data_summary, aes(x = date, y = publications, color = income_level_iso3c)) +
  geom_line(size = 0.5, lineend = "round") +  # Thicker lines with rounded ends
  labs(
    title = NULL,         # No plot title
    x = NULL,             # No x-axis label
    y = "Publications",   # y-axis label
    color = NULL          # Remove legend title
  ) +
  scale_y_continuous(
    breaks = seq(0, max(data_summary$publications, na.rm = TRUE), by = 20),
    minor_breaks = NULL
  ) +
  scale_color_npg() +     # Use Nature Publishing Group color palette
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

###############################################################################
# 6. MANUAL LINEAR REGRESSION MODELS
###############################################################################

# Remove all objects except data_summary, data, and all_indicators
rm(list = setdiff(ls(), c("data_summary", "data", "all_indicators")))

# Function to run manual linear models in two directions:
# (a) Dependent indicator ~ measure
# (b) measure ~ Independent indicator
run_manual_lms <- function(data_summary, measure, all_indicators, income_group_var = "income_level_iso3c") {
  # Define the dependent indicators indices (adjust as needed)
  dependant_indices <- c(4, 5, 7, 8:13, 16, 17)
  dependant_indicators <- all_indicators[dependant_indices]
  
  # Define independent indicators (exclude the first 3 and those already included)
  independant_indicators <- all_indicators[-c(dependant_indices)]
  
  # Initialize results data frame
  results_df <- data.frame(
    IncomeGroup = character(),
    DependentVariable = character(),
    IndependentVariable = character(),
    Coefficient = numeric(),
    StdError = numeric(),
    tValue = numeric(),
    PValue = numeric(),
    ResidualStdError = numeric(),
    DFResidual = numeric(),
    MultipleR2 = numeric(),
    AdjustedR2 = numeric(),
    FStatistic = numeric(),
    FStatisticPValue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 1) For models: (dependent_indicator ~ measure)
  for (income_group in unique(data_summary[[income_group_var]])) {
    income_group_data <- data_summary %>% filter(!!sym(income_group_var) == income_group)
    for (indicator in dependant_indicators) {
      formula_str <- paste0("`", indicator, "` ~ ", measure)
      model <- lm(as.formula(formula_str), data = income_group_data)
      s <- summary(model)
      
      coef_summary <- s$coefficients
      if (measure %in% rownames(coef_summary)) {
        Coefficient <- coef_summary[measure, "Estimate"]
        StdError    <- coef_summary[measure, "Std. Error"]
        tValue      <- coef_summary[measure, "t value"]
        PValue      <- coef_summary[measure, "Pr(>|t|)"]
      } else {
        Coefficient <- StdError <- tValue <- PValue <- NA
      }
      
      if (!is.null(s$fstatistic)) {
        f_statistic    <- s$fstatistic["value"]
        f_stat_p_value <- pf(f_statistic, s$fstatistic["numdf"], s$fstatistic["dendf"], lower.tail = FALSE)
      } else {
        f_statistic <- f_stat_p_value <- NA
      }
      
      results_df <- rbind(results_df, data.frame(
        IncomeGroup         = income_group,
        DependentVariable   = indicator,
        IndependentVariable = measure,
        Coefficient         = Coefficient,
        StdError            = StdError,
        tValue              = tValue,
        PValue              = PValue,
        ResidualStdError    = s$sigma,
        DFResidual          = s$df[2],
        MultipleR2          = s$r.squared,
        AdjustedR2          = s$adj.r.squared,
        FStatistic          = f_statistic,
        FStatisticPValue    = f_stat_p_value,
        stringsAsFactors    = FALSE
      ))
    }
  }
  
  # 2) For models: (measure ~ independent_indicator)
  for (income_group in unique(data_summary[[income_group_var]])) {
    income_group_data <- data_summary %>% filter(!!sym(income_group_var) == income_group)
    for (indicator in independant_indicators) {
      formula_str <- paste0(measure, " ~ `", indicator, "`")
      model <- lm(as.formula(formula_str), data = income_group_data)
      s <- summary(model)
      
      coef_summary <- s$coefficients
      row_name <- paste0("`", indicator, "`")
      
      if (row_name %in% rownames(coef_summary)) {
        Coefficient <- coef_summary[row_name, "Estimate"]
        StdError    <- coef_summary[row_name, "Std. Error"]
        tValue      <- coef_summary[row_name, "t value"]
        PValue      <- coef_summary[row_name, "Pr(>|t|)"]
      } else {
        Coefficient <- StdError <- tValue <- PValue <- NA
      }
      
      if (!is.null(s$fstatistic)) {
        f_statistic    <- s$fstatistic["value"]
        f_stat_p_value <- pf(f_statistic, s$fstatistic["numdf"], s$fstatistic["dendf"], lower.tail = FALSE)
      } else {
        f_statistic <- f_stat_p_value <- NA
      }
      
      results_df <- rbind(results_df, data.frame(
        IncomeGroup         = income_group,
        DependentVariable   = measure,
        IndependentVariable = indicator,
        Coefficient         = Coefficient,
        StdError            = StdError,
        tValue              = tValue,
        PValue              = PValue,
        ResidualStdError    = s$sigma,
        DFResidual          = s$df[2],
        MultipleR2          = s$r.squared,
        AdjustedR2          = s$adj.r.squared,
        FStatistic          = f_statistic,
        FStatisticPValue    = f_stat_p_value,
        stringsAsFactors    = FALSE
      ))
    }
  }
  
  return(results_df)
}

# Run the function for each measure
results_publications <- run_manual_lms(data_summary, "publications", all_indicators)
results_citations    <- run_manual_lms(data_summary, "citations", all_indicators)
results_hindex       <- run_manual_lms(data_summary, "Hindex", all_indicators)

# (Optional) Save results as CSV files
# write.csv(results_publications, "results_publications.csv", row.names = FALSE)
# write.csv(results_citations, "results_citations.csv", row.names = FALSE)
# write.csv(results_hindex, "results_hindex.csv", row.names = FALSE)

###############################################################################
# 7. HEAT MAPS
###############################################################################

# Define indicator names and their abbreviations for the heatmap
# Prepend bibliometric indicators to all_indicators vector
all_indicators <- c("citations", "publications", "Hindex", all_indicators)

# Fixed abbreviations for certain indicators
specific_values <- c("CIT", "PUB", "HDX")  # Fixed abbreviations
specific_names  <- c("citations", "publications", "Hindex")
# Create abbreviations for the remaining indicators
letters_vec <- LETTERS[1:(length(all_indicators) - length(specific_values))]
names(specific_values) <- specific_names
names(letters_vec) <- all_indicators[!(all_indicators %in% specific_names)]
equivalences <- c(specific_values, letters_vec)

# Function to assign significance asterisks based on p-value
assign_asterisks <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) "***"
  else if (p_value < 0.01) "**"
  else if (p_value < 0.05) "*"
  else ""
}

# Function to create heatmap data from regression results
create_heatmap_data <- function(results, metric_name) {
  results <- results %>%
    mutate(
      IndependentAsterisk = ifelse(IndependentVariable != metric_name, "*", ""),
      SignificanceAsterisk = case_when(
        PValue < 0.001 ~ "***",
        PValue < 0.01  ~ "**",
        PValue < 0.05  ~ "*",
        TRUE           ~ ""
      )
    ) %>%
    group_by(DependentVariable, IndependentVariable) %>%
    mutate(CoefficientNormalized = (Coefficient - mean(Coefficient, na.rm = TRUE)) / sd(Coefficient, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      DependentLetter = letters_vec[DependentVariable],
      IndependentLetter = letters_vec[IndependentVariable],
      RowLabel = ifelse(
        DependentVariable == metric_name,
        paste0(IndependentLetter, IndependentAsterisk),
        paste0(DependentLetter, IndependentAsterisk)
      ),
      CellLabel = ifelse(
        DependentVariable == metric_name,
        paste0(IndependentLetter, SignificanceAsterisk),
        paste0(DependentLetter, SignificanceAsterisk)
      )
    )
  
  heatmap_data <- results %>%
    select(IncomeGroup, RowLabel, CoefficientNormalized) %>%
    pivot_wider(names_from = IncomeGroup, values_from = CoefficientNormalized)
  
  heatmap_matrix <- as.matrix(heatmap_data[,-1])
  rownames(heatmap_matrix) <- heatmap_data$RowLabel
  heatmap_matrix[is.na(heatmap_matrix)] <- 0
  
  significance_data <- results %>%
    select(IncomeGroup, RowLabel, SignificanceAsterisk) %>%
    pivot_wider(names_from = IncomeGroup, values_from = SignificanceAsterisk)
  
  significance_matrix <- as.matrix(significance_data[,-1])
  rownames(significance_matrix) <- significance_data$RowLabel
  
  list(heatmap_matrix = heatmap_matrix, significance_matrix = significance_matrix)
}

# Function to create a custom heatmap using pheatmap
create_custom_heatmap <- function(heatmap_matrix, significance_matrix, title) {
  pheatmap(
    heatmap_matrix,
    clustering_distance_rows = "manhattan",
    clustering_distance_cols = "manhattan",
    clustering_method = "ward.D2",
    show_rownames = TRUE,
    show_colnames = TRUE,
    display_numbers = significance_matrix,
    fontsize_number = 10,
    number_color = "black",
    main = title,
    border_color = NA,
    color = colorRampPalette(c("blue", "white", "red"))(50),
    breaks = seq(-1.5, 1.5, length.out = 51)
  )
}

# Example usage of heatmaps for different measures
data_publications <- create_heatmap_data(results_publications, "publications")
create_custom_heatmap(data_publications$heatmap_matrix, data_publications$significance_matrix, "Publications")

data_citations <- create_heatmap_data(results_citations, "citations")
create_custom_heatmap(data_citations$heatmap_matrix, data_citations$significance_matrix, "Citations")

data_hindex <- create_heatmap_data(results_hindex, "Hindex")
create_custom_heatmap(data_hindex$heatmap_matrix, data_hindex$significance_matrix, "H-index")

###############################################################################
# 8. EXTRACT SIGNIFICANT RESULTS AND ADD ROW NUMBERS
###############################################################################

extract_significant_results <- function(results_df, p_cutoff = 0.05) {
  results_df %>%
    filter(PValue < p_cutoff) %>%
    select(IncomeGroup, DependentVariable, IndependentVariable, Coefficient, StdError, 
           tValue, PValue, ResidualStdError, DFResidual, MultipleR2, AdjustedR2, 
           FStatistic, FStatisticPValue)
}

significant_results_publications <- extract_significant_results(results_publications)
significant_results_citations    <- extract_significant_results(results_citations)
significant_results_hindex       <- extract_significant_results(results_hindex)

# Optionally add row numbers
significant_results_publications$RowNumber <- seq_len(nrow(significant_results_publications))
significant_results_citations$RowNumber    <- seq_len(nrow(significant_results_citations))
significant_results_hindex$RowNumber       <- seq_len(nrow(significant_results_hindex))

###############################################################################
# 9. META-ANALYSIS AND FOREST PLOTS
###############################################################################

# Remove the first three values (bibliometric indicators) from all_indicators
all_indicators_no_bibliometric <- all_indicators[-c(1:3)]

# Define dependent and independent indicator indices (adjust as needed)
dependant_indices <- c(4, 5, 7, 8:13, 16, 17)
all_indicators_dependent <- all_indicators_no_bibliometric[dependant_indices]
all_indicators_independent <- all_indicators_no_bibliometric[-c(dependant_indices)]

# Function to perform meta-analysis and create forest plots
perform_meta_analysis_and_forest_plot <- function(
    results_df,
    measure_label,
    indicator_list,
    dep_or_indep = c("DependentVariable", "IndependentVariable"),
    normalized_title = "Forest Plot (Normalized)",
    unnormalized_title = "Forest Plot (Unnormalized)",
    min_studies = 2
) {
  dep_or_indep <- match.arg(dep_or_indep)
  
  # Data frame to store meta-analysis results
  meta_results <- data.frame(
    Indicator   = character(),
    Coefficient = numeric(),
    CI_Lower    = numeric(),
    CI_Upper    = numeric(),
    Weight      = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each indicator in the list
  for (indicator in indicator_list) {
    if (dep_or_indep == "DependentVariable") {
      subset_data <- results_df %>% filter(DependentVariable == indicator)
    } else {
      subset_data <- results_df %>% filter(IndependentVariable == indicator)
    }
    
    # Remove rows with NA values
    subset_data <- subset_data %>% filter(!is.na(Coefficient), !is.na(StdError))
    
    if (nrow(subset_data) >= min_studies) {
      res <- rma(yi = Coefficient, sei = StdError, data = subset_data, method = "REML")
      weight <- sum(1 / res$vi)
      
      meta_results <- rbind(meta_results, data.frame(
        Indicator   = indicator,
        Coefficient = res$b[1],
        CI_Lower    = res$ci.lb,
        CI_Upper    = res$ci.ub,
        Weight      = weight,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(meta_results) == 0) {
    message("Meta-analysis could not be performed for ", measure_label)
    return(NULL)
  }
  
  # Normalize coefficients (Z-score)
  meta_results_norm <- meta_results %>%
    mutate(
      Coefficient_Z = (Coefficient - mean(Coefficient)) / sd(Coefficient),
      CI_Lower_Z    = (CI_Lower   - mean(Coefficient)) / sd(Coefficient),
      CI_Upper_Z    = (CI_Upper   - mean(Coefficient)) / sd(Coefficient)
    )
  
  # Create a results table with scientific notation
  meta_results_table <- meta_results %>%
    mutate(
      `Coefficient (95% CI)` = paste0(
        round(Coefficient, 2), 
        " (", round(CI_Lower, 2), ", ", round(CI_Upper, 2), ")"
      ),
      Weight = round(Weight, 2)
    ) %>%
    select(Indicator, `Coefficient (95% CI)`, Weight)
  
  # Unnormalized forest plot
  plot_original <- ggplot(meta_results, aes(x = Coefficient, y = Indicator)) +
    geom_point(aes(size = Weight), shape = 22, fill = alpha("black", 0.7)) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.8) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = paste(unnormalized_title, "-", measure_label),
      x     = "Coefficient (95% CI)",
      y     = "Indicators",
      size  = "Precision"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_blank(),
      axis.ticks       = element_blank()
    )
  
  # Normalized forest plot
  plot_normalized <- ggplot(meta_results_norm, aes(x = Coefficient_Z, y = Indicator)) +
    geom_point(aes(size = Weight), shape = 22, fill = alpha("black", 0.7)) +
    geom_errorbarh(aes(xmin = CI_Lower_Z, xmax = CI_Upper_Z), height = 0.2) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.8) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = paste(normalized_title, "-", measure_label),
      x     = "Z Coefficient (95% CI)",
      y     = "Indicators",
      size  = "Precision"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_blank(),
      axis.ticks       = element_blank()
    )
  
  list(
    meta_results       = meta_results,
    meta_results_table = meta_results_table,
    plot_original      = plot_original,
    plot_normalized    = plot_normalized
  )
}

###############################################################################
# 10. FOREST PLOT EXAMPLES
###############################################################################

# (a) Publications – Dependent indicators
res_pub_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_publications,
  measure_label  = "Publications (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

res_cit_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_citations,
  measure_label  = "Citations (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

res_hdx_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_hindex,
  measure_label  = "Hindex (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

# Display results (tables and plots)
print(res_hdx_dep$meta_results_table)
print(res_pub_dep$plot_original)
print(res_pub_dep$plot_normalized)

# (b) Publications – Independent indicators
res_pub_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_publications,
  measure_label  = "Publications (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

res_cit_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_citations,
  measure_label  = "Citations (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

res_hdx_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_hindex,
  measure_label  = "Hindex (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

# Display results (tables and plots)
print(res_hdx_ind$meta_results_table)
print(res_pub_ind$plot_original)
print(res_pub_ind$plot_normalized)

# (Optional) Export meta-analysis tables as CSV files
# write.csv(res_pub_dep$meta_results_table, "res_pub_dep.csv", row.names = FALSE)
# write.csv(res_cit_dep$meta_results_table, "res_cit_dep.csv", row.names = FALSE)
# write.csv(res_hdx_dep$meta_results_table, "res_hdx_dep.csv", row.names = FALSE)
# write.csv(res_pub_ind$meta_results_table, "res_pub_ind.csv", row.names = FALSE)
# write.csv(res_cit_ind$meta_results_table, "res_cit_ind.csv", row.names = FALSE)
# write.csv(res_hdx_ind$meta_results_table, "res_hdx_ind.csv", row.names = FALSE)

###############################################################################
# 11. ANALYSIS OF INTERESTING INDICATORS: Mortality by Sex
###############################################################################

# Add a row number column to results_publications
results_publications$RowNumber <- seq_len(nrow(results_publications))

# Extract specific rows corresponding to mortality indicators
mortality_sex <- results_publications %>%
  filter(RowNumber %in% c(8, 9, 19, 20, 30, 31, 41, 42)) %>%
  select(IncomeGroup, DependentVariable, Coefficient, StdError, PValue)

# Add a new column 'sex' based on the DependentVariable
mortality_sex$sex <- ifelse(
  mortality_sex$DependentVariable == "Mortality rate, adult, female (per 1,000 female adults)", 
  "Female", 
  "Male"
)

# Add a column with significance asterisks based on the p-value
mortality_sex <- mortality_sex %>%
  mutate(Significance = case_when(
    PValue < 0.001 ~ "***",
    PValue < 0.01 ~ "**",
    PValue < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Set the IncomeGroup factor order (reverse order)
mortality_sex$IncomeGroup <- factor(mortality_sex$IncomeGroup, levels = c("LIC", "LMC", "UMC", "HIC"))

# Calculate dynamic y-axis limits
y_min <- min(mortality_sex$Coefficient) - 0.3
y_max <- max(mortality_sex$Coefficient) + 0.3

# Calculate average coefficient per IncomeGroup
average_coefficients <- mortality_sex %>%
  group_by(IncomeGroup) %>%
  summarize(AverageCoefficient = mean(Coefficient))

# Create a bar plot with error bars and significance labels
ggplot(mortality_sex, aes(x = IncomeGroup, y = Coefficient, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Coefficient - StdError, ymax = Coefficient + StdError),
                position = position_dodge(width = 0.8), width = 0.2, color = "black", alpha = 0.6) +
  geom_text(aes(label = Significance, 
                y = Coefficient + ifelse(Coefficient > 0, StdError + 0.2, -StdError - 0.4)),
            position = position_dodge(width = 0.8), size = 4, vjust = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  geom_line(data = average_coefficients, aes(x = IncomeGroup, y = AverageCoefficient, group = 1),
            inherit.aes = FALSE, color = "black", size = 0.5, alpha = 0.7, linetype = "dashed", lineend = "round") +
  labs(
    x = "Income Group",
    y = "Coefficient"
  ) +
  scale_y_continuous(breaks = c(-5, -3, -1, 0, 1, 3, 5)) +
  scale_fill_npg(name = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray80", size = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10),
    legend.position = "top",
    plot.title = element_blank()
  )
