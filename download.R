library(tidycensus)
library(dplyr)
library(tidyr)
library(readr)

# Set up your Census API key once with:
# tidycensus::census_api_key("YOUR_KEY", install = TRUE)
years <- c(2014, 2024)

county_data <- bind_rows(lapply(years, \(year) {
  get_acs(
    geography = "county",
    state = "FL",
    survey = "acs5",
    year = year,
    variables = c(
      population = "B01003_001",
      median_household_income = "B19013_001"
    )
  ) |>
    select(GEOID, NAME, variable, estimate) |>
    mutate(year = year)
}))

county_changes <- county_data |>
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = c(variable, year),
    values_from = estimate,
    names_glue = "{variable}_{year}"
  ) |>
  mutate(
    population_change_pct =
      100 * (population_2024 - population_2014) / population_2014,
    median_income_change_pct =
      100 * (median_household_income_2024 - median_household_income_2014) /
      median_household_income_2014,
    population_growth_rank = min_rank(desc(population_change_pct)),
    among_fastest_growing = population_growth_rank <= 10,
    median_income_increased = median_income_change_pct > 0
  ) |>
  arrange(population_growth_rank) |>
  select(
    GEOID,
    county = NAME,
    population_2014,
    population_2024,
    population_change_pct,
    population_growth_rank,
    among_fastest_growing,
    median_household_income_2014,
    median_household_income_2024,
    median_income_change_pct,
    median_income_increased
  )

dir.create("data", showWarnings = FALSE)
write_csv(county_changes, "data/florida_county_growth_income.csv")

message("Saved county comparisons to data/florida_county_growth_income.csv")
message("Median household income is nominal and is a proxy for wealth, not a measure of household wealth.")
