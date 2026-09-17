# Air-pollution figures for the ScenarioMIP LN marker paper
#
# Outputs
#   Figure 1: AIM scenarios overlaid on the multi-model SSP2-LN range
#   Figure 2: Sectoral time evolution of AIM SSP2-LN emissions
#   CSV tables used to verify ranks and composition shares

# R may start in the "C" locale on Windows, which cannot resolve the Japanese
# characters in this project's OneDrive path. Switch only the character-type
# locale; the source CSV parsing locales are specified separately below.
if (.Platform$OS.type == "windows") {
  invisible(Sys.setlocale("LC_CTYPE", "Japanese_Japan.utf8"))
}

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

options(scipen = 999)

# Locate the project from the script path. This makes the script independent of
# the working directory used to invoke Rscript.
command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- command_args[str_detect(command_args, "^--file=")]

if (length(file_arg) == 1) {
  script_path <- normalizePath(
    str_remove(file_arg, "^--file="),
    winslash = "/",
    mustWork = TRUE
  )
  project_dir <- normalizePath(
    file.path(dirname(script_path), ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  candidates <- unique(c(getwd(), file.path(getwd(), "..")))
  project_dir <- candidates[
    file.exists(file.path(candidates, "define", "define.csv"))
  ][1]
  if (is.na(project_dir)) {
    stop("Could not locate the LN_Marker project directory.")
  }
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
}

data_dir <- file.path(project_dir, "data")
define_dir <- file.path(project_dir, "define")
figure_dir <- file.path(project_dir, "output", "figure", "main", "air_pollution")
table_dir <- file.path(project_dir, "output", "table", "main", "air_pollution")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# Use the common decadal grid. The source models have different native temporal
# resolutions, but every model contains these years.
plot_years <- as.character(seq(2020, 2100, by = 10))

pollutant_lookup <- tribble(
  ~VariableTotal,       ~Pollutant, ~PollutantOrder,
  "Emissions|Sulfur",  "SOx",                   1,
  "Emissions|NOx",     "NOx",                   2,
  "Emissions|BC",      "BC",                    3,
  "Emissions|OC",      "OC",                    4,
  "Emissions|CO",      "CO",                    5,
  "Emissions|VOC",     "VOC",                   6,
  "Emissions|NH3",     "NH3",                   7
)

variables_total <- pollutant_lookup$VariableTotal
sector_components_raw <- c(
  "AFOLU",
  "Energy",
  "Energy and Industrial Processes",
  "Industrial Processes",
  "Product Use",
  "Waste",
  "Other"
)

variable_lookup <- bind_rows(
  pollutant_lookup %>%
    transmute(
      Variable = VariableTotal,
      VariableTotal,
      Component = "Total"
    ),
  crossing(
    VariableTotal = variables_total,
    Component = sector_components_raw
  ) %>%
    mutate(Variable = paste(VariableTotal, Component, sep = "|")) %>%
    select(Variable, VariableTotal, Component)
)

variables_needed <- unique(variable_lookup$Variable)

# Read model/scenario aliases and the established color definitions.
df_define <- read_csv(
  file.path(define_dir, "define.csv"),
  locale = locale(encoding = "shift-jis"),
  show_col_types = FALSE
)

model_lookup <- df_define %>%
  filter(!is.na(model1), model1 != "") %>%
  transmute(
    ModelRaw = model1,
    Model = model2,
    ModelOrder = row_number()
  ) %>%
  distinct()

scenario_lookup <- df_define %>%
  filter(!is.na(model), model != "", !is.na(scenario), scenario != "") %>%
  transmute(
    ModelRaw = model,
    ScenarioRaw = scenario,
    ScenarioCategory = scenario_category,
    SSP
  ) %>%
  distinct()

model_colors <- df_define %>%
  filter(!is.na(model2), model2 != "", !is.na(color_model), color_model != "") %>%
  distinct(model2, .keep_all = TRUE) %>%
  { setNames(.$color_model, .$model2) }

model_labels <- setNames(model_lookup$Model, model_lookup$Model)
model_labels[c("MESSAGEix-GLOBIOM", "REMIND-MAgPIE")] <- c("MESSAGEix", "REMIND")

scenario_colors <- df_define %>%
  filter(!is.na(scenario1), scenario1 != "", !is.na(color_scenario), color_scenario != "") %>%
  distinct(scenario1, .keep_all = TRUE) %>%
  { setNames(.$color_scenario, .$scenario1) }

expected_prefixes <- c(
  "AIM", "COFFEE", "GCAM", "IMAGE", "MESSAGEix", "REMIND", "WITCH"
)

# Pin the ScenarioMIP data release so that the figures are reproducible and the
# files used in this analysis are explicit in the version-controlled code.
scenario_data_version <- "20260915"

model_file_pattern <- paste0(
  "^(", paste(expected_prefixes, collapse = "|"), ")(",
  scenario_data_version, ")[.]csv$"
)

model_files <- tibble(
  path = list.files(data_dir, pattern = model_file_pattern, full.names = TRUE)
) %>%
  mutate(
    file = basename(path),
    prefix = str_match(file, model_file_pattern)[, 2],
    version = str_match(file, model_file_pattern)[, 3]
  ) %>%
  arrange(match(prefix, expected_prefixes))

missing_prefixes <- setdiff(expected_prefixes, model_files$prefix)
if (length(missing_prefixes) > 0) {
  stop(
    "Missing ScenarioMIP model files: ",
    paste(missing_prefixes, collapse = ", ")
  )
}

message("ScenarioMIP data version: ", scenario_data_version)
message("Reading ScenarioMIP air-pollution data:")
for (i in seq_len(nrow(model_files))) {
  message("  ", model_files$file[i])
}

read_air_file <- function(path) {
  header <- names(fread(path, nrows = 0, header = TRUE, check.names = FALSE))
  required_columns <- c(
    "Model", "Scenario", "Region", "Variable", "Unit", plot_years
  )
  missing_columns <- setdiff(required_columns, header)
  if (length(missing_columns) > 0) {
    stop(
      basename(path), " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  fread(
    path,
    header = TRUE,
    select = required_columns,
    colClasses = "character",
    check.names = FALSE,
    showProgress = interactive()
  ) %>%
    as_tibble() %>%
    filter(Region == "World", Variable %in% variables_needed) %>%
    pivot_longer(
      cols = all_of(plot_years),
      names_to = "Year",
      values_to = "Value"
    ) %>%
    mutate(
      Year = as.integer(Year),
      Value = parse_double(Value, na = c("", "NA"))
    ) %>%
    filter(!is.na(Value))
}

air_raw <- bind_rows(lapply(model_files$path, read_air_file)) %>%
  rename(ModelRaw = Model, ScenarioRaw = Scenario)

# Report relevant rows that cannot be mapped, rather than silently dropping them.
unmapped_models <- air_raw %>%
  anti_join(model_lookup, by = "ModelRaw") %>%
  distinct(ModelRaw)
if (nrow(unmapped_models) > 0) {
  stop(
    "Unmapped model names: ",
    paste(unmapped_models$ModelRaw, collapse = ", ")
  )
}

unmapped_scenarios <- air_raw %>%
  anti_join(scenario_lookup, by = c("ModelRaw", "ScenarioRaw")) %>%
  distinct(ModelRaw, ScenarioRaw)
if (nrow(unmapped_scenarios) > 0) {
  message(
    "Note: unmapped combinations not needed for the SSP2-LN comparison will be omitted:\n",
    paste(
      paste(unmapped_scenarios$ModelRaw, unmapped_scenarios$ScenarioRaw, sep = " / "),
      collapse = "\n"
    )
  )
}

air <- air_raw %>%
  inner_join(model_lookup, by = "ModelRaw") %>%
  inner_join(scenario_lookup, by = c("ModelRaw", "ScenarioRaw")) %>%
  mutate(ScenarioID = paste(SSP, ScenarioCategory, sep = "_")) %>%
  inner_join(variable_lookup, by = "Variable") %>%
  inner_join(pollutant_lookup, by = "VariableTotal") %>%
  mutate(
    Model = factor(Model, levels = model_lookup$Model),
    ScenarioCategory = factor(
      ScenarioCategory,
      levels = df_define$scenario1[!is.na(df_define$scenario1)]
    ),
    Pollutant = factor(
      Pollutant,
      levels = pollutant_lookup$Pollutant[order(pollutant_lookup$PollutantOrder)]
    ),
    Panel = factor(
      paste0(Pollutant, "\n(", Unit, ")"),
      levels = unique(
        paste0(
          pollutant_lookup$Pollutant[order(pollutant_lookup$PollutantOrder)],
          "\n(",
          c("Mt SO2/yr", "Mt NO2/yr", "Mt BC/yr", "Mt OC/yr", "Mt CO/yr", "Mt VOC/yr", "Mt NH3/yr"),
          ")"
        )
      )
    )
  )

duplicate_rows <- air %>%
  count(Model, ScenarioID, Variable, Year) %>%
  filter(n > 1)
if (nrow(duplicate_rows) > 0) {
  stop("Duplicate model/scenario/variable/year rows were found in the source data.")
}

air_total <- air %>%
  filter(Component == "Total")

# SSP2 is held constant in the multi-model comparison so that model differences
# are not mixed with SSP differences. WITCH's raw scenario name is mapped to LN
# in define.csv, just like the other models' Low Overshoot runs.
ln_total <- air_total %>%
  filter(ScenarioCategory == "LN", SSP == "SSP2")

ln_models <- ln_total %>%
  distinct(Model) %>%
  pull(Model) %>%
  as.character()

missing_ln_models <- setdiff(model_lookup$Model, ln_models)
if (length(missing_ln_models) > 0) {
  stop(
    "No SSP2-LN air-pollution data found for: ",
    paste(missing_ln_models, collapse = ", ")
  )
}

aim_total <- air_total %>%
  filter(Model == "AIM")

aim_ln <- ln_total %>%
  filter(Model == "AIM")

aim_categories <- aim_total %>%
  distinct(ScenarioCategory) %>%
  pull(ScenarioCategory) %>%
  as.character()
missing_aim_categories <- setdiff(c("H", "HL", "M", "ML", "L", "VL", "LN"), aim_categories)
if (length(missing_aim_categories) > 0) {
  warning(
    "AIM does not contain the following ScenarioMIP categories in the downloaded file: ",
    paste(missing_aim_categories, collapse = ", ")
  )
}

ln_range <- ln_total %>%
  group_by(Panel, Pollutant, Unit, Year) %>%
  summarise(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

theme_air <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain", size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0)
  )

highlight_color <- "#100060"

# Figure 1: LN in the context of all available AIM scenarios, with the full
# SSP2-LN multi-model range shown as a shaded area.
p_position <- ggplot() +
  geom_ribbon(
    data = ln_range,
    aes(x = Year, ymin = ymin, ymax = ymax, group = Panel),
    fill = "grey70",
    alpha = 0.3
  ) +
  geom_line(
    data = aim_total,
    aes(
      x = Year,
      y = Value,
      group = interaction(ScenarioID, Model),
      colour = ScenarioCategory,
      linetype = SSP
    ),
    linewidth = 0.55,
    alpha = 0.75
  ) +
  geom_line(
    data = aim_ln,
    aes(x = Year, y = Value, group = ScenarioID),
    colour = highlight_color,
    linewidth = 1.25
  ) +
  facet_wrap(vars(Panel), scales = "free_y", ncol = 4) +
  scale_colour_manual(values = scenario_colors, drop = TRUE) +
  scale_linetype_manual(
    values = c(SSP1 = "dashed", SSP2 = "solid", SSP3 = "dotdash", SSP4 = "longdash", SSP5 = "twodash")
  ) +
  scale_x_continuous(breaks = seq(2020, 2100, by = 20)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Year",
    y = "Global emissions",
    colour = "Scenario category",
    linetype = "SSP",
    caption = paste0(
      "All panels show World totals on a common decadal time grid. ",
      "The grey shaded area shows the full SSP2-LN multi-model range; ",
      "AIM-LN is highlighted in purple."
    )
  ) +
  theme_air

# Build a mutually exclusive top-level sector decomposition. Some models report
# an "Energy and Industrial Processes" aggregate and WITCH reports Industrial
# Processes as a subset already embedded in Energy. Industrial Processes is
# therefore capped at the part of Total not already assigned to the other
# top-level sectors. This prevents double counting while retaining the finest
# common decomposition available across models.
composition_wide <- air %>%
  select(
    ModelRaw, Model, ScenarioRaw, ScenarioCategory, SSP, ScenarioID,
    Year, Pollutant, Panel, Unit, VariableTotal, Component, Value
  ) %>%
  pivot_wider(names_from = Component, values_from = Value) %>%
  filter(ScenarioCategory == "LN", SSP == "SSP2") %>%
  mutate(
    AFOLU = replace_na(AFOLU, 0),
    Energy = replace_na(Energy, 0),
    ProductUse = replace_na(`Product Use`, 0),
    Waste = replace_na(Waste, 0),
    OtherReported = replace_na(Other, 0),
    IndustrialProcessesReported = case_when(
      !is.na(`Industrial Processes`) ~ pmax(`Industrial Processes`, 0),
      !is.na(`Energy and Industrial Processes`) ~
        pmax(`Energy and Industrial Processes` - Energy, 0),
      TRUE ~ 0
    ),
    AssignedBeforeIndustrial = AFOLU + Energy + ProductUse + Waste + OtherReported,
    IndustrialProcesses = pmin(
      IndustrialProcessesReported,
      pmax(Total - AssignedBeforeIndustrial, 0)
    ),
    UnallocatedResidual = pmax(
      Total - AssignedBeforeIndustrial - IndustrialProcesses,
      0
    ),
    OtherResidual = OtherReported + UnallocatedResidual,
    EnergyShare = 100 * Energy / Total,
    PartitionTotal =
      AFOLU + Energy + IndustrialProcesses + ProductUse + Waste + OtherResidual
  )

partition_errors <- composition_wide %>%
  filter(
    !is.finite(Total) |
      Total <= 0 |
      abs(PartitionTotal - Total) > pmax(0.001, 0.0001 * abs(Total))
  )
if (nrow(partition_errors) > 0) {
  stop("The harmonized sector decomposition does not reconcile with Total emissions.")
}

sector_labels <- c(
  AFOLU = "AFOLU",
  Energy = "Energy",
  IndustrialProcesses = "Industrial processes",
  ProductUse = "Product use",
  Waste = "Waste",
  OtherResidual = "Other"
)

sector_colors <- c(
  "AFOLU" = "#4DAF4A",
  "Energy" = "#377EB8",
  "Industrial processes" = "#FF7F00",
  "Product use" = "#984EA3",
  "Waste" = "#A65628",
  "Other" = "grey70"
)

composition <- composition_wide %>%
  select(
    ModelRaw, Model, ScenarioRaw, ScenarioCategory, SSP, ScenarioID,
    Year, Pollutant, Panel, Unit, VariableTotal, Total,
    all_of(names(sector_labels))
  ) %>%
  pivot_longer(
    cols = all_of(names(sector_labels)),
    names_to = "SectorKey",
    values_to = "Emissions"
  ) %>%
  mutate(
    Sector = recode(SectorKey, !!!sector_labels),
    Sector = factor(Sector, levels = unname(sector_labels)),
    Share = 100 * Emissions / Total
  )

invalid_shares <- composition %>%
  group_by(Model, ScenarioID, Year, Pollutant) %>%
  summarise(
    ShareTotal = sum(Share),
    MinShare = min(Share),
    .groups = "drop"
  ) %>%
  filter(
    !is.finite(ShareTotal) |
      abs(ShareTotal - 100) > 0.01 |
      MinShare < -0.001
  )
if (nrow(invalid_shares) > 0) {
  stop("Invalid sector shares were found after harmonization.")
}

composition_plot <- composition %>%
  filter(Model == "AIM")

p_composition <- ggplot(
  composition_plot,
  aes(x = Year, y = Emissions, fill = Sector)
) +
  geom_area(
    colour = NA,
    linewidth = 0,
    alpha = 1
  ) +
  geom_line(
    data = aim_ln,
    aes(x = Year, y = Value),
    inherit.aes = FALSE,
    colour = "grey20",
    linewidth = 0.45
  ) +
  facet_wrap(vars(Panel), scales = "free_y", ncol = 4) +
  scale_fill_manual(values = sector_colors, drop = FALSE) +
  scale_x_continuous(breaks = seq(2020, 2100, by = 20)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    caption = paste0(
      "Other combines reported Other emissions and any unallocated remainder. ",
      "Overlapping Energy and Industrial Processes aggregates are counted once."
    ),
    x = "Year",
    y = "Global emissions",
    fill = "Sector"
  ) +
  theme_air +
  theme(
    text = element_text(size = 18),
    axis.title = element_text(size = 16.5),
    axis.text = element_text(size = 13.5),
    panel.spacing = unit(0.9, "lines"),
    strip.text = element_text(face = "plain", size = 15),
    legend.title = element_text(face = "bold", size = 18),
    legend.text = element_text(size = 16),
    plot.caption = element_text(size = 13.5, hjust = 0)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Tables for checking and reporting the plotted positions.
rank_years <- c(2050, 2100)

aim_positions <- aim_total %>%
  filter(Year %in% rank_years) %>%
  group_by(Pollutant, Unit, Year) %>%
  mutate(
    AIM_within_rank = min_rank(Value),
    AIM_within_n = n_distinct(interaction(ScenarioID, ScenarioRaw))
  ) %>%
  ungroup() %>%
  filter(ScenarioCategory == "LN", SSP == "SSP2") %>%
  select(
    Pollutant, Unit, Year,
    AIM_LN_emissions = Value,
    AIM_within_rank, AIM_within_n
  )

multimodel_positions <- ln_total %>%
  filter(Year %in% rank_years) %>%
  group_by(Pollutant, Unit, Year) %>%
  mutate(
    AIM_multimodel_rank = min_rank(Value),
    AIM_multimodel_n = n_distinct(Model)
  ) %>%
  ungroup() %>%
  filter(Model == "AIM") %>%
  select(
    Pollutant, Unit, Year,
    AIM_multimodel_rank, AIM_multimodel_n,
    LN_multimodel_min = Value
  ) %>%
  select(-LN_multimodel_min) %>%
  left_join(
    ln_total %>%
      filter(Year %in% rank_years) %>%
      group_by(Pollutant, Unit, Year) %>%
      summarise(
        LN_multimodel_min = min(Value, na.rm = TRUE),
        LN_multimodel_max = max(Value, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("Pollutant", "Unit", "Year")
  )

aim_2020 <- aim_ln %>%
  filter(Year == 2020) %>%
  select(Pollutant, Unit, AIM_2020 = Value)

aim_composition <- composition_wide %>%
  filter(Model == "AIM", Year %in% rank_years) %>%
  select(Pollutant, Unit, Year, AIM_energy_share = EnergyShare)

position_summary <- aim_positions %>%
  left_join(
    multimodel_positions,
    by = c("Pollutant", "Unit", "Year")
  ) %>%
  left_join(aim_2020, by = c("Pollutant", "Unit")) %>%
  left_join(aim_composition, by = c("Pollutant", "Unit", "Year")) %>%
  mutate(
    AIM_change_from_2020_pct = 100 * (AIM_LN_emissions / AIM_2020 - 1),
    PollutantOrder = match(as.character(Pollutant), pollutant_lookup$Pollutant)
  ) %>%
  arrange(Year, PollutantOrder) %>%
  select(-PollutantOrder)

composition_table <- composition %>%
  filter(Model == "AIM") %>%
  mutate(
    PollutantOrder = match(as.character(Pollutant), pollutant_lookup$Pollutant),
    ModelOrder = as.integer(Model)
  ) %>%
  arrange(Year, PollutantOrder, ModelOrder, Sector) %>%
  select(
    Model, ScenarioRaw, SSP, Year, Pollutant, Unit,
    Total, Sector, Emissions, Share
  )

processed_plot_data <- air %>%
  mutate(
    Model = as.character(Model),
    ScenarioCategory = as.character(ScenarioCategory),
    Pollutant = as.character(Pollutant),
    Panel = as.character(Panel)
  ) %>%
  arrange(ModelOrder, ScenarioCategory, SSP, PollutantOrder, Variable, Year)

write_excel_csv(
  position_summary,
  file.path(table_dir, "Air_Pollutant_Position_Summary.csv")
)
write_excel_csv(
  composition_table,
  file.path(table_dir, "Air_Pollutant_LN_Composition.csv")
)
write_excel_csv(
  processed_plot_data,
  file.path(table_dir, "Air_Pollutant_Processed_Data.csv")
)

save_plot <- function(plot, stem, width, height) {
  png_path <- file.path(figure_dir, paste0(stem, ".png"))
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  ggsave(
    png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 900,
    device = ragg::agg_png,
    bg = "white"
  )
  pdf_saved_path <- tryCatch(
    {
      ggsave(
        pdf_path,
        plot = plot,
        width = width,
        height = height,
        units = "in",
        device = if (capabilities("cairo")) cairo_pdf else pdf
      )
      pdf_path
    },
    error = function(e) {
      fallback_pdf_path <- file.path(
        figure_dir,
        paste0(stem, "_updated.pdf")
      )
      warning(
        "Could not overwrite ", basename(pdf_path),
        "; writing ", basename(fallback_pdf_path), " instead. ",
        conditionMessage(e),
        call. = FALSE
      )
      ggsave(
        fallback_pdf_path,
        plot = plot,
        width = width,
        height = height,
        units = "in",
        device = if (capabilities("cairo")) cairo_pdf else pdf
      )
      fallback_pdf_path
    }
  )
  c(png_path, pdf_saved_path)
}

position_paths <- save_plot(
  p_position,
  "Figure1_Air_Pollutant_Positions",
  width = 13,
  height = 7
)
composition_paths <- save_plot(
  p_composition,
  "Figure2_Air_Pollutant_LN_Composition",
  width = 17,
  height = 8.5
)

message("\nCompleted successfully.")
message("Figures:")
message("  ", paste(c(position_paths, composition_paths), collapse = "\n  "))
message("Tables:")
message("  ", file.path(table_dir, "Air_Pollutant_Position_Summary.csv"))
message("  ", file.path(table_dir, "Air_Pollutant_LN_Composition.csv"))
message("  ", file.path(table_dir, "Air_Pollutant_Processed_Data.csv"))
