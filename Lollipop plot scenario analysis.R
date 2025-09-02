# deterministic scenario analysis

library(ggplot2)
library(dplyr)

set.seed(42)

# --- Deterministic scenario results ---
scenario_results <- data.frame(
  scenario = paste("Scenario", LETTERS[1:10]),
  incremental_cost = runif(10, 8000, 20000),
  incremental_qaly = runif(10, 0.4, 1.0)
)

wtp <- 25000

scenario_results <- scenario_results %>% mutate(NMB = incremental_qaly * wtp - incremental_cost)

# Add base-case scenario with fixed INMB = 1500
base_case <- data.frame(
  scenario = "Base case",
  incremental_cost = NA,
  incremental_qaly = NA,
  NMB = 1500
)

scenario_results <- bind_rows(scenario_results, base_case)

# Reorder scenarios by deterministic NMB
scenario_results$scenario <- factor(scenario_results$scenario, 
                                    levels = scenario_results$scenario[order(scenario_results$NMB)])

# --- Plot (deterministic only) ---
ggplot() +
  # Lollipop sticks
  geom_segment(data = scenario_results,
               aes(x = scenario, xend = scenario, y = 0, yend = NMB, color = NMB),
               size = 1) +
  
  # Deterministic points
  geom_point(data = scenario_results,
             aes(x = scenario, y = NMB, color = NMB,
                 shape = ifelse(scenario == "Base case", "Base case", "Scenario")),
             size = 4) +
  
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # Color and shape scales
  scale_color_gradient2(low = "red", mid = "gray90", high = "forestgreen", midpoint = 0) +
  scale_shape_manual(values = c("Scenario" = 16, "Base case" = 17)) +
  
  # Labels and theme
  labs(title = paste0("Incremental Net Monetary Benefit (WTP = €", wtp, ")"),
       x = "Scenario", y = "Incremental NMB (€)") +
  theme_minimal() +
  coord_flip() +
  geom_text(data = scenario_results,
            aes(x = scenario, y = NMB, label = round(NMB, 0)),
            hjust = ifelse(scenario_results$NMB > 0, -0.3, 1.3),
            color = "black", size = 3) +
  theme(legend.position = "none")

# Stepwise ICERs

library(ggplot2)
library(dplyr)

# --- Inputs you can edit ---
base_nmb <- 1500
# Five incremental changes to the initial base-case (in €):
changes <- c(200, -350, 500, -100, 250)
# ---------------------------

stopifnot(length(changes) == 5)

# Step labels with explicit final naming
step_labels <- c(
  "Initial base-case",
  paste0("Step ", 1:4),
  "Step 5 (Final base-case)"
)

# Cumulative INMBs for initial + 5 steps
nmb_values <- base_nmb + cumsum(c(0, changes))

plot_df <- tibble(
  step = factor(step_labels, levels = step_labels),
  NMB  = nmb_values
) %>%
  mutate(
    # Shape: triangles for initial & final, circles for intermediates
    shape_group = case_when(
      step == "Initial base-case"        ~ "Initial/Final",
      step == "Step 5 (Final base-case)" ~ "Initial/Final",
      TRUE                               ~ "Intermediate"
    ),
    # Label position
    hjust_pos = if_else(NMB > 0, -0.3, 1.3)
  )

# --- Plot ---
ggplot(plot_df) +
  geom_segment(aes(x = step, xend = step, y = 0, yend = NMB, color = NMB), size = 1) +
  geom_point(aes(x = step, y = NMB, color = NMB, shape = shape_group), size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_text(aes(x = step, y = NMB, label = round(NMB, 0), hjust = hjust_pos),
            color = "black", size = 3) +
  scale_color_gradient2(low = "red", mid = "gray90", high = "forestgreen", midpoint = 0) +
  scale_shape_manual(values = c("Intermediate" = 16, "Initial/Final" = 17)) +
  labs(
    title = "Incremental Net Monetary Benefit – Cumulative Changes",
    subtitle = paste0(
      "Base = €", format(base_nmb, big.mark = ","), 
      " | Changes = [", paste0(changes, collapse = ", "), "]"
    ),
    x = "Cumulative step", y = "INMB (€)"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none")


# Probabilistic Scenario analysis 

library(ggplot2)
library(dplyr)

set.seed(42)

# --- Deterministic scenario results ---
scenario_results <- data.frame(
  scenario = paste("Scenario", LETTERS[1:10]),
  incremental_cost = runif(10, 8000, 20000),
  incremental_qaly = runif(10, 0.4, 1.0)
)

wtp <- 25000

scenario_results <- scenario_results %>%
  mutate(NMB = incremental_qaly * wtp - incremental_cost)

# Add base-case scenario with fixed INMB = 1500
base_case <- data.frame(
  scenario = "Base case",
  incremental_cost = NA,
  incremental_qaly = NA,
  NMB = 1500
)

scenario_results <- bind_rows(scenario_results, base_case)

# Reorder scenarios by deterministic NMB
scenario_results$scenario <- factor(scenario_results$scenario, 
                                    levels = scenario_results$scenario[order(scenario_results$NMB)])

# --- PSA Simulation for each scenario ---
n_psa <- 1000
psa_data <- scenario_results %>%
  filter(scenario != "Base case") %>%
  rowwise() %>%
  do({
    data.frame(
      scenario = .$scenario,
      NMB = rnorm(n_psa, mean = .$NMB, sd = abs(.$NMB) * 0.6) # wide densities
    )
  })

# Add base case PSA with smaller variability
psa_base <- data.frame(
  scenario = "Base case",
  NMB = rnorm(n_psa, mean = 1500, sd = 300)
)

psa_data <- bind_rows(psa_data, psa_base)

# --- Plot ---
ggplot() +
  # PSA densities (cotton candy look)
  geom_violin(data = psa_data,
              aes(x = scenario, y = NMB, fill = scenario),
              alpha = 0.3, color = NA, scale = "width") +
  
  # Lollipop sticks
  geom_segment(data = scenario_results,
               aes(x = scenario, xend = scenario, y = 0, yend = NMB, color = NMB),
               size = 1) +
  
  # Deterministic points
  geom_point(data = scenario_results,
             aes(x = scenario, y = NMB, color = NMB,
                 shape = ifelse(scenario == "Base case", "Base case", "Scenario")),
             size = 4) +
  
  # Horizontal reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  # Color and shape scales
  scale_color_gradient2(low = "red", mid = "gray90", high = "forestgreen", midpoint = 0) +
  scale_shape_manual(values = c("Scenario" = 16, "Base case" = 17)) +
  scale_fill_manual(values = rep("skyblue", length(unique(psa_data$scenario)))) +
  
  # Labels and theme
  labs(title = paste0("Incremental Net Monetary Benefit (WTP = €", wtp, ")"),
       x = "Scenario", y = "Incremental NMB (€)") +
  theme_minimal() +
  coord_flip() +
  geom_text(data = scenario_results,
            aes(x = scenario, y = NMB, label = round(NMB, 0)),
            hjust = ifelse(scenario_results$NMB > 0, -0.3, 1.3),
            color = "black", size = 3) +
  theme(legend.position = "none")
