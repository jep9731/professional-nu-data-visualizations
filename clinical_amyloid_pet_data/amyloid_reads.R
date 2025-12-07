## Amyloid Discrepant Visual SUVr reads

# Remove all files
rm(list = ls())

# Import libraries --------------------------------------------------------
library(tidyverse)
library(readr)
library(writexl)
library(ggrepel)

# Import data -------------------------------------------------------------
PET_metrics <- read.csv('/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/data/ImagingCoreDatabase-ICPETMetrics_DATA_LABELS_2025-11-25_0920.csv')
nrow(PET_metrics)

stub <- read.csv('/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/data/ImagingCoreDatabase-Stub_DATA_LABELS_2025-11-25_0830.csv')
nrow(stub)

SCAN_aPET_GAAIN <- read.csv('/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/data/v_ucberkeley_amyloid_mrifree_gaain.csv')
nrow(SCAN_aPET_GAAIN)

# Clean data --------------------------------------------------
# Stub
stub_clean <- stub %>%
  select("Ripple.Global.ID", "PTID") %>%
  rename(., global_id = Ripple.Global.ID)
stub_clean

# PET metrics data
PET_metrics_clean <- PET_metrics %>%
  select("Ripple.Global.ID", "Amyloid.PET.scan.date.of.occurence", "Amyloid.PET.Study.Affiliation..Please.check.all.that.apply.", 
         "Amyloid.PET.Clinician.read..Rater..1.", "Amyloid.PET.Clinician.read.source..Rater..1.",
         "Amyloid.PET.Clinician.read..Rater..2.", "Amyloid.PET.Clinician.read.source..Rater..2.",
         "Amyloid.PET.read.CONSENSUS..All...Researcher...Clinician.", "Amyloid.PET.Mean.SUVr...Susan.Landau.method") %>% # Select important columns to keep
  rename(., global_id = Ripple.Global.ID, scan_date = Amyloid.PET.scan.date.of.occurence,
         study_affiliation = Amyloid.PET.Study.Affiliation..Please.check.all.that.apply.,clinician_read_1 = Amyloid.PET.Clinician.read..Rater..1., 
         clinician_rater_1 = Amyloid.PET.Clinician.read.source..Rater..1., clinician_read_2 = Amyloid.PET.Clinician.read..Rater..2., 
         clinician_rater_2 = Amyloid.PET.Clinician.read.source..Rater..2., consensus_read = Amyloid.PET.read.CONSENSUS..All...Researcher...Clinician., 
         mean_SUVr = Amyloid.PET.Mean.SUVr...Susan.Landau.method) %>% # Rename columns for easier readability
  mutate(
    method = rep("IBC", nrow(PET_metrics)) # Creates new column with method type
  ) %>%
  filter(scan_date > "2020-12-31" & study_affiliation == "ADRC Imaging Core") %>% # Filter by date after 2020 (SCAN started in 2021)
  mutate(final_read = case_when(clinician_read_2 == "" ~ clinician_read_1, # if clinician_read_2 is blank, put the clinician read 1 value 
                                clinician_read_1 != clinician_read_2 ~ consensus_read, # if discrepant read, put consensus read
                                TRUE ~ clinician_read_1), # everything else, put clinician read 1 value
         ) %>% # place the new column after the consensus read column
  select(-study_affiliation) # remove study affiliation column
PET_metrics_clean

# Clean SCAN GAAIN method
SCAN_aPET_GAAIN_clean <- SCAN_aPET_GAAIN %>%
  select(PTID, AMYLOID_STATUS, SCANDATE, GAAIN_SUMMARY_SUVR, CENTILOIDS) %>% # Selects necessary columns
  rename(mean_SUVr = GAAIN_SUMMARY_SUVR, scan_date = SCANDATE) %>% # Renames SUVr and scan date columns
  mutate(
    method = rep("GAAIN", nrow(SCAN_aPET_GAAIN)), 
    .after = PTID
    ) %>% # Creates new column with method type
  mutate(
    final_read = ifelse(AMYLOID_STATUS == 0, "Negative", "Positive"), 
    .after = scan_date
    ) %>% # Creates new column with read
  select(-AMYLOID_STATUS)
SCAN_aPET_GAAIN_clean

# Determine number of reads that only have 1 read -------------------------
one_clinical_read <- PET_metrics_clean[which(PET_metrics_clean$clinician_read_2 == ""), ]
one_clinical_read

table(number_of_reads_by_one_clinician = nrow(one_clinical_read))

# Determine visual and SUVr discrepancies ---------------------------------
discrepant_reads_neg <- PET_metrics_clean[which(PET_metrics_clean$final_read == "Negative" & 
                                                  PET_metrics_clean$mean_SUVr >= 1.08), ] # Negative discrepancies
discrepant_reads_neg

discrepant_reads_pos <- PET_metrics_clean[which(PET_metrics_clean$final_read == "Positive" & 
                                                  PET_metrics_clean$mean_SUVr <= 1.08), ] # Positive discrepancies
discrepant_reads_pos

discrepant_reads_final <- rbind(discrepant_reads_neg, discrepant_reads_pos) # Combine into a dataframe
discrepant_reads_final
table(number_of_visual_suvr_discrepancies = nrow(discrepant_reads_final))

# Create final IC metrics dataset
PET_metrics_final <- PET_metrics_clean %>%
  select(global_id, scan_date, method, final_read, mean_SUVr)

# Merge datasets
IC_merged <- inner_join(PET_metrics_final, stub_clean, by = "global_id") %>%
  select(global_id, PTID, method, everything())

merged <- merge(IC_merged, SCAN_aPET_GAAIN_clean, by = intersect(names(PET_metrics_final), names(SCAN_aPET_GAAIN_clean)), 
                all = T,
                suffixes = c("_IC", "_GAAIN")) %>%
  fill(PTID_GAAIN, .direction = "down") %>%
  select(-c(PTID_IC, global_id)) %>%
  rename(PTID = PTID_GAAIN)

# SUVr plots -------------------------------------------------------------
# Plot 1: Imaging SUVr values vs Visual Reads
plot_1 <- ggplot(data = PET_metrics_final, aes(x = global_id, y = mean_SUVr)) + # Add data and mapping; x = global_id, y = SUVr
  geom_point(size = 2, aes(shape = final_read, color = final_read)) + # Create plot and distinguishing shape and color by clinical read
  geom_hline(yintercept = 1.08, color = "black", linetype = "dashed", linewidth = .5) + # Add horizontal line at SUVr cutoff of 1.08
  geom_point(data = PET_metrics_final[PET_metrics_final$final_read == "Negative" & PET_metrics_final$mean_SUVr >= 1.08, ], 
             aes(shape = final_read, color = final_read), pch=0, fill=NA, size=4, color="red", stroke=1) + # Creates circle around all negative reads that are greater than or equal 1.08
  geom_point(data = PET_metrics_final[PET_metrics_final$final_read == "Positive" & PET_metrics_final$mean_SUVr <= 1.08, ],
             aes(shape = final_read, color = final_read), pch=0, fill=NA, size=4, color="red", stroke=1) + # Creates circle around all positive reads that are less than or equal 1.08
  geom_text(data = PET_metrics_final[PET_metrics_final$final_read == "Negative" & PET_metrics_final$mean_SUVr >= 1.08, ],
            aes(label = mean_SUVr, x = global_id, y = mean_SUVr), position = position_dodge(0.9), vjust = -.95, size = 3.5) + # add data labels for neg/SUVr >= 1.08 discrepancies
  geom_text(data = PET_metrics_final[PET_metrics_final$final_read == "Positive" & PET_metrics_final$mean_SUVr <= 1.08, ],
            aes(label = mean_SUVr, x = global_id, y = mean_SUVr), position = position_dodge(0.9), vjust = -.95, size = 3.5) + # add data labels for pos/SUVr <= 1.08 discrepancies
  labs(title = "IBC Amyloid Mean SUVr by Clinical Visual Read", # Change title of plot
       x = "", # Remove x axis label
       y = "Mean SUVr", # Change y axis label
       color = "Clinical Read", # Change legend color title
       shape = "Clinical Read",
       point = "Visual/Quantitative Discrepancy") + # Change legend shape title
  scale_y_continuous(breaks = seq(0, 2.2, by = .2)) + # Change y axis tick values
  scale_color_manual(values = c("darkred", "darkblue"), # Change color of points
                     breaks = c("Negative", "Positive")) + # Add labels
  scale_shape_manual(values = c(19, 17), # Change shape of points
                     breaks = c("Negative", "Positive")) + # Add labels
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "inside", # Move legend inside graph
        legend.position.inside = c(.9, .92), # Change position of legend
        panel.background = element_blank(), # Remove plot panel background
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.border = element_blank(), # Remove panel border
        axis.ticks.x = element_blank(), # Remove x axis
        axis.line.x = element_blank(), # Remove x line
        axis.text.x = element_blank(), # Remove x values
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )
plot_1 # Show plot

# Save plot
ggsave("IC_amyloid_metrics.png", 
       plot = plot_1, width = 12, height = 6, units = "in", dpi = 300,
       path = "/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/outputs/"
       )

# Plot 2: Visual Reads vs IBC and GAAIN SUVr values
merged_clean <- merged %>%
  filter(!is.na(mean_SUVr))

plot_2 <- ggplot(data = merged_clean, aes(x = scan_date, y = mean_SUVr, group = method)) + # Add data and mapping; x = global_id, y = SUVr
  geom_point(size = 2, aes(color = final_read)) + # Create plot and distinguishing shape and color by clinical read
  geom_hline(aes(yintercept = 1.08, linetype = "IBC cutoff"), linewidth = .5, color = "black") + # Add horizontal line for IBC SUVr cutoff of 1.08
  geom_hline(aes(yintercept = 1.12, linetype = "SCAN cutoff"), linewidth = .5, color = "red") + # Add horizontal line at SCAN SUVr cutoff of 1.12
  geom_point(data = merged_clean[merged_clean$method == "GAAIN" & merged_clean$final_read == "Negative" &
                                   merged_clean$mean_SUVr >= 1.12, ],
             aes(shape = final_read, color = final_read),
             pch = 1, fill = NA, size = 4, color = "blue", stroke = 1) + # GAAIN negative but SUVr >= 1.12
  geom_text(data = merged_clean[merged_clean$method == "GAAIN" & merged_clean$final_read == "Negative" & 
                                  merged_clean$mean_SUVr >= 1.12, ],
            aes(label = mean_SUVr, x = scan_date, y = mean_SUVr), position = position_dodge(0.9), 
            vjust = -.95, size = 3.5) + # add data labels for discrepancies
  geom_point(data = merged_clean[merged_clean$method == "GAAIN" & merged_clean$final_read == "Positive" &
                                   merged_clean$mean_SUVr <= 1.12, ],
             aes(shape = final_read, color = final_read),
             pch = 1, fill = NA, size = 4, color = "blue", stroke = 1) + # GAAIN positive but SUVr <= 1.12
  geom_text(data = merged_clean[merged_clean$method == "GAAIN" & merged_clean$final_read == "Positive" & 
                                  merged_clean$mean_SUVr <= 1.12, ],
            aes(label = mean_SUVr, x = scan_date, y = mean_SUVr), position = position_dodge(0.9), 
            vjust = -.95, size = 3.5) + # add data labels for discrepancies
  geom_point(data = merged_clean[merged_clean$method == "IBC" & merged_clean$final_read == "Negative" &
                                   merged_clean$mean_SUVr >= 1.08, ], 
             aes(shape = final_read, color = final_read), 
             pch = 1, fill = NA, size = 4, color = "green", stroke = 1) + # IBC negative but SUVr >= 1.08
  geom_text_repel(data = merged_clean[merged_clean$method == "IBC" & merged_clean$final_read == "Negative" & 
                                  merged_clean$mean_SUVr >= 1.08, ],
            aes(label = mean_SUVr, x = scan_date, y = mean_SUVr), nudge_y = 0.05, size = 3.5) + # add data labels for discrepancies
  geom_point(data = merged_clean[merged_clean$method == "IBC" & merged_clean$final_read == "Positive" &
                                   merged_clean$mean_SUVr <= 1.08, ],
             aes(shape = final_read, color = final_read),
             pch = 1, fill = NA, size = 4, color = "green", stroke = 1) + #IBC positive but SUVr <= 1.08
  geom_text_repel(data = merged_clean[merged_clean$method == "IBC" & merged_clean$final_read == "Positive" & 
                                  merged_clean$mean_SUVr <= 1.08, ],
            aes(label = mean_SUVr, x = scan_date, y = mean_SUVr), nudge_y = 0.05, size = 3.5) + # add data labels for discrepancies
  facet_wrap(method ~ .) +
  labs(title = "Amyloid PET Clinical Visual Reads Between IBC & SCAN Methods", # Change title of plot
       x = "", # Remove x axis label
       y = "Mean SUVr", # Change y axis label
       color = "Clinical Read:") + # Change legend color title
  scale_y_continuous(breaks = seq(0, 2.2, by = .2)) + # Change y axis tick values
  scale_color_manual(values = c("darkred", "darkblue"), # Change color of points
                     breaks = c("Negative", "Positive")) + # Add labels
  scale_linetype_manual(name = "SUVr Cutoff:", # Change legend title
                        values = c(2, 2), # Add linetype values
                        guide = guide_legend(override.aes = list(color = c("black", "red")))) + # Override color
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "top", # Move under title
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line.x = element_blank(), # Remove x line
        axis.text.x = element_blank(), # Remove x values
        axis.ticks.x = element_blank(), # Remove x tick lines
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )
plot_2 # Show plot

# Save plot
ggsave("IC_SCAN_amyloid_metrics.png", 
       plot = plot_2, width = 12, height = 6, units = "in", dpi = 300,
       path = "/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/outputs/"
)

# Plot 3: Histogram
plot_3 <- ggplot(data = merged_clean, aes(x = mean_SUVr, group = method)) + # Add data and mapping; x = mean_SUVr, group = method
  geom_histogram(bins = 75, color = "black", position = "dodge", # Change number of bins, create black boarder, make bars dodge
                 alpha = .75, aes(fill = method)) + # Change transparency and set fill to method
  geom_vline(data = merged_clean[merged_clean$method == "IBC", ], 
             aes(xintercept = 1.08, linetype = "IBC cutoff"), linewidth = .5, color = "black") + # Add horizontal line for IBC SUVr cutoff of 1.08
  geom_vline(data = merged_clean[merged_clean$method == "GAAIN", ],
             aes(xintercept = 1.12, linetype = "SCAN cutoff"), linewidth = .5, color = "red") + # Add horizontal line at SCAN SUVr cutoff of 1.12
  facet_wrap(~factor(method, c("IBC", "GAAIN"))) + # Change order in facet wrap
  labs(title = "Amyloid PET SUVr Values Between IBC & SCAN GAAIN Method", # Change title of plot
       x = "Mean SUVr Value", # Change x axis title
       y = "Count", # Change y axis title
       fill = "Method:",
       linetype = "") + # Change legend fill title
  scale_fill_manual(values = c("darkred", "darkblue"), # Change colors of bars
                    breaks = c("IBC", "GAAIN")) + # Change order in legend
  scale_x_continuous(breaks = seq(0, 2.2, by = .2)) + # Change x axis tick values
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "top", # Move under title
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line.x = element_line(linewidth = .5, linetype = "solid"), # Change x axis line size and boldness
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size & boldness
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )
plot_3 # Show plot

# Save plot
ggsave("IC_SCAN_amyloid_hist.png", 
       plot = plot_3, width = 12, height = 6, units = "in", dpi = 300,
       path = "/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/outputs/"
)

# Centiloids --------------------------------------------------------------
merged_gaain <- merged[merged$method == "GAAIN", ]

plot_4 <- ggplot(data = merged_gaain,aes(x = scan_date, y = CENTILOIDS)) + # Add data and mapping; x = scan_date, y = centiloids
  geom_point(size = 2, aes(shape = final_read, color = final_read)) + # Create plot and distinguishing shape and color by clinical read
  geom_hline(yintercept = 20, color = "black", linetype = "dashed", linewidth = .5) + # Add horizontal line at SUVr cutoff of 1.08
  geom_point(data = merged_gaain[merged_gaain$final_read == "Negative" & merged_gaain$CENTILOIDS > 10, ], 
             aes(shape = final_read, color = final_read), pch=0, fill=NA, size=3.5, color="red", stroke=1) + # Creates circle around all negative reads that are greater than 10 CL
  geom_text_repel(data = merged_gaain[merged_gaain$final_read == "Negative" & merged_gaain$CENTILOIDS > 10, ],
            aes(label = CENTILOIDS, x = scan_date, y = CENTILOIDS), nudge_y = 0.05, size = 3.5) + # add data labels for discrepancies
  geom_point(data = merged_gaain[merged_gaain$final_read == "Positive" & merged_gaain$CENTILOIDS < 20, ],
             aes(shape = final_read, color = final_read), pch=0, fill=NA, size=3.5, color="red", stroke=1) + # Creates circle around all positive reads that are less than 20 CL
  geom_text_repel(data = merged_gaain[merged_gaain$final_read == "Positive" & merged_gaain$CENTILOIDS < 20, ],
            aes(label = CENTILOIDS, x = scan_date, y = CENTILOIDS), nudge_y = 0.05, size = 3.5) + # add data labels for discrepancies
  labs(title = "GAAIN Amyloid Centiloids by Clinical Visual Read", # Change title of plot
       x = "", # Remove x axis label
       y = "Centiloids Values", # Change y axis label
       color = "Clinical Read", # Change legend color title
       shape = "Clinical Read",
       point = "Visual/Quantitative Discrepancy") + # Change legend shape title
  scale_color_manual(values = c("darkred", "darkblue"), # Change color of points
                     breaks = c("Negative", "Positive")) + # Add labels
  scale_shape_manual(values = c(19, 17), # Change shape of points
                     breaks = c("Negative", "Positive")) + # Add labels
  theme(legend.box.background = element_rect(), # Add legend box
        legend.background = element_blank(), # Remove legend background
        legend.key = element_blank(), # Remove box around colors
        legend.position = "inside", # Move legend inside graph
        legend.position.inside = c(.95, .92), # Change position of legend
        panel.background = element_blank(), # Remove plot panel background
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.border = element_blank(), # Remove panel border
        axis.ticks.x = element_blank(), # Remove x axis
        axis.line.x = element_blank(), # Remove x line
        axis.text.x = element_blank(), # Remove x values
        axis.line.y = element_line(linewidth = .5, linetype = "solid"), # Change y axis line size
        plot.title = element_text(size = 14, hjust = .5, face = "bold") # Change title size and position
  )
plot_4 # Show plot

# Save plot
ggsave("gaain_centiloids.png", 
       plot = plot_4, width = 12, height = 6, units = "in", dpi = 300,
       path = "/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/outputs/"
)

# Export tables -----------------------------------------------------------
writexl::write_xlsx(one_clinical_read, "/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/outputs/one_clinical_read_all.xlsx")
writexl::write_xlsx(discrepant_reads_final, "/Volumes/fsmresfiles/CNADC/Imaging_Core/Imaging/imaging_projects/Individuals/Joshua_Pasaye/R_scripts/Amyloid_PET/Discrepant reads/outputs/all_visual_quantitative discrepancies.xlsx")
