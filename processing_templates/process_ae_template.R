# =============================================================================
# NAME: Template for creation of safety/AE analysis dataset for one study
# CREATED:     20-JAN-2026
# MODIFIED:     
# STUDY ID:
# =============================================================================

# LOAD CODE MODULES -This script relies on functions that are defined in separate modules /demonstration/utils/

setwd("/fh/fast/gilbert_p/agartlan/gitrepo/tb_pacts_tools")
devtools::load_all()

# CHANGE THIS FOR SPECIFIC STUDY/STUDIES
study_identifiers = c("TB-1037") # 1037 is the SimpliciTB study

# 0. Data Preparation -----------------------------------------------------
# Load packages
require(magrittr)
require(summarytools)
require(dplyr)

library(lubridate)
require(dplyr)
library(openxlsx)
library(jsonlite)

# tidyverse and data manipulation
library("knitr")
library("kableExtra")
library("readxl")
library("stringr")
library("tidyverse")
library("dplyr")
library("writexl")
library("openxlsx")

# table package
library("tibble")
library("tidyr")
library("gtsummary")

# plot packages
library("fmsb")
library("ggradar") # devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library("ggpubr")
library("ctrdata")


ae = read.csv(file.path(tbpacts_csv, 'ae.csv')) %>% 
  filter(STUDYID %in% study_identifiers )
dm = read.csv(file.path(tbpacts_csv, 'dm.csv')) %>% 
  filter(STUDYID %in% study_identifiers )

# IF YOU WANT TO SEE WHAT THE VARIABLES ARE:
ae_labels <- get_cdisc_labels("ae", metadata_folder = metadata_folder)
dm_labels <- get_cdisc_labels("dm", metadata_folder = metadata_folder)

# REMOVE ANY VARIABLE NAMES THAT COMPLETELY NA or "", set column names to tolower
ae  = rm_cols(ae,tolower=TRUE) 
dm  = rm_cols(dm,tolower=TRUE) 

summarytools::label(ae)<-"Adverse event tracking"
summarytools::label(dm)<-"Age, Gender, Race, Trial Arm data"

CDISC_labels = ae_labels
CDISC_labels$Variable <- tolower(CDISC_labels$Variable)
for (i in 1:ncol(ae)){
  if(any(CDISC_labels$Variable==names(ae)[i])){
    summarytools::label(ae[,names(ae)[i]]) <- CDISC_labels[which(CDISC_labels$Variable==names(ae)[i]),"Label"]
  }
}
CDISC_labels = dm_labels
CDISC_labels$Variable <- tolower(CDISC_labels$Variable)
for (i in 1:ncol(dm)){
  if(any(CDISC_labels$Variable==names(dm)[i])){
    summarytools::label(dm[,names(dm)[i]]) <- CDISC_labels[which(CDISC_labels$Variable==names(dm)[i]),"Label"]
  }
}

### Subsetting the data to the variables of interest (using the CDISC AE dataset)

# usubjid - Unique Subject Identifier
# aeseq - Sequence Number
# aestdy - Study Date of Start of Adverse Event
# aeendy - Study Date of Start of Adverse Event
# epoch - Epoch
# aesoc - Primary System Organ Class
# aehlgt - High Level Group Term
# aeser - Serious Event
# aerel - Causality
# aeout - Outcome Of Adverse Event
# aetoxgr - Standard Toxicity Grade
# trtemerg - Treatment Emergent Flag

ae %<>% select(usubjid,aeseq,aestdy,aeendy,epoch,aesoc,aehlgt,aeser,aerel,aeout,aetoxgr,trtemerg)
ae
# Restricting to treatment emergent AEs
ae %<>% filter(trtemerg=="Y")

# Katrina Dobinda's code has the following requirements

## ae_data: Your final analysis dataset for the reporting you wish to complete 
## It must have variables of "ae_st_date", "ae_ctcae_cat", "ae_ctcae_term_value",
## "dt_consent", "ae_sae", and "ptid"
## cutoff_dt: a variable holding cutoff dates for when ae's should have started
## to be included in reporting (defaulted to dt_consent)
## Please note that cutoff_dt and ae_st_date must be formatted as dates before
## inputted

ae %<>% mutate(ae_st_date = lubridate::add_with_rollback(ymd("20180731"),days(aestdy)), # Adds study days to study start date
               ae_ctcae_cat = aesoc,
               ae_ctcae_term_value = aehlgt,
               dt_consent = ymd("20180801"), # SimpliciTB started on August 1st, 2018
               ae_sae = ifelse(aeser=="Y","Yes","No"),
               ptid = usubjid)

# 1. Preparing for the figures --------------------------------------------

# Reviewing the Treatment Arms in SimpliciTB (using the CDISC TA dataset)
dfSummary(ta)
dm$actarmcd

# Treatment Arms are...
# A: Drug Sensitive TB: BPaMZ daily for 17 weeks (4 months)  <- Experimental Arm
# B: Drug Sensitive TB: HRZE/HR combination tablets daily for 26 weeks (6 months) <- Control Arm
# C: Drug Resistant TB: BPaMZ daily for 26 weeks (6 months) <- Secondary Arm focused on drug resistant TB

# Gathering a list of participants from arms A and B (using CDISC DM)
dfSummary(dm)

ptid <- (dm %>% filter(actarmcd %in% c("REGIMEN-A","REGIMEN-B","REGIMEN-C")) %>% select(usubjid,armcd))
ptid <- split(ptid, f = ptid$armcd)

lapply(ptid,head)

# Separating AEs into treatment arm groups
# Note that {{ }} allows me to reference ptid rather than the column name ptid
ae_A <- ae %>% filter(usubjid %in% {{ptid}}$`REGIMEN-A`$usubjid)
ae_B <- ae %>% filter(usubjid %in% {{ptid}}$`REGIMEN-B`$usubjid)
ae_C <- ae %>% filter(usubjid %in% {{ptid}}$`REGIMEN-C`$usubjid)


# Running Katrina's pre_processing_ae_data function
ae_A <- pre_processing_ae_data(ae_A)
ae_B <- pre_processing_ae_data(ae_B)
ae_C <- pre_processing_ae_data(ae_C)

# Running Katrina's Single Arm Function

## processed_ae_data: your final analysis dataset for the reporting you wish to complete
## and it must have variables of "ae_ctcae_cat", "ae_ctcae_term_value",
## and "ptid"
## It must also have "ae_sae" that is either "Yes" or "No" depending on if it is serious

## ptid_ae_evaluable: this is a vector of the patient IDS who are
## evaluable for the toxicity/safety endpoint, also defined as "participants 
## included in the assessment of adverse events (that is, the denominator for
## calculating frequency of adverse events)"

## trt_name = the name/identifier of the treatment for this arm, names column
## if end goal is to use export function, ensure that this matches the template

## sae_or_other = either "Serious" or "Other", for what type of ctgov AE report you want
## please note that other simply means not serious adverse event (non-SAE)

## You can filter your datasets within this function to input into the multi-arm
## function that will combine them together

## outputs a dataframe/tibble object

ae_A_table <- ctgov_ae_tables_single_arm(processed_ae_data = ae_A,
                           ptid_ae_evaluable = ae_A$usubjid,
                           trt_name = "REGIMEN-A",
                           sae_or_other = "Other")

ae_B_table <- ctgov_ae_tables_single_arm(processed_ae_data = ae_B,
                                         ptid_ae_evaluable = ae_B$usubjid,
                                         trt_name = "REGIMEN-B",
                                         sae_or_other = "Other")

ae_C_table <- ctgov_ae_tables_single_arm(processed_ae_data = ae_C,
                                         ptid_ae_evaluable = ae_C$usubjid,
                                         trt_name = "REGIMEN-C",
                                         sae_or_other = "Other")


# I noticed that the number of events seems to be missing.
ae_A_table %>% select(1,4,6,7:9)

# I am going to try to add numEvents
ae_A_table %<>%
  left_join(ae_A %>% group_by(term) %>% summarise(n = n()),by="term") %>%
  mutate(`REGIMEN-A{numEvents}`=n) %>%
  select(-n)

# Sanity Check:
ae_A_table %>% select(1,4,6,7:9)
all(ae_A_table$`REGIMEN-A{numEvents}` >= ae_A_table$`REGIMEN-A{numSubjectsAffected}`)
all(ae_A_table$`REGIMEN-A{numEvents}` == ae_A_table$`REGIMEN-A{numSubjectsAffected}`)

ae_B_table %<>%
  left_join(ae_B %>% group_by(term) %>% summarise(n = n()),by="term") %>%
  mutate(`REGIMEN-B{numEvents}`=n) %>%
  select(-n)

ae_C_table %<>%
  left_join(ae_C %>% group_by(term) %>% summarise(n = n()),by="term") %>%
  mutate(`REGIMEN-C{numEvents}`=n) %>%
  select(-n)


# Running Katrina's Multi Arm Function

ae_multiarm_table <- ctgov_ae_tables_multi_arm(ae_arm_data_list = list(ae_A_table,ae_B_table,ae_C_table))


# Repeating this process with serious events
ae_A_table_serious <- ctgov_ae_tables_single_arm(processed_ae_data = ae_A,
                                         ptid_ae_evaluable = ae_A$usubjid,
                                         trt_name = "REGIMEN-A",
                                         sae_or_other = "Serious")

ae_B_table_serious <- ctgov_ae_tables_single_arm(processed_ae_data = ae_B,
                                         ptid_ae_evaluable = ae_B$usubjid,
                                         trt_name = "REGIMEN-B",
                                         sae_or_other = "Serious")

ae_C_table_serious <- ctgov_ae_tables_single_arm(processed_ae_data = ae_C,
                                         ptid_ae_evaluable = ae_C$usubjid,
                                         trt_name = "REGIMEN-C",
                                         sae_or_other = "Serious")

ae_A_table_serious %<>%
  left_join(ae_A %>% group_by(term) %>% summarise(n = n()),by="term") %>%
  mutate(`REGIMEN-A{numEvents}`=n) %>%
  select(-n)

ae_B_table_serious %<>%
  left_join(ae_B %>% group_by(term) %>% summarise(n = n()),by="term") %>%
  mutate(`REGIMEN-B{numEvents}`=n) %>%
  select(-n)

ae_C_table_serious %<>%
  left_join(ae_C %>% group_by(term) %>% summarise(n = n()),by="term") %>%
  mutate(`REGIMEN-C{numEvents}`=n) %>%
  select(-n)

ae_multiarm_table_serious <- ctgov_ae_tables_multi_arm(ae_arm_data_list = list(ae_A_table_serious,
                                                                               ae_B_table_serious,
                                                                               ae_C_table_serious))


# 2. Adapting Dobinda's code -----------------------------------

renaming_dat <- read_xlsx(file.path(metadata_folder,"MEDRA_Word_Conversions.xlsx")) # AE Visualizations - Dobina//Visualizations//Standardized Renaming//MEDRA_Word_Conversions.xlsx")
renaming_dat$organSystemName <-toupper(renaming_dat$organSystemName)

filtered_ae_A <- ae_A_table %>%
  left_join(renaming_dat, by = "organSystemName") %>%
  select(-organSystemName) %>%
  rename(organSystemName = 'New Name') %>%
  relocate(organSystemName, .after=term) %>%
  arrange(desc(`REGIMEN-A{numSubjectsAffected}`)) %>%
  
  # make pct var
  mutate(pct_affected = `REGIMEN-A{numSubjectsAffected}`/`REGIMEN-A{numSubjectsAtRisk}`,
         arm_desc = "REGIMEN-A") %>%
  rename(numEvents = `REGIMEN-A{numEvents}`,  # Renaming variables to align with Dobinda's code
         numAffected = `REGIMEN-A{numSubjectsAffected}`,
         numAtRisk = `REGIMEN-A{numSubjectsAtRisk}`,
         event_type = adverseEventType)

filtered_ae_A_serious <- ae_A_table_serious %>%
  left_join(renaming_dat, by = "organSystemName") %>%
  select(-organSystemName) %>%
  rename(organSystemName = 'New Name') %>%
  relocate(organSystemName, .after=term) %>%
  arrange(desc(`REGIMEN-A{numSubjectsAffected}`)) %>%
  
  # make pct var
  mutate(pct_affected = `REGIMEN-A{numSubjectsAffected}`/`REGIMEN-A{numSubjectsAtRisk}`,
         arm_desc = "REGIMEN-A") %>%
  rename(numEvents = `REGIMEN-A{numEvents}`,
         numAffected = `REGIMEN-A{numSubjectsAffected}`,
         numAtRisk = `REGIMEN-A{numSubjectsAtRisk}`,
         event_type = adverseEventType)

filtered_ae_B <- ae_B_table %>%
  left_join(renaming_dat, by = "organSystemName") %>%
  select(-organSystemName) %>%
  rename(organSystemName = 'New Name') %>%
  relocate(organSystemName, .after=term) %>%
  arrange(desc(`REGIMEN-B{numSubjectsAffected}`)) %>%
  
  # make pct var
  mutate(pct_affected = `REGIMEN-B{numSubjectsAffected}`/`REGIMEN-B{numSubjectsAtRisk}`,
         arm_desc = "REGIMEN-B") %>%
  rename(numEvents = `REGIMEN-B{numEvents}`,
         numAffected = `REGIMEN-B{numSubjectsAffected}`,
         numAtRisk = `REGIMEN-B{numSubjectsAtRisk}`,
         event_type = adverseEventType)

filtered_ae_B_serious <- ae_B_table_serious %>%
  left_join(renaming_dat, by = "organSystemName") %>%
  select(-organSystemName) %>%
  rename(organSystemName = 'New Name') %>%
  relocate(organSystemName, .after=term) %>%
  arrange(desc(`REGIMEN-B{numSubjectsAffected}`)) %>%
  
  # make pct var
  mutate(pct_affected = `REGIMEN-B{numSubjectsAffected}`/`REGIMEN-B{numSubjectsAtRisk}`,
         arm_desc = "REGIMEN-B") %>%
  rename(numEvents = `REGIMEN-B{numEvents}`,
         numAffected = `REGIMEN-B{numSubjectsAffected}`,
         numAtRisk = `REGIMEN-B{numSubjectsAtRisk}`,
         event_type = adverseEventType)

filtered_ae_C <- ae_C_table %>%
  left_join(renaming_dat, by = "organSystemName") %>%
  select(-organSystemName) %>%
  rename(organSystemName = 'New Name') %>%
  relocate(organSystemName, .after=term) %>%
  arrange(desc(`REGIMEN-C{numSubjectsAffected}`)) %>%
  
  # make pct var
  mutate(pct_affected = `REGIMEN-C{numSubjectsAffected}`/`REGIMEN-C{numSubjectsAtRisk}`,
         arm_desc = "REGIMEN-C") %>%
  rename(numEvents = `REGIMEN-C{numEvents}`,
         numAffected = `REGIMEN-C{numSubjectsAffected}`,
         numAtRisk = `REGIMEN-C{numSubjectsAtRisk}`,
         event_type = adverseEventType)

filtered_ae_C_serious <- ae_C_table_serious %>%
  left_join(renaming_dat, by = "organSystemName") %>%
  select(-organSystemName) %>%
  rename(organSystemName = 'New Name') %>%
  relocate(organSystemName, .after=term) %>%
  arrange(desc(`REGIMEN-C{numSubjectsAffected}`)) %>%
  
  # make pct var
  mutate(pct_affected = `REGIMEN-C{numSubjectsAffected}`/`REGIMEN-C{numSubjectsAtRisk}`,
         arm_desc = "REGIMEN-C") %>%
  rename(numEvents = `REGIMEN-C{numEvents}`,
         numAffected = `REGIMEN-C{numSubjectsAffected}`,
         numAtRisk = `REGIMEN-C{numSubjectsAtRisk}`,
         event_type = adverseEventType)

filtered_ae_data <- Reduce(rbind,list(filtered_ae_A,
                                        filtered_ae_A_serious,
                                        filtered_ae_B,
                                        filtered_ae_B_serious,
                                        filtered_ae_C,
                                        filtered_ae_C_serious))

# Limitation with Dobinda's data. Need to revise because we have fuller data set.
# Area for revision!

filtered_data <- filtered_ae_data %>%
  group_by(event_type, organSystemName, arm_desc) %>% 
  arrange(desc(numAffected), .by_group=TRUE) %>%
  filter(row_number()==1) %>%
  ungroup()


# 3. Generating plot data -------------------------------------------------
event_data <- list()
spider_plot_dat_list <- list()

for (event in sort(unique(filtered_data$event_type))) {
  
  # Filtering grade and events
  filtered_event_dat <- filtered_data %>%
    filter(event_type == event) %>% 
    arrange(desc(numAffected))
  
  # Taking the 10 most prevalent overall categories
  categories <- filtered_event_dat %>% 
    group_by(organSystemName) %>% 
    mutate(total = sum(numAffected))  %>%
    arrange(desc(total))  %>%
    filter(row_number()==1)  %>%
    ungroup() %>%
    filter(row_number()==1:10) %>%
    pull(organSystemName) 
  
  filtered_event_dat <- filtered_event_dat %>% 
    filter(organSystemName %in% categories)
  
  
  # Pivoting to Wider for easiest formatting
  event_data[[event]] <- filtered_event_dat %>% 
    select(organSystemName, arm_desc, pct_affected) %>% 
    pivot_wider(id_cols = organSystemName,
                names_from = arm_desc,
                # names_prefix = "Grade ",
                values_from = pct_affected,
                values_fill = 0) %>% 
    complete(organSystemName = categories) %>%
    mutate(across(everything(), ~ replace_na(., 0)))
  
  # Making the spider plot data
  spider_plot_dat <- event_data[[event]]   %>%
    rename(Category = organSystemName) 
  
  spider_dat <- as.data.frame(t(spider_plot_dat))
  names(spider_dat) <- lapply(spider_dat[1, ], as.character)
  spider_dat <- spider_dat[-1,] %>%
    add_column(Name = rownames(spider_dat[-1,]), .before=1) %>%
    mutate(across(-Name, ~ as.numeric(.)))
  rownames(spider_dat) <- NULL
  # colnames(spider_dat) <- sapply(colnames(spider_dat), function(x) strsplit(x, " ")[[1]][1])
  
  spider_plot_dat_list[[paste(event)]] <- spider_dat %>% 
    mutate(Name = factor(Name,
                         levels = c("REGIMEN-A",
                                    "REGIMEN-B",
                                    "REGIMEN-C")))
  
}


# 4. Creating the spider plot ---------------------------------------------

color <- c("red", "blue","yellow")
title <- names(spider_plot_dat_list)
spider_plots <- list()

# Create the radar chart
for(i in names(spider_plot_dat_list)){
  
  color_fix <- color
  if (nrow(spider_plot_dat_list[[i]]) < length(color)) {
    color_fix <- color[(length(color) - nrow(spider_plot_dat_list[[i]]) + 1):length(color)]}
  
  spider_plots[[i]] <- ggradar(spider_plot_dat_list[[i]],
                               background.circle.colour = "white",
                               legend.position = "bottom",
                               font.radar = "serif",
                               grid.min = 0,
                               grid.mid = 0.5,
                               grid.max = 1,
                               axis.label.size = 3.5,
                               grid.label.size = 4,
                               legend.text.size = 10,
                               grid.line.width = 0.5,
                               group.colours = color_fix,
                               group.point.size = 2.3,
                               group.line.width = 1.2,
                               # axis.label.offset = 1.2,
                               gridline.label.offset = -0.005,
                               centre.y = -0.2)  +
    # ggtitle(paste(i, "AEs"))  +
    theme(plot.title = element_text(size = 12,
                                    hjust = 0.5,
                                    face = "bold")) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) + 
    guides(color = guide_legend(nrow = 3))
}

radar_together <- ggarrange(plotlist = spider_plots, widths = c(1.7, 1.6), common.legend = TRUE,
                            legend = "bottom")
radar_together

save_gg_pdf(
  radar_together,
  "radar_charts_large.pdf",
  save_folder = figure_save_folder,
  width = 12, 
  height = 6,
)


# 5. Creating the love plot -----------------------------------------------

love_plot_dat <- filtered_ae_data   %>%   
  group_by(term, event_type) %>%
  # creating a percent variable to calculate num effect by term/event type (ignoring arm)
  mutate(total_pct_events = (sum(numAffected)/sum(numAtRisk))) %>%
  arrange(total_pct_events) %>%
  ungroup() %>%
  mutate(pct_affeced_by_arm = numAffected/numAtRisk)


# for other events
other_chart <- love_plot_dat %>%
  filter(event_type == "Other") %>%
  mutate(term = forcats::fct_inorder(term)) %>%
  # picking events that happened over 5% of the time
  ggplot(data = .,
         aes(x = pct_affeced_by_arm*100,
             y = term,
             group = arm_desc,
             color = arm_desc)) +
  geom_point(size = 2) + 
  xlab("Percent of Patients Affected") +
  geom_line(orientation = "y", linewidth = 0.9)  + 
  theme_bw() +
  ggtitle("Other AEs") +
  scale_color_manual(values=c("red", "blue","yellow")) +
  guides(colour=guide_legend(title="Treatment Arm"))

serious_chart <- love_plot_dat %>%
  filter(event_type == "Serious") %>%
  mutate(term = forcats::fct_inorder(term)) %>%
  # picking events that happened over 5% of the time
  filter(total_pct_events > 0.0025) %>%
  ggplot(data = .,
         aes(x = pct_affeced_by_arm*100,
             y = term,
             group = arm_desc,
             color = arm_desc,
             fill = arm_desc)) +
  geom_point(size = 2)  + 
  xlab("Percent of Patients Affected") + 
  scale_color_manual(values=c("red", "blue","yellow")) +
  geom_line(orientation = "y", linewidth = 0.9)  + 
  theme_bw() +
  ggtitle("Serious AES")


combined_ae_plot = ggarrange(other_chart, serious_chart,
          legend = "bottom", common.legend=TRUE,
          ncol = 2)


save_gg_pdf(
  combined_ae_plot, 
  "ae_charts_large.pdf",
  save_folder = figure_save_folder,
  width = 18, 
  height = 12,
)

