# server/partidos_mapping.R
# Definir el mapeo de columnas disponibles para cada combinación de año y cargo
partidos_mapping <- list(
  "2006_DIPUTACION FEDERAL" = c("PAN", "APM", "PBT", "NVALZ", "ASDC", "no_reg", "vot_nul"),
  "2006_SENADURIA" = c("PAN", "APM", "PBT", "NVALZ", "ASDC", "no_reg", "vot_nul"),
  "2006_PRESIDENCIA" = c("PAN", "APM", "PBT", "NVALZ", "ASDC", "no_reg", "vot_nul"),
  "2009_DIPUTACION FEDERAL" = c("PAN", "PRI", "PRD", "PVEM", "PT", "CONV", "NVALZ", "PSD", "PRIMERO_MEXICO", "SALVEMOS_MEXICO", "no_reg", "vot_nul"),
  "2012_DIPUTACION FEDERAL" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "no_reg", "vot_nul"),
  "2012_SENADURIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "no_reg", "vot_nul"),
  "2012_PRESIDENCIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "no_reg", "vot_nul"),
  "2015_DIPUTACION FEDERAL" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "MORENA", "PH", "ES", "PAN_NVALZ", "PRI_PVEM", "PRD_PT", "CAND_IND1", "CAND_IND2", "no_reg", "vot_nul"),
  "2018_DIPUTACION FEDERAL" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "MORENA", "ES", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC", "PRI_PVEM_NVALZ", "PRI_PVEM", "PRI_NVALZ", "PVEM_NVALZ", "PT_MORENA_ES", "PT_MORENA", "PT_ES", "MORENA_ES", "CAND_IND1", "CAND_IND2", "no_reg", "vot_nul"),
  "2018_SENADURIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "MORENA", "ES", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC", "PRI_PVEM_NVALZ", "PRI_PVEM", "PRI_NVALZ", "PVEM_NVALZ", "PT_MORENA_ES", "PT_MORENA", "PT_ES", "MORENA_ES", "CAND_IND1", "CAND_IND2", "no_reg", "vot_nul"),
  "2018_PRESIDENCIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "NVALZ", "MORENA", "ES", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC", "PRI_PVEM_NVALZ", "PRI_PVEM", "PRI_NVALZ", "PVEM_NVALZ", "PT_MORENA_ES", "PT_MORENA", "PT_ES", "MORENA_ES", "CAND_IND1", "CAND_IND2", "no_reg", "vot_nul"),
  "2021_DIPUTACION FEDERAL" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "PES", "RSP", "FXM", "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA", "CAND_IND1", "no_reg", "vot_nul"),
  "2021_SENADURIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA", "no_reg", "vot_nul"),
  "2023_SENADURIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MORENA", "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PT_MORENA", "no_reg", "vot_nul"),
  "2024_DIPUTACION FEDERAL" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA", "CAND_IND1", "no_reg", "vot_nul"),
  "2024_SENADURIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA", "no_reg", "vot_nul"),
  "2024_PRESIDENCIA" = c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PVEM_PT_MORENA", "PVEM_PT", "PVEM_MORENA", "PT_MORENA", "no_reg", "vot_nul")
)