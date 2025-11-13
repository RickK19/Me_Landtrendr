#'@date: 2025/08/10
#'@author: Yannick Baidai
#'@title:  Perfomance Comparison of 4 algorithms
#'@Detail
#'------------------------------------------------

rm(list = ls())
setwd(dir = "C:/Users/Admin/Desktop/Dossiers bureau/UNA-Analyses_Stats/20250502-E_Konan/")
resDir  <- "20251009-Results"
dataFp  <- "2025.10.08_Donnees_article_Landtrendr_EK.xlsx"
try(dir.create(resDir, recursive = T))

require(openxlsx)
require(readxl)
require(dplyr)
require(caret)
library(PMCMRplus)

# Loading libraries and functions -----------------------------------------------------------------
FUNCTIONS_DIR <- "Functions/"
invisible(lapply(list.files(path = FUNCTIONS_DIR, pattern = "*.R", full.names = T, recursive = T),
                 function(x){
                   cat("\t +", basename(x), "\n")
                   source(x, echo = F, encoding = 'UTF-8')
                 })
)

# Saving excel workbook
wb_data <- openxlsx::createWorkbook()
wb_comparison <- openxlsx::createWorkbook()

# Excel style 
titleStyle  <- openxlsx::createStyle(fontSize = 12,  fontColour = "black",textDecoration = "bold")
headerStyle <- openxlsx::createStyle(fontSize = 11, fontColour = "#FFFFFF", halign = "center",fgFill = "#4F81BD",
                                     border = "TopBottomLeftRight ", borderColour = "blue", textDecoration = "bold")
bodyStyle   <- openxlsx::createStyle(fontSize = 11, border = "TopBottomLeftRight", borderColour = "blue")


# # 1. Loading and formatting input data ----------------------------------
df <- readxl::read_excel(path = dataFp, sheet = 1)%>%
  # Preliminary tests
  #dplyr::slice(1:1000)%>%
  # Formatting data
  dplyr::mutate(
    # Fixing accent issues in column name
    nom = gsub(x = nom,
               pattern = "^For.t class.e",
               replacement = "Forêt classée",
                fixed = F))%>%
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)))%>%
  dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ as.numeric(tidyr::replace_na(., 0))))%>%
  as.data.frame()


# # 2. Building comparison main data --------------------------------------
# 2.1. Reference ---------------------
refc <- df%>%
  reshape::melt.data.frame(id.vars = c("id", "left", "top", "right", "bottom",
                                       "row_index", "col_index","cate", "nom"),
                           measure.vars = c("TMF_20_rec",
                                            "TMF_21_rec2", "TMF_22_rec3",
                                            "TMF_23_rec4", "TMF_24_rec5"),
                           variable_name = "TMF")%>%
  dplyr::mutate(Year =  as.numeric(paste0(20,stringr::str_extract(string = TMF,
                                                               pattern = "\\d+"))))

refc_struct <- refc%>%
  dplyr::group_by(cate, value)%>%
  dplyr::summarise(Effectif = n(), .groups = "drop")%>%
  dplyr::mutate(Type = ifelse(cate == 0, "Domaine rural", "Forêt classée"))%>%
  dplyr::group_by(value)%>%
  dplyr::group_modify( ~ dplyr::add_row(.x,
                                        Type = "Global", 
                                        Effectif = sum(.x$Effectif)))%>%
  dplyr::group_by(Type)%>%
  dplyr::mutate(Pourcentage = 100 * Effectif / sum(Effectif))%>%
  dplyr::arrange(Type)%>%
  dplyr::select(-cate)%>%
  dplyr::relocate(Type, value)
  

# writing data in excel outputs
openxlsx::addWorksheet(wb = wb_data, sheetName = "Reference")
openxlsx::writeData(wb = wb_data, sheet = "Reference", x = refc)
openxlsx::addStyle(wb = wb_data, sheet = "Reference", style = headerStyle, 
                   cols = 1:ncol(refc), rows = 1)

openxlsx::addWorksheet(wb = wb_data, sheetName = "Data structure")
openxlsx::writeData(wb = wb_data, sheet = "Data structure",
                    x  = refc_struct, startCol = 1, startRow =  1)
openxlsx::addStyle(wb = wb_data, sheet = "Data structure", style = headerStyle, 
                   cols = 1:ncol(refc), rows = 1)


# 2.1. LTv1 ---------------------
LTV1 <- df%>%
  reshape::melt.data.frame(id.vars = c("id", "left", "top", "right", "bottom",
                                       "row_index", "col_index","cate", "nom"),
                           measure.vars = c("Change_LTv1-20",	"Change_LTv1-21",
                                            "Change_LTv1-22",	"Change_LTv1-23",
                                            "Change_LTv1-24"),
                           variable_name = "LTV1")%>%
  dplyr::mutate(Year =  as.numeric(paste0(20,stringr::str_extract(string = LTV1,
                                                               pattern = "\\d+$"))))%>%
  dplyr::rename(LTV1.value = value)

# writing data in excel outputs
openxlsx::addWorksheet(wb = wb_data, sheetName = "LTV1")
openxlsx::writeData(wb = wb_data, sheet = "LTV1", x = LTV1)
openxlsx::addStyle(wb = wb_data, sheet = "LTV1", style = headerStyle, 
                   cols = 1:ncol(LTV1), rows = 1)

# 2.2. LTv2 ---------------------
LTV2 <- df%>%
  reshape::melt.data.frame(id.vars = c("id", "left", "top", "right", "bottom",
                                       "row_index", "col_index","cate", "nom"),
                           measure.vars = c("Change_LTv2-20",	"Change_LTv2-21",
                                            "Change_LTv2-22",	"Change_LTv2-23",
                                            "Change_LTv2-24"),
                           variable_name = "LTV2")%>%
  dplyr::mutate(Year =  as.numeric(paste0(20, stringr::str_extract(string = LTV2,
                                                               pattern = "\\d+$"))))%>%
  dplyr::rename(LTV2.value = value)

# writing data in excel outputs
openxlsx::addWorksheet(wb = wb_data, sheetName = "LTV2")
openxlsx::writeData(wb = wb_data, sheet = "LTV2", x = LTV2)
openxlsx::addStyle(wb = wb_data, sheet = "LTV2", style = headerStyle, 
                   cols = 1:ncol(LTV2), rows = 1)

# 2.3. LTv3 ---------------------
LTV3 <- df%>%
  reshape::melt.data.frame(id.vars = c("id", "left", "top", "right", "bottom",
                                       "row_index", "col_index","cate", "nom"),
                           measure.vars = c("Change_LTv3-20",	"Change_LTv3-21",
                                            "Change_LTv3-22",	"Change_LTv3-23",
                                            "Change_LTv3-24"),
                           variable_name = "LTV3")%>%
  dplyr::mutate(Year =  as.numeric(paste0(20, stringr::str_extract(string = LTV3,
                                                               pattern = "\\d+$"))))%>%
  dplyr::rename(LTV3.value = value)

# writing data in excel outputs
openxlsx::addWorksheet(wb = wb_data, sheetName = "LTV3")
openxlsx::writeData(wb = wb_data, sheet = "LTV3", x = LTV3)
openxlsx::addStyle(wb = wb_data, sheet = "LTV3", style = headerStyle, 
                   cols = 1:ncol(LTV3), rows = 1)
# save file
openxlsx::saveWorkbook(wb = wb_data,
                       file = file.path(resDir, "formatted_data.xlsx"),
                       overwrite = TRUE)

# # 3. Comparisons grid --------------------------------------
# Comparisons with potential changes treated as actual
treat_potential <- c(`p as actual changes` = T, `p as no changes` = F)
  
# Global or unit-specific Comparisons
comparaisonType <- c(Global = NA_integer_, `Forêt classée` = 1, `Domaine rural` = 0)

# Comparisons main data
algoList <- c(LTV1 = "LTV1.value", LTV2 = "LTV2.value", LTV3 = "LTV3.value")
compData <- refc%>%
  merge.data.frame(y = LTV1)%>%
  merge.data.frame(y = LTV2)%>%
  merge.data.frame(y = LTV3)
  
# id des echantillons tirés au hasard pour les 20 groupes (20 fold cross comparisons)
kfold <- 20
kfold_samplesSize <- floor(nrow(df) / kfold)
kfold_samplesIds  <- split(x = sample(df$id),
                           f = rep(1:kfold, each = kfold_samplesSize))


# Comparisons loop
# 1. Potential changes consideration
require(foreach)
resultsLvl1 <- foreach(pChange = treat_potential, .combine = rbind)%do%
{
  pChangeName <- names(treat_potential[which(treat_potential==pChange)])
  cat(crayon::blue$bold("|> Comparisons with potential treated as actual :", pChange, "\n"))
  
  if(pChange == T){
    compData_sub1 <- compData%>%
      dplyr::mutate(ref.value = dplyr::case_when(value == "1p" ~ 1,
                                                 TRUE ~ as.numeric(value)))
  }else
  {
    compData_sub1 <- compData%>%
      dplyr::mutate(ref.value = dplyr::case_when(value == "1p" ~ 0,
                                                 TRUE ~ as.numeric(value)))
  }

  # 2. Global or unit specific comparison
  resLvl2 <- foreach(type = comparaisonType, .combine = rbind)%do%
  {
    unitName <- ifelse(is.na(type), 'Global', names(comparaisonType[which(comparaisonType==type)]))
    cat(crayon::magenta("\t - Comparisons type :", unitName, "\n"))
    
    # filtering unit
    if(type %in% 0:1){
      compData_sub2 <- compData_sub1 %>%
      dplyr::filter(cate == type)
    }else{
      compData_sub2 <- compData_sub1
    }

    # 3. Comparisons of each algo to the reference
    resLvl3 <- foreach(algo = algoList, .combine = rbind)%do%
    {
      algoName <- names(algoList[which(algoList==algo)])
      cat(crayon::yellow$italic("\t\t - Comparisons : Ref. vs", algoName))
      
      # selecting algorithm to be compared
      compData_sub3 <- compData_sub2%>%
        dplyr::mutate(reference = ref.value,
                      algorithm = get(algo))%>%
        dplyr::select(id, reference, algorithm)
        
        # 4. Kfold comparisons
        resLvl4 <- foreach(k = 1:kfold, .combine = rbind)%do%
        {
          cat(".")
          # sampling data
          compData_sub4 <- compData_sub3%>%
            dplyr::filter(id %in% kfold_samplesIds[[k]])
            
          #--Building Contingency table
          confTable <- table(compData_sub4[,c("reference", "algorithm")])
          # print(confTable)
        
          # Building confusion matrix
          confMatrix <- caret::confusionMatrix(confTable, positive = "1")
        
          tibble(kfold = k,
                 Effectif = nrow(compData_sub4),
                 Accuracy = confMatrix$overall["Accuracy"],
                 `P-Value [Acc > NIR]` =  confMatrix$overall["AccuracyPValue"],
                 Kappa = confMatrix$overall["Kappa"])%>%
            cbind(as.data.frame(t(confMatrix$byClass)))
        }
        
      cat(crayon::green("✓\n"))
      resLvl4$Algorithm <- algoName
      resLvl4
    }
    
    resLvl3$Unit <- unitName
    resLvl3
  }
  resLvl2$Potential <- pChange
  resLvl2
}

# writing data in excel outputs
resultsLvl1 <- resultsLvl1%>%
  dplyr::relocate(Potential, Unit, Algorithm)

resultsLvl1$F1[is.nan(resultsLvl1$F1)] <- 0


openxlsx::addWorksheet(wb = wb_comparison, sheetName = "Kfold comparisons")
openxlsx::writeData(wb = wb_comparison, sheet = "Kfold comparisons", x = resultsLvl1)
openxlsx::addStyle(wb = wb_comparison, sheet = "Kfold comparisons", style = headerStyle, 
                   cols = 1:ncol(resultsLvl1), rows = 1)


# 4. Algorithm perfomance comparisons----------------------------------------------
# Comparisons grid
# Comparisons with potential changes treated as actual
treat_potential <- c(`p as actual changes` = T, `p as no changes` = F)

# Global or unit-specific Comparisons
comparaisonType <- c(Global = NA_integer_, `Forêt classée` = 1, `Domaine rural` = 0)

cldGroups <- foreach(pChange = treat_potential, .combine = rbind)%do%
{
  cat(crayon::magenta$bold("|> Analysis with potential treated as actual :", pChange, "\n"))

  # writing data in excel outputs
  sheet <- ifelse(pChange, "Potential as chgs", "Potential as no chgs")
  openxlsx::addWorksheet(wb = wb_comparison, sheetName = sheet)
  row <- 1
  col <- 1
  
  sub_cld <- foreach(type = comparaisonType, .combine = rbind)%do%
  {
    unitName <- ifelse(is.na(type), 'Global', names(comparaisonType[which(comparaisonType==type)]))
    cat(crayon::magenta("\t - Comparisons type :", unitName, "\n"))
    
    # base comp data
    sub <- resultsLvl1%>%
      dplyr::filter(Potential == pChange)%>%
      dplyr::filter(Unit == unitName)

    
    # Statistical comparisons
    # # Kruskal-Wallis rank sum test
    # kruskTest <- kruskal.test(F1 ~ Algorithm, data = sub)%>%
    #       broom::tidy()%>%
    #       dplyr::mutate(` ` = signifCode(p.value))%>%
    #       dplyr::relocate(method,statistic, parameter, p.value, ` `)
    # cat(crayon::silver("\t\t- P value at Kruksall test : ", 
    #                    paste(format(kruskTest$p.value, scientific = T, digits = 3), kruskTest$` `),
    #                    "\n"))
    
    # Friedman rank sum test
    friedTest <- friedman.test(F1 ~ Algorithm | kfold, data = sub)%>%
      broom::tidy()%>%
      dplyr::mutate(` ` = signifCode(p.value))%>%
      dplyr::relocate(method,statistic, parameter, p.value, ` `)
    cat(crayon::silver("\t\t- P value at Friedman test : ", 
                      paste(format(friedTest$p.value, scientific = T, digits = 3), friedTest$` `),
                      "\n"))
    
    ### add in excel
    openxlsx::writeData(wb = wb_comparison, sheet = sheet, 
                        x = paste("F1 comparisons with Friedman rank sum tests :", unitName ),
                        startCol = col, startRow = row)
    openxlsx::addStyle(wb = wb_comparison, sheet = sheet, style = titleStyle, 
                       cols = col, rows = row)
    row <- row + 1
    openxlsx::writeData(wb = wb_comparison, sheet = sheet,
                        x = friedTest, 
                        startCol = col, startRow = row)
    openxlsx::addStyle(wb = wb_comparison, sheet = sheet, style = headerStyle, 
                       cols = 1:ncol(friedTest), rows = row)
    row <- row + nrow(friedTest) + 3
    
    ## Dunn posthoc test with bonferroni adjustment
    groups <- NULL
    if(friedTest$p.value <= 0.05)
    {
      nemenyi <- PMCMRplus::frdAllPairsNemenyiTest(F1 ~ Algorithm | kfold, data = sub)
      
      # --- Extraction et nettoyage de la matrice de p-valu
      algos <- unique(sub$Algorithm)
      pvals <- nemenyi$p.value
      p_full <- matrix(1, nrow = length(algos), ncol = length(algos),
                       dimnames = list(algos, algos))
      p_full[rownames(pvals), colnames(pvals)] <- pvals
      p_full[colnames(pvals), rownames(pvals)] <- t(pvals)
      groups   <- multcompView::multcompLetters(p_full, threshold = 0.05)
      groups   <- as.data.frame(t(groups$Letters))%>%
        dplyr::mutate(Unit = unitName)


      # dunnTest <- FSA::dunnTest(F1 ~ Algorithm, data = sub, method = "bonferroni")$res%>%
      #   dplyr::select(Comparison, P.adj) %>% 
      #   dplyr::mutate(Comparison = gsub(" - ", ":", Comparison),
      #                 ` ` = signifCode(P.adj))
      ### add in excel
      # openxlsx::writeData(wb = wb_comparison, sheet = sheet, 
      #                     x = paste("Pairwise comparisons (Dunn tests) from Kruskall-Wallis results :", unitName ),
      #                     startCol = col, startRow = row)
      # openxlsx::addStyle(wb = wb_comparison, sheet = sheet, style = titleStyle, 
      #                    cols = col, rows = row)
      # row <- row + 1
      # openxlsx::writeData(wb = wb_comparison, sheet = sheet,
      #                     x = dunnTest, 
      #                     startCol = col, startRow = row)
      # openxlsx::addStyle(wb = wb_comparison, sheet = sheet, style = headerStyle, 
      #                    cols = 1:ncol(dunnTest), rows = row)
      # row <- row + nrow(dunnTest) + 3
      
      
      # # Homogeneous groups
      # groups <- rcompanion::cldList(P.adj ~ Comparison, data = dunnTest, threshold = 0.05)%>%
      #   dplyr::mutate(Type = type)
      # 
      # ### add in excel
      # openxlsx::writeData(wb = wb_comparison, sheet = sheet, 
      #                     x = paste("Homogeneous groups from Pairwise comparisons (Dunn tests) : ", unitName ),
      #                     startCol = col, startRow = row)
      # openxlsx::addStyle(wb = wb_comparison, sheet = sheet, style = titleStyle, 
      #                    cols = col, rows = row)
      # row <- row + 1
      # openxlsx::writeData(wb = wb_comparison, sheet = sheet,
      #                     x = groups, 
      #                     startCol = col, startRow = row)
      # openxlsx::addStyle(wb = wb_comparison, sheet = sheet, style = headerStyle, 
      #                    cols = 1:ncol(groups), rows = row)
      # row <- row + nrow(groups) + 3
    }
    groups
  }
  sub_cld$Potential <- pChange
  sub_cld
}



# summary of all data
smry <- resultsLvl1%>%
  dplyr::group_by(Potential, Unit, Algorithm)%>%
  dplyr::summarise_all(mean)

openxlsx::addWorksheet(wb = wb_comparison, sheetName = "summary")
openxlsx::writeData(wb = wb_comparison, sheet = "summary",
                    x = smry, 
                    startCol = col, startRow = row)
openxlsx::addStyle(wb = wb_comparison, sheet = "summary", style = headerStyle, 
                   cols = 1:ncol(smry), rows = row)


# summary of F1
smry <- resultsLvl1%>%
  dplyr::group_by(Potential, Unit, Algorithm)%>%
  dplyr::summarise(`F1 moyen` = mean(F1, na.rm = T),
                   `Ecart-type F1` = sd(F1, na.rm = T))

openxlsx::addWorksheet(wb = wb_comparison, sheetName = "F1 summary")
openxlsx::writeData(wb = wb_comparison, sheet = "F1 summary",
                    x = smry, 
                    startCol = 1, startRow = 1)
openxlsx::addStyle(wb = wb_comparison, sheet = "F1 summary", style = headerStyle, 
                   cols = 1:ncol(smry), rows = 1)


# Graphiques: Boxplots
require(ggplot2)

letterGroups <- resultsLvl1%>%
  dplyr::group_by(Potential, Unit, Algorithm)%>%
  dplyr::group_modify(~as.data.frame(t(boxplot(.x$F1, plot = F)$stat)))%>%
  as.data.frame()%>%
  merge.data.frame(y = cldGroups%>%
                     reshape::melt.data.frame(measure.vars = c("LTV1", "LTV2", "LTV3"),
                                              variable_name = "Algorithm")%>%
                     dplyr::rename(group = value),
                   all.x = T,
                   all.y = F)%>%
  dplyr::rename(y = V5)%>%
  dplyr::select(Potential,Unit, Algorithm, y, group)


ggdf <- resultsLvl1%>%
  merge.data.frame(y = letterGroups,
                   all.y = F,
                   all.x = T)%>%
  dplyr::mutate(Potential = ifelse(Potential, "Potential changes considered", "Potential changes ignored"))
  

ggplot(data  = ggdf,
       aes(x = tidytext::reorder_within(Algorithm, by = F1, within = Unit),
           y = F1,
           fill = Algorithm))+
  geom_boxplot(outliers = F,
               staplewidth = 0.5, 
               width = 0.5, linewidth = 0.35,
               show.legend = F)+
  stat_summary(fun = mean, shape = 23, size = 0.65, 
               stroke = 0.6, color = "red", fill = "white")+
  geom_text(aes(y = y, label = group), size = 4,
            vjust = -0.05, hjust = 0.5)+
  tidytext::scale_x_reordered()+
  scale_fill_brewer(palette = "Set3")+
  labs(y="F1", x="")+
  facet_grid(Potential ~ Unit, scales = "free")+
  theme_linedraw(base_size = 14)+
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "dimgray", linewidth = 0.25, linetype = "dotted"),
        legend.position = "none",
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(color="black", size=12),
        axis.text  = element_text(angle=0, hjust = 0.5, vjust = 0.5, size=13))

ggsave(filename = file.path(resDir, "1-Specific_Algo_comparisons.png"),
       dpi = 1000, width = 24, height = 16, units = "cm")


# GLobal comparisons two-way anova testing difference between alog perfommance and unit effect
globalTests <- resultsLvl1%>%
  group_by(Potential)%>%
  rstatix::anova_test(F1 ~ Algorithm * Unit)
  
openxlsx::addWorksheet(wb = wb_comparison, sheetName = "ANOVA II")
openxlsx::writeData(wb = wb_comparison, sheet = "ANOVA II",
                    x = globalTests, 
                    startCol = 1, startRow = 1)
openxlsx::addStyle(wb = wb_comparison, sheet = "ANOVA II", style = headerStyle, 
                   cols = 1:ncol(globalTests), rows = 1)


# save file
openxlsx::saveWorkbook(wb = wb_comparison,
                       file = file.path(resDir, "comparisons_results.xlsx"),
                       overwrite = TRUE)



# Global comparisons
ggplot(data  = ggdf,
       aes(x = Unit,
           group = tidytext::reorder_within(Algorithm, by = F1, within = Unit),
           fill  = Algorithm,
           y = F1))+
  geom_boxplot(outliers = F,
               staplewidth = 0.65, 
               width = 0.75, linewidth = 0.35)+
  stat_summary(fun = mean, shape = 23, size = 0.35, position = position_dodge2(0.75),
               stroke = 0.75, color = "red", fill = "white")+
  tidytext::scale_x_reordered()+
  scale_fill_brewer(palette = "Set3")+
  facet_grid( ~ Potential, scales = "free")+
  labs(x="", fill = "")+
  theme_linedraw(base_size = 14)+
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "dimgray", linewidth = 0.25, linetype = "dotted"),
        legend.position = "bottom",
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(color="black", size=12),
        axis.text.x  = element_text(angle=0, hjust = 0.5, vjust = 0.5, size=10))
  
  ggsave(filename = file.path(resDir, "2-Global_Algo_comparisons.png"),
         dpi = 1000, width = 24, height = 12, units = "cm")
  