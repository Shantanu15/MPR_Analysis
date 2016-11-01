setwd("/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github")
library('dplyr')
#Path for Raw Data File --Change it to the location of your file
RawData <- read.csv("/Raw_Data/MPR_RAW_Data_25-Oct.csv",header = TRUE,sep=",")


#creating Analysis Table 
analysisTable <- as.data.frame(matrix(nrow = nrow(RawData)))
analysisTable$State <- RawData$state_name
analysisTable$DistrictName <- RawData$district_name
analysisTable$BlockName <- RawData$block_name
analysisTable$EntryMonth<- RawData$entry_month
analysisTable$EntryYear<- RawData$entry_year
analysisTable$EntityCode <-RawData$entity_code
analysisTable$BlockType <-RawData$blocktype
analysisTable$V1 <- NULL

#Physical Progress
#Calculating total no. of SHGs
analysisTable$totalSHGs <- RawData$new_shg_promoted_nrlmtargethousehold + RawData$pre_nrlm_shg_nrlmfold - RawData$no_shg_defaunct
#Calculating total no. of HHs mobilized into SHGs 
analysisTable$totHHs <- RawData$sc_member_mobilized_in_shg + RawData$st_member_mobilized_in_shg + RawData$minority_member_mobilized_in_shg + RawData$other_member_mobilized_in_shg
#Calculating total no. of VOs
analysisTable$VO <- RawData$no_vo_formed
#Calculating SHGs federated into VOs
analysisTable$SHGs_in_VOs <- RawData$no_shg_membership_vo
#Calculating total no. of CLFs
analysisTable$CLFs <- RawData$no_clf_formed


#Fund Disbursement 
#Calculating number of savings bank accounts
analysisTable$Bank_accounts <- RawData$no_new_shg_three_month_old_bank_account + RawData$no_pre_nrlm_shg_bank_account
#Calculating eligible SHGs for RF
analysisTable$eligibleRF <- RawData$no_new_shg_three_month_old_bank_account + RawData$no_pre_nrlm_shg_eligible_for_rf
#Calculating SHGs provided RF
analysisTable$numRF <- RawData$no_new_shg_provided_rf + RawData$no_pre_nrlm_shg_provided_with_rf
#Calculating Amount of RF provided to SHGs
analysisTable$amtRF <- RawData$amount_of_rf_provided_to_sc_shg_rf + RawData$amount_of_rf_provided_to_st_shg_rf +RawData$amount_of_rf_provided_to_other_shg_rf + RawData$amount_of_rf_provided_to_pwd_shg_rf
#Calculating no. of six month SHGs
analysisTable$sixMonth <- RawData$no_shg_six_month_old
#Calculating no. of MCPs prepared
analysisTable$MCP <- RawData$no_shg_mip_mcf
#Calculating no. of SHGs provided CIF
analysisTable$numCIF <- RawData$no_sc_shg_provided_cif_vrf + RawData$no_st_shg_provided_cif_vrf+RawData$no_minority_shg_provided_cif_vrf+RawData$no_other_shg_provided_cif_vrf
#Calculating amount of CIF disbursed 
analysisTable$amtCIF <- RawData$amount_of_cif_vrf_provided_to_sc_shg_rf+RawData$amount_of_cif_vrf_provided_to_st_shg_rf+RawData$amount_of_rf_provided_to_minority_shg_rf+RawData$amount_of_cif_vrf_provided_to_other_shg_rf
#Calculating No. of VOs provided VRF
analysisTable$numVRF <- RawData$no_vo_provided_vrf
#Calculating Amt. of VRF Disbursed
analysisTable$amtVRF <- analysisTable$amount_vrf_provided_vo

########SOCIAL INCLUSION########################################################
#SocialInclusion - Physical Progress - No.of HHs
analysisTable$SC_HHs <- RawData$sc_member_mobilized_in_shg
analysisTable$ST_HHs <- RawData$st_member_mobilized_in_shg
analysisTable$Min_HHs <- RawData$minority_member_mobilized_in_shg
analysisTable$Other_HHs <- RawData$other_member_mobilized_in_shg

#Social Inclusion - Physical Progress - No. of SHGs
analysisTable$SC_SHGs <- RawData$sc_shg_total
analysisTable$ST_SHGs <- RawData$st_shg_total
analysisTable$Min_SHGs <- RawData$minority_shg_total
analysisTable$Other_SHGs <- RawData$other_shg_total


#Social Inclusion - Financial Progress
#RF dibsursed -Number  
analysisTable$RF_SC_num <-RawData$no_sc_shg_provided_rf
analysisTable$RF_ST_num <-RawData$no_st_shg_provided_rf
analysisTable$RF_Min_num <-RawData$no_minority_shg_provided_rf
analysisTable$RF_Others_num <- RawData$other_member_mobilized_in_shg

#RF Disbursed - Amount
analysisTable$RF_SC_amt <-RawData$amount_of_rf_provided_to_sc_shg_rf
analysisTable$RF_ST_amt <-RawData$amount_of_rf_provided_to_st_shg_rf
analysisTable$RF_Min_amt <-RawData$amount_of_rf_provided_to_minority_shg_rf
analysisTable$RF_Others_amt <- RawData$amount_of_rf_provided_to_other_shg_rf
  
#CIF dibsursed- Number
analysisTable$CIF_SC_num <-RawData$no_sc_shg_provided_cif_vrf
analysisTable$CIF_ST_num <-RawData$no_st_shg_provided_cif_vrf
analysisTable$CIF_Min_num <-RawData$no_minority_shg_provided_cif_vrf
analysisTable$CIF_Others_num <- RawData$no_other_shg_provided_cif_vrf

#CIF Disbursed - Amount
analysisTable$CIF_SC_amt <-RawData$amount_of_cif_vrf_provided_to_sc_shg_rf
analysisTable$CIF_ST_amt <-RawData$amount_of_cif_vrf_provided_to_st_shg_rf
analysisTable$CIF_Min_amt <-RawData$amount_of_cif_vrf_provided_to_minority_shg_rf
analysisTable$CIF_Others_amt <-RawData$amount_of_cif_vrf_provided_to_other_shg_rf

#Exporting Analysis File
write.csv(analysisTable,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/Disgg.Analysis.csv")

#Grouping
#Entire Country
#Cumulative Progress

AggregateTables_Cum_Progress_Overall <- analysisTable %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts)) 

#Exporting Overall Progress - Financial Year
write.csv (AggregateTables_Cum_Progress_Overall,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/Cumulative/Overall.csv")

#Progress made in FY 16-17 
AggregateTables_FY_Progress_Overall <- filter (analysisTable,EntryYear == "2016-2017") %>%
group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))

#Exporting Overall Progress - Financial Year
write.csv(AggregateTables_FY_Progress_Overall,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/FY_16_17/Overall_FY.csv")

#NRLP Project Only 

#Cumulative Progress
AggregateTables_Cum_Progress_NRLP <- filter(analysisTable, BlockType == "NrlpI") %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))
#Exporting NRLP Progress - Cumulative 
write.csv(AggregateTables_Cum_Progress_NRLP,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/Cumulative/NRLP_Cumulative.csv")

#Progress made in FY 16-17 
AggregateTables_FY_Progress_NRLP <- filter (analysisTable, BlockType == "NrlpI", EntryYear == "2016-2017") %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))
#Exporting Files - NRLP- Financial Year
write.csv(AggregateTables_FY_Progress_NRLP,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/FY_16_17/NRLP_FY.csv")

#NRLP, NRLM and SRLP Projects 
##Creating a table for analysis (includes Gujarat and Rajasthan)- Will be removed in the later version. For now, the data for these two states may be extracted from another file
#Cumulative Progress

#Cumulative Progress
AggregateTables_Cum_Progress_NRLP_NRLM_SRLP <- filter(analysisTable,BlockType == "NrlpI" | BlockType == "NrlmI" | BlockType == "OSrlpI") %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))
#Exporting Files - NRLP/NRLM/SRLP - Cumulative
write.csv(AggregateTables_Cum_Progress_NRLP_NRLM_SRLP,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/Cumulative/NRLP_NRLM_SRLP_Cumulative.csv")

#Progress made in FY 16-17 
AggregateTables_FY_Progress_NRLP_NRLM_SRLP <- filter (analysisTable,BlockType == "NrlpI" | BlockType == "NrlmI" | BlockType == "OSrlpI", EntryYear == "2016-2017") %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))

#Exporting Files - NRLP/NRLM/SRLP - Financial Year
write.csv(AggregateTables_FY_Progress_NRLP_NRLM_SRLP,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/FY_16_17/NRLP_NRLM_SRLP_FY.csv")

#NRLP, NRLM Projects 
##Creating a table for analysis (includes Gujarat and Rajasthan)- Will be removed in the later version. For now, the data for these two states may be extracted from another file
#Cumulative Progress
AggregateTables_Cum_Progress_NRLP_NRLM <- filter(analysisTable,BlockType == "NrlpI" | BlockType == "NrlmI") %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))
#Exporting Files - NRLM/NRLM - Overall
write.csv(AggregateTables_Cum_Progress_NRLP_NRLM,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/Cumulative/NRLP_NRLM_Cumulative.csv")

#Progress made in FY 16-17 
AggregateTables_FY_Progress_NRLP_NRLM<- filter(analysisTable, BlockType == "NrlpI" | BlockType == "NrlmI", EntryYear == "2016-2017") %>%
  group_by(State) %>%
  summarize(SHGs_Prom = sum(totalSHGs),VOs_Prom = sum(VO),CLFs_Prom= sum(CLFs),HHs_Mob = sum(totHHs), Num_RF = sum(numRF), Amt_RF =sum(amtRF/100000),Num_CIF = sum(numCIF),Amt_CIF =sum(amtCIF/100000), Num_MCP = sum(MCP),eligible_RF = sum(eligibleRF),six_Month= sum(sixMonth),SHGs_VOs= sum(SHGs_in_VOs),Bank_Ac = sum(Bank_accounts))

#Exporting Files - NRLP/NRLM - Financial Year 
write.csv(AggregateTables_FY_Progress_NRLP_NRLM,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Output_files/FY_16_17/NRLP_NRLM_FY.csv")
