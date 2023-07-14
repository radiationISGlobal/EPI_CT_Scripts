EPICT analysis


Analysis have been conducted using the R package 'rERR'

A validation with Epicure has been also conducted (attached at ./validation_R_Epicure/):
	For each outcome in the overall cohort using the year grouped doses file
		linERR with continous cumulative dose
		loglinERR with the categorical variable cumcat2long 

Analysis 1 --- Table 2: RR and 95% CI per cumulative active bone marrow dose category and ERR/100 mGy by type of haematological neoplasm – analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_1_linERR_All.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

Analysis 1_2 -- Table 3: RR and 95% CI per active bone marrow dose category and ERR/100 mGy by type of neoplasm– analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_1_2_linERR_Lymphoid_All.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"NHL", "NHL_Bcell", "NHL_Tcell","NHL_Precursor_cell","HL"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

Analysis 1_3 --- Table 4: Effects of potential confounders and potential modifiers of the risk estimates in the study 
	results file:	epict_analysis_1_3_linERR_NoAdj_Birthcohort.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries, not adjusted for birthcohort


Analysis 1_9 -- Table 3: RR and 95% CI per active bone marrow dose category and ERR/100 mGy by type of neoplasm– analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_1_9_linERR_Myeloid_All.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"AML_prec_ALMP_ALAL", "MPN_MDSMPN_MDS", "AML_AL_ALMP_AL_excl_gen","MPN"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries, adjusted and not adjusted by SES 

Analysis 1_15 --- Table 2:RR and 95% CI per cumulative active bone marrow dose category and ERR/100 mGy by type of haematological neoplasm – analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_1_15_linERR_leuk_noCLL_exclusions.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"leuk_noCll_AK"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

Analysis 1_20 --- Table 5: Results of sensitivity analyses
	results file:	ec_res_lymph_ef_dose_cum_Richard_excluding_transplants.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"Lymphoid" 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK, Excluding subjects who underwent transplant



Analysis 1_21 --- Supplementary Table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	epict_analysis_1_21_linERR_leuk_Fr_UK_NE.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"leuk_france", "leuk_UK", "leuk_NE"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries


Analysis 2_2 ---- Table 2: RR and 95% CI per cumulative active bone marrow dose category and ERR/100 mGy by type of haematological neoplasm – analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_2_2_loglinERR_cat_exp_All.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries 


Analysis 2_3 --- Table 3 : RR and 95% CI per active bone marrow dose category and ERR/100 mGy by type of neoplasm– analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_2_3_loglinERR_cat_exp_Lymphoid_All.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"NHL", "NHL_Bcell", "NHL_Tcell","NHL_Precursor_cell","HL"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries 


Analysis 2_4 --- Table 3: Table 3: RR and 95% CI per active bone marrow dose category and ERR/100 mGy by type of neoplasm– analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_2_4_loglinERR_cat_exp_Myeloid_All.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"AML_prec_ALMP_ALAL", "MPN_MDSMPN_MDS", "AML_AL_ALMP_AL_excl_gen","MPN"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries 

Analysis 2_5 --- Table 2: RR and 95% CI per cumulative active bone marrow dose category and ERR/100 mGy by type of haematological neoplasm – analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_2_5_loglinERR_cat_exp_Leuk_noCLL.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"leuk_noCll_AK"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries 


Analysis 2_11 --- Supplementary Table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	epict_analysis_2_11_loglinERR_cat_exp_Leuk_Fr_UK_NE.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"leuk_france","leuk_UK","leuk_NE"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries


Analysis 3 --- Supplementary table 3: Linear ERR/100 mGy by type of haematological malignancies and by country – adjusted for age, sex, birth cohort - restricted to countries with at least 20 cases
	results file:	epict_analysis_3_linERR_subseting_countries.xlsx
	model used:		linERR ( 1 + beta * cum_dose ) )
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK / all_except_UK / France / The_Netherlands / Norway / Sweden / Others(Belgium,Denmark,Germany,Spain)
				removing: Belgium / Denmark / France / Germany / The_Netherlands / Norway / Spain / Sweden / UK

Analysis 3_1 --- Supplementary table 4: ERR/100 mGy by lymphoid and myeloid malignancy types by country – analyses stratified on sex, birth cohort and country - restricted to countries with at least 20 cases
		a. Lymphoid malignancies types
	results file:	epict_analysis_3_1_linERR_subseting_countries.xlsx
	model used:		linERR ( 1 + beta * cum_dose ) )
	outcomes		"NHL", "NHL_Bcell", "NHL_Tcell","NHL_Precursor_cell","HL"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK / all_except_UK / France / Norway / Sweden / The_Netherlands /  Others(Belgium,Denmark,Germany,Spain)
				removing: Belgium / Denmark / France / Germany / The_Netherlands / Norway / Spain / Sweden / UK

Analysis 3_2 --- Supplementary table 4: ERR/100 mGy by lymphoid and myeloid malignancy types by country – analyses stratified on sex, birth cohort and country - restricted to countries with at least 20 cases
		b. Myeloid malignancies and AL types
	results file:	epict_analysis_3_2_linERR_subseting_countries.xlsx
	model used:		linERR ( 1 + beta * cum_dose ) )
	outcomes		"AML_prec_ALMP_ALAL","MPN_MDSMPN_MDS"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK / all_except_UK / France / Norway / Sweden / The_Netherlands /  Others(Belgium,Denmark,Germany,Spain)
				removing: Belgium / Denmark / France / Germany / The_Netherlands / Norway / Spain / Sweden / UK

Analysis 3_3 --- Supplementary table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	epict_analysis_3_3_HL_country_subsets.xlsx
	model used:		linERR ( 1 + beta * cum_dose ) )
				loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"HL"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK / All

Analysis 5 --- Table 4: Effects of potential confounders and potential modifiers of the risk estimates in the study 
	results file:	epict_analysis_5_linERR_ses_adj_ses_countries.xlsx
	model used:		linERR adjusted with categorical ses ( exp(cat_SES) * ( 1 + b * cum_dose ) )
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		SES countries (France,The_Netherlands,Belgium,Spain)


Analysis 7 --- Table 4: Effects of potential confounders and potential modifiers of the risk estimates in the study 
	results file:	epict_analysis_7_linERR_age_at_exposure_overall_countries.xlsx
	model used:		linERR (1 + b1 * cdose_0_4 + b2 * cdose_5_9 + b3 * cdose_10_ )
						cdose_0_4 -> cumulative dose at time t recived when age in [0 , min(4.999 , t - lag)]
						cdose_5_9 -> cumulative dose at time t recived when age in [5 , min(9.999 , t - lag)]		
						cdose_10_ -> cumulative dose at time t recived when age in [10, t - lag]
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:		by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

Analysis 8 --- Table 4: Effects of potential confounders and potential modifiers of the risk estimates in the study 
	results file:	epict_analysis_8_linERR_gr_exposure_backwards_overall_countries.xlsx
	model used:		linERR (1 + b1 * cdose_0_4 + b2 * cdose_5_9 + b3 * cdose_10_ )
						cdose_0_4 -> cumulative dose for the relative failtime ft recived when age in [ft - 5 , ft - lag]
						cdose_5_9 -> cumulative dose for the relative failtime ft recived when age in [ft - 10, ft - 5  )
						cdose_10_ -> cumulative dose for the relative failtime ft recived when age in [...    , ft - 10 )
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:		by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

Analysis 10 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_10_linERR_lag5_overall_countries.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			5
	subsetting:		overall countries

Analysis 10_2 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_10_2_linERR_lag1_overall_countries.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			1
	subsetting:		overall countries

Analysis 15 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_15_linERR_new_outcomesv2_Median.xlsx
	model used:		linERR ( 1+ b * cum_dose ) where cum_dose is based on Median
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

Analysis 17 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_17_linERR_overall_pct99_countries.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		exclude subjects with cumdose > pct99	

Analysis 18 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_18_linERR_pct98_newoutcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		exclude subjects with cumdose > pct98

Analysis 19 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_19_linERR_pct95_newoutcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		exclude subjects with cumdose > pct95

Analysis 25 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_25_linERR_DropSomeHosp_new_outcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		drop subjects with a CT in a hspital with non complete CT history

Analysis 26 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_26_linERR_overall_DropBefCxreg_new_outcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		drop subjects born before start of cancer registry of their respective country

Analysis 27 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_27_linERR_ExitAt2yAfterLastCT_new_outcomes.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		exit at 2 years after last CT collected (different by each country)

Analysis 28 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_28_linERR_Excl5y_new_outcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	Exclusion of 5 years   

Analysis 29 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_29_linERR_Excl10y_new_outcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	Exclusion of 10 years
	

Analysis 32 --- Table 4: Effects of potential confounders and potential modifiers of the risk estimates in the study 
	results file:	epict_analysis_32_linERR_overall_bysex.xlsx
	model used:		linERR (1 + b1 * cdose_male + b2 * cdose_female)
					cdose_male = cumulative if the subject is male and 0 otherwise 
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2

Analysis 34 --- Table 5: Results of sensitivity analyses
	results file:	epict_analysis_34_linERR_overall_countries_ExclUnkVitStatus_new_outcomesv2.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		Exclude subjects with unknown vital status

n_ct/analysis_1 --- Supplementary Table 5: RR and 95% CI by category of number of CT examinations and ERR/ per examination for haematological malignancies– analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_1_linERR_n_ct_overall_countries_new_outcomesv2.xlsx
	model used:		linERR ( 1+ b * n_ct) 
	outcomes		"all_excl_therap_syndrel", "Lymphoid", "Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries

n_ct/analysis_2 --- Supplementary Table 5: RR and 95% CI by category of number of CT examinations and ERR/ per examination for haematological malignancies– analyses stratified on sex, birth cohort and country
	results file:	epict_analysis_2_loglinERR_n_ct_overall_countries_new_outcomesv2.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * n_ct_cat) )
	outcomes		"all_excl_therap_syndrel", "Lymphoid","Myeloid"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		overall countries 


analysis_UKOrigCohort/analysis_leuk_UK --- Supplementary Table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	ec_res_linERR_leuk_UK_ef_dose_cum_OriginalCohort.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"leuk_UK"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK Original Cohort

analysis_UKOrigCohort/analysis_leuk_UK_cat --- Supplementary Table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	ec_res_loglinERR_leuk_UK_ef_dose_cum_OriginalCohort.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"leuk_UK"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK Original Cohort


analysis_UKOrigCohort/analysis_HL --- Supplementary Table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	ec_res_linERR_HL_ef_dose_cum_OriginalCohort.xlsx
	model used:		linERR ( 1+ b * cum_dose ) 
	outcomes		"HL"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK Original Cohort

analysis_UKOrigCohort/analysis_HL_cat --- Supplementary Table 6: Relative risks (and 95% CIs) comparison with previously published results• Risk estimates are reported as per the original studies and as recalculated with the EPI-CT dose estimates
	results file:	ec_res_loglinERR_HL_ef_dose_cum_OriginalCohort.xlsx
	model used:		loglinERR - CoxPH ( exp( beta * categorical_cum_exposure ) )
	outcomes		"HL"
	stratified:   	by sex, country and birthcohort
	lag 			2
	subsetting:		UK Original Cohort





