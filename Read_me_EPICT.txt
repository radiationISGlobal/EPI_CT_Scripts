EPICT
How to execute the scripts

- Run "call_function.r" always before starting with the analysis. 

To prepare the data. Execute once and always there is a change in the outcomes or in the server data:

- "Leukemia_AnalysisFile.R" gets the data from the server and applies the first exclusions and incorporates the 	outcomes as a dichotomy (0/1). Output file with one row per CT.
      · INPUT:  - IARC_PatientList20180323 
		- IARC_ExaminationList20180323
		- Doses_means_3001_3200_s1_20180503
		- OutComeDef
      · OUTPUT: - Leukemia_AnalysisFile_Date.rds

- "first_transformation_cohorts_ef.r" creates the data bases ready for the analysis. Output with extra rows for       	cum_doses calculations and exclusions. For example: exams within less than two years before of 	diagnosis 	will appear but not taken into account in the cumulative doses. 
      		· INPUT:  - Leukemia_AnalysisFile_Date.rds
      		· OUTPUT: - dt1_ef.rds           
			  - dt1_ef_lag1.rds
			  - dt1_ef_lag5.rds      Outputs were saved in a folder called "transformed_cohorts"

For the analysis:

- "read_me_analysis.txt" contains the model of every analysis, which are inside the folders "analysis_xx". 

- There must be a main folder for results called "results" together with the other folders. 

- If there is the need to execute a new analysis, copy and paste one of the folders of "analysis_xx" and make the 	appropriate changes. 

- Create a new folder inside "analysis_xx" called "results". 

- Each folder "analysis_xx" contains :
	· script for every outcome to execute. 
		INPUT:  dt1_ef.rds (for example) 
		OUTPUT: "output_filename" that will be saved in the "results" folder of the analysis (determined in 				"output_path")
	· f_results_fit_x.r : called in the outcome's scripts. Fits the results in a readable table.  
	· gather_results.r : once we have every outcome result in the "result" folder, execute to merge all 				outcomes results together. The resulting excel file will be saved in the main folder "results" 				located out of the "analysis_x" folder. 
