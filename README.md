## Structure

- M5-DLinear has all the code to generate forecasts using darts DLinear, v2 being the final version
- M5-FITS has all the code to generate forecasts using the FITS model from https://github.com/VEWOXIC/FITS/tree/main
- submissions - Final evaluated submissions in "Submission analysis.ipynb"
- benchmarks - benchmark model forecasts 
- Evaluation - R code for estimating accuracy from forecast "submission"
  - submission_evaluation - has R code for estimating weights for each item, as well as all submissions with different hyperparameters of DLinear and FITS in *DLinear_eval* and *FITS_eval* 
- preprocessing-v1 - preprocessing done in the first part, resulted in 3 .pkl files of competition data
- data-preprocessing-v2 - Only kept the required data for analysis
- eda - partially recovered exploratory data analysis file
