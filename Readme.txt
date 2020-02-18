Instruction:

Come up with a scientific way for attribution allocation based on last click. Here uses Molina as an example.
- Time_difference_second as one major predictive variable (between radio airtime and lead conversion time)
- Percentage non-converted as target variable 
- Taking other factors into consideration

Steps:
1. Data cleaning in alteryx (Molina Data Prep.yxmd)
2. Run time decay model in R (Molina Time Decay Model.R)
3. Generate a result file.csv with attribution rate for each conversion record (step3_final_result(pandora+audiology).csv)
3. Generate an analysis Report. (Molina Attribution Analysis.docx)


Some difficulites I am facing: 

1. How can we perform variable selection more scientifically?
2. How do we interpret the coefficient of each variable?
3. Should we set up a cut off point (most of the data have 30 day lookback window)?
4. How can we put this into production? 
   Can we link it to alteryx instead of manually running it for every single account?