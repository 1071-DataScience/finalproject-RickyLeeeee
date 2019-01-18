# < Dat Air >

### Groups
* < 李右元, 107753027 >
* < 楊晴焱, 107155018 >

### Goal
AQI(Air Quality Index), a measurement to evaluate status of air safety and cleanliness, is derived from numerical formula defined by EPA (Environmental Protection Agency), which uses weighted values of concentration from diffrent gases and selects one with maxmium. Since it takes the result of the quantity over gases and they come from things in our daily lives, this triggers us bringing out a question: can we use the quantity of things emitting these gases contributing to AQI index to predict it?   
     

### Demo
* Code execution <br />
     We recommend that one download the entire master branch file folder as zip and decompress it, and switch the root layer under this master-branch titled folder with typing the following command on your R terminal to execute the code named after "final.R".  
```
Rscript code/final.R
```

* Data Visualization <br />
     We simply output the performances of the models as png pictures in this code by different dimensions from aiding research pourposes directly without extra access to the internet.<br />
     <br />
     The output pictures are at folder named after "results", and the directory format looks like:<br />
     results/[The Dimension Folder Name]<br />
     <br />
     For the details about analysis dimensions mentioned above, please check at the "Results" segment below for detailed information.
## Folder organization and its related information

### Docs
* Presentation slides demonstrated on Jan 15, 2019
* Feature definition of Source data in word documentation

### Data

* Source <br />     
     We collected the AQI results among all cities in Taiwan every month from 2005-2017, and the related features of AQI contributors  such as the monthly numbers of Motorbike&Car,Garbage&Waste generated, air pollution penalty&auditory cases for the project analysis. The related data reference source link is posted under "References" segment below. There are 3432 records in total after the collcection.<br /><br />
* Input format <br /><br />
     As we collected so many features and AQI Results, it took us considerable time to combine all of them into one CSV file as modeling data, for these Open Data has its own format from different breaus or authorities concerned with "unique aligning preference". The final CSV we used contains these columns in this order in English version: <br />
<br />    [Year],[Month],[City],[AQI],[Car],[Bike],[TotalGarbageT],[GeneralGarbageT],[HugeGarbageT],[RecycleGarbageT],[KitchenWasteT],[WastePerPersonKG],[PenaltyConstruction],[PenaltyPollution],[PenaltyMobilePollution],[ExamConstruction],[ExamPollution],[ExamMobliePollution]
<br /><br />
     One thing we should point out is we used "replaceChinese.csv" instead of "AllFeatures+Labelv4.csv" in our code, because "AllFeatures+Labelv4.csv" with non-English words would go haywire in display after re-download from Github. However, we still left both csv files for better comparison. Plus, for those who want to re-produce this model experiment, please switch and replace the data in the folder entitled "data" under this project folder with master-branch titled.  <br /><br />   
* Data preprocessing <br /><br />
  * Handle missing data <br />
     Fortunately, we got only about 10% data missing values, and all in the nearest features([Car] & [Bike]), we would just remove them from the data set. However, these missing values are city-oriented and time-bound, which means there would exist great bias upon predicting on these cities, for they missed half of the figures from a consecutive time interval from 2005-2010. <br /><br />
  * Scale value <br />
    Because the features are with diffrent units and the numbers varies in scale massively, one method we have here is value-standardization, for that can simply scale down the values to the same level and unify the units among these values. <br /><br />   
     
### Code

* We used [KNN], [Decision Tree], [Random Forest], three mdoels within our capabilities as we wanted to compare and optimize the performances. Meanwhile, since our data is city-oriented and time-bound, we tried to realize if they had great effect on prediction by examining the average performances amongst three dimensions: [By All-data],[By Cities],[By Months] as their titles. However, we applied cross-validation to all these three dimensions(Data split ratio: 70% training, 30% testing), and scored them at testing results by checking Precison, Recall, and F1-Score. <br /><br />

### Results

* Which metric do you use 
  * precision, recall, R-square
* Is your improvement significant?
* What is the challenge part of your project?
