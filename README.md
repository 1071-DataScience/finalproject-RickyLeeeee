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

### Data

* Source <br />     
     We collected the AQI results among all cities in Taiwan every month from 2005-2017, and the related features of AQI contributors  such as the monthly numbers of Motorbike&Car,Garbage&Waste generated, air pollution penalty&auditory cases for the project analysis. The related data reference source link is posted under "References" segment below. There are 3432 records in total after the collcection.<br /><br />
* Input format <br /><br />
     As we collected so many features and AQI Results, it took us considerable time to combine all of them into one CSV file as modeling data, for these Open Data has its own format from different breaus or authorities concerned with "unique aligning preference". The final CSV we used contains these columns in this order in English version: <br /> <br />


     * Label Columns:
     <br />    [Year],[Month],[City],[AQI]<br /><br />
     * Feature Columns [Traffic]:
     <br />    [Car],[Bike]<br /><br />
     * Feature Columns [Waste]:
     <br />    [TotalGarbageT],[GeneralGarbageT],[HugeGarbageT],[RecycleGarbageT],[KitchenWasteT],[WastePerPersonKG]<br /><br />
     * Feature Columns [Penalty]:
     <br />    [PenaltyConstruction],[PenaltyPollution],[PenaltyMobilePollution]<br /><br />
     * Feature Columns [Auditory]:
     <br />    [ExamConstruction],[ExamPollution],[ExamMobliePollution]<br /> <br />
     
     ** Hint: AQI was converted with the Rscript titled "AQICoversion.R" under folder "code". Plus, One thing we should point out is we used "replaceChinese.csv" instead of "AllFeatures+Labelv4.csv" in our code, because "AllFeatures+Labelv4.csv" with non-English words would go haywire in display after re-download from Github. However, we still left both csv files for better comparison. Plus, for those who want to re-produce this model experiment, please switch and replace the data in the folder entitled "data" under this project folder with master-branch titled.  <br /><br />   
* Data preprocessing <br /><br />
  * Handle missing data <br />
     Fortunately, we got only about 10% data missing values, and all in the nearest features([Car] & [Bike]), we would just remove them from the data set. However, these missing values are city-oriented and time-bound, which means there would exist great bias upon predicting on these cities, for they missed half of the figures from a consecutive time interval from 2005-2010. <br /><br />
  * Scale value <br />
    Because the features are with diffrent units and the numbers varies in scale massively, one method we have here is value-standardization, for that can simply scale down the values to the same level and unify the units among these values. <br /><br />   
     
### Code

* We used [KNN], [Decision Tree], [Random Forest], three mdoels within our capabilities as we wanted to compare and optimize the performances. Meanwhile, since our data is city-oriented and time-bound, we tried to realize if they had great effect on prediction by examining the average performances amongst three dimensions: [By All-data],[By Cities],[By Months] as their titles. However, we applied cross-validation to all these three dimensions(Data split ratio: 70% training, 30% testing), and scored them at testing results by checking Precison, Recall, and F1-Score individually. <br /><br />
     Upon coding processing, we asked ourselves the following questions:<br /><br />
     - Given the data we had, the city-oriented and time-bound, would the models training under these two sub-conditions separately work        better than the model training with all data (Specified Model VS General Model)? <br /><br />
     - Which model([KNN], [Decision Tree], [Random Forest]) would performance better results over Precision, Recall, F1-Score? <br /><br />


### Results

* With different performance evaluation methods( Precision, Recall, F1-Score ), and three-dimension analysis( [By All-data],[By Cities],[By Months] ), we may conclude:  <br /><br />
     * Although General Model with all data outwieghed almost 90%  of the other models with data under three dimensions, original data set had serious unbalance distribution which happened even under three dimensions split as well. The result here arose that we might need to do further statistical tests to clarify this.<br /><br />
     * The challenging parts of our projects lied in: [Poor Feature Diversity],[Unbalance Data],[Open Data Integration],[Background Knowledge Limitation].<br /><br />

### References
* Car&Motorbike Statistic in Taiwan: [https://stat.thb.gov.tw/hb01/webMain.aspx?sys=100&funid=11100] <br /><br />
* Garbage Statistic in Taiwan: [https://erdb.epa.gov.tw/DataRepository/Statistics/TrashClearExecutiveProduce.aspx?topic1=%E5%9C%B0&topic2=%E6%B1%A1%E6%9F%93%E9%98%B2%E6%B2%BB&subject=%E5%BB%A2%E6%A3%84%E7%89%A9] <br /><br />
* Air-Pollution Penalty Cases Statisic in Taiwan: [https://erdb.epa.gov.tw/DataRepository/Statistics/StatEmsEemFineCnt.aspx?topic1=%E5%85%B6%E4%BB%96&topic2=%E7%92%B0%E4%BF%9D%E7%B5%B1%E8%A8%88&subject=%E6%B1%A1%E6%9F%93%E7%AE%A1%E5%88%B6] <br /><br />
* Air-Pollution Auditory Cases Statisic in Taiwan: [https://erdb.epa.gov.tw/DataRepository/Statistics/StatEmsEemCnt.aspx?topic1=%u5176%u4ed6&topic2=%u74b0%u4fdd%u7d71%u8a08&subject=%u6c61%u67d3%u7ba1%u5236] <br /><br />
* AQI Formula: [https://taqm.epa.gov.tw/taqm/tw/b0203.aspx] <br /><br />
* AQI Statistic in Taiwan: [https://erdb.epa.gov.tw/DataRepository/EnvMonitor/AirQualityMonitorMonData.aspx?topic1=%u5927%u6c23&topic2=%u74b0%u5883%u53ca%u751f%u614b%u76e3%u6e2c&subject=%u7a7a%u6c23%u54c1%u8cea] <br /><br />
