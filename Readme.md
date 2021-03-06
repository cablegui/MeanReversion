Mean Reversion testing
========================================================

The code and data has been put up on my github account for easy access to public.


### Following sessionInfo used for this test

```
R version 3.0.3 (2014-03-06)
Platform: i386-w64-mingw32/i386 (32-bit)

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] repmis_0.3

loaded via a namespace (and not attached):
 [1] digest_0.6.4      httr_0.3          plyr_1.8.1        R.cache_0.10.0    R.methodsS3_1.6.1 R.oo_1.18.0      
 [7] R.utils_1.33.0    Rcpp_0.11.1       RCurl_1.95-4.1    rJava_0.9-6       stringr_0.6.2     tools_3.0.3      
[13] xlsx_0.5.5        xlsxjars_0.6.0   
```

### Explanation of the code

#### Data Gathering

Data for Nymex futures is stored in https://raw.githubusercontent.com/cablegui/MeanReversion/master/data/nym_ngatimeseries.csv
  
This is downloaded using the repmis package function source_data developed by Christopher Gandrud.

#### Data cleaning

- Data which has duplicate dates is removed.
- NA's are replaced with the t-1 data

#### Jump event detection for nymex 31 day

- Jump events represented as 3 times the standard deviation of the daily log returns
- Time series where the log returns are less than the 3 std mark are retained

#### Augmented dickey fuller test

- Run the adf.test function on the resulting time series
- RUn the adf.test function on the log of the time series

Results show that the p-value of slightly greater than 0.05 means that the data is slightly stationary
