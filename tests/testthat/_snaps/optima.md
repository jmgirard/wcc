# print.wcc_optima produces expected console output

    Code
      print(res)
    Message
      
      -- WCC Optima Results ----------------------------------------------------------
      Total Windows Analyzed: 1
      Valid Optima Found: 1 (100%)
      Search Method: local
      Search Mode: Peaks (Maxima)
      Threshold Applied: None
      Local Search Size: 3
      Strict Monotonic: FALSE
      Showing the first 1 result:
    Output
         i optimum_lag optimum_value
       100           0           0.9

# summary.wcc_optima calculates and prints correctly

    Code
      summary(res)
    Message
      
      -- WCC Optima Summary ----------------------------------------------------------
      
      -- Completeness --
      
      * Total time windows: 2
      * Valid optima retained: 1 (50%)
      * Optima dropped (NA): 1 (50%)
      
      -- Lag Directionality (Leadership) --
      
      * Positive Lags (x leads y): 0 (0%)
      * Negative Lags (y leads x): 0 (0%)
      * Zero Lags (Simultaneous): 1 (100%)
      
      -- Optimum Value Distribution --
      
    Output
        0%  25%  50%  75% 100% 
       0.9  0.9  0.9  0.9  0.9 

# print.wdtw_optima produces expected console output

    Code
      print(res)
    Message
      -- WDTW Optima Results ---------------------------------------------------------
      Total Windows Analyzed: 1
      Valid Optima Found: 1 (100%)
      Search Method: global
      Search Mode: Valleys (Minima)
      Threshold Applied: None
      Showing the first 1 result:
    Output
         i optimum_lag optimum_value
       100           0           1.1

# summary.wdtw_optima calculates and prints correctly

    Code
      summary(res)
    Message
      
      -- WDTW Optima Summary ---------------------------------------------------------
      
      -- Completeness --
      
      * Total time windows: 1
      * Valid optima retained: 1 (100%)
      * Optima dropped (NA): 0 (0%)
      
      -- Lag Directionality (Leadership) --
      
      * Positive Lags (x leads y): 0 (0%)
      * Negative Lags (y leads x): 1 (100%)
      * Zero Lags (Simultaneous): 0 (0%)
      
      -- Optimum Value Distribution --
      
    Output
        0%  25%  50%  75% 100% 
       1.1  1.1  1.1  1.1  1.1 

