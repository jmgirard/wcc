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

# print.wcc_optima handles empty objects and row limits correctly

    Code
      print(res_large)
    Message
      
      -- WCC Optima Results ----------------------------------------------------------
      Total Windows Analyzed: 10
      Valid Optima Found: 10 (100%)
      Search Method: global
      Search Mode: Peaks (Maxima)
      Threshold Applied: None
      Showing the first 5 results:
    Output
       i optimum_lag optimum_value
       1           0           0.9
       2           0           0.9
       3           0           0.9
       4           0           0.9
       5           0           0.9
    Message
      # ... with 5 more rows

---

    Code
      print(res_empty)
    Message
      
      -- WCC Optima Results ----------------------------------------------------------
      Total Windows Analyzed: 0
      Valid Optima Found: 0 (NaN%)
      Search Method: global
      Search Mode: Peaks (Maxima)
      Threshold Applied: None
      i No optima found matching the criteria.

# summary methods gracefully handle objects with zero valid optima

    Code
      summary(res)
    Message
      
      -- WCC Optima Summary ----------------------------------------------------------
      
      -- Completeness --
      
      * Total time windows: 1
      * Valid optima retained: 0 (0%)
      * Optima dropped (NA): 1 (100%)

# print_optima handles exactly 1 remaining row for pluralization logic

    Code
      print(res_six)
    Message
      
      -- WCC Optima Results ----------------------------------------------------------
      Total Windows Analyzed: 6
      Valid Optima Found: 6 (100%)
      Search Method: global
      Search Mode: Peaks (Maxima)
      Threshold Applied: None
      Showing the first 5 results:
    Output
       i optimum_lag optimum_value
       1           0           0.9
       2           0           0.9
       3           0           0.9
       4           0           0.9
       5           0           0.9
    Message
      # ... with 1 more row

