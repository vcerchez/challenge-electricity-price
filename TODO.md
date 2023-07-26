## Model improvement:
1.    ~~use dedicated models for FR and DE~~
1.    ~~Drop useless cols `ID` and `DAY_ID`~~
1. Test score of single xgboost model with `min_child_weight = 10` (commit fc881812f34d03850896307d015e9ae13df80c8b).
1. Plot ordered Y and Y_hat orderd by Y to see where the model makes mistakes.
1. Make a single model for FR and DE but include `COUNTRY` var.
1. Missing vals imputation.

    The following vars have most of NAs and are corelated with other vars having no missing vals:
    
    * **DE_NET_EXPORT** -> DE_WINDPOW (0.76), FR_RESIDUAL_LOAD (0.49), DE_HYDRO (-0.48), DE_SOLAR (-0.48)
    * **FR_NET_EXPORT** -> FR_GAS (-0.51), DE_COAL (-0.39)
    * **DE_WIND** -> DE_RESIDUAL_LOAD (-0.64), FR_WINDPOW (0.57)

    First draw scaterplots. Then fit imputers with OLS using test set to verify imputation model quality and then trining on the train+test sets.

1.    ~~Regularize~~
1.    ~~Try GAM~~
1.    ~~Try tree based models~~
1.    Reverse date anonymization